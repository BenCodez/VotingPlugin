package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.Logger;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.servercomm.sockets.SocketServer;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** NeoForge receiver for the existing encrypted VotingPlugin SOCKETS proxy transport. */
public final class NeoForgeProxySocketService implements NeoForgeProxyTransport {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeProxySocketService.class.getName());
    private final NeoForgeProxySocketConfiguration configuration;
    private final SharedTransportEnvelopeAuthenticator authenticator;
    private final TransportEnvelopeEncryption encryption;
    private final NeoForgeProxyMessageService messages;

    static NeoForgeProxySocketService start(Path directory, NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players) {
        Objects.requireNonNull(directory, "directory");
        EncryptionHandler encryption = new EncryptionHandler("VotingPlugin",
                directory.resolve("secretkey.key").toFile());
        SharedTransportEnvelopeAuthenticator authenticator;
        TransportEnvelopeEncryption transportEncryption;
        try {
            Path root = directory.toAbsolutePath().normalize();
            Path keyFile = root.resolve(configuration.authenticationKeyFile()).normalize();
            if (!keyFile.startsWith(root)) throw new IOException("Socket key outside data directory");
            authenticator = SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED);
            transportEncryption = TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                    TransportEnvelopeEncryption.Domain.PROXY_BACKEND,
                    configuration.communicationEncryption());
        } catch (IOException unavailable) {
            throw new IllegalStateException("Socket authentication unavailable", unavailable);
        }
        verifyListenerPortAvailable(configuration.backendHost(), configuration.backendPort());
        ClientHandler client = new ClientHandler(configuration.proxyHost(), configuration.proxyPort(),
                encryption, configuration.debug());
        SocketHandler listener;
        try {
            listener = new SocketHandler("vp-neoforge-socket", configuration.backendHost(),
                    configuration.backendPort(), encryption, configuration.debug()) {
                @Override public void log(String message) { LOGGER.info(message); }
            };
        } catch (RuntimeException failure) {
            client.stopConnection();
            throw failure;
        }
        AtomicReference<NeoForgeProxySocketService> service = new AtomicReference<>();
        listener.add(new SocketReceiver() {
            @Override public void onReceiveEnvelope(JsonEnvelope envelope) {
                NeoForgeProxySocketService active = service.get();
                if (active != null) active.receive(envelope);
            }
        });
        Runnable close = () -> closeTransport(listener, client);
        try {
            NeoForgeProxySocketService active = new NeoForgeProxySocketService(configuration, processor, players,
                    client::sendEnvelope, authenticator, transportEncryption, close, true);
            service.set(active);
            return active;
        } catch (RuntimeException failure) {
            close.run();
            throw failure;
        }
    }

    NeoForgeProxySocketService(NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players,
            Consumer<JsonEnvelope> outbound, SharedTransportEnvelopeAuthenticator authenticator) {
        this(configuration, processor, players, outbound, authenticator,
                TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND),
                () -> { }, false);
    }

    NeoForgeProxySocketService(NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players,
            Consumer<JsonEnvelope> outbound, SharedTransportEnvelopeAuthenticator authenticator,
            TransportEnvelopeEncryption encryption, Runnable transportClose, boolean scheduleHeartbeat) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        this.authenticator = Objects.requireNonNull(authenticator, "authenticator");
        this.encryption = Objects.requireNonNull(encryption, "encryption");
        Objects.requireNonNull(outbound, "outbound");
        messages = new NeoForgeProxyMessageService(configuration.server(), processor, players,
                envelope -> outbound.accept(authenticator.sign(encryption.encrypt(supportedCapabilities(envelope)),
                        Domain.SOCKET_PROXY_BACKEND, configuration.server(), configuration.proxyName())),
                transportClose, scheduleHeartbeat);
    }

    public void playerOnline(SharedVoteIdentity identity) { messages.playerOnline(identity); }

    public void playerOffline(SharedVoteIdentity identity) { messages.playerOffline(identity); }

    void receive(JsonEnvelope envelope) {
        if (!messages.isOpen() || envelope == null) return;
        String sender = envelope.getFields().get(SharedTransportEnvelopeAuthenticator.K_SENDER);
        SharedTransportEnvelopeAuthenticator.Verification verification = authenticator.verify(envelope,
                Domain.SOCKET_PROXY_BACKEND, configuration.server());
        if (!verification.accepted() || !configuration.proxyName().equals(sender)) {
            LOGGER.warning("Rejected unauthenticated socket message");
            return;
        }
        TransportEnvelopeEncryption.Decryption decryption = encryption.decrypt(verification.envelope());
        if (!decryption.accepted()) {
            LOGGER.warning("Rejected invalid encrypted socket message");
            return;
        }
        messages.receive(decryption.envelope());
    }

    @Override public void close() { messages.close(); }

    private static JsonEnvelope supportedCapabilities(JsonEnvelope envelope) {
        if (!switch (envelope.getSubChannel()) {
            case VotingPluginWire.SUB_STATUS_OKAY, VotingPluginWire.SUB_BACKEND_STARTED,
                    VotingPluginWire.SUB_BACKEND_HEARTBEAT -> true;
            default -> false;
        }) return envelope;
        JsonEnvelope authenticated = VotingPluginWire.authenticatedSocketVoteDeliveryCapability(envelope);
        JsonEnvelope.Builder builder = JsonEnvelope.builder(authenticated.getSubChannel())
                .schema(authenticated.getSchema());
        authenticated.getFields().forEach((key, value) -> {
            if (!VotingPluginWire.K_VOTE_DELAY_REJECTION_ACK_VERSION.equals(key)) builder.put(key, value);
        });
        return builder.build();
    }

    private static void verifyListenerPortAvailable(String host, int port) {
        try (ServerSocket probe = new ServerSocket()) {
            probe.setReuseAddress(false);
            probe.bind(new InetSocketAddress(host, port));
        } catch (IOException unavailable) {
            throw new IllegalStateException("Proxy listener unavailable at " + host + ":" + port,
                    unavailable);
        }
    }

    static void closeTransport(SocketHandler listener, ClientHandler client) {
        SocketServer server = listener.getServer();
        RuntimeException failure = null;
        try {
            listener.closeConnection();
        } catch (RuntimeException closeFailure) {
            failure = closeFailure;
        }
        if (server != null) {
            try {
                server.join(1_000L);
                if (server.isAlive()) LOGGER.warning("Socket stop timed out; forcing close");
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                LOGGER.warning("Socket stop interrupted; forcing close");
            } finally {
                try {
                    server.close();
                } catch (RuntimeException closeFailure) {
                    failure = appendFailure(failure, closeFailure);
                }
            }
        }
        try {
            client.stopConnection();
        } catch (RuntimeException closeFailure) {
            failure = appendFailure(failure, closeFailure);
        }
        if (failure != null) throw failure;
    }

    private static RuntimeException appendFailure(RuntimeException current, RuntimeException added) {
        if (current == null) return added;
        current.addSuppressed(added);
        return current;
    }

}
