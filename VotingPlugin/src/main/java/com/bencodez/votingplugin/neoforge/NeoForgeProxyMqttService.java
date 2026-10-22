package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import java.util.function.BiConsumer;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** MQTT network work and reconnects stay outside the native server event thread. */
final class NeoForgeProxyMqttService implements NeoForgeProxyTransport {
    interface Connection extends NeoForgeProxyBrokerService.Connection { }
    interface ConnectionFactory extends NeoForgeProxyBrokerService.ConnectionFactory {
        @Override Connection open() throws Exception;
    }
    private final NeoForgeProxyBrokerService broker;

    static NeoForgeProxyMqttService start(Path directory, NeoForgeProxyTransportConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players) throws IOException {
        var authenticator = SharedTransportEnvelopeAuthenticator.load(directory.resolve("secretkey.key"),
                configuration.sharedAuthentication());
        var encryption = TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, configuration.communicationEncryption());
        var settings = configuration.mqtt();
        return new NeoForgeProxyMqttService(configuration, processor, players, authenticator, encryption,
                () -> {
                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setConnectionTimeout(5);
                    options.setKeepAliveInterval(30);
                    options.setMaxInflight(32);
                    if (!settings.username().isEmpty()) options.setUserName(settings.username());
                    if (!settings.password().isEmpty()) options.setPassword(settings.password().toCharArray());
                    MqttServerComm client = new MqttServerComm(settings.clientId(), settings.brokerUrl(), options);
                    return new Connection() {
                        @Override public void subscribe(String topic, BiConsumer<String, byte[]> receiver)
                                throws Exception {
                            client.subscribe(topic, 2, (received, message) -> receiver.accept(received, message.getPayload()));
                        }
                        @Override public void publish(String topic, JsonEnvelope envelope) throws Exception {
                            client.publishEnvelope(topic, envelope, 2, false);
                        }
                        @Override public void close() throws Exception { client.close(); }
                    };
                }, true);
    }

    NeoForgeProxyMqttService(NeoForgeProxyTransportConfiguration configuration, NeoForgeVoteProcessor processor,
            NeoForgePlayerDirectory players, SharedTransportEnvelopeAuthenticator authenticator,
            TransportEnvelopeEncryption encryption, ConnectionFactory factory, boolean scheduleHeartbeat) {
        Objects.requireNonNull(configuration, "configuration");
        if (configuration.method() != NeoForgeProxyTransportConfiguration.Method.MQTT) {
            throw new IllegalArgumentException("MQTT settings required");
        }
        broker = new NeoForgeProxyBrokerService(configuration, processor, players,
                authenticator, encryption, factory, scheduleHeartbeat);
    }

    void receive(String topic, byte[] payload) { broker.receive(topic, payload); }
    @Override public void playerOnline(SharedVoteIdentity identity) { broker.playerOnline(identity); }
    @Override public void playerOffline(SharedVoteIdentity identity) { broker.playerOffline(identity); }
    @Override public void close() { broker.close(); }
}
