package com.bencodez.votingplugin.backendproxy.transport;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.servercomm.sockets.SocketServer;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class SocketBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private ClientHandler clientHandler;
	@Getter
	private SocketHandler socketHandler;
	private GlobalMessageHandler messageHandler;
	private EncryptionHandler encryptionHandler;
	private String bungeeHost;
	private int bungeePort;
	private String spigotHost;
	private int spigotPort;
	private boolean debug;

	public SocketBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		this.messageHandler = messageHandler;
		encryptionHandler = new EncryptionHandler(plugin.getName(),
				new File(plugin.getDataFolder(), "secretkey.key"));
		bungeeHost = plugin.getBungeeSettings().getBungeeServerHost();
		bungeePort = plugin.getBungeeSettings().getBungeeServerPort();
		spigotHost = plugin.getBungeeSettings().getSpigotServerHost();
		spigotPort = plugin.getBungeeSettings().getSpigotServerPort();
		debug = plugin.getBungeeSettings().isBungeeDebug();
		startConnections();
	}

	private void startConnections() {
		clientHandler = new ClientHandler(bungeeHost, bungeePort, encryptionHandler, debug);
		try {
			verifyListenerPortAvailable();
			socketHandler = new SocketHandler("vp-socket", spigotHost, spigotPort, encryptionHandler, debug) {
				@Override
				public void log(String str) {
					plugin.getLogger().info(str);
				}
			};
			socketHandler.add(new SocketReceiver() {
				@Override
				public void onReceiveEnvelope(JsonEnvelope envelope) {
					messageHandler.onMessage(envelope);
				}
			});
		} catch (RuntimeException failure) {
			clientHandler.stopConnection();
			clientHandler = null;
			throw failure;
		}
	}

	/**
	 * SocketHandler logs and closes itself when its constructor cannot bind. Probe
	 * first so replacement validation cannot mistake that swallowed failure for a
	 * live listener.
	 */
	private void verifyListenerPortAvailable() {
		try (ServerSocket probe = new ServerSocket()) {
			probe.setReuseAddress(false);
			probe.bind(new InetSocketAddress(spigotHost, spigotPort));
		} catch (IOException unavailable) {
			throw new IllegalStateException("Socket backend proxy listener is unavailable at " + spigotHost + ":"
					+ spigotPort, unavailable);
		}
	}

	@Override
	public boolean send(JsonEnvelope envelope) {
		if (clientHandler != null) {
			clientHandler.sendEnvelope(envelope);
			return true;
		}
		return false;
	}

	@Override
	public void validate() {
		if (clientHandler == null || socketHandler == null) {
			throw new IllegalStateException("Socket backend proxy transport initialization failed");
		}
	}

	@Override
	public void close() {
		try {
			closeSocketListener();
		} finally {
			if (clientHandler != null) {
				clientHandler.stopConnection();
				clientHandler = null;
			}
		}
	}

	private void closeSocketListener() {
		SocketHandler closing = socketHandler;
		socketHandler = null;
		if (closing == null) return;
		SocketServer server = closing.getServer();
		closing.closeConnection();
		if (server == null) return;
		try {
			server.join(1000L);
			if (server.isAlive()) {
				throw new IllegalStateException("Socket backend proxy listener did not stop before replacement");
			}
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted while retiring the socket backend proxy listener", interrupted);
		} finally {
			// SocketServer's accept-error recovery can race close() and re-bind once
			// before it observes its stopped flag. Always close the final socket, even
			// after a timeout or interruption leaves the worker incomplete.
			server.close();
		}
	}

	public void restoreAfterFailedReplacement() {
		if (messageHandler == null || encryptionHandler == null) {
			throw new IllegalStateException("Socket backend proxy transport cannot be restored before startup");
		}
		startConnections();
	}
}
