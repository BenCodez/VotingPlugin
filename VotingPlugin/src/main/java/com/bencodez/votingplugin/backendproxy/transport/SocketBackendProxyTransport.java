package com.bencodez.votingplugin.backendproxy.transport;

import java.io.File;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class SocketBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private ClientHandler clientHandler;
	@Getter
	private SocketHandler socketHandler;

	public SocketBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		EncryptionHandler encryptionHandler = new EncryptionHandler(plugin.getName(),
				new File(plugin.getDataFolder(), "secretkey.key"));
		clientHandler = new ClientHandler(plugin.getBungeeSettings().getBungeeServerHost(),
				plugin.getBungeeSettings().getBungeeServerPort(), encryptionHandler,
				plugin.getBungeeSettings().isBungeeDebug());
		socketHandler = new SocketHandler("vp-socket", plugin.getBungeeSettings().getSpigotServerHost(),
				plugin.getBungeeSettings().getSpigotServerPort(), encryptionHandler,
				plugin.getBungeeSettings().isBungeeDebug()) {
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
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (clientHandler != null) {
			clientHandler.sendEnvelope(envelope);
		}
	}

	@Override
	public void validate() {
		if (clientHandler == null || socketHandler == null) {
			throw new IllegalStateException("Socket backend proxy transport initialization failed");
		}
	}

	@Override
	public void close() {
		if (socketHandler != null) {
			socketHandler.closeConnection();
			socketHandler = null;
		}
		if (clientHandler != null) {
			clientHandler.stopConnection();
			clientHandler = null;
		}
	}
}
