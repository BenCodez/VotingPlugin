package com.bencodez.votingplugin.backendproxy.transport;

import java.io.File;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;

public class PluginMessagingBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	private GlobalMessageHandler messageHandler;

	public PluginMessagingBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		this.messageHandler = messageHandler;
		plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());
		EncryptionHandler encryptionHandler = null;
		if (plugin.getBungeeSettings().isPluginMessageEncryption()) {
			encryptionHandler = new EncryptionHandler(plugin.getName(),
					new File(plugin.getDataFolder(), "secretkey.key"));
		}
		plugin.getPluginMessaging().setEncryptionHandler(encryptionHandler);
		plugin.getPluginMessaging().setDebug(plugin.getBungeeSettings().isBungeeDebug());
		plugin.activateBackendPluginMessageHandler(messageHandler);
	}

	@Override
	public void send(JsonEnvelope envelope) {
		plugin.getPluginMessaging().sendEnvelope(envelope);
	}

	@Override
	public void close() {
		if (messageHandler != null) {
			plugin.deactivateBackendPluginMessageHandler(messageHandler);
			messageHandler = null;
		}
	}
}
