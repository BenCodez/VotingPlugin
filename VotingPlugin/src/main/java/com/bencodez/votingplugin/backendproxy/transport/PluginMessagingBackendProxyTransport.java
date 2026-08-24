package com.bencodez.votingplugin.backendproxy.transport;

import java.io.File;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;

public class PluginMessagingBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;

	public PluginMessagingBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		plugin.registerBungeeChannels(plugin.getBungeeSettings().getPluginMessagingChannel());
		if (plugin.getBungeeSettings().isPluginMessageEncryption()) {
			EncryptionHandler encryptionHandler = new EncryptionHandler(plugin.getName(),
					new File(plugin.getDataFolder(), "secretkey.key"));
			plugin.getPluginMessaging().setEncryptionHandler(encryptionHandler);
		}
		plugin.getPluginMessaging().setDebug(plugin.getBungeeSettings().isBungeeDebug());
		plugin.getPluginMessaging().add(new PluginMessageHandler() {
			@Override
			public void onReceive(JsonEnvelope envelope) {
				messageHandler.onMessage(envelope);
			}
		});
	}

	@Override
	public void send(JsonEnvelope envelope) {
		plugin.getPluginMessaging().sendEnvelope(envelope);
	}

	@Override
	public void close() {
		// Plugin messaging is owned by the plugin lifecycle.
	}
}
