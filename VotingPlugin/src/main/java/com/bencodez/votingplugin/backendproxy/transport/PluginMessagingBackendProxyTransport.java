package com.bencodez.votingplugin.backendproxy.transport;

import java.io.File;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;

public class PluginMessagingBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	private GlobalMessageHandler messageHandler;
	private String channel;
	private EncryptionHandler encryptionHandler;
	private boolean debug;
	private boolean active;

	public PluginMessagingBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		this.messageHandler = messageHandler;
		channel = plugin.getBungeeSettings().getPluginMessagingChannel();
		encryptionHandler = null;
		if (plugin.getBungeeSettings().isPluginMessageEncryption()) {
			encryptionHandler = new EncryptionHandler(plugin.getName(),
					new File(plugin.getDataFolder(), "secretkey.key"));
		}
		debug = plugin.getBungeeSettings().isBungeeDebug();
	}

	@Override
	public void activateAfterPublication() {
		if (messageHandler != null && !active) {
			publishSharedState();
			active = true;
		}
	}

	void restoreAfterFailedReplacement() {
		if (messageHandler != null) {
			publishSharedState();
			active = true;
		}
	}

	private void publishSharedState() {
		if (plugin.getPluginMessaging() == null || !channel.equals(plugin.getBungeeChannel())) {
			plugin.registerBungeeChannels(channel);
		}
		plugin.getPluginMessaging().setEncryptionHandler(encryptionHandler);
		plugin.getPluginMessaging().setDebug(debug);
		plugin.activateBackendPluginMessageHandler(messageHandler);
	}

	@Override
	public void send(JsonEnvelope envelope) {
		plugin.getPluginMessaging().sendEnvelope(envelope);
	}

	@Override
	public void close() {
		if (messageHandler != null && active) {
			plugin.deactivateBackendPluginMessageHandler(messageHandler);
		}
		active = false;
		messageHandler = null;
		channel = null;
		encryptionHandler = null;
	}
}
