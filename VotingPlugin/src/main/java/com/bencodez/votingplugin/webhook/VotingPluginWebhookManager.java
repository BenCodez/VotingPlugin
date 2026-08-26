package com.bencodez.votingplugin.webhook;

import com.bencodez.votingplugin.VotingPluginMain;

/** Owns VotingPlugin webhook startup, reload, and shutdown lifecycle. */
public final class VotingPluginWebhookManager {

	private final VotingPluginMain plugin;
	private VotingPluginWebhooks webhooks;

	public VotingPluginWebhookManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void load() {
		if (webhooks == null) {
			webhooks = new VotingPluginWebhooks(plugin);
		}
		reload();
	}

	public void reload() {
		if (webhooks != null) {
			webhooks.reload(plugin.getConfig().getConfigurationSection("Webhooks"));
		}
	}

	public void shutdown() {
		if (webhooks != null) {
			webhooks.shutdown();
			webhooks = null;
		}
	}

	public VotingPluginWebhooks getWebhooks() {
		return webhooks;
	}
}
