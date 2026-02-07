package com.bencodez.votingplugin.webhook;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.plugin.java.JavaPlugin;

/**
 * Convenience wrapper to manage webhook lifecycle within VotingPlugin.
 */
public final class VotingPluginWebhooks {

	private final WebhookService service;
	private volatile Map<String, WebhookDefinition> definitions = Collections.emptyMap();

	/**
	 * Creates a webhook manager.
	 *
	 * @param plugin plugin instance
	 */
	public VotingPluginWebhooks(JavaPlugin plugin) {
		Objects.requireNonNull(plugin, "plugin");
		this.service = new WebhookService(msg -> plugin.getLogger().warning(msg));
	}

	/**
	 * Reloads webhook definitions from config and ensures the service is started.
	 *
	 * @param webhooksRoot root section (usually config.getConfigurationSection("Webhooks"))
	 */
	public void reload(ConfigurationSection webhooksRoot) {
		Map<String, WebhookDefinition> defs = WebhookConfigLoader.load(webhooksRoot);
		this.definitions = defs;
		service.setDefinitions(defs);
		service.start();
	}

	/**
	 * Stops the webhook service.
	 */
	public void shutdown() {
		service.stop();
	}

	/**
	 * Creates a reward executor using the current definitions.
	 *
	 * @return executor
	 */
	public WebhookRewardExecutor createExecutor() {
		return new WebhookRewardExecutor(service, definitions);
	}
}
