package com.bencodez.votingplugin.rewards.builtin;

import java.util.HashMap;
import java.util.List;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.webhook.WebhookRewardEntry;
import com.bencodez.votingplugin.webhook.WebhookRewardParser;

/** VotingPlugin's injected webhook reward. */
public class RewardWebhook extends RewardInjectConfigurationSection {

	private final VotingPluginMain plugin;

	public RewardWebhook(VotingPluginMain plugin) {
		super("WebhookReward");
		this.plugin = plugin;
	}

	@Override
	public String onRewardRequested(Reward reward, com.bencodez.advancedcore.api.user.AdvancedCoreUser user,
			ConfigurationSection section, HashMap<String, String> placeholders) {
		if (section == null || plugin.getWebhooks() == null) {
			return null;
		}
		List<WebhookRewardEntry> entries = WebhookRewardParser.parse(section);
		if (entries == null || entries.isEmpty()) {
			return null;
		}
		plugin.getWebhooks().createExecutor().executeAll(entries,
				input -> PlaceholderUtils.replacePlaceHolder(input, placeholders));
		return null;
	}
}
