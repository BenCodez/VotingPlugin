package com.bencodez.votingplugin.rewards.builtin;

import java.util.HashMap;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.votingplugin.VotingPluginMain;

/** VotingPlugin's injected vote progress boss-bar reward. */
public class RewardVoteBossBar extends RewardInjectConfigurationSection {

	private final VotingPluginMain plugin;

	public RewardVoteBossBar(VotingPluginMain plugin) {
		super("VoteBossBar");
		this.plugin = plugin;
	}

	@Override
	public String onRewardRequested(Reward reward, com.bencodez.advancedcore.api.user.AdvancedCoreUser user,
			ConfigurationSection section, HashMap<String, String> placeholders) {
		if (section.getBoolean("Enabled")) {
			user.sendBossBar(PlaceholderUtils.replacePlaceHolder(section.getString("Message", ""), placeholders),
					section.getString("Color", "BLUE"), section.getString("Style", "SOLID"),
					(double) plugin.getVotingPluginUserManager().getVotingPluginUser(user).getSitesVotedOn()
							/ plugin.getVoteSites().size(),
					section.getInt("Delay", 30));
		}
		return null;
	}
}
