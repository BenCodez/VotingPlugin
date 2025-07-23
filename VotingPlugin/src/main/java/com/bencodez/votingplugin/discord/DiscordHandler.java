package com.bencodez.votingplugin.discord;

import java.awt.Color;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

import github.scarsz.discordsrv.DiscordSRV;
import github.scarsz.discordsrv.api.Subscribe;
import github.scarsz.discordsrv.api.events.DiscordReadyEvent;
import github.scarsz.discordsrv.dependencies.jda.api.EmbedBuilder;
import github.scarsz.discordsrv.dependencies.jda.api.JDA;
import github.scarsz.discordsrv.dependencies.jda.api.entities.TextChannel;
import github.scarsz.discordsrv.util.DiscordUtil;

public class DiscordHandler {

	private final VotingPluginMain plugin;
	private final AtomicLong topVoterMessageId = new AtomicLong(-1);
	private final AtomicBoolean discordReady = new AtomicBoolean(false);

	public DiscordHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		long savedId = plugin.getServerData().getTopVoterMessageId();
		topVoterMessageId.set(savedId);
	}

	/** Call this early, e.g. from onEnable() in main plugin class */
	public void load() {
		DiscordSRV.api.subscribe(this);
		plugin.getLogger().info("DiscordHandler loaded; awaiting Discord readiness.");

		// if DiscordSRV is already up and running, mark it ready now
		if (DiscordUtil.getJda().getStatus() == JDA.Status.CONNECTED) {
			discordReady.set(true);
			plugin.getLogger().info("DiscordSRV was already ready; manual update now available.");
		}

		plugin.getRewardHandler().addInjectedReward(new RewardInjectConfigurationSection("DiscordSRV") {

			@Override
			public String onRewardRequested(Reward arg0, com.bencodez.advancedcore.api.user.AdvancedCoreUser user,
					ConfigurationSection section, HashMap<String, String> placeholders) {
				if (section.getBoolean("Enabled")) {
					sendRewardMessage(PlaceholderUtils
							.replacePlaceHolder(section.getString("Message", "Please set a message"), placeholders),
							section.getString("ChannelID", "0"));
				}
				return null;
			}
		});
	}

	/** Fired once JDA is fully initialized */
	@Subscribe
	public void onDiscordReady(DiscordReadyEvent event) {
		discordReady.set(true);
		plugin.getLogger().info("DiscordSRV is ready; manual update now available.");
	}

	/**
	 * Public method to update the Top Voter message on demand. Checks Discord
	 * readiness internally.
	 */
	public void updateDiscordLeaderboard() {
		if (!discordReady.get()) {
			plugin.getLogger().warning("DiscordSRV not ready yet; cannot update Top Voters.");
			return;
		}

		long channelId = plugin.getConfigFile().getDiscordSRVTopVoterChannel();
		LinkedHashMap<TopVoterPlayer, Integer> topVoters = plugin.getTopVoter(TopVoter.Monthly);

		EmbedBuilder eb = new EmbedBuilder().setTitle("Top Voters This Month").setColor(Color.CYAN)
				.setTimestamp(Instant.now());

		int rank = 1;
		for (Entry<TopVoterPlayer, Integer> entry : topVoters.entrySet()) {
			if (rank > 10)
				break;
			eb.addField("#" + rank++ + " • " + entry.getKey().getPlayerName(), entry.getValue() + " votes", false);
		}

		TextChannel channel = DiscordUtil.getJda().getTextChannelById(channelId);
		if (channel == null) {
			plugin.getLogger().warning("Discord channel ID " + channelId + " not found.");
			return;
		}

		if (topVoterMessageId.get() <= 0) {
			channel.sendMessageEmbeds(eb.build()).queue(msg -> {
				long newId = msg.getIdLong();
				topVoterMessageId.set(newId);
				plugin.getServerData().setTopVoterMessageId(newId);
				plugin.getLogger().info("Posted new Top Voters (ID: " + newId + ")");
			}, err -> plugin.getLogger().warning("Error sending Top Voters: " + err.getMessage()));
		} else {
			channel.editMessageEmbedsById(topVoterMessageId.get(), eb.build()).queue(
					m -> plugin.getLogger().info("Edited Top Voters (ID: " + topVoterMessageId.get() + ")"),
					err -> plugin.getLogger().warning("Error editing Top Voters: " + err.getMessage()));
		}
	}

	public void sendRewardMessage(String message, String channelId) {
		TextChannel channel = DiscordUtil.getJda().getTextChannelById(channelId);
		if (channel != null) {
			channel.sendMessage(message).queue();
		} else {
			plugin.getLogger().warning("Discord channel ID " + channelId + " not found for reward message.");
		}
	}
}
