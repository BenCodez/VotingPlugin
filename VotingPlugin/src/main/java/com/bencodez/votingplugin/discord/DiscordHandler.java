package com.bencodez.votingplugin.discord;

import java.awt.Color;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectConfigurationSection;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserStartup;
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
import lombok.Getter;

public class DiscordHandler {

	private final VotingPluginMain plugin;
	@Getter
	private final HashMap<TopVoter, Long> topVoterMessageIds = new HashMap<TopVoter, Long>();
	private final AtomicBoolean discordReady = new AtomicBoolean(false);

	public DiscordHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		for (TopVoter top : TopVoter.values()) {
			long savedId = plugin.getServerData().getTopVoterMessageId(top);
			topVoterMessageIds.put(top, savedId);
		}
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
					String msg = PlaceholderUtils
							.replacePlaceHolder(section.getString("Message", "Please set a message"), placeholders);
					msg = PlaceholderUtils.replacePlaceHolders(user.getOfflinePlayer(), msg);
					sendRewardMessage(msg, section.getString("ChannelID", "0"));
				}
				return null;
			}
		});

		plugin.addUserStartup(new UserStartup() {

			@Override
			public void onStartUp(AdvancedCoreUser user) {

			}

			@Override
			public void onStart() {

			}

			@Override
			public void onPostFinish() {

				plugin.getTimer().scheduleAtFixedRate(new Runnable() {

					@Override
					public void run() {
						updateDiscordLeaderboard();
					}
				}, 130, 60 * 60, TimeUnit.SECONDS); // 10-minute interval
			}

			@Override
			public void onFinish() {

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

		for (TopVoter top : TopVoter.values()) {
			if (plugin.getConfigFile().isDiscordSRVTopVoterEnabled(top)) {
				plugin.debug("Updating Discord Top Voter: " + top.toString());
				updateTopVoterMessageId(top);
			}
		}
	}

	public void updateTopVoterMessageId(TopVoter top) {
		plugin.extraDebug("Updating Discord Top Voter message for: " + top);

		long channelId = plugin.getConfigFile().getDiscordSRVTopVoterChannel(top);
		LinkedHashMap<TopVoterPlayer, Integer> topVoters = plugin.getTopVoter(top);

		String title = plugin.getConfigFile().getDiscordSRVTopVoterTitle(top);
		boolean newMessage = plugin.getConfigFile().isDiscordSRVTopVoterNewMessageOnUpdate(top);

		EmbedBuilder eb = new EmbedBuilder().setTitle(title).setColor(Color.CYAN).setTimestamp(Instant.now());

		if (topVoters == null || topVoters.isEmpty()) {
			eb.setDescription("No votes recorded yet.");
		} else {
			StringBuilder sb = new StringBuilder();
			sb.append("```");
			int rank = 1;

			for (Entry<TopVoterPlayer, Integer> entry : topVoters.entrySet()) {
				if (rank > 10)
					break;

				String prefix = rank <= 3 ? new String[] { "🥇", "🥈", "🥉" }[rank - 1] : rank + ".";

				String player = entry.getKey().getPlayerName();
				int votes = entry.getValue();

				// simple alignment: trim long names, pad to 16 chars
				if (player.length() > 16)
					player = player.substring(0, 16);

				sb.append(String.format("%-3s %-16s %4d votes%n", prefix, player, votes));
				rank++;
			}
			sb.append("```");

			eb.setDescription(sb.toString()).setFooter("Top 10 • Updated");
		}

		TextChannel channel = DiscordUtil.getJda().getTextChannelById(channelId);
		if (channel == null) {
			plugin.getLogger().warning("Discord channel ID " + channelId + " not found: " + top);
			return;
		}

		long existingId = topVoterMessageIds.getOrDefault(top, 0L);

		if (existingId <= 0 || newMessage) {
			channel.sendMessageEmbeds(eb.build()).queue(msg -> {
				long newId = msg.getIdLong();
				topVoterMessageIds.put(top, newId);
				if (!newMessage)
					plugin.getServerData().setTopVoterMessageId(top, newId);
				plugin.getLogger().info("Posted new Top Voters " + top + " (ID: " + newId + ")");
			}, err -> plugin.getLogger().warning("Error sending Top Voters " + top + ": " + err.getMessage()));
		} else {
			channel.editMessageEmbedsById(existingId, eb.build()).queue(
					m -> plugin.getLogger().info("Edited Top Voters " + top + " (ID: " + existingId + ")"),
					err -> plugin.getLogger().warning("Error editing Top Voters " + top + ": " + err.getMessage()));
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
