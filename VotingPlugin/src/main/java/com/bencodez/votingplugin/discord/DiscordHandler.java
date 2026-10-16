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
import github.scarsz.discordsrv.dependencies.jda.api.requests.ErrorResponse;
import github.scarsz.discordsrv.dependencies.jda.api.exceptions.ErrorResponseException;
import github.scarsz.discordsrv.util.DiscordUtil;
import lombok.Getter;

/**
 * Handler for Discord integration.
 */
public class DiscordHandler {

	private final VotingPluginMain plugin;
	@Getter
	private final HashMap<TopVoter, Long> topVoterMessageIds = new HashMap<TopVoter, Long>();
	/** Identity, rather than generation alone, owns every request and its callbacks. */
	private static final class Publication {
		final long generation;
		final boolean persistent;
		boolean sending, recovering, again, completing;
		Publication(long generation, boolean persistent, boolean sending) {
			this.generation = generation; this.persistent = persistent; this.sending = sending;
		}
	}
	private final java.util.EnumMap<TopVoter, Publication> publications = new java.util.EnumMap<>(TopVoter.class);
	private final AtomicBoolean discordReady = new AtomicBoolean(false);
	private static final class DisplayUpdate {
		private long generation;
		private boolean again;
		private DisplayUpdate(long generation) { this.generation = generation; }
	}
	private final java.util.EnumMap<TopVoter, DisplayUpdate> preparing = new java.util.EnumMap<>(TopVoter.class);

	/**
	 * Constructs a new Discord handler.
	 *
	 * @param plugin the plugin instance
	 */
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

	/**
	 * Fired once JDA is fully initialized.
	 *
	 * @param event the Discord ready event
	 */
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

	/**
	 * Updates the top voter message ID for the specified top voter.
	 *
	 * @param top the top voter
	 */
	public void updateTopVoterMessageId(TopVoter top) {
		DisplayUpdate update;
		synchronized (preparing) {
			DisplayUpdate pending = preparing.get(top);
			if (pending != null) {
				pending.again = true;
				pending.generation = plugin.getTopVoterDisplayGeneration();
				return;
			}
			update = new DisplayUpdate(plugin.getTopVoterDisplayGeneration());
			preparing.put(top, update);
		}
		try {
			plugin.getTopVoterDisplayAsync(top).whenComplete((snapshot, failure) -> {
				try {
					plugin.getTimer().execute(() -> {
						boolean again;
						synchronized (preparing) {
							if (preparing.get(top) != update) return;
							preparing.remove(top);
							again = update.again;
							if (!plugin.isEnabled() || update.generation != plugin.getTopVoterDisplayGeneration()) return;
						}
						// Joins and TTL/scope changes can revoke omissions while publication is queued.
						// Recheck current evidence without opening another asynchronous deadline.
						if (again || (failure == null && snapshot != null))
							publishTopVoterMessage(top, plugin.getTopVoterDisplay(top));
						else plugin.getLogger().warning("Unable to prepare Discord Top Voters: " + top);
					});
				} catch (RuntimeException rejected) {
					synchronized (preparing) { preparing.remove(top, update); }
				}
			});
		} catch (RuntimeException unavailable) {
			synchronized (preparing) { preparing.remove(top, update); }
		}
	}

	void publishTopVoterMessage(TopVoter top, LinkedHashMap<TopVoterPlayer, Integer> prepared) {
		long generation = plugin.getTopVoterDisplayGeneration();
		if (!currentGeneration(generation)) return;
		boolean newMessage = plugin.getConfigFile().isDiscordSRVTopVoterNewMessageOnUpdate(top);
		TextChannel channel = leaderboardChannel(plugin.getConfigFile().getDiscordSRVTopVoterChannel(top));
		if (channel == null) return;
		Publication publication;
		long existingId;
		synchronized (publications) {
			Publication pending = publications.get(top);
			if (pending != null && pending.generation == generation && (pending.sending || pending.completing)) {
				pending.again = true;
				return;
			}
			existingId = topVoterMessageIds.getOrDefault(top, 0L);
			publication = new Publication(generation, !newMessage, existingId <= 0 || newMessage);
			publications.put(top, publication);
		}
		try {
			var embed = buildTopVoterEmbed(plugin.getConfigFile().getDiscordSRVTopVoterTitle(top), plugin.getTopVoterDisplay(top)).build();
			if (!current(top, publication)) { finish(top, publication, false); return; }
			if (publication.sending) send(top, channel, publication, embed);
			else channel.editMessageEmbedsById(existingId, embed).queue(
					message -> finish(top, publication, true),
					error -> handleMessageUpdateFailure(top, channel, publication, error));
		} catch (RuntimeException unavailable) { finish(top, publication, false); }
	}

	private boolean current(TopVoter top, Publication publication) {
		if (!currentGeneration(publication.generation)) return false;
		synchronized (publications) { return publications.get(top) == publication; }
	}

	/** Never hold the publication lock while waiting for the ServerData save monitor. */
	private boolean persist(TopVoter top, Publication publication, long id) {
		var data = plugin.getServerData();
		synchronized (data) {
			if (!current(top, publication)) return false;
			data.setTopVoterMessageId(top, id);
		}
		return current(top, publication);
	}

	private void finish(TopVoter top, Publication publication, boolean flush) {
		boolean again;
		synchronized (publications) {
			if (publications.get(top) != publication) return;
			publications.remove(top);
			again = publication.again;
		}
		if (flush && again) dispatchLeaderboard(publication.generation,
				() -> publishTopVoterMessage(top, plugin.getTopVoterDisplay(top)), () -> { });
	}

	private void send(TopVoter top, TextChannel channel, Publication publication,
			github.scarsz.discordsrv.dependencies.jda.api.entities.MessageEmbed embed) {
		if (!current(top, publication)) { finish(top, publication, false); return; }
		channel.sendMessageEmbeds(embed).queue(message -> {
			synchronized (publications) {
				if (publications.get(top) != publication || publication.completing) return;
				publication.completing = true;
			}
			dispatchLeaderboard(publication.generation, () -> {
				try {
					if (!current(top, publication)) return;
					long id = message.getIdLong();
					if (publication.persistent && !persist(top, publication, id)) return;
					if (!current(top, publication)) return;
					synchronized (publications) {
						if (publications.get(top) == publication) topVoterMessageIds.put(top, id);
					}
				} finally { finish(top, publication, true); }
			}, () -> finish(top, publication, false));
		}, failure -> finish(top, publication, false));
	}

	TextChannel leaderboardChannel(long channelId) {
		return DiscordUtil.getJda().getTextChannelById(channelId);
	}

	/** Render an inspected display snapshot without accessing the Discord runtime. */
	static EmbedBuilder buildTopVoterEmbed(String title, LinkedHashMap<TopVoterPlayer, Integer> topVoters) {
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

		return eb;
	}

	/**
	 * Handles a failed update of a stored leaderboard message.
	 *
	 * Discord message IDs become invalid when a message is deleted. Treat that
	 * condition as recoverable so one deleted message cannot permanently stop
	 * leaderboard updates.
	 */
	private boolean currentGeneration(long generation) {
		return plugin.isEnabled() && plugin.getTopVoterDisplayGeneration() == generation;
	}

	private void dispatchLeaderboard(long generation, Runnable action, Runnable discarded) {
		if (!currentGeneration(generation)) { discarded.run(); return; }
		try {
			plugin.getTimer().execute(() -> {
				if (currentGeneration(generation)) action.run(); else discarded.run();
			});
		} catch (RuntimeException rejected) { discarded.run(); }
	}

	private void handleMessageUpdateFailure(TopVoter top, TextChannel channel, Publication publication,
			Throwable error) {
		if (!current(top, publication)) { finish(top, publication, false); return; }
		if (!plugin.getConfigFile().isDiscordSRVTopVoterAutoRecoverMessageOnFailure()
				|| !(error instanceof ErrorResponseException response)
				|| response.getErrorResponse() != ErrorResponse.UNKNOWN_MESSAGE) {
			finish(top, publication, false);
			return;
		}
		synchronized (publications) {
			if (publications.get(top) != publication || publication.recovering) return;
			publication.recovering = true;
			publication.sending = true;
		}
		dispatchLeaderboard(publication.generation, () -> {
			try {
				if (!persist(top, publication, 0L)) { finish(top, publication, false); return; }
				synchronized (publications) {
					if (publications.get(top) != publication) return;
					topVoterMessageIds.put(top, 0L);
				}
				// Persistence can block while a join, TTL, reload or disable revokes the earlier frame.
				var replacement = buildTopVoterEmbed(plugin.getConfigFile().getDiscordSRVTopVoterTitle(top),
						plugin.getTopVoterDisplay(top)).build();
				send(top, channel, publication, replacement);
			} catch (RuntimeException failure) { finish(top, publication, false); }
		}, () -> {
			// Rejected admission has no effects; the same failed edit may be retried.
			synchronized (publications) {
				if (publications.get(top) == publication) {
					publication.recovering = false; publication.sending = false;
				}
			}
		});
	}

	/**
	 * Sends a reward message to a Discord channel.
	 *
	 * @param message the message to send
	 * @param channelId the Discord channel ID
	 */
	public void sendRewardMessage(String message, String channelId) {
		TextChannel channel = DiscordUtil.getJda().getTextChannelById(channelId);
		if (channel != null) {
			channel.sendMessage(message).queue();
		} else {
			plugin.getLogger().warning("Discord channel ID " + channelId + " not found for reward message.");
		}
	}
}
