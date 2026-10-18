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
import github.scarsz.discordsrv.dependencies.jda.api.entities.Message;
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
		final AtomicBoolean sendCompleted = new AtomicBoolean(false);
		Publication(long generation, boolean persistent, boolean sending) {
			this.generation = generation; this.persistent = persistent; this.sending = sending;
		}
	}
	private static final int PENDING = 0, WRITING = 1, RETAINED = 2, DISCARDED = 3, DETACHED = 4;
	private static final int MAX_OWNED_RECEIPTS = 128;
	/** External success is owned here before any cancellable worker handoff. */
	private static final class SentReceipt {
		final TopVoter top;
		final Message message;
		final long id;
		final AtomicBoolean cleanupRequested = new AtomicBoolean(false);
		int state = PENDING;
		boolean confirmed;
		SentReceipt(TopVoter top, Message message) {
			this.top = top; this.message = message; this.id = message.getIdLong();
		}
	}
	private final java.util.EnumMap<TopVoter, Publication> publications = new java.util.EnumMap<>(TopVoter.class);
	/** Guarded by publications; includes uncertain saves until confirmed replacement or explicit detachment. */
	private final java.util.Set<SentReceipt> ownedReceipts = new java.util.HashSet<>();
	private final AtomicBoolean closed = new AtomicBoolean(false);
	private final AtomicBoolean discordReady = new AtomicBoolean(false);
	private static final class DisplayUpdate {
		private final long generation;
		private long requestedGeneration;
		private boolean again;
		private DisplayUpdate(long generation) { this.generation = generation; this.requestedGeneration = generation; }
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
		prepareTopVoterMessage(top, plugin.getTopVoterDisplayGeneration());
	}

	private void prepareTopVoterMessage(TopVoter top, long generation) {
		DisplayUpdate update;
		synchronized (preparing) {
			if (!currentGeneration(generation)) return;
			DisplayUpdate pending = preparing.get(top);
			if (pending != null) {
				pending.again = true;
				pending.requestedGeneration = generation;
				return;
			}
			update = new DisplayUpdate(generation);
			preparing.put(top, update);
		}
		try {
			plugin.getTopVoterDisplayAsync(top).whenComplete((snapshot, failure) -> {
				try {
					plugin.getTimer().execute(() -> {
						if (!releasePreparation(top, update)) return;
						if (!currentGeneration(update.generation)) {
							retryRequestedPreparation(top, update);
							return;
						}
						// Joins and TTL/scope changes can revoke omissions while publication is queued.
						// Recheck current evidence without opening another asynchronous deadline.
						if (update.again || (failure == null && snapshot != null)) {
							try { publishTopVoterMessage(top, plugin.getTopVoterDisplay(top), update.generation); }
							catch (RuntimeException unavailable) { warnLeaderboardFailure("preparing a current display for " + top); }
						} else plugin.getLogger().warning("Unable to prepare Discord Top Voters: " + top);
					});
				} catch (RuntimeException rejected) {
					if (releasePreparation(top, update)) {
						try { if (currentGeneration(update.generation)) warnLeaderboardFailure("preparation worker admission for " + top); }
						finally { retryRequestedPreparation(top, update); }
					}
				}
			});
		} catch (RuntimeException unavailable) {
			if (releasePreparation(top, update)) {
				try { if (currentGeneration(update.generation)) warnLeaderboardFailure("preparing evidence for " + top); }
				finally { retryRequestedPreparation(top, update); }
			}
		}
	}

	private boolean releasePreparation(TopVoter top, DisplayUpdate update) {
		synchronized (preparing) { return preparing.remove(top, update); }
	}

	private void retryRequestedPreparation(TopVoter top, DisplayUpdate update) {
		// The retired request's evidence never changes ownership to a replacement filter.
		if (update.again && update.requestedGeneration != update.generation)
			prepareTopVoterMessage(top, update.requestedGeneration);
	}

	void publishTopVoterMessage(TopVoter top, LinkedHashMap<TopVoterPlayer, Integer> prepared) {
		publishTopVoterMessage(top, prepared, plugin.getTopVoterDisplayGeneration());
	}

	void publishTopVoterMessage(TopVoter top, LinkedHashMap<TopVoterPlayer, Integer> prepared, long generation) {
		if (!currentGeneration(generation)) return;
		boolean newMessage = plugin.getConfigFile().isDiscordSRVTopVoterNewMessageOnUpdate(top);
		TextChannel channel;
		try { channel = leaderboardChannel(plugin.getConfigFile().getDiscordSRVTopVoterChannel(top)); }
		catch (RuntimeException unavailable) { warnLeaderboardFailure("channel lookup for " + top); return; }
		if (channel == null) { warnLeaderboardFailure("missing channel for " + top); return; }
		Publication publication;
		long existingId;
		synchronized (publications) {
			if (!currentGeneration(generation)) return;
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
			if (!current(top, publication)) { finish(top, publication); return; }
			if (publication.sending) send(top, channel, publication, embed);
			else channel.editMessageEmbedsById(existingId, embed).queue(
					message -> finishEdit(top, publication, existingId),
					error -> handleMessageUpdateFailure(top, channel, publication, error));
		} catch (RuntimeException unavailable) { failPublication(top, publication, "preparing or submitting a message"); }
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

	private void finish(TopVoter top, Publication publication) {
		boolean again;
		synchronized (publications) {
			if (publications.get(top) != publication) return;
			publications.remove(top);
			again = publication.again;
		}
		// A queued request is independent of whether the previous network operation succeeded.
		if (again) dispatchLeaderboard(publication.generation,
				() -> publishTopVoterMessage(top, plugin.getTopVoterDisplay(top), publication.generation), () -> { });
	}

	private void warnLeaderboardFailure(String stage) {
		// Provider exceptions can contain request URLs or message content. Report the phase only.
		plugin.getLogger().warning("Unable to publish Discord Top Voters (" + stage
				+ "). Check the Discord channel, permissions and configuration; a later update can retry.");
	}

	private void failPublication(TopVoter top, Publication publication, String stage) {
		try { if (current(top, publication)) warnLeaderboardFailure(stage + " for " + top); }
		finally { finish(top, publication); }
	}

	private void send(TopVoter top, TextChannel channel, Publication publication,
			github.scarsz.discordsrv.dependencies.jda.api.entities.MessageEmbed embed) {
		if (!current(top, publication)) { finish(top, publication); return; }
		channel.sendMessageEmbeds(embed).queue(message -> {
			if (!publication.sendCompleted.compareAndSet(false, true)) return;
			SentReceipt receipt = new SentReceipt(top, message);
			boolean admitted;
			synchronized (publications) {
				admitted = !closed.get() && (!publication.persistent || ownedReceipts.size() < MAX_OWNED_RECEIPTS);
				if (admitted && publication.persistent) ownedReceipts.add(receipt);
				publication.completing = true;
			}
			if (!admitted) {
				discardCreatedMessage(publication, receipt); finish(top, publication); return;
			}
			dispatchLeaderboard(publication.generation, () -> {
				try {
					if (!current(top, publication)) return;
					if (publication.persistent) { retainCreatedMessage(top, publication, receipt); return; }
					if (!current(top, publication)) return;
					synchronized (publications) {
						if (publications.get(top) == publication) topVoterMessageIds.put(top, receipt.id);
					}
				} catch (RuntimeException failure) { warnLeaderboardFailure("storing a message ID for " + top); }
				finally { discardCreatedMessage(publication, receipt); finish(top, publication); }
			}, () -> { discardCreatedMessage(publication, receipt); finish(top, publication); });
		}, failure -> failPublication(top, publication, "sending a message"));
	}

	/** Retry an uncertain ID save after a successful edit, without creating another external post. */
	private void finishEdit(TopVoter top, Publication publication, long id) {
		SentReceipt retry;
		synchronized (publications) {
			retry = ownedReceipts.stream().filter(receipt -> receipt.top == top && receipt.id == id
					&& receipt.state == RETAINED && !receipt.confirmed).findFirst().orElse(null);
		}
		if (retry == null) { finish(top, publication); return; }
		dispatchLeaderboard(publication.generation, () -> {
			try { retainCreatedMessage(top, publication, retry); }
			finally { discardCreatedMessage(publication, retry); finish(top, publication); }
		}, () -> finish(top, publication));
	}

	/** Never hold publication ownership while waiting for the existing save monitor or file I/O. */
	private void retainCreatedMessage(TopVoter top, Publication publication, SentReceipt receipt) {
		var obsolete = new java.util.ArrayList<SentReceipt>();
		var data = plugin.getServerData();
		boolean uncertain = false;
		synchronized (data) {
			if (!current(top, publication)) return;
			synchronized (publications) {
				if (!current(top, publication) || (receipt.state != PENDING && receipt.state != RETAINED)) return;
				receipt.state = WRITING;
			}
			try {
				boolean confirmed = data.setTopVoterMessageIdConfirmed(top, receipt.id);
				long actual = data.getTopVoterMessageId(top);
				synchronized (publications) {
					if (receipt.state == DETACHED) return;
					// A reentrant newer confirmed write already owns the file. Reconcile only this older post.
					if (actual != receipt.id && ownedReceipts.stream().anyMatch(other -> other != receipt
							&& other.top == top && other.id == actual && other.state == RETAINED && other.confirmed)) return;
					receipt.state = RETAINED;
					receipt.confirmed = confirmed && actual == receipt.id;
					uncertain = !receipt.confirmed;
					if (actual == receipt.id && (publications.get(top) == publication
							|| topVoterMessageIds.getOrDefault(top, 0L) <= 0)) topVoterMessageIds.put(top, receipt.id);
					if (receipt.confirmed) {
						var iterator = ownedReceipts.iterator();
						while (iterator.hasNext()) {
							SentReceipt previous = iterator.next();
							if (previous != receipt && previous.top == top && previous.state == RETAINED) {
								previous.state = DISCARDED; iterator.remove(); obsolete.add(previous);
							}
						}
					}
				}
			} catch (RuntimeException failure) {
				// The legacy writer may have mutated or saved before throwing; do not delete a possibly recorded post.
				synchronized (publications) {
					if (receipt.state == DETACHED) return;
					receipt.state = RETAINED; receipt.confirmed = false;
					if (publications.get(top) == publication && topVoterMessageIds.getOrDefault(top, 0L) <= 0)
						topVoterMessageIds.put(top, receipt.id);
				}
				uncertain = true;
			}
		}
		obsolete.forEach(this::removeCreatedMessage);
		if (uncertain) warnMessageStorage();
	}

	private void discardCreatedMessage(Publication publication, SentReceipt receipt) {
		if (!publication.persistent) return; // Append mode deliberately keeps its posts.
		synchronized (publications) {
			if (receipt.state != PENDING && receipt.state != WRITING) return;
			receipt.state = DISCARDED; ownedReceipts.remove(receipt);
		}
		removeCreatedMessage(receipt);
	}

	/** Detach the administrator's old posts; clearing an ID never authorizes deleting those posts. */
	public boolean clearTopVoterMessageId(TopVoter top) {
		var data = plugin.getServerData();
		synchronized (data) {
			synchronized (publications) {
				publications.remove(top); preparing.remove(top); topVoterMessageIds.put(top, 0L);
				ownedReceipts.removeIf(receipt -> {
					if (receipt.top != top || (receipt.state != PENDING && receipt.state != RETAINED
							&& receipt.state != WRITING)) return false;
					receipt.state = DETACHED; return true;
				});
			}
			try { if (data.setTopVoterMessageIdConfirmed(top, 0L)) return true; }
			catch (RuntimeException failure) { /* The outgoing diagnostic remains redacted. */ }
		}
		warnMessageStorage(); return false;
	}

	/** Runs before reload/worker cancellation; never waits for storage or a network completion. */
	public void retireLeaderboardPublications() {
		var abandoned = new java.util.ArrayList<SentReceipt>();
		synchronized (publications) {
			publications.clear(); preparing.clear();
			var iterator = ownedReceipts.iterator();
			while (iterator.hasNext()) {
				SentReceipt receipt = iterator.next();
				if (receipt.state == PENDING) {
					receipt.state = DISCARDED; iterator.remove(); abandoned.add(receipt);
				}
			}
		}
		abandoned.forEach(this::removeCreatedMessage);
	}

	public void close() {
		synchronized (publications) {
			if (!closed.compareAndSet(false, true)) return;
		}
		retireLeaderboardPublications();
		boolean uncertain;
		synchronized (publications) {
			uncertain = ownedReceipts.stream().anyMatch(receipt -> receipt.state == WRITING || !receipt.confirmed);
		}
		if (uncertain) warnMessageStorage();
	}

	private void warnMessageStorage() {
		plugin.getLogger().warning("Unable to confirm Discord Top Voters message ID storage. "
				+ "The post remains tracked in memory; check storage before restarting and reconcile posts manually.");
	}

	/** JDA owns asynchronous deletion; cleanup does not depend on the retiring plugin worker. */
	private void removeCreatedMessage(SentReceipt receipt) {
		if (!receipt.cleanupRequested.compareAndSet(false, true)) return;
		try {
			receipt.message.delete().queue(ignored -> { }, failure -> {
				if (!(failure instanceof ErrorResponseException response)
						|| response.getErrorResponse() != ErrorResponse.UNKNOWN_MESSAGE) warnMessageCleanup();
			});
		} catch (RuntimeException failure) { warnMessageCleanup(); }
	}

	private void warnMessageCleanup() {
		plugin.getLogger().warning("Unable to remove an untracked Discord Top Voters message. "
				+ "Check Discord permissions and remove the unwanted post manually.");
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
		return !closed.get() && plugin.isEnabled() && plugin.getTopVoterDisplayGeneration() == generation;
	}

	private void dispatchLeaderboard(long generation, Runnable action, Runnable discarded) {
		if (!currentGeneration(generation)) { discarded.run(); return; }
		try {
			plugin.getTimer().execute(() -> {
				try {
					if (currentGeneration(generation)) action.run(); else discarded.run();
				} catch (RuntimeException failure) {
					try { if (currentGeneration(generation)) warnLeaderboardFailure("asynchronous publication"); }
					finally { discarded.run(); }
				}
			});
		} catch (RuntimeException rejected) {
			try { if (currentGeneration(generation)) warnLeaderboardFailure("worker admission"); }
			finally { discarded.run(); }
		}
	}

	private void handleMessageUpdateFailure(TopVoter top, TextChannel channel, Publication publication,
			Throwable error) {
		if (!current(top, publication)) { finish(top, publication); return; }
		if (!plugin.getConfigFile().isDiscordSRVTopVoterAutoRecoverMessageOnFailure()
				|| !(error instanceof ErrorResponseException response)
				|| response.getErrorResponse() != ErrorResponse.UNKNOWN_MESSAGE) {
			failPublication(top, publication, "editing a message");
			return;
		}
		synchronized (publications) {
			if (publications.get(top) != publication || publication.recovering) return;
			publication.recovering = true;
			publication.sending = true;
		}
		dispatchLeaderboard(publication.generation, () -> {
			try {
				if (!persist(top, publication, 0L)) { finish(top, publication); return; }
				synchronized (publications) {
					if (publications.get(top) != publication) return;
					topVoterMessageIds.put(top, 0L);
				}
				// Persistence can block while a join, TTL, reload or disable revokes the earlier frame.
				var replacement = buildTopVoterEmbed(plugin.getConfigFile().getDiscordSRVTopVoterTitle(top),
						plugin.getTopVoterDisplay(top)).build();
				send(top, channel, publication, replacement);
			} catch (RuntimeException failure) { failPublication(top, publication, "recovering a deleted message"); }
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
