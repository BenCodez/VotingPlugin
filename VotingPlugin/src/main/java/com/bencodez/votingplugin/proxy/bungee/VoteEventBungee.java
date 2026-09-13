package com.bencodez.votingplugin.proxy.bungee;

import java.util.concurrent.TimeUnit;
import java.util.UUID;

import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.proxy.VotingPluginProxy.VoteRetryException;
import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import net.md_5.bungee.event.EventHandler;

/**
 * Handles vote events from Bungee proxy.
 */
public class VoteEventBungee implements net.md_5.bungee.api.plugin.Listener {
	private VotingPluginBungee plugin;

	/**
	 * Constructs a new Bungee vote event handler.
	 * @param plugin the plugin instance
	 */
	public VoteEventBungee(VotingPluginBungee plugin) {
		this.plugin = plugin;
	}

	/**
	 * Handles Votifier vote events.
	 * @param event the votifier event
	 */
	@EventHandler
	public void onVote(VotifierEvent event) {
		Vote vote = event.getVote();
		String serviceSite = vote.getServiceName().isEmpty() ? "Empty" : vote.getServiceName();
		plugin.getProxy().getScheduler().runAsync(plugin,
				new RetryingVote(vote.getUsername(), serviceSite));

	}

	private final class RetryingVote implements Runnable {
		private static final int MAX_ATTEMPTS = 12;
		private final String player;
		private final String service;
		private final UUID voteId = UUID.randomUUID();
		private int attempts;

		private RetryingVote(String player, String service) {
			this.player = player;
			this.service = service;
		}

		@Override
		public void run() {
			plugin.getLogger().info("Vote received " + MinecraftUsernameValidator.sanitizeForLog(player)
					+ " from service site " + MinecraftUsernameValidator.sanitizeForLog(service));
			try {
				plugin.getVotingPluginProxy().vote(player, service, true, true, 0, null, null, voteId);
			} catch (VoteRetryException retryable) {
				attempts++;
				if (attempts < MAX_ATTEMPTS) {
					plugin.getLogger().warning("Vote processing is waiting for durable storage; retrying shortly");
					plugin.getProxy().getScheduler().schedule(plugin, this, 5, TimeUnit.SECONDS);
				} else {
					plugin.getVotingPluginProxy().abandonLiveVoteRetry(voteId);
					plugin.getLogger().severe("Vote processing exhausted bounded durable-storage retries for "
							+ MinecraftUsernameValidator.sanitizeForLog(player));
				}
			}
		}
	}

}
