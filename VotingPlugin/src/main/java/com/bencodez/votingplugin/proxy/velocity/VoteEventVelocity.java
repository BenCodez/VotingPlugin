package com.bencodez.votingplugin.proxy.velocity;

import java.util.concurrent.TimeUnit;
import java.util.UUID;

import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.proxy.VotingPluginProxy.VoteRetryException;
import com.velocitypowered.api.event.Subscribe;
import com.vexsoftware.votifier.velocity.event.VotifierEvent;

/**
 * Handles vote events from Velocity proxy.
 */
public class VoteEventVelocity {
	private VotingPluginVelocity plugin;

	/**
	 * Constructs a new Velocity vote event handler.
	 * @param plugin the plugin instance
	 */
	public VoteEventVelocity(VotingPluginVelocity plugin) {
		this.plugin = plugin;
	}

	/**
	 * Handles Votifier vote events.
	 * @param event the votifier event
	 */
	@Subscribe
	public void onVotifierEvent(VotifierEvent event) {
		final String serviceSiteVote = event.getVote().getServiceName();
		final String name = event.getVote().getUsername();
		plugin.getTimer().execute(new RetryingVote(name,
				serviceSiteVote == null || serviceSiteVote.isEmpty() ? "Empty" : serviceSiteVote));

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
			try {
				IncomingVoteRuntimeResult result = plugin.processIncomingVote(player, service, voteId);
				if (result == IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD) {
					plugin.getTimer().schedule(this, 1, TimeUnit.SECONDS);
					return;
				}
				if (result == IncomingVoteRuntimeResult.RUNTIME_UNAVAILABLE) {
					plugin.getLogger().error("Vote received while VotingPlugin proxy runtime is not operational; vote was not processed for {}",
							MinecraftUsernameValidator.sanitizeForLog(player));
					return;
				}
				plugin.getLogger().info("Vote received " + MinecraftUsernameValidator.sanitizeForLog(player)
						+ " from service site " + MinecraftUsernameValidator.sanitizeForLog(service));
			} catch (VoteRetryException retryable) {
				attempts++;
				if (attempts < MAX_ATTEMPTS) {
					plugin.getLogger().warn("Vote processing is waiting for durable storage; retrying shortly");
					plugin.getTimer().schedule(this, 5, TimeUnit.SECONDS);
				} else {
					plugin.getVotingPluginProxy().abandonLiveVoteRetry(voteId);
					plugin.getLogger().error("Vote processing exhausted bounded durable-storage retries for {}",
							MinecraftUsernameValidator.sanitizeForLog(player));
				}
			}
		}
	}
}
