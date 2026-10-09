package com.bencodez.votingplugin.user;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;

import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Console-only, preview-confirmed recovery of ambiguously delivered offline
 * vote rewards. Never guesses whether nontransactional commands were executed.
 */
public final class OfflineVoteRewardRecoveryService {
	private static final long PREVIEW_TTL_MILLIS = 5 * 60 * 1000L;

	private final VotingPluginMain plugin;
	private final ConcurrentMap<UUID, Preview> previews = new ConcurrentHashMap<>();

	public OfflineVoteRewardRecoveryService(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Dispatch all inspection/mutation work to the shared user-data executor.
	 * Completion is a textual result that the caller must present on Bukkit's
	 * appropriate command-sender scheduler.
	 */
	public void handle(String actor, String rawUuid, String action, String token, Consumer<String> completion) {
		handle(actor, rawUuid, action, token, false, completion);
	}

	/**
	 * Shared SQL may span multiple backend JVMs. The local in-flight check is
	 * necessary but insufficient; a recovery operator must explicitly confirm
	 * quiescence across every connected backend before any mutation is allowed.
	 */
	public void handle(String actor, String rawUuid, String action, String token,
			boolean allBackendsQuiesced, Consumer<String> completion) {
		UUID uuid;
		try {
			uuid = UUID.fromString(rawUuid);
		} catch (RuntimeException invalid) {
			completion.accept("Invalid UUID; supply the exact stored player UUID");
			return;
		}
		if (!"status".equalsIgnoreCase(action) && !"retry".equalsIgnoreCase(action)
				&& !"delivered".equalsIgnoreCase(action) && !"already-cleared".equalsIgnoreCase(action)) {
			completion.accept("Actions: status, delivered, retry, already-cleared");
			return;
		}

		try {
			plugin.getUserManager().getDataManager().getTimer().execute(() -> {
				try {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
					if (user == null) {
						completion.accept("No stored VotingPlugin user found for " + uuid);
						return;
					}
					user.cache();
					if (user.isOfflineVoteRewardReplayActive()) {
						completion.accept("Offline reward delivery is active; recovery is blocked until it finishes");
						return;
					}
					if ("status".equalsIgnoreCase(action)) {
						completion.accept(preview(uuid, user));
						return;
					}
					if (plugin.getUserManager().getDataManager().hasSharedSqlBackend()
							&& !allBackendsQuiesced) {
						completion.accept("Shared SQL may have active delivery on another backend. "
								+ "Suspend reward processing on ALL backend servers, confirm external effects, "
								+ "then repeat with final argument all-backends-quiesced.");
						return;
					}
					completion.accept(resolve(actor, uuid, user, action, token));
				} catch (RuntimeException | Error failure) {
					plugin.getLogger().warning("Unable to inspect/reconcile offline vote rewards for " + uuid
							+ ": " + failure.getMessage());
					plugin.debug(failure);
					completion.accept("Offline reward recovery failed; check console and preview again");
				}
			});
		} catch (RuntimeException failure) {
			plugin.debug(failure);
			completion.accept("The user-storage executor is unavailable; no recovery changes made");
		}
	}

	private String preview(UUID uuid, VotingPluginUser user) {
		ArrayList<String> pending = user.getPendingOfflineVoteRewardBatch();
		ArrayList<String> queue = user.getOfflineVotes();
		if (pending.isEmpty()) {
			previews.remove(uuid);
			return "No pending offline reward batch for " + uuid + "; queued votes=" + queue.size();
		}
		boolean prefixMatches = hasPrefix(queue, pending);
		String token = UUID.randomUUID().toString();
		previews.put(uuid, new Preview(token, List.copyOf(pending), List.copyOf(queue),
				System.currentTimeMillis() + PREVIEW_TTL_MILLIS));
		String sample = String.join(", ", pending.subList(0, Math.min(pending.size(), 8)));
		return "Offline reward recovery for " + uuid + ": pending=" + pending.size() + " [" + sample
				+ "], queued=" + queue.size() + ", prefixMatches=" + prefixMatches
				+ ". AFTER verifying external effects: /av OfflineVoteRecovery " + uuid
				+ " delivered " + token + " (effects given; remove pending prefix), or retry " + token
				+ " (no effects given; retain votes). If the prefix was already removed, use already-cleared "
				+ token + ". This token expires in 5 minutes and is single-use.";
	}

	private String resolve(String actor, UUID uuid, VotingPluginUser user, String action, String token) {
		Preview preview = previews.get(uuid);
		if (preview == null || token == null || !preview.token().equals(token)
				|| preview.expiresAt() < System.currentTimeMillis()) {
			return "Invalid or expired recovery token; run /av OfflineVoteRecovery " + uuid + " status";
		}
		if (!previews.remove(uuid, preview)) {
			return "Recovery preview has already been used; inspect again";
		}
		user.reconcileOfflineVoteRewardBatch(preview.pending(), preview.queued(), action);
		plugin.getLogger().warning("Offline reward recovery applied: UUID=" + uuid + ", mode="
				+ action + ", operator=" + actor);
		return "Offline reward recovery applied (" + action + ") for " + uuid
				+ ". Remaining new votes are unchanged unless the confirmed prefix was removed.";
	}

	private static boolean hasPrefix(List<String> queue, List<String> pending) {
		return queue.size() >= pending.size() && queue.subList(0, pending.size()).equals(pending);
	}

	private record Preview(String token, List<String> pending, List<String> queued, long expiresAt) { }
}
