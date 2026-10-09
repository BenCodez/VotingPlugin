package com.bencodez.votingplugin.specialrewards;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Console-only, preview-confirmed reconciliation for interrupted NameMC rewards.
 * Neither a timeout nor a server restart proves whether external effects ran.
 */
public final class NameMCLikeRewardRecoveryService {
	private static final long PREVIEW_TTL_MILLIS = 5 * 60 * 1000L;

	private final VotingPluginMain plugin;
	private final ConcurrentMap<UUID, Preview> previews = new ConcurrentHashMap<>();

	public NameMCLikeRewardRecoveryService(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Reads and mutates persisted user claims only on the AdvancedCore user-data
	 * executor; the caller schedules the resulting message onto its sender owner.
	 */
	public void handle(String actor, String rawUuid, String action, String token, Consumer<String> completion) {
		handle(actor, rawUuid, action, token, false, completion);
	}

	/** Require explicit network-wide quiescence before shared-SQL claim resolution. */
	public void handle(String actor, String rawUuid, String action, String token,
			boolean allBackendsQuiesced, Consumer<String> completion) {
		final UUID uuid;
		try {
			uuid = UUID.fromString(rawUuid);
		} catch (RuntimeException invalid) {
			completion.accept("Invalid UUID; supply the exact stored player UUID");
			return;
		}
		if (!"status".equalsIgnoreCase(action) && !"delivered".equalsIgnoreCase(action)
				&& !"retry".equalsIgnoreCase(action)) {
			completion.accept("Actions: status, delivered, retry");
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
					NameMCLikeCheckerTask checker = plugin.getNameMCLikeCheckerTask();
					if (checker != null && checker.getInFlight().contains(uuid)) {
						completion.accept("NameMC like reward is still active; recovery is blocked");
						return;
					}
					if ("status".equalsIgnoreCase(action)) {
						completion.accept(preview(uuid, user));
						return;
					}
					if (plugin.getUserManager().getDataManager().hasSharedSqlBackend()
							&& !allBackendsQuiesced) {
						completion.accept("Shared SQL may have active grants on another backend. "
								+ "Suspend NameMC reward processing on ALL backend servers, verify the reward, "
								+ "then repeat with final argument all-backends-quiesced.");
						return;
					}
					completion.accept(resolve(actor, uuid, user, action, token));
				} catch (RuntimeException | Error failure) {
					plugin.getLogger().warning("Unable to reconcile NameMC claim for " + uuid
							+ ": " + failure.getMessage());
					plugin.debug(failure);
					completion.accept("NameMC recovery failed; check console and request a fresh preview");
				}
			});
		} catch (RuntimeException failure) {
			plugin.debug(failure);
			completion.accept("The user-storage worker is unavailable; no recovery changes made");
		}
	}

	private String preview(UUID uuid, VotingPluginUser user) {
		boolean pending = user.isNameMCLikeRewardPending();
		boolean claimed = user.hasClaimedNameMCLikeReward();
		if (!pending) {
			previews.remove(uuid);
			return "No pending NameMC claim for " + uuid + " (claimed=" + claimed + ")";
		}
		String token = UUID.randomUUID().toString();
		previews.put(uuid, new Preview(token, pending, claimed,
				System.currentTimeMillis() + PREVIEW_TTL_MILLIS));
		return "NameMC recovery for " + uuid + ": pending=true, claimed=" + claimed
				+ ". AFTER checking external rewards, use /av NameMCLikeRecovery " + uuid
				+ " delivered " + token + " (reward fully delivered; confirm claimed), or retry "
				+ token + " (no reward effects delivered; permit retry). "
				+ "The token expires after 5 minutes and is single-use.";
	}

	private String resolve(String actor, UUID uuid, VotingPluginUser user, String action, String token) {
		Preview preview = previews.get(uuid);
		if (preview == null || token == null || !preview.token().equals(token)
				|| preview.expiresAt() < System.currentTimeMillis()) {
			return "Invalid or expired NameMC recovery token; use /av NameMCLikeRecovery " + uuid + " status";
		}
		if (!previews.remove(uuid, preview)) return "Recovery token already used; preview again";
		if (user.isNameMCLikeRewardPending() != preview.pending()
				|| user.hasClaimedNameMCLikeReward() != preview.claimed()
				|| !preview.pending()) {
			throw new IllegalStateException("NameMC claim state changed; preview again");
		}

		if ("delivered".equalsIgnoreCase(action)) {
			// A confirmed claim must persist before releasing the replay fence.
			user.setClaimedNameMCLikeReward(true);
			user.setNameMCLikeRewardPending(false);
		} else if ("retry".equalsIgnoreCase(action)) {
			if (preview.claimed()) {
				throw new IllegalStateException("Claim is already complete; cannot reset it for retry");
			}
			// The operator must have verified that NO effects were delivered.
			user.setNameMCLikeRewardPending(false);
		} else {
			throw new IllegalArgumentException("Unrecognized NameMC recovery mode");
		}
		plugin.getLogger().warning("NameMC reward recovery applied: UUID=" + uuid
				+ ", mode=" + action + ", operator=" + actor);
		return "NameMC recovery applied (" + action + ") for " + uuid;
	}

	private record Preview(String token, boolean pending, boolean claimed, long expiresAt) { }
}
