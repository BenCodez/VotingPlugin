package com.bencodez.votingplugin.specialrewards;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletionStage;

import com.bencodez.advancedcore.api.user.usercache.UserDataManager;

import org.bukkit.scheduler.BukkitRunnable;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import lombok.Getter;
import lombok.Setter;

/**
 * Periodically checks NameMC likes for the configured server and gives a
 * one-time reward to players who have not claimed it yet.
 */
@Getter
@Setter
public class NameMCLikeCheckerTask extends BukkitRunnable {

	private VotingPluginMain plugin;

	/** Prevent overlapping asynchronous checks from processing a UUID twice. */
	private final Set<UUID> inFlight = ConcurrentHashMap.newKeySet();

	/**
	 * Creates a new NameMC like checker task.
	 *
	 * @param plugin the plugin
	 */
	public NameMCLikeCheckerTask(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void run() {
		if (!plugin.getSpecialRewardsConfig().isNameMCLikeRewardEnabled()) {
			return;
		}

		String urlValue = plugin.getSpecialRewardsConfig().getNameMCLikeRewardUrl();
		if (urlValue == null || urlValue.trim().isEmpty()) {
			return;
		}

		Set<UUID> likedUuids = fetchLikedUuids(urlValue);
		if (likedUuids.isEmpty()) {
			return;
		}

		for (UUID uuid : likedUuids) {
			processUuid(uuid);
		}
	}

	/**
	 * Processes a UUID returned by NameMC.
	 *
	 * @param uuid the uuid
	 */
	void processUuid(UUID uuid) {
		if (uuid == null || !inFlight.add(uuid)) return;
		try {
			plugin.getUserManager().getUserAsync(uuid, resolved -> {
				try {
					if (!plugin.isEnabled() || !plugin.getSpecialRewardsConfig().isNameMCLikeRewardEnabled()) {
						inFlight.remove(uuid);
						return;
					}
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(resolved);
					UserDataManager manager = plugin.getUserManager().getDataManager();
					if (manager == null) {
						inFlight.remove(uuid);
						return;
					}
					// Identity and live online state are captured before switching
					// back to the persistence worker. No storage-backed getters here.
					boolean online = user.isOnline();
					if (manager.hasSharedSqlBackend()) {
						boolean deferred = manager.deferSharedStorageResultFromPlatform(() -> {
							processOnStorageWorker(uuid, user, manager, online);
							return Boolean.TRUE;
						}, ignored -> { }, failure -> {
							logProcessingFailure(uuid, failure);
							inFlight.remove(uuid);
						});
						if (!deferred) {
							plugin.getLogger().warning("NameMC like storage worker unavailable for " + uuid);
							inFlight.remove(uuid);
						}
					} else {
						// Even the legacy SQLite path must not read/write user data
						// from getUserAsync's platform callback.
						manager.getTimer().execute(() -> processOnStorageWorker(uuid, user, manager, online));
					}
				} catch (RuntimeException | Error failure) {
					logProcessingFailure(uuid, failure);
					inFlight.remove(uuid);
				}
			}, failure -> {
				logProcessingFailure(uuid, failure);
				inFlight.remove(uuid);
			});
		} catch (RuntimeException | Error failure) {
			inFlight.remove(uuid);
			throw failure;
		}
	}

	/**
	 * The pending marker is persisted before any effects can start. It fences
	 * ambiguous partial grants across restarts and must be reconciled manually,
	 * rather than replaying commands/money twice after a failed async injection.
	 */
	private void processOnStorageWorker(UUID uuid, VotingPluginUser user, UserDataManager manager, boolean online) {
		boolean awaitingDelivery = false;
		try {
			if (!plugin.isEnabled() || !plugin.getSpecialRewardsConfig().isNameMCLikeRewardEnabled()) return;
			user.cache();
			if (user.hasClaimedNameMCLikeReward()) return;
			if (user.isNameMCLikeRewardPending()) {
				plugin.debug("NameMC reward requires pending-claim reconciliation for " + uuid);
				return;
			}
			// Force immediate worker-side persistence, not an unconfirmed queued
			// cache update that might vanish after reward dispatch.
			user.setNameMCLikeRewardPending(true);
			CompletionStage<Void> delivery = new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
					plugin.getSpecialRewardsConfig().getNameMCLikeRewardPath()).setOnline(online)
					.withPlaceHolder("NameMCServer", plugin.getSpecialRewardsConfig().getNameMCLikeRewardUrl())
					.sendAsync(user);
			if (delivery == null) throw new IllegalStateException("NameMC reward returned no completion stage");
			awaitingDelivery = true;
			delivery.whenComplete((ignored, failure) -> {
				if (failure != null) {
					logProcessingFailure(uuid, failure);
					// Pending remains durable. Do not label failed/partial delivery
					// claimed, and do not automatically replay its external effects.
					inFlight.remove(uuid);
					return;
				}
				try {
					manager.getTimer().execute(() -> completeClaimOnStorageWorker(uuid, user));
				} catch (RuntimeException | Error failureToQueue) {
					logProcessingFailure(uuid, failureToQueue);
					// Success is ambiguous until the durable claimed write lands.
					// The pending marker keeps future scans from duplicating it.
					inFlight.remove(uuid);
				}
			});
		} catch (RuntimeException | Error failure) {
			logProcessingFailure(uuid, failure);
			// A failed pending write cannot safely authorize reward delivery.
		} finally {
			if (!awaitingDelivery) inFlight.remove(uuid);
		}
	}

	/** Commit the confirmed result on the same storage lane as the pending fence. */
	private void completeClaimOnStorageWorker(UUID uuid, VotingPluginUser user) {
		try {
			user.cache();
			user.setClaimedNameMCLikeReward(true);
			user.setNameMCLikeRewardPending(false);
			plugin.debug("Gave NameMC like reward to " + user.getPlayerName() + " (" + uuid + ")");
		} catch (RuntimeException | Error failure) {
			logProcessingFailure(uuid, failure);
			// Leave the pending state on disk if the commit was not completed.
		} finally {
			inFlight.remove(uuid);
		}
	}

	private void logProcessingFailure(UUID uuid, Throwable failure) {
		plugin.getLogger().warning("NameMC like reward/claim needs review for " + uuid
				+ ": " + failure.getMessage());
		plugin.debug(failure);
	}

	/**
	 * Fetches all liked UUIDs from NameMC for the configured server.
	 *
	 * @param serverUrl the server URL or IP
	 * @return set of liked UUIDs
	 */
	private Set<UUID> fetchLikedUuids(String serverUrl) {
		Set<UUID> uuids = new HashSet<>();

		try {
			String url = "https://api.namemc.com/server/" + serverUrl + "/likes";

			HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

			HttpRequest request = HttpRequest.newBuilder().uri(URI.create(url)).timeout(Duration.ofSeconds(15))
					.header("Accept", "application/json").GET().build();

			HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

			if (response.statusCode() == 200) {
				JsonArray array = JsonParser.parseString(response.body()).getAsJsonArray();

				for (JsonElement element : array) {
					try {
						UUID uuid = UUID.fromString(element.getAsString());
						uuids.add(uuid);
					} catch (IllegalArgumentException ignored) {
					}
				}
			} else {
				plugin.debug("NameMC API returned status: " + response.statusCode());
			}

		} catch (Exception e) {
			plugin.debug("Failed to fetch NameMC likes: " + e.getMessage());
			e.printStackTrace();
		}

		return uuids;
	}
}