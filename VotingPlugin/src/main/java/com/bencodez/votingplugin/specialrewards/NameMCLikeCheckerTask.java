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
	private void processUuid(UUID uuid) {
		if (uuid == null || !inFlight.add(uuid)) {
			return;
		}

		try {
			plugin.getUserManager().getUserAsync(uuid, resolved -> {
				try {
					// AdvancedCore delivers resolved users on the platform thread,
					// where reward actions may safely access Bukkit APIs.
					if (!plugin.isEnabled() || !plugin.getSpecialRewardsConfig().isNameMCLikeRewardEnabled()) {
						return;
					}
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(resolved);
					if (user.hasClaimedNameMCLikeReward()) {
						return;
					}

					new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
							plugin.getSpecialRewardsConfig().getNameMCLikeRewardPath()).setOnline(user.isOnline())
							.withPlaceHolder("NameMCServer", plugin.getSpecialRewardsConfig().getNameMCLikeRewardUrl()).send(user);

					user.setClaimedNameMCLikeReward(true);
					plugin.debug("Gave NameMC like reward to " + user.getPlayerName() + " (" + uuid + ")");
				} finally {
					inFlight.remove(uuid);
				}
			}, failure -> {
				try {
					plugin.getLogger().warning("Failed to resolve NameMC like user " + uuid + ": " + failure.getMessage());
					plugin.debug(failure);
				} finally {
					inFlight.remove(uuid);
				}
			});
		} catch (RuntimeException | Error failure) {
			inFlight.remove(uuid);
			throw failure;
		}
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