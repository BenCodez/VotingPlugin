package com.bencodez.votingplugin.listeners;

import java.util.UUID;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerQuitEvent;

import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

public class PlayerJoinEvent implements Listener {

	/** The plugin. */
	private final VotingPluginMain plugin;

	/**
	 * Instantiates a new player join event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerJoinEvent(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/** Capture the entity owner before asynchronous user notifications can need it. */
	@EventHandler(priority = EventPriority.LOWEST)
	public void onPlayerJoin(org.bukkit.event.player.PlayerJoinEvent event) {
		if (event == null || event.getPlayer() == null) return;
		Player player = event.getPlayer();
		plugin.getPlaceholderPlayerPresence().playerOnline(placeholderUuid(player), player);
	}

	private UUID placeholderUuid(Player player) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		return user == null || user.getJavaUUID() == null ? player.getUniqueId() : user.getJavaUUID();
	}

	private static boolean isBlank(String s) {
		return s == null || s.trim().isEmpty() || "null".equalsIgnoreCase(s.trim());
	}

	private void clearPlaceholderCachesIfOffline(UUID uuid) {
		PlaceHolders placeholders = plugin.getPlaceholders();
		if (placeholders != null) {
			plugin.getPlaceholderPlayerPresence().runIfOffline(uuid, () -> placeholders.onLogout(uuid));
		}
	}

	/**
	 * On AdvancedCore login event (post-auth / delayed login).
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerLogin(AdvancedCoreLoginEvent event) {
		if (event == null || !plugin.isMySQLOkay() || event.isCancelled() || event.getUser() == null) {
			return;
		}

		// UUID is authoritative here (String)
		String uuid = event.getUuid();
		if (isBlank(uuid)) {
			return;
		}

		// "Has data" should come from AdvancedCore event (storage presence)
		boolean hasData = event.isUserInStorage();

		// Resolve VotingPluginUser by the storage UUID string (offline-mode
		// name-derived UUID)
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (user == null) {
			return;
		}

		Player player = event.getPlayer();
		if (player != null) {
			plugin.getPlaceholderPlayerPresence().playerOnline(user.getJavaUUID(), player);
		}

		if (player != null && plugin.isYmlError()) {
			BukkitCompletionScheduler.run(plugin, player, () -> {
				if (player.isOp()) {
					user.sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
				}
			}, () -> { }, () -> { });
		}

		// Proxy routing needs live presence even when offline reward replay fails.
		plugin.getUserManager().getDataManager().getTimer().execute(() -> {
			if (player != null && plugin.getPlaceholderPlayerPresence().schedulerOwner(user.getJavaUUID()) != player) {
				return;
			}
			if (plugin.getBungeeSettings().isUseBungeecoord()) {
				plugin.getBackendProxyHandler().playerOnline(user.getPlayerName(), user.getUUID());
			}
		});
		Runnable afterOfflineVotes = () -> {
			// Replay may finish after quit or a replacement login for the same storage UUID.
			if (player != null && plugin.getPlaceholderPlayerPresence().schedulerOwner(user.getJavaUUID()) != player) {
				return;
			}
			user.loginRewardsAsync().whenComplete((ignored, failure) -> {
				if (failure != null) {
					plugin.getLogger().warning("Login rewards failed for " + uuid + ": " + failure);
					return;
				}
				try {
					// Reward completion can run on a player owner; storage-backed placeholders cannot.
					plugin.getUserManager().getDataManager().getTimer().execute(() -> {
						if (player != null && plugin.getPlaceholderPlayerPresence().schedulerOwner(user.getJavaUUID()) != player) {
							return;
						}
						plugin.getPlaceholders().onUpdate(user, true);
					});
				} catch (RuntimeException rejected) {
					plugin.getLogger().warning("Login placeholder update rejected for " + uuid + ": " + rejected);
				}
			});
		};
		if (hasData) {
			// Follow-up work requires confirmed durable replay, not task admission.
			user.offVoteAndThen(player, afterOfflineVotes);
		} else {
			plugin.debug("No data detected for " + user.getUUID() + "/" + user.getPlayerName());
			// Preserve the no-data replay skip and keep dependent work off entity owners.
			plugin.getUserManager().getDataManager().getTimer().execute(afterOfflineVotes);
		}
	}

	/**
	 * Handles player quit events.
	 *
	 * @param event the player quit event
	 */
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerQuit(PlayerQuitEvent event) {
		if (plugin == null || !plugin.isEnabled() || event == null) {
			return;
		}

		final Player player = event.getPlayer();
		if (player == null) {
			return;
		}
		UUID placeholderUuid = plugin.getPlaceholderPlayerPresence().storageUuid(player);
		if (placeholderUuid == null) placeholderUuid = placeholderUuid(player);
		boolean retiredPresence = plugin.getPlaceholderPlayerPresence().playerOffline(placeholderUuid, player);
		if (retiredPresence) clearPlaceholderCachesIfOffline(placeholderUuid);

		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.getBackendProxyHandler().playerOffline(player.getName());
		}

		plugin.getLoginTimer().execute(new Runnable() {

			@Override
			public void run() {
				VotingPluginMain.plugin.getAdvancedTab().remove(player.getUniqueId());

				// Use AdvancedCore UUIDLookup to derive the correct UUID (online/offline aware)
				String uuid = UuidLookup.getInstance().getUUID(player.getName());
				if (isBlank(uuid)) {
					// Fallback to online UUID if lookup fails
					uuid = player.getUniqueId().toString();
				}

				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
				if (user == null) {
					return;
				}

				user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
				user.logoutRewards();
				clearPlaceholderCachesIfOffline(user.getJavaUUID());
			}
		});
	}
}
