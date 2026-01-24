package com.bencodez.votingplugin.listeners;

import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerQuitEvent;

import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * The Class PlayerJoinEvent.
 */
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

	private static boolean isBlank(String s) {
		return s == null || s.trim().isEmpty() || "null".equalsIgnoreCase(s.trim());
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

		if (player != null && player.isOp() && plugin.isYmlError()) {
			user.sendMessage("&cVotingPlugin: Detected yml error, please check console for details");
		}

		if (hasData) {
			// give offline vote (if they voted offline)
			user.offVote();
		} else {
			plugin.debug("No data detected for " + user.getUUID() + "/" + user.getPlayerName());
		}

		user.loginRewards();

		plugin.getPlaceholders().onUpdate(user, true);

		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.getBungeeHandler().getGlobalMessageHandler().sendMessage(VotingPluginWire.login(user.getPlayerName(),
					user.getUUID(), plugin.getBungeeSettings().getServer()));
		}
	}

	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPlayerQuit(PlayerQuitEvent event) {
		if (plugin == null || !plugin.isEnabled() || event == null) {
			return;
		}

		final Player player = event.getPlayer();
		if (player == null) {
			return;
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
				plugin.getPlaceholders().onLogout(user);
			}
		});
	}
}
