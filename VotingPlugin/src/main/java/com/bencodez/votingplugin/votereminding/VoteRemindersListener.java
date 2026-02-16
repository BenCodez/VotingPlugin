package com.bencodez.votingplugin.votereminding;

import java.util.HashMap;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerQuitEvent;

import com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderType;

/**
 * Listener for vote reminder events.
 */
public class VoteRemindersListener implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Constructs a new VoteRemindersListener.
	 *
	 * @param plugin the main plugin instance
	 */
	public VoteRemindersListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Handles player post vote events for reminder tracking.
	 *
	 * @param event the player post vote event
	 */
	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onPostVote(PlayerPostVoteEvent event) {
		plugin.getVoteRemindersManager().onVoteCast(event.getUser(), event.getVoteSite().getDisplayName());
	}

	/**
	 * Handles player login events for reminder checks.
	 *
	 * @param event the advanced core login event
	 */
	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onLogin(AdvancedCoreLoginEvent event) {
		plugin.getVoteRemindersManager().onJoin(event.getPlayer(),
				plugin.getVotingPluginUserManager().getVotingPluginUser(event.getUser()));
	}

	/**
	 * Handles player quit events for reminder cleanup.
	 *
	 * @param event the player quit event
	 */
	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onQuit(PlayerQuitEvent event) {
		plugin.getVoteRemindersManager().onQuit(event.getPlayer());
	}

	/**
	 * Handles cooldown end events for all sites.
	 *
	 * @param event the player vote cooldown end event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		HashMap<String, String> placeholders = new HashMap<>();
		plugin.getVoteRemindersManager().onCooldownTrigger(event.getPlayer(), VoteReminderType.COOLDOWN_END_ALL_SITES,
				placeholders);
	}

	/**
	 * Handles cooldown end events for specific sites.
	 *
	 * @param event the player vote site cooldown end event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteSiteCoolDownEndEvent event) {
		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("votesite", event.getSite().getDisplayName());
		placeholders.put("votesite_id", event.getSite().getKey());
		plugin.getVoteRemindersManager().onCooldownTrigger(event.getPlayer(), VoteReminderType.COOLDOWN_END_ANY_SITE,
				placeholders);

	}

}
