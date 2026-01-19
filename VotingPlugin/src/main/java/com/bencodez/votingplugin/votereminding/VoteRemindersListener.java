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

public class VoteRemindersListener implements Listener {

	private VotingPluginMain plugin;

	public VoteRemindersListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onPostVote(PlayerPostVoteEvent event) {
		plugin.getVoteRemindersManager().onVoteCast(event.getUser(), event.getVoteSite().getDisplayName());
	}

	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onLogin(AdvancedCoreLoginEvent event) {
		plugin.getVoteRemindersManager().onJoin(event.getPlayer(),
				plugin.getVotingPluginUserManager().getVotingPluginUser(event.getUser()));
	}

	@EventHandler(ignoreCancelled = true, priority = EventPriority.MONITOR)
	public void onQuit(PlayerQuitEvent event) {
		plugin.getVoteRemindersManager().onQuit(event.getPlayer());
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		HashMap<String, String> placeholders = new HashMap<>();
		plugin.getVoteRemindersManager().onCooldownTrigger(event.getPlayer(), VoteReminderType.COOLDOWN_END_ALL_SITES,
				placeholders);
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteSiteCoolDownEndEvent event) {
		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("votesite", event.getSite().getDisplayName());
		placeholders.put("votesite_id", event.getSite().getKey());
		plugin.getVoteRemindersManager().onCooldownTrigger(event.getPlayer(), VoteReminderType.COOLDOWN_END_ANY_SITE,
				placeholders);

	}

}
