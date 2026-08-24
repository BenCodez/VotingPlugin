package com.bencodez.votingplugin.backendproxy.voteparty;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

/**
 * Owns backend-side cached proxy vote-party state and proxy vote-party events.
 */
public class BackendVotePartySync {

	private final VotingPluginMain plugin;

	@Getter
	private int current;
	@Getter
	private int required;

	public BackendVotePartySync(VotingPluginMain plugin) {
		this.plugin = plugin;
		current = plugin.getServerData().getBungeeVotePartyCurrent();
		required = plugin.getServerData().getBungeeVotePartyRequired();
	}

	public void update(int votePartyCurrent, int votePartyRequired) {
		if (votePartyCurrent >= 0 || current == -2) {
			current = votePartyCurrent;
		}
		if (votePartyRequired >= 0 || required == -2) {
			required = votePartyRequired;
		}
		persist();
	}

	public void replace(int votePartyCurrent, int votePartyRequired) {
		current = votePartyCurrent;
		required = votePartyRequired;
		persist();
	}

	public void persist() {
		plugin.getServerData().setBungeeVotePartyCurrent(current);
		plugin.getServerData().setBungeeVotePartyRequired(required);
	}

	public void runGlobalRewards() {
		for (final String command : plugin.getBungeeSettings().getBungeeVotePartyGlobalCommands()) {
			plugin.getBukkitScheduler().runTask(plugin,
					() -> Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), command));
		}
		for (Player player : Bukkit.getOnlinePlayers()) {
			new RewardBuilder(plugin.getBungeeSettings().getData(), "BungeeVotePartyRewards").send(player);
		}
	}

	public void broadcast(String message) {
		MiscUtils.getInstance().broadcast(message);
	}
}
