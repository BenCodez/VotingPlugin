package com.bencodez.votingplugin.topvoter;

import java.util.UUID;

import org.bukkit.inventory.ItemStack;

import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

public class TopVoterPlayer {
	@Getter
	@Setter
	private UUID uuid;
	@Getter
	@Setter
	private String playerName;

	@Getter
	@Setter
	private Long lastVoteTime;

	public TopVoterPlayer(UUID uuid, String playerName, Long lastVoteTime) {
		this.uuid = uuid;
		this.playerName = playerName;
		this.lastVoteTime = lastVoteTime;
	}

	public ItemStack getPlayerHead() {
		return PlayerUtils.getInstance().getPlayerSkull(getPlayerName(), false);
	}

	public VotingPluginUser getUser() {
		return VotingPluginMain.plugin.getVotingPluginUserManager().getVotingPluginUser(getUuid(), getPlayerName());
	}
}
