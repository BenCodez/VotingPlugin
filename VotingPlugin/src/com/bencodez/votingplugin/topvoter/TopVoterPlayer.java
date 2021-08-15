package com.bencodez.votingplugin.topvoter;

import java.util.UUID;

import org.bukkit.inventory.ItemStack;

import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

public class TopVoterPlayer {
	@Getter
	@Setter
	private String playerName;
	@Getter
	@Setter
	private UUID uuid;

	public TopVoterPlayer(UUID uuid, String playerName) {
		this.uuid = uuid;
		this.playerName = playerName;
	}

	public ItemStack getPlayerHead() {
		return PlayerUtils.getInstance().getPlayerSkull(getPlayerName(), false);
	}

	public VotingPluginUser getUser() {
		return UserManager.getInstance().getVotingPluginUser(getUuid(), getPlayerName());
	}
}
