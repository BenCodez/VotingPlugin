package com.bencodez.votingplugin.topvoter;

import java.util.UUID;

import org.bukkit.inventory.ItemStack;

import com.bencodez.advancedcore.api.misc.PlayerUtils;

import lombok.Getter;
import lombok.Setter;

public class TopVoterPlayer {
	@Getter
	@Setter
	private UUID uuid;
	@Getter
	@Setter
	private String playerName;

	public TopVoterPlayer(UUID uuid, String playerName) {
		this.uuid = uuid;
		this.playerName = playerName;
	}
	
	public ItemStack getPlayerHead() {
		return PlayerUtils.getInstance().getPlayerSkull(getPlayerName(), false);
	}
}
