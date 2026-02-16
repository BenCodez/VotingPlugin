package com.bencodez.votingplugin.topvoter;

import java.util.UUID;

import org.bukkit.inventory.ItemStack;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a top voter player with ranking information.
 */
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

	/**
	 * Constructs a new top voter player.
	 * @param uuid the player UUID
	 * @param playerName the player name
	 * @param lastVoteTime the last vote time
	 */
	public TopVoterPlayer(UUID uuid, String playerName, Long lastVoteTime) {
		this.uuid = uuid;
		this.playerName = playerName;
		this.lastVoteTime = lastVoteTime;
	}

	/**
	 * Gets the player head item stack.
	 * @return the player head item
	 */
	public ItemStack getPlayerHead() {
		return VotingPluginMain.plugin.getSkullCacheHandler().getSkull(uuid, playerName);
	}

	/**
	 * Gets the VotingPluginUser for this top voter.
	 * @return the voting plugin user
	 */
	public VotingPluginUser getUser() {
		return VotingPluginMain.plugin.getVotingPluginUserManager().getVotingPluginUser(getUuid(), getPlayerName());
	}
}
