package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class PlayerVoteEvent extends Event {

	private User player;
	private VoteSite voteSite;
	private boolean cancelled;

	public PlayerVoteEvent(VoteSite voteSite, User player) {
		super();
		this.setPlayer(player);
		this.setVoteSite(voteSite);
	}

	public boolean isCancelled() {
		return cancelled;
	}

	public void setCancelled(boolean bln) {
		this.cancelled = bln;
	}

	@Override
	public HandlerList getHandlers() {
		return null;
	}

	public VoteSite getVoteSite() {
		return voteSite;
	}

	public void setVoteSite(VoteSite voteSite) {
		this.voteSite = voteSite;
	}

	public User getPlayer() {
		return player;
	}

	public void setPlayer(User player) {
		this.player = player;
	}

}
