package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class PlayerVoteEvent extends Event {
	private static final HandlerList handlers = new HandlerList();
	private User player;
	private VoteSite voteSite;
	private boolean cancelled;

	public PlayerVoteEvent(VoteSite voteSite, User player) {
		super();
		setPlayer(player);
		setVoteSite(voteSite);
	}

	public static HandlerList getHandlerList() {
		return handlers;
	}

	public User getPlayer() {
		return player;
	}

	public VoteSite getVoteSite() {
		return voteSite;
	}

	public boolean isCancelled() {
		return cancelled;
	}

	public void setCancelled(boolean bln) {
		cancelled = bln;
	}

	public void setPlayer(User player) {
		this.player = player;
	}

	public void setVoteSite(VoteSite voteSite) {
		this.voteSite = voteSite;
	}

	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

}
