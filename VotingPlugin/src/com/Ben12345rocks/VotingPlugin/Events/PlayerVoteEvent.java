package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerVoteEvent extends Event {

	/** The Constant handlers. */
	private static final HandlerList handlers = new HandlerList();

	/**
	 * Gets the handler list.
	 *
	 * @return the handler list
	 */
	public static HandlerList getHandlerList() {
		return handlers;
	}

	/** The player. */
	private User player;

	/** The vote site. */
	private VoteSite voteSite;

	/** The cancelled. */
	private boolean cancelled;

	/**
	 * Instantiates a new player vote event.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param player
	 *            the player
	 */
	public PlayerVoteEvent(VoteSite voteSite, User player) {
		super();
		setPlayer(player);
		setVoteSite(voteSite);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.bukkit.event.Event#getHandlers()
	 */
	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

	/**
	 * Gets the player.
	 *
	 * @return the player
	 */
	public User getPlayer() {
		return player;
	}

	/**
	 * Gets the vote site.
	 *
	 * @return the vote site
	 */
	public VoteSite getVoteSite() {
		return voteSite;
	}

	/**
	 * Checks if is cancelled.
	 *
	 * @return true, if is cancelled
	 */
	public boolean isCancelled() {
		return cancelled;
	}

	/**
	 * Sets the cancelled.
	 *
	 * @param bln
	 *            the new cancelled
	 */
	public void setCancelled(boolean bln) {
		cancelled = bln;
	}

	/**
	 * Sets the player.
	 *
	 * @param player
	 *            the new player
	 */
	public void setPlayer(User player) {
		this.player = player;
	}

	/**
	 * Sets the vote site.
	 *
	 * @param voteSite
	 *            the new vote site
	 */
	public void setVoteSite(VoteSite voteSite) {
		this.voteSite = voteSite;
	}

}
