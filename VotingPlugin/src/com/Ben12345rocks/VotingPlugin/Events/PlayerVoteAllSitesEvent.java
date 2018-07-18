package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.VoteUser;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerVoteAllSitesEvent extends Event {

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

	private VoteUser user;

	/** The cancelled. */
	private boolean cancelled;

	public PlayerVoteAllSitesEvent(VoteUser user) {
		super();
		setUser(user);
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
	 * @return the user
	 */
	public VoteUser getUser() {
		return user;
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
	 * @param user
	 *            the user to set
	 */
	public void setUser(VoteUser user) {
		this.user = user;
	}

}
