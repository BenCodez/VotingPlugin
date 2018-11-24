package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class VotePartyEvent extends Event {

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

	@Getter
	@Setter
	private boolean cancelled;

	public VotePartyEvent() {
		super(true);
	}

	/*
	 * (non-Javadoc)
	 * @see org.bukkit.event.Event#getHandlers()
	 */
	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

}
