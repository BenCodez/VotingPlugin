package com.bencodez.votingplugin.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerReceivePointsEvent extends Event {

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
	private VotingPluginUser player;

	@Getter
	@Setter
	private int points;

	@Getter
	@Setter
	private boolean cancelled;

	@Getter
	@Setter
	private String serviceSite = "";

	public PlayerReceivePointsEvent(VotingPluginUser user, int points) {
		super(true);
		this.player = user;
		this.points = points;
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
