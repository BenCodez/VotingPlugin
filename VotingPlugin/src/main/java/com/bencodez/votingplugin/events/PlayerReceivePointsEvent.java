package com.bencodez.votingplugin.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;
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
	private boolean cancelled;

	@Getter
	@Setter
	private VotingPluginUser player;

	@Getter
	@Setter
	private int points;

	@Getter
	@Setter
	private String serviceSite = "";

	/**
	 * Constructs a new PlayerReceivePointsEvent.
	 *
	 * @param user the voting plugin user
	 * @param points the points received
	 */
	public PlayerReceivePointsEvent(VotingPluginUser user, int points) {
		super(true);
		this.player = user;
		this.points = points;
	}
	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

}
