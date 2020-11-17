package com.bencodez.votingplugin.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerVoteCoolDownEndEvent extends Event {

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
	private VoteSite voteSite;

	public PlayerVoteCoolDownEndEvent(VotingPluginUser user, VoteSite site) {
		super(true);
		player = user;
		voteSite = site;
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

}
