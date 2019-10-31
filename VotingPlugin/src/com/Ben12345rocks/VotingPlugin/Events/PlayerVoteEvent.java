package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

import lombok.Getter;
import lombok.Setter;

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

	@Getter
	@Setter
	private String player;

	@Getter
	@Setter
	private VoteSite voteSite;

	@Getter
	@Setter
	private boolean cancelled;

	@Getter
	@Setter
	private boolean realVote = true;

	@Getter
	@Setter
	private String serviceSite = "";
	
	@Getter
	@Setter
	private long time;

	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote) {
		super(true);
		this.player = voteUsername;
		this.voteSite = voteSite;
		this.realVote = realVote;
		this.serviceSite = serviceSite;
	}

	/*
	 * (non-Javadoc)
	 * @see org.bukkit.event.Event#getHandlers()
	 */
	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

	@Getter
	@Setter
	private boolean bungee = false;
	
	@Getter
	@Setter
	private boolean forceBungee = false;

}
