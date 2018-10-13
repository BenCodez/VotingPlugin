package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

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
	private String player;

	/** The vote site. */
	private VoteSite voteSite;

	/** The cancelled. */
	private boolean cancelled;

	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote) {
		super(true);
		setPlayer(voteUsername);
		setVoteSite(voteSite);
		setRealVote(realVote);
		setServiceSite(serviceSite);
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
	public String getPlayer() {
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

	public void setPlayer(String voteUsername) {
		this.player = voteUsername;
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

	private boolean realVote = true;

	public void setRealVote(boolean b) {
		realVote = b;
	}

	/**
	 * @return the realVote
	 */
	public boolean isRealVote() {
		return realVote;
	}

	private String serviceSite = "";

	public void setServiceSite(String value) {
		serviceSite = value;
	}

	/**
	 * @return the serviceSite
	 */
	public String getServiceSite() {
		return serviceSite;
	}

}
