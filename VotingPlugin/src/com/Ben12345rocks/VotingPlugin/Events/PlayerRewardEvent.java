package com.Ben12345rocks.VotingPlugin.Events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerRewardEvent extends Event {

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

	private Reward reward;

	/** The cancelled. */
	private boolean cancelled;

	/**
	 * Instantiates a new player reward event
	 *
	 * @param voteSite
	 *            the vote site
	 * @param player
	 *            the player
	 */
	public PlayerRewardEvent(Reward reward, User player) {
		super();
		setPlayer(player);
		setReward(reward);
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

	public Reward getReward() {
		return reward;
	}

	public void setReward(Reward reward) {
		this.reward = reward;
	}

}
