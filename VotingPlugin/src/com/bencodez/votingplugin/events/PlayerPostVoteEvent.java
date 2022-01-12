package com.bencodez.votingplugin.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.bungee.BungeeMessageData;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlayerVoteEvent.
 */
public class PlayerPostVoteEvent extends Event {

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
	private boolean bungee = false;

	@Getter
	@Setter
	private BungeeMessageData bungeeTextTotals;

	@Getter
	@Setter
	private boolean cancelled;

	@Getter
	@Setter
	private boolean forceBungee = false;

	@Getter
	@Setter
	private VotingPluginUser user;

	@Getter
	@Setter
	private boolean realVote = true;

	@Getter
	@Setter
	private VoteSite voteSite;

	@Getter
	@Setter
	private VotingPluginUser votingPluginUser;

	public PlayerPostVoteEvent(VoteSite voteSite, VotingPluginUser user, boolean realVote, boolean forceBungee) {
		super(true);
		this.user = user;
		this.voteSite = voteSite;
		this.realVote = realVote;
		this.forceBungee = forceBungee;
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
