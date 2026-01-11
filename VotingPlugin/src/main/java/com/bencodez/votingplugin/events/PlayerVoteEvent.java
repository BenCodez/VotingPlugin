package com.bencodez.votingplugin.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.proxy.BungeeMessageData;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

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
	private boolean addTotals = true;

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
	private String player;

	@Getter
	@Setter
	private boolean realVote = true;

	@Getter
	@Setter
	private String serviceSite = "";

	@Getter
	@Setter
	private long time;

	@Getter
	@Setter
	private VoteSite voteSite;

	@Getter
	@Setter
	private VotingPluginUser votingPluginUser;

	@Getter
	@Setter
	private boolean wasOnline;

	@Getter
	@Setter
	private boolean broadcast = true;

	@Getter
	@Setter
	private int voteNumber = 1;

	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote) {
		super(true);
		this.player = voteUsername;
		this.voteSite = voteSite;
		this.realVote = realVote;
		this.serviceSite = serviceSite;
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
