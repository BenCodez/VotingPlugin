package com.bencodez.votingplugin.events;

import java.util.UUID;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;
import lombok.Setter;

public class PlayerVoteEvent extends Event {

	/** Backend-local monotonic observation, never a timestamp from another node. */
	@Getter
	private final long backendObservationOrder = System.nanoTime();

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
	private VoteTotalsSnapshot bungeeTextTotals;

	/** Stable identity supplied by a proxy delivery, independent of totals. */
	@Getter
	@Setter
	private UUID proxyVoteId;

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

	/** Whether this is an identified queued proxy delivery. */
	@Getter
	@Setter
	private boolean queuedProxyVote;

	/** Whether the proxy explicitly classified this as a live or queued delivery. */
	@Getter
	@Setter
	private boolean proxyQueueClassificationKnown;

	/** Whether the proxy explicitly supplied its delay-validation decision. */
	@Getter
	@Setter
	private boolean proxyDelayValidationKnown;

	@Getter
	@Setter
	private boolean targetedProxyVote;

	@Getter
	@Setter
	private boolean broadcast = true;

	@Getter
	@Setter
	private int voteNumber = 1;

	/**
	 * Constructs a new PlayerVoteEvent.
	 *
	 * @param voteSite the vote site
	 * @param voteUsername the username of the voter
	 * @param serviceSite the service site name
	 * @param realVote whether this is a real vote
	 */
	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote) {
		super(true);
		this.player = voteUsername;
		this.voteSite = voteSite;
		this.realVote = realVote;
		this.serviceSite = serviceSite;
	}

	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

}
