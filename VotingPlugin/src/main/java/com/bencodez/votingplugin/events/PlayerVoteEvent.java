package com.bencodez.votingplugin.events;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;
import lombok.Setter;

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
	private VoteTotalsSnapshot bungeeTextTotals;

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

	/** Stable delivery identity supplied by proxy transports and durable retries. */
	@Getter
	@Setter
	private UUID voteId;

	@Getter
	@Setter
	private boolean accountingAdmissionFailed;

	/** Processing began but did not reach the post-vote completion boundary. */
	@Getter
	@Setter
	private boolean processingFailed;

	/** A non-idempotent effect may already have run, so automatic replay is unsafe. */
	@Getter
	@Setter
	private boolean replayUnsafe;

	/** The durable producer will retire the replay fence after its own acknowledgement. */
	@Getter
	@Setter
	private boolean deferredDeliveryCompletion;

	private final CompletableFuture<PlayerVoteEvent> processingCompletion = new CompletableFuture<>();

	@Getter
	private volatile boolean processingPending;

	/**
	 * Completion signal for producers that fire this event from the platform thread.
	 * Processing is moved to the vote executor in that case, so callers must not
	 * interpret a synchronous return from Bukkit event dispatch as vote completion.
	 */
	public CompletionStage<PlayerVoteEvent> getProcessingCompletion() {
		return processingCompletion.minimalCompletionStage();
	}

	/** Marks asynchronous vote processing as admitted but not yet complete. */
	public void markProcessingPending() {
		processingPending = true;
	}

	/** Publishes the final processing flags to an asynchronous event producer. */
	public void completeProcessing() {
		processingPending = false;
		processingCompletion.complete(this);
	}

	public boolean isProcessingIncomplete() {
		return accountingAdmissionFailed || processingFailed;
	}

	/**
	 * Constructs a new PlayerVoteEvent.
	 *
	 * @param voteSite the vote site
	 * @param voteUsername the username of the voter
	 * @param serviceSite the service site name
	 * @param realVote whether this is a real vote
	 */
	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote) {
		this(voteSite, voteUsername, serviceSite, realVote, true);
	}

	/**
	 * Constructs a vote event with an explicit Bukkit dispatch contract.
	 *
	 * @param voteSite the vote site
	 * @param voteUsername the username of the voter
	 * @param serviceSite the service site name
	 * @param realVote whether this is a real vote
	 * @param asynchronous whether Bukkit will dispatch the event off its primary thread
	 */
	public PlayerVoteEvent(VoteSite voteSite, String voteUsername, String serviceSite, boolean realVote,
			boolean asynchronous) {
		super(asynchronous);
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
