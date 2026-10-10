package com.bencodez.votingplugin.events;

import java.util.UUID;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;
import lombok.Setter;

public class PlayerPostVoteEvent extends Event {
	/** Local recovered ingress cannot prove ordering relative to a new guide. */
	@Getter @Setter private boolean unconfirmedLocalSessionDelivery;

	/** Backend-local ingress order forwarded by the accepted pipeline. Zero means unknown. */
	@Getter @Setter private long backendObservationOrder;
	@Getter @Setter private boolean proxyQueueClassificationKnown;
	@Getter @Setter private boolean queuedProxyVote;
	@Getter @Setter private boolean proxySessionDelivery;
	/** Guide-only provenance; independent of normal reward queue/delay policy. */
	@Getter @Setter private boolean unconfirmedProxySessionDelivery;
	/** Fresh local ingress with no supplied historical occurrence timestamp. */
	@Getter @Setter private boolean liveLocalSessionDelivery;

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
	private VoteTotalsSnapshot bungeeTextTotals;

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
	private long voteTime = 0;

	@Getter
	@Setter
	private boolean cached;

	@Getter
	@Setter
	private String service;

	@Getter
	@Setter
	private UUID uuid;
	@Getter
	@Setter
	private String playerName;

	@Getter
	@Setter
	private UUID voteUUID;

	/**
	 * Constructs a new PlayerPostVoteEvent.
	 *
	 * @param voteSite the vote site
	 * @param user the voting plugin user
	 * @param realVote whether this is a real vote
	 * @param forceBungee whether to force Bungee processing
	 * @param voteTime the vote timestamp
	 * @param cached whether the vote was cached
	 * @param service the service name
	 * @param uuid the player UUID
	 * @param playerName the player name
	 * @param voteUUID the vote UUID
	 */
	public PlayerPostVoteEvent(VoteSite voteSite, VotingPluginUser user, boolean realVote, boolean forceBungee,
			long voteTime, boolean cached, String service, UUID uuid, String playerName, UUID voteUUID) {
		super(true);
		this.user = user;
		this.voteSite = voteSite;
		this.realVote = realVote;
		this.forceBungee = forceBungee;
		this.voteTime = voteTime;
		this.cached = cached;
		this.service = service;
		this.uuid = uuid;
		this.playerName = playerName;
		this.voteUUID = voteUUID;
	}

	@Override
	public HandlerList getHandlers() {
		return handlers;
	}

}
