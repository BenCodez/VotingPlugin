package com.bencodez.votingplugin.events;

import java.util.UUID;

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
