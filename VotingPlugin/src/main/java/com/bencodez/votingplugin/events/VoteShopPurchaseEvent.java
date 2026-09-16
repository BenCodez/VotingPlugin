package com.bencodez.votingplugin.events;

import java.util.UUID;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

/**
 * Fired when a player attempts to purchase an item from VotingPlugin's
 * VoteShop.
 */
public class VoteShopPurchaseEvent extends Event {

	private static final HandlerList HANDLERS = new HandlerList();

    @Getter
	private UUID playerUuid;
	@Getter
	private String playerName;
	@Getter
	private VotingPluginUser user;

	@Getter
	private String identifier;

	@Getter
	private int cost;

	public VoteShopPurchaseEvent(UUID playerUUID, String playerName, VotingPluginUser user, String identifier,
			int cost) {
		this(playerUUID, playerName, user, identifier, cost, true);
	}

	/**
	 * Creates a purchase event with the dispatch lane made explicit. Internal
	 * entity-lane purchases are synchronous; the legacy constructor remains
	 * asynchronous for source and behavioral compatibility with external callers.
	 */
	public VoteShopPurchaseEvent(UUID playerUUID, String playerName, VotingPluginUser user, String identifier,
			int cost, boolean async) {
		super(async);
		this.playerName = playerName;
		this.user = user;
		this.playerUuid = playerUUID;
		this.identifier = identifier;
		this.cost = cost;
	}

	@Override
	public HandlerList getHandlers() {
		return HANDLERS;
	}

	public static HandlerList getHandlerList() {
		return HANDLERS;
	}
}
