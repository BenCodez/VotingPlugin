package com.bencodez.votingplugin.proxy;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents an offline bungee vote.
 */
public class OfflineBungeeVote {

	@Getter
	private String playerName;
	@Getter
	private boolean realVote;
	@Getter
	private String service;
	@Getter
	@Setter
	private String text;
	@Getter
	private long time;
	@Getter
	private String uuid;
	@Getter
	private UUID voteId;
	@Getter
	@Setter
	private boolean broadcastForwarded;
	@Getter
	@Setter
	private boolean proxyBroadcastHandled;
	@Getter
	private Set<String> broadcastTargets;
	@Getter
	private Set<String> broadcastForwardedServers;
	@Getter
	@Setter
	private boolean rewardDelivered;
	@Getter
	@Setter
	private boolean deliveryStateDirty;

	/**
	 * Constructor with UUID voteId.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this(voteId, playerName, uuid, service, time, realVote, text, false);
	}

	/**
	 * Constructor with UUID voteId and proxy broadcast delivery state.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded whether the proxy already forwarded the broadcast
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded) {
		this(voteId, playerName, uuid, service, time, realVote, text, broadcastForwarded, false,
				Collections.emptySet(), Collections.emptySet(), false);
	}

	/**
	 * Constructor with full proxy broadcast delivery state.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded legacy aggregate forwarded state
	 * @param proxyBroadcastHandled whether standalone proxy routing was selected
	 * @param broadcastTargets original standalone broadcast targets
	 * @param broadcastForwardedServers targets that accepted standalone delivery
	 * @param rewardDelivered whether the cached reward vote was already delivered
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
		this.voteId = voteId;
		this.broadcastForwarded = broadcastForwarded;
		this.proxyBroadcastHandled = proxyBroadcastHandled;
		setBroadcastTargets(broadcastTargets);
		setBroadcastForwardedServers(broadcastForwardedServers);
		this.rewardDelivered = rewardDelivered;
	}
	
	/**
	 * Constructor with String voteId.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this(voteId, playerName, uuid, service, time, realVote, text, false);
	}

	/**
	 * Constructor with String voteId and proxy broadcast delivery state.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded whether the proxy already forwarded the broadcast
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded);
	}

	/**
	 * Constructor with String voteId and full proxy broadcast delivery state.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded legacy aggregate forwarded state
	 * @param proxyBroadcastHandled whether standalone proxy routing was selected
	 * @param broadcastTargets original standalone broadcast targets
	 * @param broadcastForwardedServers targets that accepted standalone delivery
	 * @param rewardDelivered whether the cached reward vote was already delivered
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded,
				proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, rewardDelivered);
	}

	private static UUID parseVoteId(String voteId) {
		return voteId == null || voteId.isEmpty() ? null : UUID.fromString(voteId);
	}

	/**
	 * Replaces the original standalone broadcast targets.
	 * @param targets target servers
	 */
	public void setBroadcastTargets(Set<String> targets) {
		broadcastTargets = targets == null ? new LinkedHashSet<>() : new LinkedHashSet<>(targets);
	}

	/**
	 * Replaces the targets that accepted standalone broadcast delivery.
	 * @param forwardedServers delivered target servers
	 */
	public void setBroadcastForwardedServers(Set<String> forwardedServers) {
		broadcastForwardedServers = forwardedServers == null ? new LinkedHashSet<>()
				: new LinkedHashSet<>(forwardedServers);
	}

	/**
	 * Checks whether every original standalone target accepted delivery.
	 * @return true when standalone routing is complete
	 */
	public boolean isProxyBroadcastComplete() {
		return proxyBroadcastHandled && broadcastForwardedServers.containsAll(broadcastTargets);
	}

	/**
	 * Checks whether this cached vote still needs to broadcast on a server.
	 * @param server backend server receiving the cached vote
	 * @return true when this server is an original undelivered target
	 */
	public boolean needsBroadcastOn(String server) {
		if (!proxyBroadcastHandled) {
			return !broadcastForwarded;
		}
		return server != null && broadcastTargets.contains(server) && !broadcastForwardedServers.contains(server);
	}

	/**
	 * Encodes the original broadcast targets for cache storage.
	 * @return encoded target set
	 */
	public String encodeBroadcastTargets() {
		return VoteTimeQueue.encodeBroadcastServers(broadcastTargets);
	}

	/**
	 * Encodes delivered broadcast targets for cache storage.
	 * @return encoded delivered target set
	 */
	public String encodeBroadcastForwardedServers() {
		return VoteTimeQueue.encodeBroadcastServers(broadcastForwardedServers);
	}

	@Override
	public String toString() {
		return "VoteCache:" + playerName + "/" + uuid + "/" + service + "/" + time + "/" + realVote + "/" + text + "/"
				+ voteId + "/" + broadcastForwarded + "/" + proxyBroadcastHandled + "/" + broadcastTargets + "/"
				+ broadcastForwardedServers + "/" + rewardDelivered;
	}

}
