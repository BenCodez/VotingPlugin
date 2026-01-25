package com.bencodez.votingplugin.proxy.broadcast;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

/**
 * Decides where vote broadcasts should execute (proxy-side routing only).
 *
 * Backend servers still decide WHAT to broadcast (VoteBroadcast.Type/Format).
 *
 * QUEUE behavior:
 * - This class returns no immediate targets when player is offline and OfflineMode=QUEUE.
 * - The "queue" is the existing online vote cache (addOnlineVote + checkOnlineVotes on login).
 */
public final class ProxyBroadcastDecider {

	private final Supplier<VotingPluginProxyConfig> config;
	private final Supplier<Set<String>> allServers;
	private final Predicate<String> isServerValid;
	private final Predicate<String> isBlockedServer;

	public ProxyBroadcastDecider(Supplier<VotingPluginProxyConfig> config,
			Supplier<Set<String>> allServers,
			Predicate<String> isServerValid,
			Predicate<String> isBlockedServer) {
		this.config = config;
		this.allServers = allServers;
		this.isServerValid = isServerValid;
		this.isBlockedServer = isBlockedServer;
	}

	/**
	 * Compute which backend servers should broadcast for this vote.
	 *
	 * @param playerOnline whether the voting player is currently online
	 * @param currentPlayerServer the server the player is on (only used when playerOnline=true)
	 */
	public Set<String> resolveTargets(boolean playerOnline, String currentPlayerServer) {
		VotingPluginProxyConfig cfg = config.get();
		if (cfg == null || !cfg.getProxyBroadcastEnabled()) {
			return Collections.emptySet();
		}

		ScopeMode scope = ScopeMode.parse(cfg.getProxyBroadcastScopeMode(), ScopeMode.ALL_SERVERS);
		List<String> scopeServers = cfg.getProxyBroadcastScopeServers();

		Set<String> eligible = eligibleServers();

		switch (scope) {
		case ALL_SERVERS:
			return eligible;

		case SERVERS:
			return intersect(scopeServers, eligible);

		case ALL_EXCEPT:
			return subtract(scopeServers, eligible);

		case PLAYER_SERVER:
		default:
			if (playerOnline) {
				if (currentPlayerServer != null && eligible.contains(currentPlayerServer)) {
					Set<String> out = new LinkedHashSet<>();
					out.add(currentPlayerServer);
					return out;
				}
				return Collections.emptySet();
			}

			// player offline + PLAYER_SERVER scope
			OfflineMode offlineMode = OfflineMode.parse(cfg.getProxyBroadcastOfflineMode(), OfflineMode.QUEUE);

			if (offlineMode == OfflineMode.FORWARD) {
				// FORWARD uses OfflineForward.Servers (list only; mode is implicit SERVERS)
				return intersect(cfg.getProxyBroadcastOfflineForwardServers(), eligible);
			}

			// NONE or QUEUE => no immediate targets
			// QUEUE is handled by existing vote cache + checkOnlineVotes() on login
			return Collections.emptySet();
		}
	}

	/**
	 * Convenience check for "should this specific server broadcast?"
	 */
	public boolean shouldBroadcast(String server, Set<String> targets) {
		return targets != null && server != null && targets.contains(server);
	}

	private Set<String> eligibleServers() {
		Set<String> out = new LinkedHashSet<>();
		for (String s : allServers.get()) {
			if (s == null || s.isEmpty()) {
				continue;
			}
			if (!isServerValid.test(s)) {
				continue;
			}
			if (isBlockedServer.test(s)) {
				continue;
			}
			out.add(s);
		}
		return out;
	}

	private static Set<String> intersect(List<String> list, Set<String> eligible) {
		if (list == null || list.isEmpty() || eligible.isEmpty()) {
			return Collections.emptySet();
		}
		Set<String> out = new LinkedHashSet<>();
		for (String s : list) {
			if (s == null) {
				continue;
			}
			String v = s.trim();
			if (!v.isEmpty() && eligible.contains(v)) {
				out.add(v);
			}
		}
		return out;
	}

	private static Set<String> subtract(List<String> list, Set<String> eligible) {
		if (eligible.isEmpty()) {
			return Collections.emptySet();
		}
		Set<String> out = new LinkedHashSet<>(eligible);
		if (list == null || list.isEmpty()) {
			return out;
		}
		for (String s : list) {
			if (s == null) {
				continue;
			}
			String v = s.trim();
			if (!v.isEmpty()) {
				out.remove(v);
			}
		}
		return out;
	}
}
