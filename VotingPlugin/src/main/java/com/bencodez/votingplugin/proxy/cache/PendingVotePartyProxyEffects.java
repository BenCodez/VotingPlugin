package com.bencodez.votingplugin.proxy.cache;

import java.util.List;

/** Durable, ordered proxy-side effects for one committed HTTP vote party. */
public record PendingVotePartyProxyEffects(String broadcast, List<String> commands) {
	public static final int MAX_COMMANDS = 1_024;
	public static final int MAX_TOTAL_CHARACTERS = 262_144;

	public PendingVotePartyProxyEffects {
		broadcast = broadcast == null ? "" : broadcast;
		commands = commands == null ? List.of() : List.copyOf(commands);
		if (commands.size() > MAX_COMMANDS) {
			throw new IllegalArgumentException("Too many pending vote-party proxy commands");
		}
		long characters = broadcast.length();
		if (characters > MAX_TOTAL_CHARACTERS) {
			throw new IllegalArgumentException("Pending vote-party proxy effects are too large");
		}
		for (String command : commands) {
			if (command == null) throw new IllegalArgumentException("Null vote-party proxy command");
			characters += command.length();
			if (characters > MAX_TOTAL_CHARACTERS) {
				throw new IllegalArgumentException("Pending vote-party proxy effects are too large");
			}
		}
	}

	public static PendingVotePartyProxyEffects empty() {
		return new PendingVotePartyProxyEffects("", List.of());
	}

	public boolean isEmpty() {
		return broadcast.isEmpty() && commands.isEmpty();
	}
}
