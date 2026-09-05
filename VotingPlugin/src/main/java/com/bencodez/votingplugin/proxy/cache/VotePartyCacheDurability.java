package com.bencodez.votingplugin.proxy.cache;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Verifies that the cache library actually persisted the vote-party transaction. */
public final class VotePartyCacheDurability {
	private VotePartyCacheDurability() { }

	public static void saveAndVerify(Path file, IVoteCache cache) throws IOException {
		Map<String, Set<String>> expectedPending = pending(cache);
		int expectedVotes = cache.getVotePartyCurrentVotes();
		int expectedIncrease = cache.getVotePartyInreaseVotesRequired();
		cache.save();
		DurableFiles.forceFile(file);
		DurableFiles.forceDirectory(file.toAbsolutePath().normalize().getParent());
		try {
			JsonObject root = JsonParser.parseString(Files.readString(file, StandardCharsets.UTF_8)).getAsJsonObject();
			JsonObject voteParty = object(root.get("VoteParty"));
			if (integer(voteParty, "CurrentVotes") != expectedVotes
					|| integer(voteParty, "IncreaseVotes") != expectedIncrease
					|| !pending(voteParty).equals(expectedPending))
				throw new IOException("Vote-party cache state was not persisted");
		} catch (IOException failure) {
			throw failure;
		} catch (RuntimeException invalid) {
			throw new IOException("Vote-party cache state is unreadable", invalid);
		}
	}

	private static Map<String, Set<String>> pending(IVoteCache cache) {
		Map<String, Set<String>> pending = new HashMap<>();
		var servers = cache.getPendingVotePartyRewardServers();
		if (servers == null) return pending;
		for (String server : servers) {
			var ids = cache.getPendingVotePartyRewardIds(server);
			if (ids != null && !ids.isEmpty()) pending.put(server, new HashSet<>(ids));
		}
		return pending;
	}

	private static Map<String, Set<String>> pending(JsonObject voteParty) {
		Map<String, Set<String>> pending = new HashMap<>();
		JsonObject encodedServers = object(voteParty.get("PendingRewards"));
		for (Map.Entry<String, JsonElement> server : encodedServers.entrySet()) {
			String serverId = new String(Base64.getUrlDecoder().decode(server.getKey()), StandardCharsets.UTF_8);
			JsonObject rewards = object(server.getValue());
			Set<String> ids = new HashSet<>();
			for (Map.Entry<String, JsonElement> reward : rewards.entrySet())
				if (reward.getValue().isJsonPrimitive() && reward.getValue().getAsBoolean()) ids.add(reward.getKey());
			if (!ids.isEmpty()) pending.put(serverId, ids);
		}
		return pending;
	}

	private static JsonObject object(JsonElement element) {
		return element == null || element.isJsonNull() ? new JsonObject() : element.getAsJsonObject();
	}

	private static int integer(JsonObject object, String name) {
		JsonElement value = object.get(name);
		return value == null || value.isJsonNull() ? 0 : value.getAsInt();
	}
}
