package com.bencodez.votingplugin.proxy.cache;

import java.nio.file.Path;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;

/** Forces and reads back emergency JSON journal entries before they are exposed in memory. */
public final class VoteCacheDurability {
	private VoteCacheDurability() { }

	public static boolean saveAndVerifyServerVote(IVoteCache cache, String server, int index,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, () -> cache.getServerVotes(server, String.valueOf(index)), expected);
	}

	public static boolean saveAndVerifyOnlineVote(IVoteCache cache, String uuid, int index,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, () -> cache.getOnlineVotes(uuid, String.valueOf(index)), expected);
	}

	public static boolean saveAndVerifyTimeVote(IVoteCache cache, int index, VoteTimeQueue expected) {
		return saveAndVerify(cache, () -> cache.getTimedVoteCache(String.valueOf(index)),
				data -> matches(data, expected));
	}

	private static boolean saveAndVerify(IVoteCache cache, java.util.function.Supplier<DataNode> read,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, read, data -> matches(data, expected));
	}

	private static boolean saveAndVerify(IVoteCache cache, java.util.function.Supplier<DataNode> read,
			java.util.function.Predicate<DataNode> verify) {
		if (cache == null || verify == null) return false;
		Path path = cache.getStoragePath();
		if (path == null) return false;
		try {
			try {
				cache.saveDurably();
			} catch (DurableFiles.PublishedException published) {
				// The new file is already active. Verify and keep its matching in-memory
				// state rather than rolling back and later overwriting the publication.
			}
			cache.reload();
			return verify.test(read.get());
		} catch (RuntimeException | java.io.IOException failure) {
			return false;
		}
	}

	private static boolean matches(DataNode data, VoteTimeQueue expected) {
		if (expected == null || data == null || !data.isObject() || !data.has("UUID")
				|| !data.has("Service") || !data.has("Time")) return false;
		if (!expected.getUuid().equals(data.get("UUID").asString())
				|| !expected.getService().equals(data.get("Service").asString())
				|| expected.getTime() != data.get("Time").asLong()) return false;
		if (expected.getVoteId() == null) return true;
		String voteId = data.has("VoteId") ? data.get("VoteId").asString()
				: data.has("VoteID") ? data.get("VoteID").asString() : "";
		return expected.getVoteId().toString().equals(voteId);
	}

	private static boolean matches(DataNode data, OfflineBungeeVote expected) {
		if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service") || !data.has("Time"))
			return false;
		if (!expected.getUuid().equals(data.get("UUID").asString())
				|| !expected.getService().equals(data.get("Service").asString())
				|| expected.getTime() != data.get("Time").asLong()) return false;
		if (expected.getVoteId() == null) return true;
		String voteId = data.has("VoteId") ? data.get("VoteId").asString()
				: data.has("VoteID") ? data.get("VoteID").asString() : "";
		return expected.getVoteId().toString().equals(voteId);
	}
}
