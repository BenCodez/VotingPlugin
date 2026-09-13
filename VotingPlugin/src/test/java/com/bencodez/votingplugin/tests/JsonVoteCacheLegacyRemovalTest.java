package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Path;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.velocity.VelocityJsonVoteCache;

class JsonVoteCacheLegacyRemovalTest {
	@TempDir
	Path temporaryDirectory;

	@Test
	void bungeeExactRemovalHonorsLegacyVoteId() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		BungeeJsonVoteCache cache = new BungeeJsonVoteCache(plugin);
		UUID removedId = UUID.randomUUID();
		UUID retainedId = UUID.randomUUID();
		putLegacyBungeeVote(cache, "0", removedId);
		putLegacyBungeeVote(cache, "1", retainedId);

		cache.removeVote("server", vote(removedId));

		assertTrue(cache.getConf().getAsJsonObject("VoteCache").getAsJsonObject("server").get("0").isJsonNull());
		assertTrue(cache.getConf().getAsJsonObject("VoteCache").getAsJsonObject("server").get("1").isJsonObject());
	}

	@Test
	void velocityExactRemovalHonorsLegacyVoteId() {
		VelocityJsonVoteCache cache = new VelocityJsonVoteCache(
				temporaryDirectory.resolve("velocity-vote-cache.json").toFile());
		UUID removedId = UUID.randomUUID();
		UUID retainedId = UUID.randomUUID();
		putLegacyVelocityVote(cache, "0", removedId);
		putLegacyVelocityVote(cache, "1", retainedId);

		cache.removeVote("server", vote(removedId));

		assertFalse(cache.contains("VoteCache", "server", "0"));
		assertTrue(cache.contains("VoteCache", "server", "1"));
	}

	private static OfflineBungeeVote vote(UUID voteId) {
		return new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true, "totals");
	}

	private static void putLegacyBungeeVote(BungeeJsonVoteCache cache, String index, UUID voteId) {
		String root = "VoteCache.server." + index;
		cache.setString(root + ".VoteID", voteId.toString());
		cache.setString(root + ".UUID", "player-uuid");
		cache.setString(root + ".Service", "Service");
		cache.setLong(root + ".Time", 100L);
	}

	private static void putLegacyVelocityVote(VelocityJsonVoteCache cache, String index, UUID voteId) {
		cache.set(new Object[] { "VoteCache", "server", index, "VoteID" }, voteId.toString());
		cache.set(new Object[] { "VoteCache", "server", index, "UUID" }, "player-uuid");
		cache.set(new Object[] { "VoteCache", "server", index, "Service" }, "Service");
		cache.set(new Object[] { "VoteCache", "server", index, "Time" }, 100L);
	}
}
