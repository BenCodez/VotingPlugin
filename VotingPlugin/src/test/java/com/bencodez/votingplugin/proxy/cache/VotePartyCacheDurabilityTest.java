package com.bencodez.votingplugin.proxy.cache;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class VotePartyCacheDurabilityTest {
	@TempDir Path directory;

	@Test
	void detectsWhenCacheSaveReturnsWithoutUpdatingTheFile() throws Exception {
		IVoteCache cache = mock(IVoteCache.class);
		when(cache.getPendingVotePartyRewardServers()).thenReturn(Set.of("lobby"));
		when(cache.getPendingVotePartyRewardIds("lobby")).thenReturn(Set.of("delivery-id"));
		when(cache.getVotePartyCurrentVotes()).thenReturn(3);
		when(cache.getVotePartyInreaseVotesRequired()).thenReturn(2);
		when(cache.getPendingVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		when(cache.getQuarantinedVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		Path file = directory.resolve("votecache.json");
		Files.writeString(file, "{\"VoteParty\":{\"CurrentVotes\":3,\"IncreaseVotes\":2}}");

		assertThrows(java.io.IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
	}

	@Test
	void detectsWhenPendingProxyEffectsAreMissingFromTheFile() throws Exception {
		IVoteCache cache = mock(IVoteCache.class);
		when(cache.getPendingVotePartyRewardServers()).thenReturn(Set.of());
		when(cache.getVotePartyCurrentVotes()).thenReturn(3);
		when(cache.getVotePartyInreaseVotesRequired()).thenReturn(2);
		when(cache.getPendingVotePartyProxyEffects())
				.thenReturn(new PendingVotePartyProxyEffects("Party time", java.util.List.of("reward all")));
		when(cache.getQuarantinedVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		Path file = directory.resolve("votecache.json");
		Files.writeString(file, "{\"VoteParty\":{\"CurrentVotes\":3,\"IncreaseVotes\":2}}");

		assertThrows(java.io.IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
	}

	@Test
	void detectsWhenQuarantinedProxyEffectsAreMissingFromTheFile() throws Exception {
		IVoteCache cache = mock(IVoteCache.class);
		when(cache.getPendingVotePartyRewardServers()).thenReturn(Set.of());
		when(cache.getVotePartyCurrentVotes()).thenReturn(3);
		when(cache.getVotePartyInreaseVotesRequired()).thenReturn(2);
		when(cache.getPendingVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		when(cache.getQuarantinedVotePartyProxyEffects())
				.thenReturn(new PendingVotePartyProxyEffects("", java.util.List.of("uncertain reward")));
		Path file = directory.resolve("votecache.json");
		Files.writeString(file, "{\"VoteParty\":{\"CurrentVotes\":3,\"IncreaseVotes\":2}}");

		assertThrows(java.io.IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
	}
}
