
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

public class VotingPluginProxyVoteCacheTest {
	private VotingPluginProxy proxy;
	private VotingPluginProxyConfig config;
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes;
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes;

	@BeforeEach
	public void setUp() {
		proxy = mock(VotingPluginProxy.class, CALLS_REAL_METHODS);
		config = mock(VotingPluginProxyConfig.class);
		when(proxy.getConfig()).thenReturn(config);
		cachedOnlineVotes = new ConcurrentHashMap<>();
		cachedVotes = new ConcurrentHashMap<>();
		proxy.getVoteCacheHandler().setCachedOnlineVotes(cachedOnlineVotes);
		proxy.getVoteCacheHandler().setCachedVotes(cachedVotes);
	}

//	@Test
//	void checkVoteCacheTime_removesExpiredVotesFromCachedOnlineVotes() {
//		when(config.getVoteCacheTime()).thenReturn(1); // 1 day
//		long currentTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
//		long expiredTime = currentTime - (2 * 24 * 60 * 60 * 1000); // 2 days ago
//
//		ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
//		votes.add(new OfflineBungeeVote("player1", "uuid1", "service1", expiredTime, true, "text1"));
//		cachedOnlineVotes.put("server1", votes);
//
//		proxy.checkVoteCacheTime();
//
//		assertTrue(cachedOnlineVotes.get("server1").isEmpty());
//	}
//
//	@Test
//	void checkVoteCacheTime_doesNotRemoveNonExpiredVotesFromCachedOnlineVotes() {
//		when(config.getVoteCacheTime()).thenReturn(1); // 1 day
//		long currentTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
//		long validTime = currentTime - (12 * 60 * 60 * 1000); // 12 hours ago
//
//		ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
//		votes.add(new OfflineBungeeVote("player1", "uuid1", "service1", validTime, true, "text1"));
//		cachedOnlineVotes.put("server1", votes);
//
//		proxy.checkVoteCacheTime();
//
//		assertFalse(cachedOnlineVotes.get("server1").isEmpty());
//	}
//
//	@Test
//	void checkVoteCacheTime_removesExpiredVotesFromCachedVotes() {
//		when(config.getVoteCacheTime()).thenReturn(1); // 1 day
//		long currentTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
//		long expiredTime = currentTime - (2 * 24 * 60 * 60 * 1000); // 2 days ago
//
//		ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
//		votes.add(new OfflineBungeeVote("player1", "uuid1", "service1", expiredTime, true, "text1"));
//		cachedVotes.put("server1", votes);
//
//		proxy.checkVoteCacheTime();
//
//		assertTrue(cachedVotes.get("server1").isEmpty());
//	}
//
//	@Test
//	void checkVoteCacheTime_doesNotRemoveNonExpiredVotesFromCachedVotes() {
//		when(config.getVoteCacheTime()).thenReturn(1); // 1 day
//		long currentTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
//		long validTime = currentTime - (12 * 60 * 60 * 1000); // 12 hours ago
//
//		ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
//		votes.add(new OfflineBungeeVote("player1", "uuid1", "service1", validTime, true, "text1"));
//		cachedVotes.put("server1", votes);
//
//		proxy.checkVoteCacheTime();
//
//		assertFalse(cachedVotes.get("server1").isEmpty());
//	}
}
