package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.spy;

import java.util.Collections;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.google.gson.JsonObject;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.GsonDataNode;
import com.bencodez.votingplugin.proxy.cache.ProxyOnlineVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyTimedVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/**
 * Regression tests for proxy vote identity across caches and delayed processing.
 */
public class VoteCacheHandlerVoteIdTest {
	@TempDir
	Path tempDir;

	private IVoteCache storage;
	private VoteCacheHandler handler;

	@BeforeEach
	public void setUp() {
		storage = mock(IVoteCache.class);
		when(storage.getStoragePath()).thenReturn(tempDir.resolve("vote-cache.json"));
		when(storage.getServerVotes("server")).thenReturn(Collections.emptyList());
		when(storage.getOnlineVotes("player-uuid")).thenReturn(Collections.emptyList());
		handler = newHandler(storage);
	}

	@Test
	public void serverCacheRejectsDuplicateVoteId() {
		UUID voteId = UUID.randomUUID();

		handler.addServerVote("server", vote(voteId, 100L));
		handler.addServerVote("server", vote(voteId, 101L));

		assertEquals(1, handler.getVotes("server").size());
		assertEquals(100L, handler.getVotes("server").get(0).getTime());
		verify(storage).addVote("server", 0, handler.getVotes("server").get(0));
	}

	@Test
	public void onlineCacheRejectsDuplicateVoteId() {
		UUID voteId = UUID.randomUUID();

		handler.addOnlineVote("player-uuid", vote(voteId, 100L));
		handler.addOnlineVote("player-uuid", vote(voteId, 101L));

		assertEquals(1, handler.getOnlineVotes("player-uuid").size());
		verify(storage).addVoteOnline("player-uuid", 0, handler.getOnlineVotes("player-uuid").get(0));
	}

	@Test
	public void concurrentServerDeliveriesReserveVoteIdAtomically() throws Exception {
		UUID voteId = UUID.randomUUID();

		runConcurrently(() -> handler.addServerVote("server", vote(voteId, 100L)),
				() -> handler.addServerVote("server", vote(voteId, 101L)));

		assertEquals(1, handler.getVotes("server").size());
		verify(storage, times(1)).addVote(org.mockito.ArgumentMatchers.eq("server"),
				org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
	}

	@Test
	public void concurrentOnlineDeliveriesReserveVoteIdAtomically() throws Exception {
		UUID voteId = UUID.randomUUID();

		runConcurrently(() -> handler.addOnlineVote("player-uuid", vote(voteId, 100L)),
				() -> handler.addOnlineVote("player-uuid", vote(voteId, 101L)));

		assertEquals(1, handler.getOnlineVotes("player-uuid").size());
		verify(storage, times(1)).addVoteOnline(org.mockito.ArgumentMatchers.eq("player-uuid"),
				org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
	}

	@Test
	public void multiProxyCompletionSurvivesHandlerRestart() {
		UUID voteId = UUID.randomUUID();

		assertTrue(handler.markMultiProxyVoteCompletedDurably(voteId));
		assertTrue(handler.hasMultiProxyVoteCompletion(voteId));

		VoteCacheHandler restarted = newHandler(storage);
		assertTrue(restarted.hasMultiProxyVoteCompletion(voteId));
	}

	@Test
	public void multiProxyCompletionRetirementIsDurableAndIdempotent() {
		UUID voteId = UUID.randomUUID();
		assertTrue(handler.markMultiProxyVoteCompletedDurably(voteId));

		assertTrue(handler.removeMultiProxyVoteCompletion(voteId));
		assertFalse(handler.hasMultiProxyVoteCompletion(voteId));
		assertTrue(handler.removeMultiProxyVoteCompletion(voteId));
	}

	@Test
	public void failedCompletionPublicationDoesNotEvictAnExistingFence() throws Exception {
		Path completionDirectory = tempDir.resolve("vote-cache.json.completed-multiproxy-votes");
		Files.createDirectories(completionDirectory);
		for (int index = 0; index < 4096; index++) {
			Files.writeString(completionDirectory.resolve(UUID.randomUUID().toString()), "existing-" + index);
		}
		UUID blockedVoteId = UUID.randomUUID();
		// A directory at the target path makes the atomic publication fail after
		// staging, exercising the failure window without changing file permissions.
		Files.createDirectory(completionDirectory.resolve(blockedVoteId.toString()));

		assertFalse(handler.markMultiProxyVoteCompletedDurably(blockedVoteId));
		try (java.util.stream.Stream<Path> files = Files.list(completionDirectory)) {
			assertEquals(4096L, files.filter(Files::isRegularFile).count());
		}
		assertTrue(Files.isDirectory(completionDirectory.resolve(blockedVoteId.toString())));
	}

	@Test
	public void completionFencesRemainWhileSenderOutboxesCanRetryIndefinitely() throws Exception {
		Path completionDirectory = tempDir.resolve("vote-cache.json.completed-multiproxy-votes");
		Files.createDirectories(completionDirectory);
		UUID oldestVoteId = UUID.randomUUID();
		Files.writeString(completionDirectory.resolve(oldestVoteId.toString()), oldestVoteId.toString());
		for (int index = 1; index < 4096; index++) {
			UUID voteId = UUID.randomUUID();
			Files.writeString(completionDirectory.resolve(voteId.toString()), voteId.toString());
		}
		UUID newestVoteId = UUID.randomUUID();

		assertTrue(handler.markMultiProxyVoteCompletedDurably(newestVoteId));
		assertTrue(handler.hasMultiProxyVoteCompletion(oldestVoteId));
		assertTrue(handler.hasMultiProxyVoteCompletion(newestVoteId));
		try (java.util.stream.Stream<Path> files = Files.list(completionDirectory)) {
			assertEquals(4097L, files.filter(Files::isRegularFile).count());
		}
	}

	@Test
	public void distinctVoteIdsAreNotCollapsed() {
		handler.addServerVote("server", vote(UUID.randomUUID(), 100L));
		handler.addServerVote("server", vote(UUID.randomUUID(), 101L));

		assertEquals(2, handler.getVotes("server").size());
	}

	@Test
	public void missingVoteIdsRemainLegacyCompatible() {
		handler.addServerVote("server", vote(null, 100L));
		handler.addServerVote("server", vote(null, 101L));

		assertEquals(2, handler.getVotes("server").size());
	}

	@Test
	public void serverVoteIsNotExposedWhenJsonJournalCannotSave() {
		doThrow(new RuntimeException("save failed")).when(storage).save();

		assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 100L)));
		assertTrue(handler.getVotes("server").isEmpty());
	}

	@Test
	public void onlineVoteIsNotExposedWhenJsonJournalCannotSave() {
		doThrow(new RuntimeException("save failed")).when(storage).save();

		assertFalse(handler.addOnlineVoteDurably("player-uuid", vote(UUID.randomUUID(), 100L)));
		assertTrue(handler.getOnlineVotes("player-uuid").isEmpty());
	}

	@Test
	public void failedServerInsertIsPublishedOnlyAfterPersistenceRetrySucceeds() {
		OfflineBungeeVote pending = vote(UUID.randomUUID(), 100L);
		doThrow(new RuntimeException("save failed")).doNothing().when(storage).save();

		assertFalse(handler.addServerVoteDurably("server", pending));
		assertTrue(handler.retainServerVoteForPersistenceRetry("server", pending));
		assertTrue(handler.getVotes("server").isEmpty());

		assertTrue(handler.retryPendingVotePersistence());
		assertEquals(List.of(pending), handler.getVotes("server"));
	}

	@Test
	public void failedOnlineInsertIsPublishedOnlyAfterPersistenceRetrySucceeds() {
		OfflineBungeeVote pending = vote(UUID.randomUUID(), 100L);
		doThrow(new RuntimeException("save failed")).doNothing().when(storage).save();

		assertFalse(handler.addOnlineVoteDurably("player-uuid", pending));
		assertTrue(handler.retainOnlineVoteForPersistenceRetry("player-uuid", pending));
		assertTrue(handler.getOnlineVotes("player-uuid").isEmpty());

		assertTrue(handler.retryPendingVotePersistence());
		assertEquals(List.of(pending), handler.getOnlineVotes("player-uuid"));
	}

	@Test
	public void failedPersistenceRetryAdmissionIsBoundedAcrossCacheTypes() {
		OfflineBungeeVote first = vote(UUID.randomUUID(), 0L);
		assertTrue(handler.retainServerVoteForPersistenceRetry("server", first));
		for (int i = 1; i < 1024; i++) {
			assertTrue(handler.retainServerVoteForPersistenceRetry("server", vote(UUID.randomUUID(), i)));
		}

		assertFalse(handler.retainOnlineVoteForPersistenceRetry("another-player", vote(UUID.randomUUID(), 1025L)));
		assertTrue(handler.retainServerVoteForPersistenceRetry("server", first));
	}

	@Test
	public void expiredOnlineVoteRemovalKeepsCollidingVoteId() {
		UUID retainedId = UUID.randomUUID();
		OfflineBungeeVote expired = vote(UUID.randomUUID(), 100L);
		OfflineBungeeVote retained = vote(retainedId, 100L);
		handler.addOnlineVote("player-uuid", expired);
		handler.addOnlineVote("player-uuid", retained);

		handler.removeOnlineVotes(new ArrayList<>(List.of(expired)));

		assertEquals(List.of(retained), handler.getOnlineVotes("player-uuid"));
		verify(storage).removeOnlineVote(same(expired));
	}

	@Test
	public void failedExpiredOnlineVoteRemovalKeepsInMemoryEntries() {
		OfflineBungeeVote expired = vote(UUID.randomUUID(), 100L);
		OfflineBungeeVote retained = vote(UUID.randomUUID(), 101L);
		handler = newHandler(storage, false);
		handler.addOnlineVote("player-uuid", expired);
		handler.addOnlineVote("player-uuid", retained);

		handler.removeOnlineVotes(new ArrayList<>(List.of(expired)));

		assertEquals(List.of(expired, retained), handler.getOnlineVotes("player-uuid"));
	}

	@Test
	public void failedDurableServerSaveRollsBackJsonMutationBeforeRetry() throws Exception {
		Path journal = Files.createTempFile("votingplugin-server-rollback", ".json");
		try {
			ArrayList<String> keys = new ArrayList<>();
			when(storage.getStoragePath()).thenReturn(journal);
			when(storage.getServerVotes("server")).thenAnswer(invocation -> new ArrayList<>(keys));
			org.mockito.Mockito.doAnswer(invocation -> {
				keys.add(String.valueOf(invocation.getArgument(1, Integer.class)));
				return null;
			}).when(storage).addVote(eq("server"), org.mockito.ArgumentMatchers.anyInt(),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			doThrow(new java.io.IOException("save failed")).when(storage).saveDurably();
			org.mockito.Mockito.doAnswer(invocation -> { keys.clear(); return null; }).when(storage).reload();
			handler = newVerifyingHandler(storage);

			assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 100L)));
			assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 101L)));

			verify(storage, times(2)).addVote(eq("server"), eq(0),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			verify(storage, never()).addVote(eq("server"), eq(1),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			verify(storage, times(2)).reload();
		} finally {
			Files.deleteIfExists(journal);
		}
	}

	@Test
	public void failedDurableOnlineSaveRollsBackJsonMutationBeforeRetry() throws Exception {
		Path journal = Files.createTempFile("votingplugin-online-rollback", ".json");
		try {
			ArrayList<String> keys = new ArrayList<>();
			when(storage.getStoragePath()).thenReturn(journal);
			when(storage.getOnlineVotes("player-uuid")).thenAnswer(invocation -> new ArrayList<>(keys));
			org.mockito.Mockito.doAnswer(invocation -> {
				keys.add(String.valueOf(invocation.getArgument(1, Integer.class)));
				return null;
			}).when(storage).addVoteOnline(eq("player-uuid"), org.mockito.ArgumentMatchers.anyInt(),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			doThrow(new java.io.IOException("save failed")).when(storage).saveDurably();
			org.mockito.Mockito.doAnswer(invocation -> { keys.clear(); return null; }).when(storage).reload();
			handler = newVerifyingHandler(storage);

			assertFalse(handler.addOnlineVoteDurably("player-uuid", vote(UUID.randomUUID(), 100L)));
			assertFalse(handler.addOnlineVoteDurably("player-uuid", vote(UUID.randomUUID(), 101L)));

			verify(storage, times(2)).addVoteOnline(eq("player-uuid"), eq(0),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			verify(storage, never()).addVoteOnline(eq("player-uuid"), eq(1),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			verify(storage, times(2)).reload();
		} finally {
			Files.deleteIfExists(journal);
		}
	}

	@Test
	public void failedRollbackReloadQuarantinesJsonJournal() throws Exception {
		Path journal = Files.createTempFile("votingplugin-quarantined-journal", ".json");
		try {
			when(storage.getStoragePath()).thenReturn(journal);
			doThrow(new java.io.IOException("save failed")).when(storage).saveDurably();
			doThrow(new IllegalStateException("reload failed")).when(storage).reload();
			handler = newVerifyingHandler(storage);

			assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 100L)));
			assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 101L)));
			handler.removeVote("server", "player-uuid");
			handler.removeVotes("server");
			handler.removeOnlineVotes("player-uuid");
			handler.saveVoteCache();

			verify(storage, times(1)).addVote(eq("server"), eq(0),
					org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
			verify(storage, times(1)).saveDurably();
			verify(storage, never()).removeServerVote("server", "player-uuid");
			verify(storage, never()).removeServerVotes("server");
			verify(storage, never()).removeOnlineVotes("player-uuid");
			verify(storage, never()).save();
		} finally {
			Files.deleteIfExists(journal);
		}
	}

	@Test
	public void serverVoteIsNotExposedWhenJsonSaveDoesNotReachDisk() throws Exception {
		Path journal = Files.createTempFile("votingplugin-empty-journal", ".json");
		try {
			when(storage.getStoragePath()).thenReturn(journal);
			handler = newVerifyingHandler(storage);

			assertFalse(handler.addServerVoteDurably("server", vote(UUID.randomUUID(), 100L)));
			assertTrue(handler.getVotes("server").isEmpty());
			verify(storage).reload();
		} finally {
			Files.deleteIfExists(journal);
		}
	}

	@Test
	public void serverVoteUpdateIsRejectedWhenJsonSaveDoesNotReachDisk() throws Exception {
		Path journal = Files.createTempFile("votingplugin-empty-update-journal", ".json");
		try {
			DataNode voteNode = mock(DataNode.class);
			when(voteNode.isObject()).thenReturn(true);
			stubString(voteNode, "UUID", "player-uuid");
			stubString(voteNode, "Service", "Service");
			stubLong(voteNode, "Time", 100L);
			when(storage.getStoragePath()).thenReturn(journal);
			when(storage.getServerVotes("server")).thenReturn(List.of("4"));
			when(storage.getServerVotes("server", "4")).thenReturn(voteNode, (DataNode) null);
			handler = newVerifyingHandler(storage);
			OfflineBungeeVote updated = vote(null, 100L);

			assertFalse(handler.updateServerVote("server", updated));
			verify(storage).reload();
		} finally {
			Files.deleteIfExists(journal);
		}
	}

	@Test
	public void currentVoteIdKeyLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId);

		handler.load();

		assertEquals(voteId, handler.getVotes("server").get(0).getVoteId());
	}

	@Test
	public void legacyVoteIdKeyLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteID", voteId);

		handler.load();

		assertEquals(voteId, handler.getVotes("server").get(0).getVoteId());
	}

	@Test
	public void legacyJsonCacheEntryStillNeedsItsBroadcast() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId);

		handler.load();

		assertFalse(handler.getVotes("server").get(0).isBroadcastForwarded());
	}

	@Test
	public void forwardedBroadcastStateLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId, true);

		handler.load();

		assertTrue(handler.getVotes("server").get(0).isBroadcastForwarded());
	}

	@Test
	public void timedVoteIsPersistedImmediatelyWithItsId() {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L);
		handler.addTimeVoteToCache(queued);

		verify(storage).addTimedVote(eq(0), same(queued));
		verify(storage).save();
		assertEquals(voteId, handler.getTimeChangeQueue().element().getVoteId());
	}

	@Test
	public void legacyTimedVoteIdIsDurableBeforeReliableForwarding() {
		VoteTimeQueue queued = new VoteTimeQueue("Player", "Service", 100L);
		queued.setTimedVoteCacheJsonKey("2");
		JsonObject legacyJson = new JsonObject();
		legacyJson.addProperty("Name", "Player");
		legacyJson.addProperty("Service", "Service");
		legacyJson.addProperty("Time", 100L);
		AtomicReference<DataNode> stored = new AtomicReference<>(new GsonDataNode(legacyJson));
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenAnswer(ignored -> stored.get());
		org.mockito.Mockito.doAnswer(invocation -> {
			stored.set(timedVoteNode(invocation.getArgument(1)));
			return null;
		}).when(storage).addTimedVote(eq(2), org.mockito.ArgumentMatchers.any(VoteTimeQueue.class));
		UUID voteId = UUID.randomUUID();

		assertTrue(handler.assignLegacyTimeVoteId(queued, voteId));
		assertEquals(voteId, queued.getVoteId());
		assertEquals(voteId.toString(), stored.get().get("VoteId").asString());
		verify(storage).save();
	}

	@Test
	public void identicalLegacyTimedJsonRowsKeepDistinctIdsAndDeleteIndividually() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		Files.writeString(tempDir.resolve("votecache.json"), """
				{"TimedVoteCache":{"0":{"Name":"Player","Service":"Service","Time":100,"UUID":"player-uuid"},
				"1":{"Name":"Player","Service":"Service","Time":100,"UUID":"player-uuid"}}}
				""");
		BungeeJsonVoteCache durableStorage = new BungeeJsonVoteCache(plugin);

		VoteCacheHandler durableHandler = newVerifyingHandler(durableStorage);
		durableHandler.load();
		VoteTimeQueue first = durableHandler.getTimeChangeQueue().stream()
				.filter(vote -> "0".equals(vote.getTimedVoteCacheJsonKey())).findFirst().orElseThrow();
		VoteTimeQueue second = durableHandler.getTimeChangeQueue().stream()
				.filter(vote -> "1".equals(vote.getTimedVoteCacheJsonKey())).findFirst().orElseThrow();
		UUID firstId = first.legacyTimedVoteId();
		UUID secondId = second.legacyTimedVoteId();

		assertNotEquals(firstId, secondId);
		assertEquals(firstId, first.legacyTimedVoteId());
		assertTrue(durableHandler.assignLegacyTimeVoteId(first, firstId));
		assertTrue(durableHandler.assignLegacyTimeVoteId(second, secondId));
		assertTrue(durableHandler.removeTimeVote(first));

		assertFalse(durableStorage.getTimedVoteCache().contains("0"));
		assertTrue(durableStorage.getTimedVoteCache().contains("1"));
		assertEquals(secondId.toString(), durableStorage.getTimedVoteCache("1").get("VoteId").asString());
	}

	@Test
	public void legacySqlAndJsonTimedRowsRemainDistinctWithoutVoteIds() throws Exception {
		DataNode jsonVote = mock(DataNode.class);
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenReturn(jsonVote);
		when(storage.getServers()).thenReturn(Collections.emptyList());
		when(storage.getPlayers()).thenReturn(Collections.emptyList());
		when(jsonVote.isObject()).thenReturn(true);
		stubString(jsonVote, "Name", "Player");
		stubString(jsonVote, "Service", "Service");
		stubLong(jsonVote, "Time", 100L);

		VoteTimeQueue sqlVote = legacyTimedVote();
		sqlVote.setTimedVoteCacheRowId(41);
		handler.getTimeChangeQueue().add(sqlVote);
		var loadEmergencyVotes = VoteCacheHandler.class.getDeclaredMethod("loadJsonEmergencyVotes");
		loadEmergencyVotes.setAccessible(true);

		loadEmergencyVotes.invoke(handler);

		assertEquals(2, handler.getTimeChangeQueue().size());
		VoteTimeQueue jsonLoaded = handler.getTimeChangeQueue().stream()
				.filter(vote -> "2".equals(vote.getTimedVoteCacheJsonKey())).findFirst().orElseThrow();
		assertNull(jsonLoaded.getVoteId());
		assertEquals(-1, jsonLoaded.getTimedVoteCacheRowId());
	}

	@Test
	public void identicalLegacyServerJsonRowsKeepDistinctEntryIdentitiesAndDeleteIndividually() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		Files.writeString(tempDir.resolve("votecache.json"), """
				{"VoteCache":{"server":{"0":{"Name":"Player","Service":"Service","UUID":"player-uuid","Time":100},
				"1":{"Name":"Player","Service":"Service","UUID":"player-uuid","Time":100}}}}
				""");
		BungeeJsonVoteCache durableStorage = new BungeeJsonVoteCache(plugin);
		VoteCacheHandler durableHandler = newVerifyingHandler(durableStorage);

		durableHandler.load();
		OfflineBungeeVote first = durableHandler.getVotes("server").stream()
				.filter(vote -> "0".equals(vote.getServerVoteCacheJsonKey())).findFirst().orElseThrow();
		OfflineBungeeVote second = durableHandler.getVotes("server").stream()
				.filter(vote -> "1".equals(vote.getServerVoteCacheJsonKey())).findFirst().orElseThrow();

		assertEquals(2, durableHandler.getVotes("server").size());
		durableHandler.removeServerVotes("server", new ArrayList<>(List.of(first)));

		assertFalse(durableStorage.getServerVotes("server").contains("0"));
		assertTrue(durableStorage.getServerVotes("server").contains("1"));
		assertEquals("1", second.getServerVoteCacheJsonKey());
	}

	@Test
	public void expirationDoesNotReuseAJsonEntryKeyAcrossServers() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		long expired = System.currentTimeMillis() - java.util.concurrent.TimeUnit.DAYS.toMillis(2);
		long current = System.currentTimeMillis();
		Files.writeString(tempDir.resolve("votecache.json"), """
				{"VoteCache":{"server-a":{"0":{"Name":"A","Service":"Service","UUID":"player-a","Time":%d}},
				"server-b":{"0":{"Name":"B","Service":"Service","UUID":"player-b","Time":%d}}}}
				""".formatted(expired, current));
		BungeeJsonVoteCache durableStorage = new BungeeJsonVoteCache(plugin);
		VoteCacheHandler durableHandler = newVerifyingHandler(durableStorage);
		durableHandler.load();

		durableHandler.checkVoteCacheTime(1);

		assertTrue(durableStorage.getServerVotes("server-a") == null
				|| durableStorage.getServerVotes("server-a").isEmpty());
		assertTrue(durableStorage.getServerVotes("server-b").contains("0"));
		assertEquals(1, durableHandler.getVotes("server-b").size());
	}

	@Test
	public void identicalLegacyOnlineJsonRowsKeepDistinctEntryIdentitiesAndDeleteIndividually() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		Files.writeString(tempDir.resolve("votecache.json"), """
				{"OnlineCache":{"player-uuid":{"0":{"Name":"Player","Service":"Service","UUID":"player-uuid","Time":100},
				"1":{"Name":"Player","Service":"Service","UUID":"player-uuid","Time":100}}}}
				""");
		BungeeJsonVoteCache durableStorage = new BungeeJsonVoteCache(plugin);
		VoteCacheHandler durableHandler = newVerifyingHandler(durableStorage);

		durableHandler.load();
		OfflineBungeeVote first = durableHandler.getOnlineVotes("player-uuid").stream()
				.filter(vote -> "0".equals(vote.getOnlineVoteCacheJsonKey())).findFirst().orElseThrow();
		OfflineBungeeVote second = durableHandler.getOnlineVotes("player-uuid").stream()
				.filter(vote -> "1".equals(vote.getOnlineVoteCacheJsonKey())).findFirst().orElseThrow();

		assertEquals(2, durableHandler.getOnlineVotes("player-uuid").size());
		assertTrue(durableHandler.tryRemoveOnlineVote("player-uuid", first));

		assertFalse(durableStorage.getOnlineVotes("player-uuid").contains("0"));
		assertTrue(durableStorage.getOnlineVotes("player-uuid").contains("1"));
		assertEquals("1", second.getOnlineVoteCacheJsonKey());
	}

	@Test
	public void onlineJsonEntryKeysAreScopedToTheirPlayer() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
		Files.writeString(tempDir.resolve("votecache.json"), """
				{"OnlineCache":{"player-a":{"0":{"Name":"A","Service":"Service","UUID":"player-a","Time":100}},
				"player-b":{"0":{"Name":"B","Service":"Service","UUID":"player-b","Time":100}}}}
				""");
		BungeeJsonVoteCache durableStorage = new BungeeJsonVoteCache(plugin);
		VoteCacheHandler durableHandler = newVerifyingHandler(durableStorage);
		durableHandler.load();
		OfflineBungeeVote playerB = durableHandler.getOnlineVotes("player-b").get(0);

		assertTrue(durableHandler.tryRemoveOnlineVote("player-b", playerB));

		assertTrue(durableStorage.getOnlineVotes("player-a").contains("0"));
		assertFalse(durableStorage.getOnlineVotes("player-b").contains("0"));
	}

	@Test
	public void unpairedLegacyServerSqlRowDoesNotDeleteJsonRows() throws Exception {
		OfflineBungeeVote sqlVote = vote(null, 100L);
		sqlVote.setServerVoteCacheRowId(41);
		cachedVotes(handler, "cachedVotes").put("server", new ArrayList<>(List.of(sqlVote)));
		ProxyVoteCacheTable sql = mock(ProxyVoteCacheTable.class);
		when(sql.tryRemoveVote(same(sqlVote), eq("server"))).thenReturn(true);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "voteCacheTable", sql);

		handler.removeServerVotes("server", new ArrayList<>(List.of(sqlVote)));

		verify(storage, never()).removeVote(eq("server"), same(sqlVote));
		assertTrue(handler.getVotes("server").isEmpty());
	}

	@Test
	public void unpairedLegacyOnlineSqlRowDoesNotDeleteJsonRows() throws Exception {
		OfflineBungeeVote sqlVote = vote(null, 100L);
		sqlVote.setOnlineVoteCacheRowId(42);
		cachedVotes(handler, "cachedOnlineVotes").put("player-uuid", new ArrayList<>(List.of(sqlVote)));
		ProxyOnlineVoteCacheTable sql = mock(ProxyOnlineVoteCacheTable.class);
		when(sql.tryRemoveVote(same(sqlVote))).thenReturn(true);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "onlineVoteCacheTable", sql);

		assertTrue(handler.tryRemoveOnlineVote("player-uuid", sqlVote));

		verify(storage, never()).removeOnlineVote(same(sqlVote));
		assertTrue(handler.getOnlineVotes("player-uuid").isEmpty());
	}

	@Test
	public void identicalLegacyTimedSqlRowsUsePrimaryKeysForDistinctStableIds() {
		VoteTimeQueue first = legacyTimedVote();
		VoteTimeQueue second = legacyTimedVote();
		first.setTimedVoteCacheRowId(41);
		second.setTimedVoteCacheRowId(42);

		assertNotEquals(first.legacyTimedVoteId(), second.legacyTimedVoteId());
		assertEquals(first.legacyTimedVoteId(), first.legacyTimedVoteId());
	}

	@Test
	public void timedVoteDeliveryStateIsUpdatedByVoteId() {
		UUID voteId = UUID.randomUUID();
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		stubString(stored, "VoteId", voteId.toString());
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenReturn(stored);
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L, true,
				Set.of("Server1"), Set.of("Server1"));

		handler.updateTimeVote(queued);

		verify(storage).addTimedVote(2, queued);
		verify(storage).save();
	}

	@Test
	public void timedVoteSqlTwinRejectsJsonOnlyDeliveryUpdate() throws Exception {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L);
		queued.setTimedVoteCacheRowId(42);
		queued.setTimedVoteCacheJsonKey("2");
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		stubString(stored, "VoteId", voteId.toString());
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenReturn(stored);
		ProxyTimedVoteCacheTable sql = mock(ProxyTimedVoteCacheTable.class);
		when(sql.updateTimedVote(queued)).thenReturn(false);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "timedVoteCacheTable", sql);

		assertFalse(handler.updateTimeVote(queued));

		verify(sql).updateTimedVote(queued);
		verify(storage, never()).addTimedVote(org.mockito.ArgumentMatchers.anyInt(), same(queued));
	}

	@Test
	public void timedVoteSqlTwinRejectsJsonOnlyRemoval() throws Exception {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L);
		queued.setTimedVoteCacheRowId(42);
		queued.setTimedVoteCacheJsonKey("2");
		handler.getTimeChangeQueue().add(queued);
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		stubString(stored, "VoteId", voteId.toString());
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenReturn(stored);
		ProxyTimedVoteCacheTable sql = mock(ProxyTimedVoteCacheTable.class);
		when(sql.removeVote(queued)).thenReturn(false);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "timedVoteCacheTable", sql);

		assertFalse(handler.removeTimeVote(queued));

		assertTrue(handler.getTimeChangeQueue().contains(queued));
		verify(sql).removeVote(queued);
		verify(storage, never()).removeTimedVotes();
	}

	@Test
	public void serverVoteSqlTwinRejectsJsonOnlyDeliveryUpdate() throws Exception {
		OfflineBungeeVote vote = vote(UUID.randomUUID(), 100L);
		vote.setServerVoteCacheRowId(41);
		vote.setServerVoteCacheJsonKey("4");
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		when(storage.getServerVotes("server")).thenReturn(List.of("4"));
		when(storage.getServerVotes("server", "4")).thenReturn(stored);
		ProxyVoteCacheTable sql = mock(ProxyVoteCacheTable.class);
		when(sql.updateProxyBroadcastState(vote, "server")).thenReturn(false);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "voteCacheTable", sql);

		assertFalse(handler.updateServerVote("server", vote));

		verify(sql).updateProxyBroadcastState(vote, "server");
		verify(storage, never()).addVote(eq("server"), org.mockito.ArgumentMatchers.anyInt(), same(vote));
	}

	@Test
	public void onlineVoteSqlTwinRejectsJsonOnlyDeliveryUpdate() throws Exception {
		OfflineBungeeVote vote = vote(UUID.randomUUID(), 100L);
		vote.setOnlineVoteCacheRowId(42);
		vote.setOnlineVoteCacheJsonKey("3");
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		when(storage.getOnlineVotes("player-uuid")).thenReturn(List.of("3"));
		when(storage.getOnlineVotes("player-uuid", "3")).thenReturn(stored);
		ProxyOnlineVoteCacheTable sql = mock(ProxyOnlineVoteCacheTable.class);
		when(sql.updateProxyBroadcastState(vote)).thenReturn(false);
		setPrivateField(handler, "useMySQL", true);
		setPrivateField(handler, "onlineVoteCacheTable", sql);

		assertFalse(handler.updateOnlineVote("player-uuid", vote));

		verify(sql).updateProxyBroadcastState(vote);
		verify(storage, never()).addVoteOnline(eq("player-uuid"), org.mockito.ArgumentMatchers.anyInt(), same(vote));
	}

	@Test
	public void timedVoteAcknowledgementOutboxSurvivesHandlerRestart() {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L, true,
				Set.of("backend-1"), Set.of("backend-1"), "totals", false, false, "player-uuid",
				Map.of("backend-1", "00000000-0000-0000-0000-000000000181"));
		queued.requireMultiProxyAcknowledgements("origin-proxy", Set.of("backend-1"));
		queued.acknowledgeMultiProxyRecipient("backend-1");
		queued.acknowledgeMultiProxyRetirement("backend-1");
		handler.addTimeVoteToCache(queued);

		when(storage.getTimedVoteCache()).thenReturn(List.of("0"));
		when(storage.getTimedVoteCache("0")).thenReturn(timedVoteNode(queued));
		VoteCacheHandler restarted = newHandler(storage);
		restarted.load();

		VoteTimeQueue recovered = restarted.getTimeChangeQueue().element();
		assertEquals(voteId, recovered.getVoteId());
		assertTrue(recovered.isMultiProxyForwardingRequired());
		assertEquals(Set.of("backend-1"), recovered.getMultiProxyRecipients());
		assertTrue(recovered.hasCompletedMultiProxyRetirements());
		assertEquals("00000000-0000-0000-0000-000000000181",
				recovered.getHttpBroadcastDeliveryId("backend-1"));
	}

	@Test
	public void timedVoteIsRemovedFromMemoryOnlyAfterJsonSaveSucceeds() throws Exception {
		VoteTimeQueue queued = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		assertTrue(handler.addTimeVoteToCache(queued));

		assertTrue(handler.removeTimeVote(queued));

		assertTrue(handler.getTimeChangeQueue().isEmpty());
		verify(storage).saveDurably();
	}

	@Test
	public void timedVoteRemainsQueuedWhenJsonDeleteCannotBeSaved() throws Exception {
		VoteTimeQueue queued = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		assertTrue(handler.addTimeVoteToCache(queued));
		doThrow(new java.io.IOException("save failed")).when(storage).saveDurably();

		assertFalse(handler.removeTimeVote(queued));

		assertTrue(handler.getTimeChangeQueue().contains(queued));
	}

	@Test
	public void timedVoteCompletionTombstoneSurvivesIndependentCacheFailure() {
		VoteTimeQueue queued = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L, false,
				Set.of(), Set.of(), "totals", false, "player-uuid");

		assertTrue(handler.markTimeVoteCompletedDurably(queued));
		assertTrue(handler.hasTimeVoteCompletion(queued));

		handler.clearTimeVoteCompletion(queued);
		assertFalse(handler.hasTimeVoteCompletion(queued));
	}

	@Test
	public void legacyTimedVoteConstructorHasNoId() {
		VoteTimeQueue vote = new VoteTimeQueue("Player", "Service", 100L);

		assertNull(vote.getVoteId());
		assertFalse(vote.isProxyBroadcastHandled());
		assertTrue(vote.getBroadcastTargets().isEmpty());
		assertTrue(vote.getBroadcastForwardedServers().isEmpty());
	}

	@Test
	public void timedVoteBroadcastStateRoundTripsThroughStorageEncoding() {
		Set<String> targets = new LinkedHashSet<>(List.of("Server1", "Server,Two", "Server Three"));
		Set<String> forwarded = Set.of("Server1");
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L, true, targets,
				forwarded);

		assertEquals(targets, VoteTimeQueue.decodeBroadcastForwardedServers(vote.encodeBroadcastTargets()));
		assertEquals(forwarded,
				VoteTimeQueue.decodeBroadcastForwardedServers(vote.encodeBroadcastForwardedServers()));
	}

	@Test
	public void timedVoteHttpBroadcastDeliveryIdsRoundTripThroughStorageEncoding() {
		String deliveryId = "00000000-0000-0000-0000-000000000164";
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L, true,
				Set.of("Server1"), Set.of(), "totals", false, "uuid",
				Map.of("Server1", deliveryId));

		assertEquals(deliveryId, vote.getHttpBroadcastDeliveryId("server1"));
		assertEquals(deliveryId,
				VoteTimeQueue.decodeHttpBroadcastDeliveryIds(vote.encodeHttpBroadcastDeliveryIds())
						.get("server1"));
	}

	@Test
	public void cachedBroadcastStateSuppressesNonTargetsAndDeliveredTargets() {
		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", false, true, Set.of("Lobby", "Survival"), Set.of("Lobby"), false);

		assertFalse(vote.needsBroadcastOn("Lobby"));
		assertTrue(vote.needsBroadcastOn("Survival"));
		assertFalse(vote.needsBroadcastOn("Creative"));
		assertFalse(vote.isProxyBroadcastComplete());
	}

	@Test
	public void updatedServerBroadcastStateIsPersistedInPlace() {
		DataNode voteNode = mock(DataNode.class);
		when(storage.getServerVotes("server")).thenReturn(List.of("4"));
		when(storage.getServerVotes("server", "4")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);

		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateServerVote("server", vote);

		verify(storage).addVote("server", 4, vote);
		verify(storage).save();
	}

	@Test
	public void updatedOnlineBroadcastStateIsPersistedInPlace() {
		DataNode voteNode = mock(DataNode.class);
		when(storage.getOnlineVotes("player-uuid")).thenReturn(List.of("3"));
		when(storage.getOnlineVotes("player-uuid", "3")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);

		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateOnlineVote("player-uuid", vote);

		verify(storage).addVoteOnline("player-uuid", 3, vote);
		verify(storage).save();
	}

	@Test
	public void serverBroadcastStateUpdateUsesVoteIdWhenTimestampsCollide() {
		UUID voteId = UUID.randomUUID();
		DataNode matching = storedVoteNode(voteId, 100L);
		DataNode other = storedVoteNode(UUID.randomUUID(), 100L);
		when(storage.getServerVotes("server")).thenReturn(List.of("4", "5"));
		when(storage.getServerVotes("server", "4")).thenReturn(other);
		when(storage.getServerVotes("server", "5")).thenReturn(matching);

		OfflineBungeeVote vote = new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true,
				"totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateServerVote("server", vote);

		verify(storage, never()).addVote("server", 4, vote);
		verify(storage).addVote("server", 5, vote);
	}

	@Test
	public void onlineBroadcastStateUpdateUsesVoteIdWhenTimestampsCollide() {
		UUID voteId = UUID.randomUUID();
		DataNode matching = storedVoteNode(voteId, 100L);
		DataNode other = storedVoteNode(UUID.randomUUID(), 100L);
		when(storage.getOnlineVotes("player-uuid")).thenReturn(List.of("3", "7"));
		when(storage.getOnlineVotes("player-uuid", "3")).thenReturn(other);
		when(storage.getOnlineVotes("player-uuid", "7")).thenReturn(matching);

		OfflineBungeeVote vote = new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true,
				"totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateOnlineVote("player-uuid", vote);

		verify(storage, never()).addVoteOnline("player-uuid", 3, vote);
		verify(storage).addVoteOnline("player-uuid", 7, vote);
	}

	@Test
	public void globalRewardClearRetainsOnlyIncompleteForwardBroadcasts() {
		VoteCacheHandler guarded = spy(handler);
		OfflineBungeeVote pending = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service",
				100L, true, "totals", false, true, Set.of("Lobby", "Survival"), Set.of("Lobby"), false);
		OfflineBungeeVote complete = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service",
				101L, true, "totals", true, true, Set.of("Lobby"), Set.of("Lobby"), false);
		guarded.addOnlineVote("player-uuid", pending);
		guarded.addOnlineVote("player-uuid", complete);
		doReturn(true).when(guarded).updateOnlineVote(eq("player-uuid"), org.mockito.ArgumentMatchers.any());

		guarded.clearOnlineVoteRewards("player-uuid");

		assertEquals(List.of(pending), guarded.getOnlineVotes("player-uuid"));
		assertTrue(pending.isRewardDelivered());
		assertTrue(pending.needsBroadcastOn("Survival"));
	}

	@Test
	public void failedGlobalRewardClearCannotLeaveRetainedVoteEligible() {
		VoteCacheHandler guarded = spy(handler);
		OfflineBungeeVote vote = vote(UUID.randomUUID(), 100L);
		guarded.addOnlineVote("player-uuid", vote);
		doReturn(false, true).when(guarded).updateOnlineVote("player-uuid", vote);
		doReturn(false).when(guarded).tryRemoveOnlineVote("player-uuid", vote);

		guarded.clearOnlineVoteRewards("player-uuid");

		assertTrue(vote.isRewardDelivered());
		assertTrue(vote.isDeliveryStateDirty());
		assertEquals(List.of(vote), guarded.getOnlineVotes("player-uuid"));
		verify(guarded, never()).tryRemoveOnlineVote("player-uuid", vote);

		guarded.clearOnlineVoteRewards("player-uuid");

		assertFalse(vote.isDeliveryStateDirty());
		assertEquals(List.of(vote), guarded.getOnlineVotes("player-uuid"));
		verify(guarded).tryRemoveOnlineVote("player-uuid", vote);
	}

	@Test
	public void completedOnlineVoteRemovalKeepsCollidingVoteId() {
		UUID removedId = UUID.randomUUID();
		OfflineBungeeVote removed = vote(removedId, 100L);
		OfflineBungeeVote retained = vote(UUID.randomUUID(), 100L);
		handler.addOnlineVote("player-uuid", removed);
		handler.addOnlineVote("player-uuid", retained);

		handler.removeOnlineVote("player-uuid", removed);

		assertEquals(List.of(retained), handler.getOnlineVotes("player-uuid"));
	}

	@Test
	public void completedServerVoteRemovalKeepsCollidingVoteId() {
		OfflineBungeeVote removed = vote(UUID.randomUUID(), 100L);
		OfflineBungeeVote retained = vote(UUID.randomUUID(), 100L);
		handler.addServerVote("server", removed);
		handler.addServerVote("server", retained);

		handler.removeServerVotes("server", new ArrayList<>(List.of(removed)));

		assertEquals(List.of(retained), handler.getVotes("server"));
	}

	@Test
	public void timedVoteBroadcastStateLoadsFromJsonCache() {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode timedNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(List.of("0"));
		when(stored.getTimedVoteCache("0")).thenReturn(timedNode);
		when(stored.getServers()).thenReturn(Collections.emptyList());
		when(stored.getPlayers()).thenReturn(Collections.emptyList());
		when(timedNode.isObject()).thenReturn(true);

		UUID voteId = UUID.randomUUID();
		stubString(timedNode, "Name", "Player");
		stubString(timedNode, "Service", "Service");
		stubLong(timedNode, "Time", 100L);
		stubString(timedNode, "VoteId", voteId.toString());
		stubBoolean(timedNode, "ProxyBroadcastHandled", true);
		stubString(timedNode, "BroadcastTargets",
				VoteTimeQueue.encodeBroadcastServers(Set.of("Server1", "Server2")));
		stubString(timedNode, "BroadcastForwardedServers",
				new VoteTimeQueue(voteId, "Player", "Service", 100L, true, Set.of("Server1"))
						.encodeBroadcastForwardedServers());
		String totals = new com.bencodez.votingplugin.proxy.VoteTotalsSnapshot(10, 2, 2, 1, 4, 0, 20, 2)
				.toStorageString();
		stubString(timedNode, "Totals", totals);
		stubBoolean(timedNode, "Processed", true);

		handler = newHandler(stored);
		handler.load();

		VoteTimeQueue loaded = handler.getTimeChangeQueue().element();
		assertEquals(voteId, loaded.getVoteId());
		assertTrue(loaded.isProxyBroadcastHandled());
		assertEquals(Set.of("Server1", "Server2"), loaded.getBroadcastTargets());
		assertEquals(Set.of("Server1"), loaded.getBroadcastForwardedServers());
		assertEquals(totals, loaded.getTotals());
		assertTrue(loaded.isProcessed());
	}

	@Test
	public void timedVoteHttpBroadcastDeliveryIdLoadsFromJsonCache() {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode timedNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(List.of("0"));
		when(stored.getTimedVoteCache("0")).thenReturn(timedNode);
		when(stored.getServers()).thenReturn(Collections.emptyList());
		when(stored.getPlayers()).thenReturn(Collections.emptyList());
		when(timedNode.isObject()).thenReturn(true);
		String deliveryId = "00000000-0000-0000-0000-000000000165";
		UUID voteId = UUID.randomUUID();
		stubString(timedNode, "Name", "Player");
		stubString(timedNode, "Service", "Service");
		stubLong(timedNode, "Time", 100L);
		stubString(timedNode, "VoteId", voteId.toString());
		stubString(timedNode, "UUID", "uuid");
		stubBoolean(timedNode, "ProxyBroadcastHandled", true);
		stubBoolean(timedNode, "MultiProxyForwardingHandled", true);
		stubString(timedNode, "BroadcastTargets", VoteTimeQueue.encodeBroadcastServers(Set.of("Server1")));
		stubString(timedNode, "BroadcastForwardedServers", "");
		stubString(timedNode, "HttpBroadcastDeliveryIds", new VoteTimeQueue(voteId, "Player", "Service", 100L,
				true, Set.of("Server1"), Set.of(), "", false, "uuid", Map.of("Server1", deliveryId))
					.encodeHttpBroadcastDeliveryIds());

		handler = newHandler(stored);
		handler.load();

		assertEquals(deliveryId, handler.getTimeChangeQueue().element().getHttpBroadcastDeliveryId("SERVER1"));
		assertTrue(handler.getTimeChangeQueue().element().isMultiProxyForwardingHandled());
	}

	@Test
	public void legacyTimedVoteDefaultsMultiProxyForwardingFenceToFalse() {
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);

		assertFalse(vote.isMultiProxyForwardingHandled());
		vote.setMultiProxyForwardingHandled(true);
		assertTrue(vote.isMultiProxyForwardingHandled());
	}

	@Test
	public void onlineVoteBroadcastTargetsAndRewardStateLoadFromJsonCache() {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode voteNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(Collections.emptyList());
		when(stored.getServers()).thenReturn(Collections.emptyList());
		when(stored.getPlayers()).thenReturn(List.of("player-uuid"));
		when(stored.getOnlineVotes("player-uuid")).thenReturn(List.of("0"));
		when(stored.getOnlineVotes("player-uuid", "0")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);

		UUID voteId = UUID.randomUUID();
		stubString(voteNode, "Name", "Player");
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);
		stubBoolean(voteNode, "Real", true);
		stubString(voteNode, "Text", "totals");
		stubString(voteNode, "VoteId", voteId.toString());
		stubBoolean(voteNode, "ProxyBroadcastHandled", true);
		stubString(voteNode, "BroadcastTargets", VoteTimeQueue.encodeBroadcastServers(Set.of("Lobby", "Survival")));
		stubString(voteNode, "BroadcastForwardedServers", VoteTimeQueue.encodeBroadcastServers(Set.of("Lobby")));
		stubBoolean(voteNode, "RewardDelivered", true);

		handler = newHandler(stored);
		handler.load();

		OfflineBungeeVote loaded = handler.getOnlineVotes("player-uuid").get(0);
		assertTrue(loaded.isProxyBroadcastHandled());
		assertEquals(Set.of("Lobby", "Survival"), loaded.getBroadcastTargets());
		assertEquals(Set.of("Lobby"), loaded.getBroadcastForwardedServers());
		assertTrue(loaded.isRewardDelivered());
		assertTrue(loaded.needsBroadcastOn("Survival"));
	}

	private VoteCacheHandler handlerForStoredVote(String idKey, UUID voteId) {
		return handlerForStoredVote(idKey, voteId, null);
	}

	private VoteCacheHandler handlerForStoredVote(String idKey, UUID voteId, Boolean broadcastForwarded) {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode voteNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(Collections.emptyList());
		when(stored.getServers()).thenReturn(List.of("server"));
		when(stored.getServerVotes("server")).thenReturn(List.of("0"));
		when(stored.getServerVotes("server", "0")).thenReturn(voteNode);
		when(stored.getPlayers()).thenReturn(Collections.emptyList());
		when(voteNode.isObject()).thenReturn(true);

		stubString(voteNode, "Name", "Player");
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);
		stubBoolean(voteNode, "Real", true);
		stubString(voteNode, "Text", "totals");
		stubString(voteNode, idKey, voteId.toString());
		if (broadcastForwarded != null) {
			stubBoolean(voteNode, "BroadcastForwarded", broadcastForwarded.booleanValue());
		}
		if ("VoteID".equals(idKey)) {
			when(voteNode.has("VoteId")).thenReturn(false);
		}

		return newHandler(stored);
	}

	private static void stubString(DataNode parent, String key, String value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asString()).thenReturn(value);
	}

	private static void stubLong(DataNode parent, String key, long value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asLong()).thenReturn(value);
	}

	private static void stubBoolean(DataNode parent, String key, boolean value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asBoolean()).thenReturn(value);
	}

	private static DataNode storedVoteNode(UUID voteId, long time) {
		DataNode voteNode = mock(DataNode.class);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", time);
		stubString(voteNode, "VoteId", voteId.toString());
		return voteNode;
	}

	private static void runConcurrently(Runnable first, Runnable second) throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(2);
		CountDownLatch ready = new CountDownLatch(2);
		CountDownLatch start = new CountDownLatch(1);
		try {
			Future<?> firstFuture = executor.submit(() -> {
				ready.countDown();
				await(start);
				first.run();
			});
			Future<?> secondFuture = executor.submit(() -> {
				ready.countDown();
				await(start);
				second.run();
			});
			ready.await();
			start.countDown();
			firstFuture.get();
			secondFuture.get();
		} finally {
			executor.shutdownNow();
		}
	}

	private static void await(CountDownLatch latch) {
		try {
			latch.await();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(e);
		}
	}

	private static OfflineBungeeVote vote(UUID voteId, long time) {
		return new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", time, true, "totals");
	}

	private static VoteTimeQueue legacyTimedVote() {
		VoteTimeQueue vote = new VoteTimeQueue("Player", "Service", 100L);
		vote.setUuid("player-uuid");
		return vote;
	}

	private static DataNode timedVoteNode(VoteTimeQueue vote) {
		JsonObject data = new JsonObject();
		data.addProperty("Name", vote.getName());
		data.addProperty("Service", vote.getService());
		data.addProperty("Time", vote.getTime());
		data.addProperty("VoteId", vote.getVoteId().toString());
		data.addProperty("UUID", vote.getUuid());
		data.addProperty("ProxyBroadcastHandled", vote.isProxyBroadcastHandled());
		data.addProperty("Totals", vote.getTotals());
		data.addProperty("Processed", vote.isProcessed());
		data.addProperty("MultiProxyForwardingHandled", vote.isMultiProxyForwardingHandled());
		data.addProperty("MultiProxyForwardingRequired", vote.isMultiProxyForwardingRequired());
		data.addProperty("RealVote", vote.isRealVote());
		data.addProperty("MultiProxyOrigin", vote.getMultiProxyOrigin());
		data.addProperty("MultiProxyCompletionPending", vote.isMultiProxyCompletionPending());
		data.addProperty("MultiProxyRecipients", vote.encodeMultiProxyRecipients());
		data.addProperty("MultiProxyAcknowledgedServers", vote.encodeMultiProxyAcknowledgedServers());
		data.addProperty("BroadcastTargets", vote.encodeBroadcastTargets());
		data.addProperty("BroadcastForwardedServers", vote.encodeBroadcastForwardedServers());
		data.addProperty("HttpBroadcastDeliveryIds", vote.encodeHttpBroadcastDeliveryIds());
		return new GsonDataNode(data);
	}

	private static VoteCacheHandler newHandler(IVoteCache storage) {
		return newHandler(storage, true);
	}

	private static VoteCacheHandler newHandler(IVoteCache storage, boolean removalSucceeds) {
		return new VoteCacheHandler(null, false, false, null, false, storage) {
			@Override
			protected boolean verifyJsonServerVote(String server, int index, OfflineBungeeVote vote) {
				storage.save();
				return true;
			}

			@Override
			protected boolean verifyJsonOnlineVote(String uuid, int index, OfflineBungeeVote vote) {
				storage.save();
				return true;
			}

			@Override
			protected boolean verifyJsonTimeVote(int index, VoteTimeQueue vote) {
				storage.save();
				return true;
			}

			@Override
			protected boolean removeJsonOnlineVoteDurably(String uuid, OfflineBungeeVote vote) {
				storage.removeOnlineVote(vote);
				storage.save();
				return removalSucceeds;
			}

			@Override
			protected boolean removeJsonServerVoteDurably(String server, OfflineBungeeVote vote) {
				storage.removeVote(server, vote);
				storage.save();
				return true;
			}

			@Override
			public void logInfo1(String msg) {
			}

			@Override
			public void logSevere1(String msg) {
			}

			@Override
			public void debug1(Exception e) {
			}

			@Override
			public void debug1(Throwable e) {
			}

			@Override
			public void debug1(String msg) {
			}
		};
	}

	@SuppressWarnings("unchecked")
	private static Map<String, ArrayList<OfflineBungeeVote>> cachedVotes(VoteCacheHandler target, String fieldName)
			throws Exception {
		var field = VoteCacheHandler.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		return (Map<String, ArrayList<OfflineBungeeVote>>) field.get(target);
	}

	private static void setPrivateField(VoteCacheHandler target, String fieldName, Object value) throws Exception {
		var field = VoteCacheHandler.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static VoteCacheHandler newVerifyingHandler(IVoteCache storage) {
		return new VoteCacheHandler(null, false, false, null, false, storage) {
			@Override public void logInfo1(String msg) { }
			@Override public void logSevere1(String msg) { }
			@Override public void debug1(Exception e) { }
			@Override public void debug1(Throwable e) { }
			@Override public void debug1(String msg) { }
		};
	}
}
