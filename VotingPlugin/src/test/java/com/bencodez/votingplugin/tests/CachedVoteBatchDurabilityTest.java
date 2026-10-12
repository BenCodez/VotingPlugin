package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.lang.reflect.Field;
import java.sql.SQLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.ProxyOnlineVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyTimedVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.ProxyVoteCacheTable;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.velocity.VelocityJsonVoteCache;

/** Exercises real adapter publication/reload, with SQL represented only at its table boundary. */
@Execution(ExecutionMode.SAME_THREAD)
class CachedVoteBatchDurabilityTest {
	@TempDir Path directory;
	private static final String BACKEND = "backend-a";
	private static final String PLAYER = UUID.randomUUID().toString();

	@FunctionalInterface
	private interface LaneTest { void run(boolean velocity, boolean online) throws Exception; }

	private Stream<DynamicTest> lanes(LaneTest test) {
		return Stream.of(false, true).flatMap(velocity -> Stream.of(false, true).map(online ->
				DynamicTest.dynamicTest((velocity ? "Velocity" : "Bungee") + (online ? "/online" : "/server"), () -> {
					Path factoryDirectory = directory;
					directory = Files.createTempDirectory(factoryDirectory, "lane-");
					try { test.run(velocity, online); }
					finally { directory = factoryDirectory; }
				})));
	}

	private IVoteCache cache(boolean velocity) {
		if (velocity) return new VelocityJsonVoteCache(directory.resolve("votecache.json").toFile());
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(directory.toFile());
		return new BungeeJsonVoteCache(plugin);
	}

	private VoteCacheHandler handler(IVoteCache storage) {
		return new VoteCacheHandler(null, false, false, null, false, storage) {
			@Override public void logInfo1(String message) { }
			@Override public void logSevere1(String message) { }
			@Override public void debug1(Exception failure) { }
			@Override public void debug1(Throwable failure) { }
			@Override public void debug1(String message) { }
		};
	}

	private OfflineBungeeVote vote(UUID id) {
		OfflineBungeeVote vote = new OfflineBungeeVote(id, "test-player", PLAYER, "test-service", 100L, true, "totals");
		vote.setWasOnline(true);
		vote.setDelayValidated(true);
		vote.setProxyBroadcastHandled(true);
		vote.setBroadcastTargets(Set.of(BACKEND, "backend-b"));
		vote.setBroadcastForwardedServers(Set.of(BACKEND));
		vote.setHttpDeliveryId(BACKEND, UUID.randomUUID().toString());
		vote.setHttpBroadcastDeliveryId("backend-b", UUID.randomUUID().toString());
		return vote;
	}

	private String owner(boolean online) { return online ? PLAYER : BACKEND; }
	private ArrayList<OfflineBungeeVote> votes(VoteCacheHandler handler, boolean online) {
		return online ? handler.getOnlineVotes(PLAYER) : handler.getVotes(BACKEND);
	}
	private void add(VoteCacheHandler handler, boolean online, OfflineBungeeVote vote) {
		assertTrue(online ? handler.addOnlineVoteDurably(PLAYER, vote) : handler.addServerVoteDurably(BACKEND, vote));
	}
	private void remove(VoteCacheHandler handler, boolean online, OfflineBungeeVote vote) {
		if (online) assertTrue(handler.tryRemoveOnlineVote(PLAYER, vote));
		else handler.removeServerVotes(BACKEND, new ArrayList<>(List.of(vote)));
	}

	@TestFactory
	Stream<DynamicTest> partialReplayAndRestartRetainOriginalPositionAndDeliveryFlags() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity);
			VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote first = vote(UUID.randomUUID()), second = vote(UUID.randomUUID());
			add(writer, online, first); add(writer, online, second);
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(first, second), 2));
			assertEquals(1, first.getCachedBatchNumber());
			assertEquals(2, second.getCachedBatchNumber());
			remove(writer, online, first);
			assertEquals(1, votes(writer, online).size());

			VoteCacheHandler restarted = handler(cache(velocity));
			restarted.load();
			OfflineBungeeVote restored = votes(restarted, online).get(0);
			assertEquals(second.getVoteId(), restored.getVoteId());
			assertEquals(2, restored.getCachedBatchNumber());
			assertEquals(2, restored.getCachedBatchSize());
			assertTrue(restored.isCachedBatchDurable());
			assertTrue(restored.isWasOnline());
			assertTrue(restored.isDelayValidated());
			assertTrue(restored.isProxyBroadcastHandled());
			assertEquals(second.getBroadcastTargets(), restored.getBroadcastTargets());
			assertEquals(second.getBroadcastForwardedServers(), restored.getBroadcastForwardedServers());
			assertEquals(second.getHttpDeliveryIds(), restored.getHttpDeliveryIds());
			assertEquals(second.getHttpBroadcastDeliveryIds(), restored.getHttpBroadcastDeliveryIds());
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			assertEquals(2, restored.getCachedBatchNumber());
			assertEquals(2, restored.getCachedBatchSize());
			restored.setRewardDelivered(true);
			assertTrue(online ? restarted.updateOnlineVote(PLAYER, restored) : restarted.updateServerVote(BACKEND, restored));
			VoteCacheHandler next = handler(cache(velocity)); next.load();
			assertTrue(votes(next, online).get(0).isRewardDelivered());
			assertEquals(2, votes(next, online).get(0).getCachedBatchNumber());
		});
	}

	@TestFactory
	Stream<DynamicTest> newVotesBesideAPartialBatchHaveTheirOwnCompletePositions() {
		return lanes((velocity, online) -> {
			VoteCacheHandler writer = handler(cache(velocity));
			OfflineBungeeVote first = vote(UUID.randomUUID()), second = vote(UUID.randomUUID());
			add(writer, online, first); add(writer, online, second);
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(first, second), 2));
			remove(writer, online, first);
			OfflineBungeeVote fresh = vote(UUID.randomUUID());
			add(writer, online, fresh);
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(second, fresh), 2));
			assertEquals(2, second.getCachedBatchNumber());
			assertEquals(2, second.getCachedBatchSize());
			assertEquals(1, fresh.getCachedBatchNumber());
			assertEquals(1, fresh.getCachedBatchSize());
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			assertEquals(2, votes(restarted, online).size());
			OfflineBungeeVote old = votes(restarted, online).stream()
					.filter(vote -> second.getVoteId().equals(vote.getVoteId())).findFirst().orElseThrow();
			OfflineBungeeVote added = votes(restarted, online).stream()
					.filter(vote -> fresh.getVoteId().equals(vote.getVoteId())).findFirst().orElseThrow();
			assertEquals(2, old.getCachedBatchNumber());
			assertEquals(2, old.getCachedBatchSize());
			assertEquals(1, added.getCachedBatchNumber());
			assertEquals(1, added.getCachedBatchSize());
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(old, added), 2));
			assertEquals(2, old.getCachedBatchSize());
			assertEquals(1, added.getCachedBatchSize());
		});
	}

	@TestFactory
	Stream<DynamicTest> failedPublicationLeavesOriginalsAndDiskUnchangedAndRetryUsesSameEntries() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity));
			VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote first = vote(UUID.randomUUID()), second = vote(UUID.randomUUID());
			add(writer, online, first); add(writer, online, second);
			String durableBefore = Files.readString(storage.getStoragePath());
			doThrow(new IOException("synthetic publication failure")).doCallRealMethod().when(storage).saveDurably();
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(first, second), 2));
			assertEquals(0, first.getCachedBatchNumber());
			assertEquals(0, second.getCachedBatchNumber());
			assertFalse(first.isCachedBatchDurable());
			assertEquals(durableBefore, Files.readString(storage.getStoragePath()));
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(first, second), 2));
			assertEquals(2, online ? storage.getOnlineVotes(PLAYER).size() : storage.getServerVotes(BACKEND).size());
		});
	}

	@TestFactory
	Stream<DynamicTest> verifiedEmptyStagingDoesNotMutateAQuarantinedJournal() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity));
			VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote durable = vote(UUID.randomUUID()), fresh = vote(UUID.randomUUID());
			add(writer, online, durable);
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(durable), 1));
			add(writer, online, fresh);
			String before = Files.readString(storage.getStoragePath());
			doThrow(new IOException("synthetic publication failure")).when(storage).saveDurably();
			doThrow(new IllegalStateException("synthetic rollback reload failure")).when(storage).reload();
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
			assertTrue(writer.isJsonStorageQuarantined());
			clearInvocations(storage);

			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(), 0));
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(durable), 1));
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
			assertFalse(fresh.hasCachedVoteBatch());
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(vote(UUID.randomUUID())), 1));
			durable.setCachedVoteBatch(2, 1);
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(durable), 1));
			assertTrue(writer.isJsonStorageQuarantined());
			assertEquals(before, Files.readString(storage.getStoragePath()));
			verify(storage, never()).saveDurably();
			verify(storage, never()).reload();
			verify(storage, never()).addVote(anyString(), anyInt(), any());
			verify(storage, never()).addVoteOnline(anyString(), anyInt(), any());
			assertTrue(handler(null).prepareCachedVoteBatch(owner(online), online, List.of(), 0));
		});
	}

	@TestFactory
	Stream<DynamicTest> legacyRowsWithoutBatchMetadataAreUpgradedWithoutRenumberingOwners() {
		return lanes((velocity, online) -> {
			String lane = online ? "OnlineCache" : "VoteCache";
			Files.writeString(directory.resolve("votecache.json"), "{\"" + lane + "\":{\"" + owner(online)
					+ "\":{\"4\":{\"Name\":\"test-player\",\"UUID\":\"" + PLAYER
					+ "\",\"Service\":\"test-service\",\"Time\":100}}}}");
			VoteCacheHandler writer = handler(cache(velocity)); writer.load();
			OfflineBungeeVote restored = votes(writer, online).get(0);
			assertEquals(0, restored.getCachedBatchNumber());
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			assertEquals("4", online ? restored.getOnlineVoteCacheJsonKey() : restored.getServerVoteCacheJsonKey());
			assertEquals(1, restored.getCachedBatchNumber());
			assertNotNull(restored.getVoteId());
			UUID identity = restored.getVoteId();
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			assertEquals(identity, votes(restarted, online).get(0).getVoteId());
		});
	}

	@TestFactory
	Stream<DynamicTest> corruptOrMissingJournalCannotOverwriteDurableStateAndValidReloadRecovers() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote vote = vote(UUID.randomUUID()); add(writer, online, vote);
			String before = Files.readString(storage.getStoragePath());
			Files.writeString(storage.getStoragePath(), "{\"VoteCache\":");
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(vote), 1));
			assertEquals(0, vote.getCachedBatchNumber());
			storage.save();
			assertEquals("{\"VoteCache\":", Files.readString(storage.getStoragePath()));
			Files.writeString(storage.getStoragePath(), before);
			writer.load();
			OfflineBungeeVote recovered = votes(writer, online).get(0);
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(recovered), 1));
			Files.delete(storage.getStoragePath());
			assertThrows(RuntimeException.class, storage::reload);
			storage.save();
			assertFalse(Files.exists(storage.getStoragePath()));
			assertThrows(IOException.class, storage::saveDurably);
		});
	}

	@TestFactory
	Stream<DynamicTest> failedCheckedSqlReloadRetainsJournalAndMapsThenNullTwinRecovers() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity);
            OfflineBungeeVote legacy = vote(null);
            VoteCacheHandler sql = sqlHandler(storage, online, List.of(legacy), List.of(71));
            sql.load();
            OfflineBungeeVote original = votes(sql, online).get(0);
            assertTrue(sql.prepareCachedVoteBatch(owner(online), online, List.of(original), 1));
            assertEquals(71, original.getCachedBatchSourceRowId());
            String bytes = Files.readString(storage.getStoragePath());
			Object table = getField(sql, online ? "onlineVoteCacheTable" : "voteCacheTable");
			if (online) doThrow(new SQLException("initial read failed")).when((ProxyOnlineVoteCacheTable) table).getAllVotesChecked();
			else doThrow(new SQLException("initial read failed")).when((ProxyVoteCacheTable) table).getAllVotesChecked();
			assertThrows(IllegalStateException.class, sql::load);
			assertEquals(bytes, Files.readString(storage.getStoragePath()));
			assertEquals(1, votes(sql, online).size());

            VoteCacheHandler cold = sqlHandler(cache(velocity), online, List.of(legacy), List.of(71));
            Object coldTable = getField(cold, online ? "onlineVoteCacheTable" : "voteCacheTable");
            if (online) doThrow(new SQLException("synthetic initial read failure")).when((ProxyOnlineVoteCacheTable) coldTable).getAllVotesChecked();
            else doThrow(new SQLException("synthetic initial read failure")).when((ProxyVoteCacheTable) coldTable).getAllVotesChecked();
            assertThrows(IllegalStateException.class, cold::load);
            assertTrue(votes(cold, online).isEmpty(), "unknown SQL absence published a JSON reward owner");
            assertEquals(bytes, Files.readString(storage.getStoragePath()));

			VoteCacheHandler recovered = sqlHandler(storage, online, List.of(new OfflineBungeeVote((UUID) null,
				original.getPlayerName(), original.getUuid(), original.getService(), original.getTime(), true,
				original.getText())), List.of(71));
			recovered.load();
			assertEquals(1, votes(recovered, online).size());
			OfflineBungeeVote restored = votes(recovered, online).get(0);
			assertEquals(original.getVoteId(), restored.getVoteId());
			assertEquals(original.getCachedBatchNumber(), restored.getCachedBatchNumber());
			assertEquals(original.getCachedBatchSize(), restored.getCachedBatchSize());
		});
	}

	private static Object getField(Object target, String name) throws Exception {
		Field field = VoteCacheHandler.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}

	@TestFactory
	Stream<DynamicTest> invalidBatchValuesAreRetainedButNeverPreparedForDelivery() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote vote = vote(UUID.randomUUID()); add(writer, online, vote);
			Path file = storage.getStoragePath();
			Files.writeString(file, Files.readString(file).replace("\"CachedBatchNumber\": 0", "\"CachedBatchNumber\": 1.5"));
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			OfflineBungeeVote restored = votes(restarted, online).get(0);
			assertFalse(restored.isCachedVoteBatchValid());
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			assertEquals(1, votes(restarted, online).size());
		});
	}

	@TestFactory
	Stream<DynamicTest> routineSaveCannotInterleaveBatchPublicationAndReadback() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity)); VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote first = vote(UUID.randomUUID()), second = vote(UUID.randomUUID());
			add(writer, online, first); add(writer, online, second);
			CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1), saving = new CountDownLatch(1);
			doAnswer(invocation -> {
				entered.countDown();
				assertTrue(release.await(5, TimeUnit.SECONDS));
				return invocation.callRealMethod();
			}).when(storage).saveDurably();
			var workers = Executors.newFixedThreadPool(2);
			try {
				var preparing = workers.submit(() -> writer.prepareCachedVoteBatch(owner(online), online, List.of(first, second), 2));
				assertTrue(entered.await(5, TimeUnit.SECONDS));
				assertEquals(0, first.getCachedBatchNumber());
				var routineSave = workers.submit(() -> { saving.countDown(); storage.save(); });
				assertTrue(saving.await(5, TimeUnit.SECONDS));
				assertFalse(routineSave.isDone());
				release.countDown();
				assertTrue(preparing.get(5, TimeUnit.SECONDS));
				routineSave.get(5, TimeUnit.SECONDS);
			} finally { release.countDown(); workers.shutdownNow(); }
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			assertEquals(List.of(1, 2), votes(restarted, online).stream().map(OfflineBungeeVote::getCachedBatchNumber).toList());
		});
	}

	@Test
	void repeatedEmptyCacheSavesRemainOneCompleteObject() throws Exception {
		for (boolean velocity : List.of(false, true)) {
			IVoteCache storage = cache(velocity);
			for (int save = 0; save < 3; save++) {
				storage.clearData();
				storage.saveDurably();
				assertEquals("{}", Files.readString(storage.getStoragePath()).trim());
				storage.reload();
			}
		}
	}

	@Test
	void malformedDocumentsPauseBothAdaptersUntilACompleteObjectIsLoaded() throws Exception {
		for (boolean velocity : List.of(false, true)) {
			IVoteCache storage = cache(velocity);
			for (String malformed : List.of("[]", "{} {}", "{\"a\":1,\"a\":2}", "{unquoted:1}", "")) {
				Files.writeString(storage.getStoragePath(), malformed);
				assertThrows(RuntimeException.class, storage::reload);
				storage.save(); assertEquals(malformed, Files.readString(storage.getStoragePath()));
				assertThrows(IOException.class, storage::saveDurably);
				Files.writeString(storage.getStoragePath(), "{}"); storage.reload(); storage.saveDurably();
			}
		}
	}

	@TestFactory
	Stream<DynamicTest> invalidGeneratedJournalCannotReplaceTheLastDurableCache() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity);
			OfflineBungeeVote durable = vote(UUID.randomUUID());
			add(handler(storage), online, durable);
			String before = Files.readString(storage.getStoragePath());
			java.util.Map<String, Object> nested = new java.util.LinkedHashMap<>();
			java.util.Map<String, Object> child = nested;
			for (int depth = 0; depth < 70; depth++) {
				java.util.Map<String, Object> next = new java.util.LinkedHashMap<>();
				child.put("nested", next);
				child = next;
			}
			child.put("value", "synthetic");
			if (storage instanceof VelocityJsonVoteCache adapter)
				adapter.getConf().node("invalid").set(nested);
			else ((BungeeJsonVoteCache) storage).getConf().add("invalid", new com.google.gson.Gson().toJsonTree(nested));
			assertThrows(IOException.class, storage::saveDurably);
			assertEquals(before, Files.readString(storage.getStoragePath()));
			storage.reload();
			VoteCacheHandler recovered = handler(storage); recovered.load();
			assertEquals(durable.getVoteId(), votes(recovered, online).get(0).getVoteId());
			storage.saveDurably();
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			assertEquals(1, votes(restarted, online).size());
			assertEquals(durable.getVoteId(), votes(restarted, online).get(0).getVoteId());
		});
	}

	@TestFactory
	Stream<DynamicTest> legacySqlTwinsRequirePositiveExactRowAndCompleteIdentity() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity);
			OfflineBungeeVote first = vote(null), second = vote(null);
			// Equal original payloads are distinct durable occurrences identified by rows 71 and 72.
			second.setHttpDeliveryId(BACKEND, first.getHttpDeliveryId(BACKEND));
			second.setHttpBroadcastDeliveryId("backend-b", first.getHttpBroadcastDeliveryId("backend-b"));
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(first, second), List.of(71, 72));
			sql.load();
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(sql, online)), 2));
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(first, second), List.of(71, 72));
			restarted.load();
			assertEquals(2, votes(restarted, online).size());
			assertNotNull(votes(restarted, online).get(0).getVoteId());
			assertNotNull(votes(restarted, online).get(1).getVoteId());
			assertNotEquals(votes(restarted, online).get(0).getVoteId(), votes(restarted, online).get(1).getVoteId());
			assertEquals(votes(sql, online).stream().map(OfflineBungeeVote::getVoteId).toList(),
					votes(restarted, online).stream().map(OfflineBungeeVote::getVoteId).toList());
			assertEquals(List.of(71, 72), votes(restarted, online).stream().map(OfflineBungeeVote::getCachedBatchSourceRowId).toList());
			assertEquals(List.of(1, 2), votes(restarted, online).stream().map(OfflineBungeeVote::getCachedBatchNumber).toList());
			remove(restarted, online, votes(restarted, online).get(0));
			assertEquals(1, votes(restarted, online).size());
			assertEquals(72, votes(restarted, online).get(0).getCachedBatchSourceRowId());
		});
	}

	@TestFactory
	Stream<DynamicTest> missingLegacySourceRowsAreRetiredWithoutCollapsingOtherSqlRows() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); OfflineBungeeVote original = vote(null);
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(sql, online)), 1));
			VoteCacheHandler differentRow = sqlHandler(cache(velocity), online, List.of(original), List.of(72)); differentRow.load();
			assertEquals(1, votes(differentRow, online).size());
			assertEquals(72, online ? votes(differentRow, online).get(0).getOnlineVoteCacheRowId()
					: votes(differentRow, online).get(0).getServerVoteCacheRowId());
			assertNull(votes(differentRow, online).get(0).getVoteId());
			assertFalse(votes(differentRow, online).get(0).hasCachedVoteBatch());
			IVoteCache reloaded = cache(velocity);
			assertTrue((online ? reloaded.getOnlineVotes(PLAYER) : reloaded.getServerVotes(BACKEND)).isEmpty());
		});
	}

	@TestFactory
	Stream<DynamicTest> mismatchedExistingSqlSourcePayloadStaysOutsideReplay() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); OfflineBungeeVote original = vote(null);
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(sql, online)), 1));
			String before = Files.readString(storage.getStoragePath());
			OfflineBungeeVote differentPayload = new OfflineBungeeVote((UUID) null, "test-player", PLAYER,
					"test-service", 100L, false, "other-totals");
			VoteCacheHandler differentIdentity = sqlHandler(cache(velocity), online, List.of(differentPayload), List.of(71));
			differentIdentity.load(); assertEquals(1, votes(differentIdentity, online).size());
			assertEquals(before, Files.readString(storage.getStoragePath()));
			assertEquals(0, votes(differentIdentity, online).stream().filter(OfflineBungeeVote::hasCachedVoteBatch).count());
		});
	}

    @TestFactory
    Stream<DynamicTest> rejectedSqlDeletionPreservesPairedCompletionMetadata() {
        return lanes((velocity, online) -> {
            IVoteCache storage = spy(cache(velocity)); OfflineBungeeVote original = vote(null);
            VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
            OfflineBungeeVote retained = votes(sql, online).get(0);
            assertTrue(sql.prepareCachedVoteBatch(owner(online), online, List.of(retained), 1));
            String before = Files.readString(storage.getStoragePath());
            if (online) when(((ProxyOnlineVoteCacheTable) getField(sql, "onlineVoteCacheTable"))
                    .tryRemoveVote(any())).thenReturn(false);
            else when(((ProxyVoteCacheTable) getField(sql, "voteCacheTable"))
                    .tryRemoveVote(any(), anyString())).thenReturn(false);
            sql.checkVoteCacheTime(1);
            assertEquals(List.of(retained), votes(sql, online));
            assertEquals(before, Files.readString(storage.getStoragePath()),
                    "failed SQL deletion discarded the paired durable metadata");
            if (online) verify(storage, never()).removeOnlineVote(any(OfflineBungeeVote.class));
            else verify(storage, never()).removeVote(anyString(), any(OfflineBungeeVote.class));
        });
    }

	@TestFactory
	Stream<DynamicTest> expiredSqlBatchTwinsDoNotReturnAfterFailedJsonRemoval() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity)); OfflineBungeeVote original = vote(null);
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			OfflineBungeeVote expired = votes(sql, online).get(0);
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, List.of(expired), 1));
			assertFalse(expired.isRewardDelivered());
			String before = Files.readString(storage.getStoragePath());
			doThrow(new IOException("synthetic deletion publication failure")).when(storage).saveDurably();

			sql.checkVoteCacheTime(1);

			verifySqlRemoval(sql, online, expired);
			assertEquals(before, Files.readString(storage.getStoragePath()));
            assertTrue(votes(sql, online).isEmpty(), "confirmed SQL deletion left an expired owner eligible in this process");
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(), List.of());
			restarted.load();
			assertTrue(votes(restarted, online).isEmpty(), "expired SQL owner returned as a reward-bearing JSON vote");
			IVoteCache reloaded = cache(velocity);
			assertTrue((online ? reloaded.getOnlineVotes(PLAYER) : reloaded.getServerVotes(BACKEND)).isEmpty());
		});
	}

	@TestFactory
	Stream<DynamicTest> interruptedPairedRemovalDoesNotReactivateAConfirmedMissingSource() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity)); OfflineBungeeVote original = vote(UUID.randomUUID());
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			OfflineBungeeVote removed = votes(sql, online).get(0);
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, List.of(removed), 1));
			String before = Files.readString(storage.getStoragePath());
			if (online) doThrow(new IllegalStateException("synthetic stop between store deletions")).when(storage).removeOnlineVote(removed);
			else doThrow(new IllegalStateException("synthetic stop between store deletions")).when(storage).removeVote(BACKEND, removed);

			assertThrows(IllegalStateException.class, () -> remove(sql, online, removed));

			verifySqlRemoval(sql, online, removed);
			assertEquals(before, Files.readString(storage.getStoragePath()));
			IVoteCache recoveringStorage = spy(cache(velocity));
			doThrow(new IOException("synthetic orphan cleanup failure")).when(recoveringStorage).saveDurably();
			VoteCacheHandler restarted = sqlHandler(recoveringStorage, online, List.of(), List.of());
			restarted.load();
			assertTrue(votes(restarted, online).isEmpty(), "failed orphan cleanup published a removed reward owner");
			assertEquals(before, Files.readString(storage.getStoragePath()));
			VoteCacheHandler recovered = sqlHandler(cache(velocity), online, List.of(), List.of());
			recovered.load();
			assertTrue(votes(recovered, online).isEmpty());
			IVoteCache reloaded = cache(velocity);
			assertTrue((online ? reloaded.getOnlineVotes(PLAYER) : reloaded.getServerVotes(BACKEND)).isEmpty());
		});
	}

	private void verifySqlRemoval(VoteCacheHandler handler, boolean online, OfflineBungeeVote removed) throws Exception {
		if (online) verify((ProxyOnlineVoteCacheTable) getField(handler, "onlineVoteCacheTable")).tryRemoveVote(removed);
		else verify((ProxyVoteCacheTable) getField(handler, "voteCacheTable")).tryRemoveVote(removed, BACKEND);
	}

	@TestFactory
	Stream<DynamicTest> stableVoteIdsBindBatchMetadataWithoutCreatingDuplicateSqlDeliveries() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); OfflineBungeeVote original = vote(UUID.randomUUID());
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(sql, online)), 1));
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(original), List.of(71)); restarted.load();
			assertEquals(1, votes(restarted, online).size());
			assertEquals(original.getVoteId(), votes(restarted, online).get(0).getVoteId());
			assertTrue(votes(restarted, online).get(0).isCachedBatchDurable());
		});
	}

	@TestFactory
	Stream<DynamicTest> nullIdRowsWithoutPersistedSourceRemainDistinctAndAcquireDifferentDurableIds() {
		return lanes((velocity, online) -> {
			IVoteCache storage = cache(velocity); OfflineBungeeVote original = vote(null);
			if (online) storage.addVoteOnline(PLAYER, 0, original); else storage.addVote(BACKEND, 0, original);
			storage.saveDurably(); storage.markEmergencyJournalUsed();
			VoteCacheHandler sql = sqlHandler(storage, online, List.of(original), List.of(71)); sql.load();
			assertEquals(2, votes(sql, online).size());
			assertTrue(sql.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(sql, online)), 2));
			assertNotEquals(votes(sql, online).get(0).getVoteId(), votes(sql, online).get(1).getVoteId());
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(original), List.of(71)); restarted.load();
			assertEquals(2, votes(restarted, online).size());
		});
	}

	@TestFactory
	Stream<DynamicTest> failedLegacyIdMigrationDoesNotPublishAnIdentityUntilItsJournalCommit() {
		return lanes((velocity, online) -> {
			IVoteCache storage = spy(cache(velocity)); VoteCacheHandler writer = handler(storage);
			OfflineBungeeVote legacy = vote(null); add(writer, online, legacy);
			doThrow(new IOException("synthetic publication failure")).doCallRealMethod().when(storage).saveDurably();
			assertFalse(writer.prepareCachedVoteBatch(owner(online), online, List.of(legacy), 1));
			assertNull(legacy.getVoteId()); assertEquals(0, legacy.getCachedBatchNumber());
			assertTrue(writer.prepareCachedVoteBatch(owner(online), online, List.of(legacy), 1));
			UUID identity = legacy.getVoteId(); assertNotNull(identity);
			VoteCacheHandler restarted = handler(cache(velocity)); restarted.load();
			assertEquals(identity, votes(restarted, online).get(0).getVoteId());
		});
	}

	private VoteCacheHandler sqlHandler(IVoteCache storage, boolean online, List<OfflineBungeeVote> votes,
			List<Integer> rows) throws Exception {
		VoteCacheHandler handler = handler(storage);
		ProxyVoteCacheTable serverTable = mock(ProxyVoteCacheTable.class);
		ProxyOnlineVoteCacheTable onlineTable = mock(ProxyOnlineVoteCacheTable.class);
		ProxyTimedVoteCacheTable timedTable = mock(ProxyTimedVoteCacheTable.class);
		List<ProxyVoteCacheTable.VoteRow> serverRows = new ArrayList<>();
		List<ProxyOnlineVoteCacheTable.VoteRow> onlineRows = new ArrayList<>();
		for (int index = 0; index < votes.size(); index++) {
			OfflineBungeeVote v = votes.get(index); String id = v.getVoteId() == null ? null : v.getVoteId().toString();
			if (online) onlineRows.add(new ProxyOnlineVoteCacheTable.VoteRow(rows.get(index), id, v.getUuid(),
					v.getPlayerName(), v.getService(), v.getTime(), v.isRealVote(), v.isWasOnline(), v.isWasOnlineKnown(),
					1, v.getText(), v.isBroadcastForwarded(), v.isProxyBroadcastHandled(), v.encodeBroadcastTargets(),
					v.encodeBroadcastForwardedServers(), v.isRewardDelivered(), v.encodeHttpDeliveryIds(), v.encodeHttpBroadcastDeliveryIds()));
			else serverRows.add(new ProxyVoteCacheTable.VoteRow(rows.get(index), id, v.getUuid(), v.getPlayerName(),
					v.getService(), v.getTime(), v.isRealVote(), v.isWasOnline(), v.isWasOnlineKnown(), 1, v.getText(),
					v.isBroadcastForwarded(), v.isProxyBroadcastHandled(), v.encodeBroadcastTargets(),
					v.encodeBroadcastForwardedServers(), v.isRewardDelivered(), v.encodeHttpDeliveryIds(),
					v.encodeHttpBroadcastDeliveryIds(), BACKEND));
		}
		when(serverTable.getAllVotesChecked()).thenReturn(serverRows); when(onlineTable.getAllVotesChecked()).thenReturn(onlineRows);
		when(timedTable.getAllVotesChecked()).thenReturn(List.of());
		when(serverTable.tryRemoveVote(any(), anyString())).thenReturn(true);
		when(onlineTable.tryRemoveVote(any())).thenReturn(true);
		for (var pair : List.of(new Object[] {"useMySQL", true}, new Object[] {"voteCacheTable", serverTable},
				new Object[] {"onlineVoteCacheTable", onlineTable}, new Object[] {"timedVoteCacheTable", timedTable})) {
			Field field = VoteCacheHandler.class.getDeclaredField((String) pair[0]);
			field.setAccessible(true); field.set(handler, pair[1]);
		}
		return handler;
	}
}
