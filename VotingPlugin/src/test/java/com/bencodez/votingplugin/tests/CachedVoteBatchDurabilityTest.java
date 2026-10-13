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
import java.util.function.Consumer;
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
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

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
			JsonObject document = JsonParser.parseString(Files.readString(file)).getAsJsonObject();
			journalRows(document, online).entrySet().forEach(entry ->
					entry.getValue().getAsJsonObject().addProperty("CachedBatchNumber", 1.5));
			Files.writeString(file, document.toString());
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

	private record PreparedJournal(IVoteCache storage, List<OfflineBungeeVote> sqlVotes,
			List<UUID> identities, String cleanBytes) { }

	private PreparedJournal preparedJournal(boolean velocity, boolean online, boolean stableSqlId) throws Exception {
		Files.deleteIfExists(directory.resolve("votecache.json"));
		IVoteCache storage = cache(velocity);
		List<OfflineBungeeVote> originals = List.of(vote(stableSqlId ? UUID.randomUUID() : null), vote(null));
		VoteCacheHandler writer = sqlHandler(storage, online, originals, List.of(41, 42));
		writer.load();
		assertTrue(writer.prepareCachedVoteBatch(owner(online), online, new ArrayList<>(votes(writer, online)), 2));
		return new PreparedJournal(storage, originals,
				votes(writer, online).stream().map(OfflineBungeeVote::getVoteId).toList(),
				Files.readString(storage.getStoragePath()));
	}

	private JsonObject journalRows(JsonObject journal, boolean online) {
		return journal.getAsJsonObject(online ? "OnlineCache" : "VoteCache").getAsJsonObject(owner(online));
	}

	private JsonObject sourceRow(JsonObject journal, boolean online, int source) {
		return journalRows(journal, online).entrySet().stream().map(entry -> entry.getValue().getAsJsonObject())
				.filter(row -> row.get("CachedBatchSourceRowId").getAsInt() == source).findFirst().orElseThrow();
	}

	private OfflineBungeeVote sqlRow(VoteCacheHandler handler, boolean online, int row) {
		return votes(handler, online).stream().filter(vote -> (online ? vote.getOnlineVoteCacheRowId()
				: vote.getServerVoteCacheRowId()) == row).findFirst().orElseThrow();
	}

	private void verifyOnlyCheckedSqlReads(VoteCacheHandler handler) throws Exception {
		verifyOnlyCheckedSqlReads(handler, 1);
	}

	private void verifyOnlyCheckedSqlReads(VoteCacheHandler handler, int reads) throws Exception {
		ProxyVoteCacheTable server = (ProxyVoteCacheTable) getField(handler, "voteCacheTable");
		ProxyOnlineVoteCacheTable online = (ProxyOnlineVoteCacheTable) getField(handler, "onlineVoteCacheTable");
		ProxyTimedVoteCacheTable timed = (ProxyTimedVoteCacheTable) getField(handler, "timedVoteCacheTable");
		verify(server, times(reads)).getAllVotesChecked();
		verify(online, times(reads)).getAllVotesChecked();
		verify(timed, times(reads)).getAllVotesChecked();
		verifyNoMoreInteractions(server, online, timed);
	}

	@TestFactory
	Stream<DynamicTest> malformedSqlBatchPositionsKeepIdentityAndSourceWithoutBecomingDurable() {
		return lanes((velocity, online) -> {
			List<Consumer<JsonObject>> corruptions = List.of(
					row -> row.addProperty("CachedBatchNumber", "not-a-number"),
					row -> row.addProperty("CachedBatchNumber", 1.5),
					row -> row.addProperty("CachedBatchNumber", 2147483648L),
					row -> row.remove("CachedBatchSize"),
					row -> row.addProperty("CachedBatchSize", 0));
			for (Consumer<JsonObject> corrupt : corruptions) {
				PreparedJournal prepared = preparedJournal(velocity, online, false);
				JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
				corrupt.accept(sourceRow(journal, online, 41));
				String malformed = journal.toString();
				Files.writeString(prepared.storage().getStoragePath(), malformed);
				VoteCacheHandler restarted = sqlHandler(cache(velocity), online,
						List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
				restarted.load();
				assertEquals(3, votes(restarted, online).size(), "malformed batch created a second reward owner");
				OfflineBungeeVote invalid = sqlRow(restarted, online, 41);
				assertEquals(prepared.identities().get(0), invalid.getVoteId());
				assertEquals(41, invalid.getCachedBatchSourceRowId());
				assertFalse(invalid.isCachedVoteBatchValid());
				assertFalse(invalid.isCachedBatchDurable());
				assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(invalid), 1));
				OfflineBungeeVote clean = sqlRow(restarted, online, 42), fresh = sqlRow(restarted, online, 43);
				assertEquals(prepared.identities().get(1), clean.getVoteId());
				assertEquals(2, clean.getCachedBatchNumber());
				assertTrue(clean.isCachedBatchDurable());
				assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(clean), 1));
				assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
				assertNull(fresh.getVoteId());
				assertFalse(fresh.hasCachedVoteBatch());
				assertEquals(malformed, Files.readString(prepared.storage().getStoragePath()));
				verifyOnlyCheckedSqlReads(restarted);
			}
		});
	}

	@TestFactory
	Stream<DynamicTest> malformedRowsFenceNewBatchesAcrossLanesWhileCleanPreparedWorkContinues() {
		return lanes((velocity, online) -> {
			List<Consumer<JsonObject>> corruptions = List.of(
					row -> row.remove("CachedBatchSourceRowId"),
					row -> row.addProperty("CachedBatchSourceRowId", -41),
					row -> row.addProperty("CachedBatchSourceRowId", "not-a-number"),
					row -> row.addProperty("Real", "true"),
					row -> row.addProperty("Time", "100"),
					row -> row.addProperty("Time", 100.5),
					row -> row.add("Name", new JsonObject()));
			// The malformed row is in the opposite lane from the clean and fresh SQL work.
			for (int variant = 0; variant < corruptions.size() + 2; variant++) {
				PreparedJournal prepared = preparedJournal(velocity, online, false);
				JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
				JsonObject malformedRow = sourceRow(journal, online, 41).deepCopy();
				JsonObject oppositeRows = new JsonObject(), oppositeOwners = new JsonObject();
				if (variant < corruptions.size()) {
					corruptions.get(variant).accept(malformedRow);
					oppositeRows.add("9", malformedRow);
				} else oppositeRows.add("9", JsonParser.parseString(variant == corruptions.size() ? "null" : "7"));
				oppositeOwners.add(owner(!online), oppositeRows);
				journal.add(online ? "VoteCache" : "OnlineCache", oppositeOwners);
				String malformed = journal.toString();
				Files.writeString(prepared.storage().getStoragePath(), malformed);
				VoteCacheHandler restarted = sqlHandler(cache(velocity), online,
						List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
				restarted.load();
				OfflineBungeeVote clean = sqlRow(restarted, online, 42), fresh = sqlRow(restarted, online, 43);
				assertTrue(clean.isCachedBatchDurable(), "unrelated malformed row blocked clean recovery, variant " + variant);
				assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(clean), 1));
				assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1),
						"malformed row did not fence the other lane, variant " + variant);
				assertNull(fresh.getVoteId());
				assertFalse(fresh.hasCachedVoteBatch());
				assertEquals(malformed, Files.readString(prepared.storage().getStoragePath()));
				verifyOnlyCheckedSqlReads(restarted);
			}
		});
	}

	@TestFactory
	Stream<DynamicTest> malformedRecoveryFenceClearsOnlyAfterSuccessfulFullCheckedLoad() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, false);
			JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
			sourceRow(journal, online, 41).addProperty("CachedBatchNumber", "invalid");
			String malformed = journal.toString();
			Files.writeString(prepared.storage().getStoragePath(), malformed);
			IVoteCache storage = cache(velocity);
			VoteCacheHandler restarted = sqlHandler(storage, online,
					List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
			restarted.load();
			OfflineBungeeVote fresh = sqlRow(restarted, online, 43);
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
			assertEquals(malformed, Files.readString(storage.getStoragePath()));
			Files.writeString(storage.getStoragePath(), prepared.cleanBytes());
			storage.reload();
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1),
					"adapter-only reload cleared the handler recovery fence");
			ProxyTimedVoteCacheTable timed = (ProxyTimedVoteCacheTable) getField(restarted, "timedVoteCacheTable");
			doThrow(new SQLException("last SQL lane unreadable")).when(timed).getAllVotesChecked();
			assertThrows(IllegalStateException.class, restarted::load);
			assertSame(fresh, sqlRow(restarted, online, 43));
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1),
					"partially checked SQL snapshot cleared the recovery fence");
			assertEquals(prepared.cleanBytes(), Files.readString(storage.getStoragePath()));
			doReturn(List.of()).when(timed).getAllVotesChecked();
			restarted.load();
			OfflineBungeeVote restored = sqlRow(restarted, online, 41);
			assertEquals(prepared.identities().get(0), restored.getVoteId());
			assertTrue(restored.isCachedBatchDurable());
			OfflineBungeeVote admitted = sqlRow(restarted, online, 43);
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(admitted), 1));
			assertNotNull(admitted.getVoteId());
			assertEquals(43, admitted.getCachedBatchSourceRowId());
			assertTrue(admitted.isCachedBatchDurable());
			assertEquals(3, votes(restarted, online).size());
			verifyOnlyCheckedSqlReads(restarted, 3);
		});
	}

	@TestFactory
	Stream<DynamicTest> failedSqlReconstructionRetainsOwnedInstancesAndFenceUntilFullRecovery() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, false);
			IVoteCache storage = cache(velocity);
			VoteCacheHandler restarted = sqlHandler(storage, online,
					List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
			restarted.load();
			ArrayList<OfflineBungeeVote> owners = votes(restarted, online);
			OfflineBungeeVote durable = sqlRow(restarted, online, 41), fresh = sqlRow(restarted, online, 43);
			var timedOwners = restarted.getTimeChangeQueue();
			assertEquals(prepared.identities().get(0), durable.getVoteId());
			assertTrue(durable.isCachedBatchDurable());
			String before = Files.readString(storage.getStoragePath());
			verifyOnlyCheckedSqlReads(restarted);

			ProxyVoteCacheTable serverTable = (ProxyVoteCacheTable) getField(restarted, "voteCacheTable");
			ProxyOnlineVoteCacheTable onlineTable = (ProxyOnlineVoteCacheTable) getField(restarted, "onlineVoteCacheTable");
			ProxyTimedVoteCacheTable timedTable = (ProxyTimedVoteCacheTable) getField(restarted, "timedVoteCacheTable");
			// Every checked read succeeds, but a malformed row fails the subsequent reconstruction.
			List<?> healthyRows = online ? onlineTable.getAllVotesChecked() : serverTable.getAllVotesChecked();
			if (online) doReturn(java.util.Collections.singletonList(null)).when(onlineTable).getAllVotesChecked();
			else doReturn(java.util.Collections.singletonList(null)).when(serverTable).getAllVotesChecked();
			clearInvocations(serverTable, onlineTable, timedTable);

			assertThrows(RuntimeException.class, restarted::load);
			assertSame(owners, votes(restarted, online));
			assertSame(durable, sqlRow(restarted, online, 41));
			assertSame(fresh, sqlRow(restarted, online, 43));
			assertSame(timedOwners, restarted.getTimeChangeQueue());
			assertEquals(prepared.identities(), owners.subList(0, 2).stream().map(OfflineBungeeVote::getVoteId).toList());
			assertEquals(41, durable.getCachedBatchSourceRowId());
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(durable), 1));
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
			assertNull(fresh.getVoteId());
			assertFalse(fresh.hasCachedVoteBatch());
			assertEquals(before, Files.readString(storage.getStoragePath()));

			if (online) doReturn(healthyRows).when(onlineTable).getAllVotesChecked();
			else doReturn(healthyRows).when(serverTable).getAllVotesChecked();
			restarted.load();
			OfflineBungeeVote recovered = sqlRow(restarted, online, 41), admitted = sqlRow(restarted, online, 43);
			assertEquals(durable.getVoteId(), recovered.getVoteId());
			assertEquals(41, recovered.getCachedBatchSourceRowId());
			assertTrue(recovered.isCachedBatchDurable());
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(admitted), 1));
			assertNotNull(admitted.getVoteId());
			assertTrue(admitted.isCachedBatchDurable());
			assertEquals(3, votes(restarted, online).size());
			verifyOnlyCheckedSqlReads(restarted, 2);
		});
	}

	@TestFactory
	Stream<DynamicTest> healthyUpdatesAndRemovalsPreserveMalformedRowsAndRestartFence() {
		return lanes((velocity, online) -> {
			for (int variant = 0; variant < 9; variant++) {
				PreparedJournal prepared = preparedJournal(velocity, online, false);
				JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
				JsonObject malformedObject = sourceRow(journal, online, 41).deepCopy();
				if (variant == 2) malformedObject.add("Name", new JsonObject());
				if (variant == 3) malformedObject.addProperty("CachedBatchSize", 0);
				if (variant == 4) malformedObject.add("VoteId", com.google.gson.JsonNull.INSTANCE);
				if (variant == 5) malformedObject.add("RewardDelivered", com.google.gson.JsonNull.INSTANCE);
				if (variant == 6) malformedObject.add("HttpDeliveryIds", com.google.gson.JsonNull.INSTANCE);
				if (variant == 7) {
					malformedObject.addProperty("WasOnlineKnown", true);
					malformedObject.add("WasOnline", com.google.gson.JsonNull.INSTANCE);
				}
				if (variant == 8) malformedObject.addProperty("VoteId", "not-a-uuid");
				var malformed = variant < 2 ? JsonParser.parseString(variant == 0 ? "null" : "7") : malformedObject;
				JsonObject oppositeRows = new JsonObject(), oppositeOwners = new JsonObject();
				oppositeRows.add("9", malformed);
				oppositeOwners.add(owner(!online), oppositeRows);
				journal.add(online ? "VoteCache" : "OnlineCache", oppositeOwners);
				Files.writeString(prepared.storage().getStoragePath(), journal.toString());
				IVoteCache storage = spy(cache(velocity));
				OfflineBungeeVote freshSource = vote(null);
				VoteCacheHandler recovering = sqlHandler(storage, online,
						List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), freshSource), List.of(41, 42, 43));
				recovering.load();
				OfflineBungeeVote clean = sqlRow(recovering, online, 42), fresh = sqlRow(recovering, online, 43);
				assertTrue(clean.isCachedBatchDurable());
				assertFalse(recovering.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
				ProxyVoteCacheTable serverTable = (ProxyVoteCacheTable) getField(recovering, "voteCacheTable");
				ProxyOnlineVoteCacheTable onlineTable = (ProxyOnlineVoteCacheTable) getField(recovering, "onlineVoteCacheTable");
				ProxyTimedVoteCacheTable timedTable = (ProxyTimedVoteCacheTable) getField(recovering, "timedVoteCacheTable");
				if (online) when(onlineTable.updateProxyBroadcastState(clean)).thenReturn(true);
				else when(serverTable.updateProxyBroadcastState(clean, BACKEND)).thenReturn(true);
				clean.setRewardDelivered(true);
				assertTrue(online ? recovering.updateOnlineVote(PLAYER, clean) : recovering.updateServerVote(BACKEND, clean));
				JsonObject updated = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
				assertTrue(journalRows(updated, !online).has("9"), "healthy update erased unresolved row, variant " + variant);
				assertEquals(malformed, journalRows(updated, !online).get("9"));
				assertTrue(sourceRow(updated, online, 42).get("RewardDelivered").getAsBoolean(),
						"healthy delivery update was not actually persisted");
				verify(storage).saveDurably();

				remove(recovering, online, clean);
				JsonObject removed = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
				assertTrue(journalRows(removed, !online).has("9"), "healthy removal erased unresolved row, variant " + variant);
				assertEquals(malformed, journalRows(removed, !online).get("9"));
				assertTrue(journalRows(removed, online).entrySet().stream().noneMatch(entry ->
						entry.getValue().getAsJsonObject().get("VoteId").getAsString().equals(clean.getVoteId().toString())));
				verify(storage, times(2)).saveDurably();
				verify(serverTable).getAllVotesChecked();
				verify(onlineTable).getAllVotesChecked();
				verify(timedTable).getAllVotesChecked();
				if (online) {
					verify(onlineTable).updateProxyBroadcastState(clean);
					verify(onlineTable).tryRemoveVote(clean);
				} else {
					verify(serverTable).updateProxyBroadcastState(clean, BACKEND);
					verify(serverTable).tryRemoveVote(clean, BACKEND);
				}
				verifyNoMoreInteractions(serverTable, onlineTable, timedTable);

				VoteCacheHandler restarted = sqlHandler(cache(velocity), online,
						List.of(prepared.sqlVotes().get(0), freshSource), List.of(41, 43));
				restarted.load();
				OfflineBungeeVote durable = sqlRow(restarted, online, 41), blocked = sqlRow(restarted, online, 43);
				assertEquals(2, votes(restarted, online).size());
				assertEquals(prepared.identities().get(0), durable.getVoteId());
				assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(durable), 1));
				assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(blocked), 1),
						"restart cleared the unresolved row fence after healthy writes, variant " + variant);
				assertNull(blocked.getVoteId());
				assertFalse(blocked.hasCachedVoteBatch());
				JsonObject reopened = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
				assertTrue(journalRows(reopened, !online).has("9"));
				assertEquals(malformed, journalRows(reopened, !online).get("9"));
				verifyOnlyCheckedSqlReads(restarted);
			}
		});
	}

	@TestFactory
	Stream<DynamicTest> sqlInsertFallbackReservesRawNullKeysAndRetainsNewVoteIdentity() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, false);
			JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
			JsonObject rows = new JsonObject(), owners = new JsonObject();
			rows.add("0", com.google.gson.JsonNull.INSTANCE);
			owners.add(owner(online), rows);
			journal.add(online ? "OnlineCache" : "VoteCache", owners);
			Files.writeString(prepared.storage().getStoragePath(), journal.toString());
			IVoteCache storage = cache(velocity);
			VoteCacheHandler recovering = sqlHandler(storage, online, List.of(), List.of());
			recovering.load();
			OfflineBungeeVote fresh = vote(UUID.randomUUID());
			// The SQL mock returns zero for insertion, exercising the actual emergency fallback.
			add(recovering, online, fresh);
			assertEquals("1", online ? fresh.getOnlineVoteCacheJsonKey() : fresh.getServerVoteCacheJsonKey());
			JsonObject saved = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertTrue(journalRows(saved, online).has("0"));
			assertTrue(journalRows(saved, online).get("0").isJsonNull());
			assertEquals(fresh.getVoteId().toString(), journalRows(saved, online).getAsJsonObject("1").get("VoteId").getAsString());
			assertFalse(recovering.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
			assertFalse(fresh.hasCachedVoteBatch());
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(), List.of());
			restarted.load();
			assertEquals(1, votes(restarted, online).size());
			OfflineBungeeVote restored = votes(restarted, online).get(0);
			assertEquals(fresh.getVoteId(), restored.getVoteId());
			assertEquals("1", online ? restored.getOnlineVoteCacheJsonKey() : restored.getServerVoteCacheJsonKey());
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			JsonObject reopened = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertEquals(journalRows(saved, online), journalRows(reopened, online));
			verifyOnlyCheckedSqlReads(restarted);
		});
	}

	@TestFactory
	Stream<DynamicTest> invalidBatchOwnerDeliveryUpdatesPersistWithoutReplacingReplayEvidence() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, false);
			JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
			JsonObject malformed = sourceRow(journal, online, 41);
			malformed.addProperty("CachedBatchNumber", "invalid-position");
			Files.writeString(prepared.storage().getStoragePath(), journal.toString());
			IVoteCache storage = cache(velocity);
			VoteCacheHandler recovering = sqlHandler(storage, online, prepared.sqlVotes(), List.of(41, 42));
			recovering.load();
			OfflineBungeeVote invalid = sqlRow(recovering, online, 41), clean = sqlRow(recovering, online, 42);
			assertEquals(prepared.identities().get(0), invalid.getVoteId());
			assertEquals(41, invalid.getCachedBatchSourceRowId());
			assertFalse(invalid.isCachedVoteBatchValid());
			assertFalse(invalid.isCachedBatchDurable());
			ProxyVoteCacheTable serverTable = (ProxyVoteCacheTable) getField(recovering, "voteCacheTable");
			ProxyOnlineVoteCacheTable onlineTable = (ProxyOnlineVoteCacheTable) getField(recovering, "onlineVoteCacheTable");
			if (online) when(onlineTable.updateProxyBroadcastState(any())).thenReturn(true);
			else when(serverTable.updateProxyBroadcastState(any(), eq(BACKEND))).thenReturn(true);
			invalid.setRewardDelivered(true);
			assertTrue(online ? recovering.updateOnlineVote(PLAYER, invalid) : recovering.updateServerVote(BACKEND, invalid));
			// The next checked SQL snapshot reflects the successful SQL delivery-state update.
			prepared.sqlVotes().get(0).setRewardDelivered(true);
			JsonObject expected = malformed.deepCopy();
			expected.addProperty("RewardDelivered", true);
			JsonObject updated = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertEquals(expected, sourceRow(updated, online, 41), "delivery success did not preserve updated flags and original evidence");
			assertFalse(recovering.prepareCachedVoteBatch(owner(online), online, List.of(invalid), 1));

			clean.setRewardDelivered(true);
			assertTrue(online ? recovering.updateOnlineVote(PLAYER, clean) : recovering.updateServerVote(BACKEND, clean));
			JsonObject healthyUpdated = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertTrue(sourceRow(healthyUpdated, online, 42).get("RewardDelivered").getAsBoolean());
			remove(recovering, online, clean);
			JsonObject afterRemoval = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertEquals(expected, sourceRow(afterRemoval, online, 41));
			assertEquals(1, journalRows(afterRemoval, online).size());

			// A different occurrence written at the occupied key cannot claim its delivery state or evidence.
			String occupiedKey = journalRows(afterRemoval, online).keySet().iterator().next();
			OfflineBungeeVote differentIdentity = vote(UUID.randomUUID());
			differentIdentity.setRewardDelivered(false);
			if (online) storage.addVoteOnline(PLAYER, Integer.parseInt(occupiedKey), differentIdentity);
			else storage.addVote(BACKEND, Integer.parseInt(occupiedKey), differentIdentity);
			storage.saveDurably();
			JsonObject guarded = JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject();
			assertEquals(expected, journalRows(guarded, online).getAsJsonObject(occupiedKey));
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(prepared.sqlVotes().get(0)), List.of(41));
			restarted.load();
			assertEquals(1, votes(restarted, online).size());
			OfflineBungeeVote restored = sqlRow(restarted, online, 41);
			assertEquals(invalid.getVoteId(), restored.getVoteId());
			assertTrue(restored.isRewardDelivered());
			assertFalse(restored.isCachedVoteBatchValid());
			assertFalse(restored.isCachedBatchDurable());
			assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			assertEquals(expected, sourceRow(JsonParser.parseString(Files.readString(storage.getStoragePath())).getAsJsonObject(), online, 41));
			verifyOnlyCheckedSqlReads(restarted);
		});
	}

	@TestFactory
	Stream<DynamicTest> unusedNullFieldsAndIgnoredVoteIdAliasStillAllowLegacyBatchStaging() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, false);
			JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
			JsonObject rows = journalRows(journal, online);
			String otherKey = rows.entrySet().stream()
					.filter(entry -> entry.getValue().getAsJsonObject().get("CachedBatchSourceRowId").getAsInt() == 42)
					.map(java.util.Map.Entry::getKey).findFirst().orElseThrow();
			rows.remove(otherKey);
			JsonObject legacy = sourceRow(journal, online, 41);
			legacy.addProperty("CachedBatchNumber", 0);
			legacy.addProperty("CachedBatchSize", 0);
			legacy.addProperty("CachedBatchSourceRowId", 0);
			legacy.add("UnknownOptionalField", com.google.gson.JsonNull.INSTANCE);
			legacy.addProperty("WasOnlineKnown", false);
			legacy.add("WasOnline", com.google.gson.JsonNull.INSTANCE);
			legacy.addProperty("DelayValidationKnown", false);
			legacy.add("DelayValidated", com.google.gson.JsonNull.INSTANCE);
			// A valid canonical VoteId takes precedence over this unused old alias.
			legacy.add("VoteID", com.google.gson.JsonNull.INSTANCE);
			Files.writeString(prepared.storage().getStoragePath(), journal.toString());
			VoteCacheHandler restarted = sqlHandler(cache(velocity), online, List.of(), List.of());
			restarted.load();
			assertEquals(1, votes(restarted, online).size());
			OfflineBungeeVote restored = votes(restarted, online).get(0);
			assertEquals(prepared.identities().get(0), restored.getVoteId());
			assertTrue(restored.isCachedVoteBatchValid());
			assertFalse(restored.hasCachedVoteBatch());
			assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1),
					"unused null values blocked an otherwise valid legacy occurrence");
			assertEquals(prepared.identities().get(0), restored.getVoteId());
			assertTrue(restored.isCachedBatchDurable());
			assertEquals(1, restored.getCachedBatchNumber());
			assertEquals(1, restored.getCachedBatchSize());
			verifyOnlyCheckedSqlReads(restarted);
			VoteCacheHandler next = sqlHandler(cache(velocity), online, List.of(), List.of());
			next.load();
			assertEquals(1, votes(next, online).size());
			assertEquals(restored.getVoteId(), votes(next, online).get(0).getVoteId());
			assertTrue(votes(next, online).get(0).isCachedBatchDurable());
			verifyOnlyCheckedSqlReads(next);
		});
	}

	@TestFactory
	Stream<DynamicTest> explicitNullBatchMetadataCannotBecomeLegacyWorkAfterFullReload() {
		return lanes((velocity, online) -> {
			List<List<String>> nullFields = List.of(List.of("CachedBatchNumber"), List.of("CachedBatchSize"),
					List.of("CachedBatchSourceRowId"),
					List.of("CachedBatchNumber", "CachedBatchSize", "CachedBatchSourceRowId"));
			for (List<String> fields : nullFields) {
				PreparedJournal prepared = preparedJournal(velocity, online, false);
				JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
				JsonObject malformed = sourceRow(journal, online, 41);
				fields.forEach(field -> malformed.add(field, com.google.gson.JsonNull.INSTANCE));
				JsonObject retained = malformed.deepCopy();
				String malformedBytes = journal.toString();
				Files.writeString(prepared.storage().getStoragePath(), malformedBytes);
				VoteCacheHandler restarted = sqlHandler(cache(velocity), online,
						List.of(prepared.sqlVotes().get(0), prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
				for (int reload = 0; reload < 2; reload++) {
					restarted.load();
					var journalOwners = votes(restarted, online).stream()
							.filter(vote -> prepared.identities().get(0).equals(vote.getVoteId())).toList();
					assertTrue(journalOwners.size() <= 1, "null metadata duplicated the original journal identity: " + fields);
					for (OfflineBungeeVote journalOwner : journalOwners) {
						assertFalse(journalOwner.isCachedVoteBatchValid(), "null metadata became valid legacy work: " + fields);
						assertFalse(journalOwner.isCachedBatchDurable());
						assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(journalOwner), 1));
					}
					OfflineBungeeVote originalSqlOwner = sqlRow(restarted, online, 41);
					assertTrue(originalSqlOwner.getVoteId() == null
							|| prepared.identities().get(0).equals(originalSqlOwner.getVoteId()));
					assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(originalSqlOwner), 1),
							"null metadata allowed the legacy SQL owner to acquire a new identity: " + fields);
					OfflineBungeeVote clean = sqlRow(restarted, online, 42), fresh = sqlRow(restarted, online, 43);
					assertEquals(prepared.identities().get(1), clean.getVoteId());
					assertTrue(clean.isCachedBatchDurable());
					assertTrue(restarted.prepareCachedVoteBatch(owner(online), online, List.of(clean), 1));
					assertFalse(restarted.prepareCachedVoteBatch(owner(online), online, List.of(fresh), 1));
					assertNull(fresh.getVoteId());
					assertFalse(fresh.hasCachedVoteBatch());
					JsonObject raw = JsonParser.parseString(Files.readString(prepared.storage().getStoragePath())).getAsJsonObject();
					JsonObject rawOwner = journalRows(raw, online).entrySet().stream()
							.map(entry -> entry.getValue().getAsJsonObject())
							.filter(row -> row.get("VoteId").getAsString().equals(prepared.identities().get(0).toString()))
							.findFirst().orElseThrow();
					assertEquals(retained, rawOwner, "full reload normalized the unresolved null metadata: " + fields);
					assertEquals(malformedBytes, Files.readString(prepared.storage().getStoragePath()));
				}
				verifyOnlyCheckedSqlReads(restarted, 2);
			}
		});
	}

	@TestFactory
	Stream<DynamicTest> malformedPositionsBindExactStableSqlIdsButNeverReusedRowPayloads() {
		return lanes((velocity, online) -> {
			PreparedJournal prepared = preparedJournal(velocity, online, true);
			JsonObject journal = JsonParser.parseString(prepared.cleanBytes()).getAsJsonObject();
			sourceRow(journal, online, 41).addProperty("CachedBatchNumber", 1.5);
			String malformed = journal.toString();
			Files.writeString(prepared.storage().getStoragePath(), malformed);
			// A stable identity still binds when the SQL physical row moved.
			VoteCacheHandler stable = sqlHandler(cache(velocity), online, prepared.sqlVotes(), List.of(51, 42));
			stable.load();
			assertEquals(2, votes(stable, online).size());
			OfflineBungeeVote restored = sqlRow(stable, online, 51);
			assertEquals(prepared.identities().get(0), restored.getVoteId());
			assertFalse(restored.isCachedVoteBatchValid());
			assertFalse(restored.isCachedBatchDurable());
			assertFalse(stable.prepareCachedVoteBatch(owner(online), online, List.of(restored), 1));
			assertEquals(malformed, Files.readString(prepared.storage().getStoragePath()));
			verifyOnlyCheckedSqlReads(stable);

			// Reusing the old physical row for another occurrence must not claim the old identity.
			OfflineBungeeVote replacement = new OfflineBungeeVote((UUID) null, "test-player", PLAYER,
					"test-service", 100L, false, "other-totals");
			VoteCacheHandler reused = sqlHandler(cache(velocity), online,
					List.of(replacement, prepared.sqlVotes().get(1), vote(null)), List.of(41, 42, 43));
			reused.load();
			OfflineBungeeVote replacementOwner = sqlRow(reused, online, 41);
			assertNull(replacementOwner.getVoteId());
			assertFalse(replacementOwner.hasCachedVoteBatch());
			assertEquals(3, votes(reused, online).size(), "unmatched linked journal became a new reward owner");
			assertFalse(reused.prepareCachedVoteBatch(owner(online), online, List.of(replacementOwner), 1));
			assertFalse(reused.prepareCachedVoteBatch(owner(online), online, List.of(sqlRow(reused, online, 43)), 1));
			assertTrue(reused.prepareCachedVoteBatch(owner(online), online, List.of(sqlRow(reused, online, 42)), 1));
			assertEquals(malformed, Files.readString(prepared.storage().getStoragePath()));
			verifyOnlyCheckedSqlReads(reused);
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
