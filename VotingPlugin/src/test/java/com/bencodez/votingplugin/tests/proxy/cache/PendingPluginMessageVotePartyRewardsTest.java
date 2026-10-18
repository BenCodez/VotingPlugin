package com.bencodez.votingplugin.tests.proxy.cache;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.PendingPluginMessageVotePartyRewards;
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
import com.bencodez.votingplugin.proxy.cache.VoteCacheDurability;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.cache.VotePartyCacheDurability;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.cache.PendingPluginMessageVotePartyRewards.Delivery;
import com.bencodez.votingplugin.proxy.cache.PendingPluginMessageVotePartyRewards.State;
import com.bencodez.votingplugin.proxy.velocity.VelocityJsonVoteCache;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

class PendingPluginMessageVotePartyRewardsTest {
	@TempDir Path directory;

	@TestFactory
	Stream<DynamicTest> bothAdaptersPreserveIdsStateOrderAndExistingPartyState() {
		return Stream.of("bungee", "velocity").map(platform -> DynamicTest.dynamicTest(platform, () -> {
			Path folder = directory.resolve(platform);
			IVoteCache cache = createCache(platform, folder);
			PendingPluginMessageVotePartyRewards original = snapshot();
			cache.setPluginMessageVotePartyRewards(original);
			cache.setVotePartyCurrentVotes(3);
			cache.setVotePartyInreaseVotesRequired(2);
			cache.setPendingVotePartyReward("http-backend", "http-delivery", true);
			VotePartyCacheDurability.saveAndVerify(cache.getStoragePath(), cache);

			IVoteCache restarted = createCache(platform, folder);
			assertEquals(original, restarted.getPluginMessageVotePartyRewards());
			assertEquals(3, restarted.getVotePartyCurrentVotes());
			assertEquals(2, restarted.getVotePartyInreaseVotesRequired());
			assertEquals(Set.of("http-backend"), Set.copyOf(restarted.getPendingVotePartyRewardServers()));
			assertEquals(Set.of("http-delivery"), Set.copyOf(restarted.getPendingVotePartyRewardIds("http-backend")));
			assertEquals(original.deliveries().stream().map(Delivery::deliveryId).toList(),
					restarted.getPluginMessageVotePartyRewards().deliveries().stream().map(Delivery::deliveryId).toList());
			PendingPluginMessageVotePartyRewards changed = original.withState(id(1), State.HANDOFF_FENCED).without(id(2));
			restarted.setPluginMessageVotePartyRewards(changed);
			VotePartyCacheDurability.saveAndVerify(restarted.getStoragePath(), restarted);
			restarted.reload();
			assertEquals(changed, restarted.getPluginMessageVotePartyRewards());
			restarted.setPluginMessageVotePartyRewards(PendingPluginMessageVotePartyRewards.empty());
			VotePartyCacheDurability.saveAndVerify(restarted.getStoragePath(), restarted);
			restarted.reload();
			assertTrue(restarted.getPluginMessageVotePartyRewards().deliveries().isEmpty());
			assertFalse(VoteCacheDurability.readJournal(restarted.getStoragePath())
					.getAsJsonObject("VoteParty").has("PluginMessageRewards"));
		}));
	}

	@TestFactory
	Stream<DynamicTest> missingNamespaceIsEmptyAndDoesNotInventHttpOwnership() {
		return Stream.of("bungee", "velocity").map(platform -> DynamicTest.dynamicTest(platform, () -> {
			IVoteCache cache = createCache(platform, directory.resolve(platform));
			assertEquals(PendingPluginMessageVotePartyRewards.empty(), cache.getPluginMessageVotePartyRewards());
			cache.setPluginMessageVotePartyRewards(snapshot());
			VotePartyCacheDurability.saveAndVerify(cache.getStoragePath(), cache);
			cache.reload();
			assertTrue(cache.getPendingVotePartyRewardServers().isEmpty());
			assertEquals(snapshot(), cache.getPluginMessageVotePartyRewards());
		}));
	}

	@TestFactory
	Stream<DynamicTest> invalidReloadKeepsLastSnapshotAndFencesWrites() {
		return Stream.of("bungee", "velocity").map(platform -> DynamicTest.dynamicTest(platform, () -> {
			IVoteCache cache = createCache(platform, directory.resolve(platform));
			cache.setPluginMessageVotePartyRewards(snapshot());
			VotePartyCacheDurability.saveAndVerify(cache.getStoragePath(), cache);
			String valid = Files.readString(cache.getStoragePath());
			String invalid = valid.replace("READY", "UNKNOWN");
			Files.writeString(cache.getStoragePath(), invalid);
			assertThrows(RuntimeException.class, cache::reload);
			assertEquals(snapshot(), cache.getPluginMessageVotePartyRewards());
			assertThrows(IOException.class, cache::saveDurably);
			assertEquals(invalid, Files.readString(cache.getStoragePath()));
			Files.writeString(cache.getStoragePath(), valid);
			cache.reload();
			assertDoesNotThrow(() -> VotePartyCacheDurability.saveAndVerify(cache.getStoragePath(), cache));
		}));
	}

	@TestFactory
	Stream<DynamicTest> bothAdaptersRejectMalformedPersistedDeliveryShapes() {
		List<JsonObject> malformed = new ArrayList<>();
		for (String field : List.of("batchId", "deliveryId", "server", "state")) {
			JsonObject missing = entry(); missing.remove(field); malformed.add(missing);
			JsonObject numeric = entry(); numeric.addProperty(field, 1); malformed.add(numeric);
		}
		JsonObject unknown = entry(); unknown.addProperty("extra", "value"); malformed.add(unknown);
		JsonObject shortId = entry(); shortId.addProperty("deliveryId", "1-1-1-1-1"); malformed.add(shortId);
		JsonObject phase = entry(); phase.addProperty("state", "ready"); malformed.add(phase);
		JsonObject server = entry(); server.addProperty("server", " "); malformed.add(server);
		JsonObject longServer = entry(); longServer.addProperty("server", "s".repeat(257)); malformed.add(longServer);
		return Stream.of("bungee", "velocity").flatMap(platform ->
				java.util.stream.IntStream.range(0, malformed.size()).mapToObj(index ->
						DynamicTest.dynamicTest(platform + "-shape-" + index, () -> {
							Path folder = directory.resolve(platform + "-shape-" + index);
							Files.createDirectories(folder);
							JsonArray rewards = new JsonArray(); rewards.add(malformed.get(index));
							Files.writeString(folder.resolve("votecache.json"), document(rewards).toString());
							assertThrows(RuntimeException.class, () -> createCache(platform, folder));
						})));
	}

	@Test
	void strictDecoderRejectsNullNonarraysDuplicateIdsAndUnknownPhases() {
		assertEquals(PendingPluginMessageVotePartyRewards.empty(), PendingPluginMessageVotePartyRewards.fromJson(null));
		assertThrows(IllegalArgumentException.class,
				() -> PendingPluginMessageVotePartyRewards.fromJson(com.google.gson.JsonNull.INSTANCE));
		assertThrows(IllegalArgumentException.class,
				() -> PendingPluginMessageVotePartyRewards.fromJson(new JsonObject()));
		JsonArray duplicate = new JsonArray(); duplicate.add(entry()); duplicate.add(entry());
		assertThrows(IllegalArgumentException.class, () -> PendingPluginMessageVotePartyRewards.fromJson(duplicate));
		assertThrows(IllegalArgumentException.class,
				() -> new Delivery(null, id(1), "synthetic", State.READY));
		assertThrows(IllegalArgumentException.class,
				() -> new Delivery(batch(), id(1), "synthetic", null));
		assertThrows(IllegalArgumentException.class,
				() -> new PendingPluginMessageVotePartyRewards(null));
	}

	@Test
	void identitiesAndOrderRemainImmutableThroughChanges() {
		List<Delivery> source = new ArrayList<>(snapshot().deliveries());
		PendingPluginMessageVotePartyRewards rewards = new PendingPluginMessageVotePartyRewards(source);
		source.clear();
		assertEquals(3, rewards.deliveries().size());
		assertThrows(UnsupportedOperationException.class, () -> rewards.deliveries().clear());
		assertEquals(" Synthetic.Backend_1 ", rewards.deliveries().get(0).server());
		assertEquals(List.of(id(1), id(2), id(3)), rewards.withState(id(1), State.HANDOFF_FENCED)
				.deliveries().stream().map(Delivery::deliveryId).toList());
		assertThrows(IllegalArgumentException.class,
				() -> rewards.with(new Delivery(batch(), id(1), "different-server", State.READY)));
		assertThrows(IllegalArgumentException.class,
				() -> rewards.with(new Delivery(new UUID(3, 1), id(1), "synthetic.backend_1", State.READY)));
		assertThrows(IllegalArgumentException.class, () -> rewards.withState(id(999), State.READY));
		JsonArray encoded = rewards.toJson(); encoded.remove(0);
		assertEquals(3, rewards.deliveries().size());
		assertEquals(rewards, PendingPluginMessageVotePartyRewards.fromJson(rewards.toJson()));
	}

	@Test
	void capacityChecksIncludeCanonicalServerCountsAndUtf8Bytes() {
		assertEquals(64, new PendingPluginMessageVotePartyRewards(deliveries(64, 64, "synthetic"))
				.deliveries().size());
		assertThrows(IllegalArgumentException.class,
				() -> new PendingPluginMessageVotePartyRewards(deliveries(65, 65, "synthetic")));
		assertEquals(256, new PendingPluginMessageVotePartyRewards(deliveries(256, 1, "synthetic"))
				.deliveries().size());
		assertThrows(IllegalArgumentException.class,
				() -> new PendingPluginMessageVotePartyRewards(deliveries(257, 1, "synthetic")));
		assertEquals(1024, new PendingPluginMessageVotePartyRewards(deliveries(1024, 64, "synthetic"))
				.deliveries().size());
		assertThrows(IllegalArgumentException.class,
				() -> new PendingPluginMessageVotePartyRewards(deliveries(1025, 64, "synthetic")));
		assertDoesNotThrow(() -> new Delivery(batch(), id(1), "é".repeat(128), State.READY));
		assertThrows(IllegalArgumentException.class,
				() -> new Delivery(batch(), id(1), "é".repeat(129), State.READY));
		assertThrows(IllegalArgumentException.class,
				() -> new Delivery(batch(), id(1), "bad\u0000server", State.READY));
		assertThrows(IllegalArgumentException.class,
				() -> new Delivery(batch(), id(1), "bad\uD800server", State.READY));
		assertThrows(IllegalArgumentException.class,
				() -> new PendingPluginMessageVotePartyRewards(deliveries(1024, 64, "<".repeat(200))));
	}

	@Test
	void readbackDetectsMissingOrChangedPluginMessageOwnership() throws Exception {
		IVoteCache cache = mock(IVoteCache.class);
		when(cache.getPendingVotePartyRewardServers()).thenReturn(Set.of());
		when(cache.getPendingVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		when(cache.getQuarantinedVotePartyProxyEffects()).thenReturn(PendingVotePartyProxyEffects.empty());
		when(cache.getPluginMessageVotePartyRewards()).thenReturn(snapshot());
		Path file = directory.resolve("readback.json");
		Files.writeString(file, "{}");
		assertThrows(IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
		Files.writeString(file, document(snapshot().withState(id(1), State.HANDOFF_FENCED).toJson()).toString());
		assertThrows(IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
		Files.writeString(file, document(snapshot().toJson()).toString());
		assertDoesNotThrow(() -> VotePartyCacheDurability.saveAndVerify(file, cache));
		String duplicateField = document(snapshot().toJson()).toString()
				.replace("\"state\":\"READY\"", "\"state\":\"READY\",\"state\":\"READY\"");
		Files.writeString(file, duplicateField);
		assertThrows(IOException.class, () -> VotePartyCacheDurability.saveAndVerify(file, cache));
	}

	@Test
	void legacyStorageDefaultsCannotAcceptNonemptyOwnership() {
		IVoteCache legacy = mock(IVoteCache.class, CALLS_REAL_METHODS);
		assertEquals(PendingPluginMessageVotePartyRewards.empty(), legacy.getPluginMessageVotePartyRewards());
		assertDoesNotThrow(() -> legacy.setPluginMessageVotePartyRewards(PendingPluginMessageVotePartyRewards.empty()));
		assertThrows(UnsupportedOperationException.class, () -> legacy.setPluginMessageVotePartyRewards(snapshot()));
	}

	@Test
	void handlerTransactionsHoldBothMonitorsAndRejectUnavailableStorage() throws Exception {
		IVoteCache storage = createCache("velocity", directory.resolve("handler"));
		VoteCacheHandler handler = handler(storage);
		handler.runVotePartyCacheTransaction(() -> {
			assertTrue(Thread.holdsLock(handler));
			assertTrue(Thread.holdsLock(storage));
			handler.setPluginMessageVotePartyRewards(snapshot());
			assertEquals(snapshot(), handler.getPluginMessageVotePartyRewards());
		});
		assertThrows(IllegalStateException.class, () -> handler(null).getPluginMessageVotePartyRewards());
		assertThrows(IllegalStateException.class,
				() -> handler(null).runVotePartyCacheTransaction(() -> fail("unavailable callback ran")));
		java.lang.reflect.Field quarantine = VoteCacheHandler.class.getDeclaredField("jsonStorageQuarantined");
		quarantine.setAccessible(true); ReflectiveAccess.set(quarantine, handler, true);
		assertThrows(IllegalStateException.class, handler::getPluginMessageVotePartyRewards);
		assertThrows(IllegalStateException.class, () -> handler.setPluginMessageVotePartyRewards(snapshot()));
		assertThrows(IllegalStateException.class,
				() -> handler.runVotePartyCacheTransaction(() -> fail("quarantined callback ran")));
	}

	private static VoteCacheHandler handler(IVoteCache storage) {
		return new VoteCacheHandler(null, false, false, null, false, storage) {
			@Override public void logInfo1(String message) { }
			@Override public void logSevere1(String message) { }
			@Override public void debug1(Exception failure) { }
			@Override public void debug1(Throwable failure) { }
			@Override public void debug1(String message) { }
		};
	}

	private static IVoteCache createCache(String platform, Path folder) throws IOException {
		Files.createDirectories(folder);
		Path file = folder.resolve("votecache.json");
		if (!Files.exists(file)) Files.writeString(file, "{}");
		if (platform.equals("velocity")) return new VelocityJsonVoteCache(file.toFile());
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.getDataFolder()).thenReturn(folder.toFile());
		return new BungeeJsonVoteCache(plugin);
	}

	private static PendingPluginMessageVotePartyRewards snapshot() {
		return new PendingPluginMessageVotePartyRewards(List.of(
				new Delivery(batch(), id(1), " Synthetic.Backend_1 ", State.READY),
				new Delivery(batch(), id(2), "backend-2", State.HANDOFF_FENCED),
				new Delivery(new UUID(1, 2), id(3), "synthetic.backend_1", State.READY)));
	}

	private static List<Delivery> deliveries(int count, int perServer, String name) {
		List<Delivery> result = new ArrayList<>();
		for (int index = 0; index < count; index++)
			result.add(new Delivery(batch(), id(index + 1), name + (index / perServer), State.READY));
		return result;
	}

	private static UUID batch() { return new UUID(1, 1); }
	private static UUID id(int value) { return new UUID(2, value); }
	private static JsonObject entry() { return snapshot().toJson().get(0).getAsJsonObject(); }
	private static JsonObject document(JsonArray rewards) {
		JsonObject party = new JsonObject(); party.add("PluginMessageRewards", rewards);
		JsonObject root = new JsonObject(); root.add("VoteParty", party); return root;
	}
}
