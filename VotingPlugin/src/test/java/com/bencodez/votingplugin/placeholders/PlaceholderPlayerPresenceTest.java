package com.bencodez.votingplugin.placeholders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

class PlaceholderPlayerPresenceTest {
	@Test
	void joinQuitReloadAndDisableSnapshotsStayConsistent() {
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		UUID firstId = UUID.randomUUID();
		UUID secondId = UUID.randomUUID();
		Player first = player(firstId);
		Player second = player(secondId);

		assertFalse(presence.isOnline(firstId));
		AtomicInteger offlinePublications = new AtomicInteger();
		assertTrue(presence.runIfOffline(firstId, offlinePublications::incrementAndGet));
		assertEquals(1, offlinePublications.get());
		presence.playerOnline(first);
		assertTrue(presence.isOnline(firstId));
		assertSame(first, presence.schedulerOwner(firstId));
		assertFalse(presence.runIfOffline(firstId, offlinePublications::incrementAndGet));
		assertEquals(1, offlinePublications.get());

		presence.playerOffline(firstId);
		assertFalse(presence.isOnline(firstId));
		UUID storageId = UUID.randomUUID();
		presence.playerOnline(storageId, first);
		assertTrue(presence.isOnline(storageId));
		assertSame(first, presence.schedulerOwner(storageId));
		assertEquals(storageId, presence.storageUuid(first));
		presence.playerOffline(storageId, second);
		assertTrue(presence.isOnline(storageId), "a replacement owner must not retire the captured player");
		Player refreshedFirst = player(firstId);
		presence.replace(List.of(refreshedFirst, second));
		assertTrue(presence.isOnline(storageId), "reload must retain the authoritative storage UUID");
		assertFalse(presence.isOnline(firstId));
		assertSame(refreshedFirst, presence.schedulerOwner(storageId));
		assertTrue(presence.isOnline(secondId));
		presence.playerOffline(storageId, refreshedFirst);
		assertFalse(presence.isOnline(storageId));
		presence.replace(List.of(first, second));
		assertTrue(presence.isOnline(firstId));
		assertTrue(presence.isOnline(secondId));

		presence.clear();
		assertFalse(presence.isOnline(firstId));
		assertFalse(presence.isOnline(secondId));
	}

	@Test
	void snapshotReplacementDoesNotLoseConcurrentLifecycleChanges() throws Exception {
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		UUID firstId = UUID.randomUUID();
		UUID joinedId = UUID.randomUUID();
		Player first = player(firstId);
		Player joined = player(joinedId);
		CountDownLatch resolving = new CountDownLatch(1);
		CountDownLatch releaseResolution = new CountDownLatch(1);
		CountDownLatch joinStarted = new CountDownLatch(1);
		ExecutorService workers = Executors.newFixedThreadPool(2);
		try {
			Future<?> refresh = workers.submit(() -> presence.replace(() -> {
				resolving.countDown();
				await(releaseResolution);
				return List.of(first);
			}, player -> firstId));
			assertTrue(resolving.await(2, TimeUnit.SECONDS));
			Future<?> join = workers.submit(() -> {
				joinStarted.countDown();
				presence.playerOnline(joinedId, joined);
			});
			assertTrue(joinStarted.await(2, TimeUnit.SECONDS));
			assertFalse(join.isDone(), "join must wait for the atomic snapshot replacement");
			releaseResolution.countDown();
			refresh.get(2, TimeUnit.SECONDS);
			join.get(2, TimeUnit.SECONDS);
			assertSame(first, presence.schedulerOwner(firstId));
			assertSame(joined, presence.schedulerOwner(joinedId));
		} finally {
			workers.shutdownNow();
			assertTrue(workers.awaitTermination(2, TimeUnit.SECONDS));
		}
	}

	private void await(CountDownLatch latch) {
		try {
			if (!latch.await(2, TimeUnit.SECONDS)) throw new AssertionError("timed out waiting for test release");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new AssertionError(interrupted);
		}
	}

	private Player player(UUID uuid) {
		Player player = mock(Player.class);
		when(player.getUniqueId()).thenReturn(uuid);
		return player;
	}
}
