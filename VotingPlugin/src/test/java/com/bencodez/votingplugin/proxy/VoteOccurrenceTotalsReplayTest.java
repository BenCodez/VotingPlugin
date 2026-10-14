package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Exercises durable row decoding, real proxy replay, and the complete backend wire envelope. */
class VoteOccurrenceTotalsReplayTest {
    private static final UUID PLAYER = new UUID(1, 731);
    private static final UUID OCCURRENCE = new UUID(2, 731);
    private static final long QUEUE_TIME = 1788201600123L;
    private static final long ORIGINAL_TIME = 1788201500456L;

    @TempDir Path directory;

    private record Payload(String name, String text, int current, int required, int allTime, boolean metadata) { }

    @TestFactory
    Stream<DynamicTest> retainedRows() {
        List<Payload> payloads = List.of(
                new Payload("legacy-empty", "", 7, 40, 0, false),
                new Payload("occurrence-only", "", 7, 40, 0, true),
                new Payload("explicit-zero", "v2//0//0//0//0//0//0//0//0", 0, 0, 0, true),
                new Payload("versioned-totals", "v2//11//12//13//14//15//16//17//18", 16, 17, 11, true),
                new Payload("legacy-totals", "11//12//13//14//15//999//16//17//18", 16, 17, 11, true));
        return Stream.of(false, true).flatMap(applied -> payloads.stream().map(payload -> {
            String name = payload.name() + "-already-applied-" + applied;
            return DynamicTest.dynamicTest(name, () -> durableRestartReplayPreservesFallbackOrSuppliedTotals(
                    payload.text(), applied, payload.current(), payload.required(), payload.allTime(),
                    payload.metadata(), Files.createDirectories(directory.resolve(name))));
        }));
    }

    private void durableRestartReplayPreservesFallbackOrSuppliedTotals(String payload, boolean applied,
            int expectedCurrent, int expectedRequired, int expectedAllTime, boolean metadata,
            Path directory) throws Exception {
        ReplayProxy producer = new ReplayProxy(directory);
        VoteTimeQueue row = row(payload, applied, metadata);
        assertTrue(producer.cache.addTimeVoteToCache(row));

        ReplayProxy restarted = new ReplayProxy(directory);
        restarted.cache.load();
        VoteTimeQueue restored = restarted.cache.getTimeChangeQueue().peek();
        assertNotNull(restored);
        assertEquals(OCCURRENCE, restored.getVoteId());
        assertEquals(PLAYER.toString(), restored.getUuid());
        assertEquals(QUEUE_TIME, restored.getTime());
        assertEquals(applied, restored.isTotalsApplied());
        assertEquals(metadata ? ORIGINAL_TIME : 0L, restored.getCanonicalOccurrenceTime());

        restarted.processQueue();
        assertTrue(restarted.cache.getTimeChangeQueue().isEmpty(), "successful replay must retire its source row");
        assertRewardAndBackend(restarted, expectedCurrent, expectedRequired, expectedAllTime,
                metadata ? ORIGINAL_TIME : -1L);
        restarted.processQueue();
        assertEquals(1, restarted.cache.getOnlineVotes(PLAYER.toString()).size(),
                "repeated queue processing must not create another reward occurrence");

        ReplayProxy secondRestart = new ReplayProxy(directory);
        secondRestart.cache.load();
        assertTrue(secondRestart.cache.getTimeChangeQueue().isEmpty());
        assertEquals(1, secondRestart.cache.getOnlineVotes(PLAYER.toString()).size());
        assertRewardAndBackend(secondRestart, expectedCurrent, expectedRequired, expectedAllTime,
                metadata ? ORIGINAL_TIME : -1L);
    }

    @Test
    void alreadyAppliedRetryWithoutAnExplicitSnapshotDoesNotParseMetadataAsZeros(@TempDir Path directory)
            throws Exception {
        ReplayProxy proxy = new ReplayProxy(directory);
        VoteTimeQueue row = row("", true, true);
        assertTrue(proxy.cache.addTimeVoteToCache(row));
        assertEquals(VotingPluginProxy.QueuedVoteResult.SUCCESS, proxy.replay(row, null));
        assertRewardAndBackend(proxy, 7, 40, 0, ORIGINAL_TIME);
    }

    @Test
    void alreadyAppliedRetryRestoresARealSnapshotWhenTheCallerHasNone(@TempDir Path directory) throws Exception {
        ReplayProxy proxy = new ReplayProxy(directory);
        VoteTimeQueue row = row("v2//11//12//13//14//15//16//17//18", true, true);
        assertTrue(proxy.cache.addTimeVoteToCache(row));
        assertEquals(VotingPluginProxy.QueuedVoteResult.SUCCESS, proxy.replay(row, null));
        assertRewardAndBackend(proxy, 16, 17, 11, ORIGINAL_TIME);
    }

    private static VoteTimeQueue row(String payload, boolean applied, boolean metadata) {
        VoteTimeQueue row = new VoteTimeQueue(OCCURRENCE, "Synthetic", "synthetic-service", QUEUE_TIME);
        row.setUuid(PLAYER.toString());
        row.setWasOnline(false);
        row.setTotals(payload);
        if (metadata) row.setCanonicalOccurrenceTime(ORIGINAL_TIME);
        row.setVotePartyApplied(true);
        row.setTotalsApplied(applied);
        return row;
    }

    private static void assertRewardAndBackend(ReplayProxy proxy, int current, int required, int allTime,
            long occurrenceTime) throws Exception {
        List<OfflineBungeeVote> rewards = proxy.cache.getOnlineVotes(PLAYER.toString());
        assertEquals(1, rewards.size());
        OfflineBungeeVote reward = rewards.get(0);
        assertEquals(OCCURRENCE, reward.getVoteId());
        assertEquals(PLAYER.toString(), reward.getUuid());
        assertEquals(QUEUE_TIME, reward.getTime());
        assertEquals(occurrenceTime, reward.getCanonicalOccurrenceTime());
        VoteTotalsSnapshot totals = VoteTotalsSnapshot.parseStorage(reward.getText());
        assertEquals(current, totals.getVotePartyCurrent());
        assertEquals(required, totals.getVotePartyRequired());
        assertEquals(allTime, totals.getAllTimeTotal());

        // Serialize the complete reward wrapper before entering the actual backend router.
        JsonEnvelope envelope = JsonEnvelopeCodec.decode(JsonEnvelopeCodec.encode(VotingPluginWire.vote(
                reward.getPlayerName(), reward.getUuid(), reward.getService(), reward.getTime(), false,
                reward.isRealVote(), reward.getText(), reward.getVoteId(), false, false, 1, 1)));
        VotingPluginWire.Vote wire = VotingPluginWire.readVote(envelope);
        assertEquals(OCCURRENCE, wire.voteId);
        assertEquals(QUEUE_TIME, wire.time);
        assertEquals(occurrenceTime, wire.canonicalOccurrenceTime);

        VotingPluginMain plugin = mock(VotingPluginMain.class);
        UserManager users = mock(UserManager.class);
        VotingPluginUser user = mock(VotingPluginUser.class);
        BackendVotePartySync party = mock(BackendVotePartySync.class);
        when(plugin.getVotingPluginUserManager()).thenReturn(users);
        when(users.getVotingPluginUser(PLAYER, "Synthetic")).thenReturn(user);
        when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
        when(plugin.getServerData()).thenReturn(mock(ServerData.class));
        BackendProxyMessageRouter router = new BackendProxyMessageRouter(plugin,
                mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class), party,
                new ProcessedVoteCache());
        AtomicReference<OrderedVoteOutcome> completion = new AtomicReference<>();
        router.handleOrderedVote(envelope, completion::set);
        assertEquals(OrderedVoteOutcome.COMPLETE, completion.get());
        verify(party).replace(current, required);
        verify(user).bungeeVotePluginMessaging(eq("synthetic-service"), eq(QUEUE_TIME),
                argThat(value -> value.getAllTimeTotal() == allTime && value.getVotePartyCurrent() == current
                        && value.getVotePartyRequired() == required), eq(true), eq(false), eq(false), eq(1),
                eq(false), eq(false), eq(true), eq(OCCURRENCE), eq(false), eq(true), eq(occurrenceTime));
    }

    private static final class ReplayProxy extends VotingPluginProxyTestImpl {
        final VoteCacheHandler cache;

        ReplayProxy(Path directory) throws Exception {
            setDataFolder(directory.toFile());
            setMethod(BungeeMethod.PLUGINMESSAGING);
            setVotePartyVotes(7);
            setCurrentVotePartyVotesRequired(40);
            setSchedulerForTest(mock(ScheduledExecutorService.class));
            setField(this, "enabled", true);
            when(getConfig().getOnlineMode()).thenReturn(true);
            when(getConfig().getMultiProxySupport()).thenReturn(true);
            when(getConfig().getPrimaryServer()).thenReturn(false);
            when(getConfig().getBlockedServers()).thenReturn(List.of());
            VotingPluginBungee platform = mock(VotingPluginBungee.class);
            when(platform.getDataFolder()).thenReturn(directory.toFile());
            BungeeJsonVoteCache storage = new BungeeJsonVoteCache(platform);
            cache = new VoteCacheHandler(null, false, false, null, false, storage) {
                @Override public void logInfo1(String message) { }
                @Override public void logSevere1(String message) { }
                @Override public void debug1(Exception failure) { }
                @Override public void debug1(Throwable failure) { }
                @Override public void debug1(String message) { }
            };
            setField(this, "voteCacheHandler", cache);
        }

        @Override public boolean isPlayerOnline(String name) { return false; }
        @Override public Set<String> getAllAvailableServers() { return Set.of(); }
        VotingPluginProxy.QueuedVoteResult replay(VoteTimeQueue row, VoteTotalsSnapshot totals) {
            return super.replayQueuedVote(row, totals, true);
        }
    }

    private static void setField(Object owner, String name, Object value) throws Exception {
        Field field = VotingPluginProxy.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(owner, value);
    }
}
