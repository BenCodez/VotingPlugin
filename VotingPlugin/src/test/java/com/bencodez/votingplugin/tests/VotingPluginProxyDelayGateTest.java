package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import com.bencodez.advancedcore.bungeeapi.time.BungeeTimeChecker;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class VotingPluginProxyDelayGateTest {
    private static final String PLAYER_ID = "00000000-0000-0000-0000-000000000001";
    private static final LocalDateTime NOW = LocalDateTime.of(2026, 7, 1, 12, 0);

    @TestFactory
    Stream<DynamicTest> withinWaitUntilVoteDelayUsesNewestMatchingOccurrenceAcrossPendingAndStoredVotes() {
        return Stream.of("online", "server", "rollover", "stored")
                .flatMap(source -> Stream.of(false, true).map(reverse -> DynamicTest.dynamicTest(
                        source + (reverse ? " newest first" : " oldest first"), () -> {
                            VotingPluginProxyTestImpl proxy = spy(new VotingPluginProxyTestImpl());
                            VoteCacheHandler cache = mock(VoteCacheHandler.class);
                            BungeeTimeChecker clock = mock(BungeeTimeChecker.class);
                            doReturn(cache).when(proxy).getVoteCacheHandler();
                            doReturn(clock).when(proxy).getBungeeTimeChecker();
                            when(clock.getTime()).thenReturn(NOW);
                            when(proxy.getConfig().getWaitUntilVoteDelaySites()).thenReturn(List.of("Site"));
                            when(proxy.getConfig().getWaitUntilVoteDelayService("Site")).thenReturn("Service");
                            when(proxy.getConfig().getWaitUntilVoteDelayVoteDelay("Site")).thenReturn(1);

                            long oldTime = millis(NOW.minusHours(2));
                            long recentTime = millis(NOW.minusMinutes(30));
                            List<OfflineBungeeVote> votes = new ArrayList<>(List.of(
                                    vote(PLAYER_ID, "Service", oldTime), vote(PLAYER_ID, "Service", recentTime)));
                            if (reverse) Collections.reverse(votes);
                            // A different service must not reserve this site's delay slot.
                            votes.add(vote(PLAYER_ID, "OtherService", millis(NOW.plusDays(1))));
                            when(cache.hasOnlineVotes(PLAYER_ID)).thenReturn(source.equals("online"));
                            when(cache.getOnlineVotes(PLAYER_ID)).thenReturn(new ArrayList<>(votes));
                            ArrayList<OfflineBungeeVote> serverVotes = source.equals("server")
                                    ? new ArrayList<>(votes) : new ArrayList<>();
                            serverVotes.add(vote("another-player", "Service", millis(NOW.plusDays(1))));
                            when(cache.getVotes("Server1")).thenReturn(serverVotes);
                            when(cache.getVotes("Server2")).thenReturn(new ArrayList<>());

                            ConcurrentLinkedQueue<VoteTimeQueue> rollover = new ConcurrentLinkedQueue<>();
                            if (source.equals("rollover")) {
                                for (OfflineBungeeVote vote : votes) {
                                    rollover.add(new VoteTimeQueue(vote.getVoteId(), "Player", vote.getService(), vote.getTime()));
                                }
                            }
                            rollover.add(new VoteTimeQueue(UUID.randomUUID(), "OtherPlayer", "Service", millis(NOW.plusDays(1))));
                            when(cache.getTimeChangeQueue()).thenReturn(rollover);
                            ArrayList<Column> stored = new ArrayList<>();
                            if (source.equals("stored")) {
                                String times = reverse ? recentTime + "%line%Site//" + oldTime
                                        : oldTime + "%line%Site//" + recentTime;
                                stored.add(new Column("LastVotes", new DataValueString(
                                        "Site//" + times + "%line%OtherSite//" + millis(NOW.plusDays(1)))));
                            }

                            assertFalse(proxy.checkVoteDelay(PLAYER_ID, "Player", "service", stored, true));
                            assertTrue(proxy.checkVoteDelay(PLAYER_ID, "Player", "UnconfiguredService", stored, true));
                            if (source.equals("rollover")) {
                                assertTrue(proxy.checkVoteDelay(PLAYER_ID, "Player", "Service", stored, false));
                            }
                            when(clock.getTime()).thenReturn(NOW.plusHours(2));
                            assertTrue(proxy.checkVoteDelay(PLAYER_ID, "Player", "Service", stored, true));
                        })));
    }

    private static OfflineBungeeVote vote(String playerId, String service, long time) {
        return new OfflineBungeeVote(UUID.randomUUID(), "Player", playerId, service, time, true, "");
    }

    private static long millis(LocalDateTime time) {
        return time.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
    }
}
