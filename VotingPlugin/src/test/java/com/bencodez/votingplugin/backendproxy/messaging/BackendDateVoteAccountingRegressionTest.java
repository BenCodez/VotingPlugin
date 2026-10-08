package com.bencodez.votingplugin.backendproxy.messaging;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.Bukkit;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.Event;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.user.validation.UserValidationResult;
import com.bencodez.advancedcore.api.user.validation.ValidationSource;
import com.bencodez.advancedcore.api.user.validation.ValidationStatus;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteLedger;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.votesites.VoteSite;

/** Exercises the router, native event adapter, accepted listener and real date ledger together. */
class BackendDateVoteAccountingRegressionTest {
    private static final UUID PLAYER = UUID.fromString("e5baec32-9b2c-4fc8-9aed-0e0285e3c33d");
    private static final long OCCURRED_AT = Instant.parse("2026-10-15T00:00:00Z").toEpochMilli();
    private static final String SERVICE = "known.example";
    private static final int POINTS = 7;

    @TempDir Path root;
    private final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    private final VotingPluginUser user = mock(VotingPluginUser.class);
    private final VoteSite site = mock(VoteSite.class);
    private final YamlConfiguration config = new YamlConfiguration();
    private final List<PlayerVoteEvent> accepted = new ArrayList<>();
    private final List<PlayerPostVoteEvent> posted = new ArrayList<>();
    private final AtomicInteger storedPoints = new AtomicInteger(37);
    private final ProcessedVoteCache cache = spy(new ProcessedVoteCache());
    private BackendProxyMessageRouter router;
    private MockedStatic<Bukkit> bukkit;
    private MockedStatic<com.bencodez.advancedcore.AdvancedCorePlugin> advancedCore;

    @BeforeEach
    void setUp() throws Exception {
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getStorageType()).thenReturn(null); // Exercise ordinary synchronous point storage.
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true);
        when(plugin.getBungeeSettings().getServer()).thenReturn("owner");
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getOptions().getServer()).thenReturn("owner");
        advancedCore=mockStatic(com.bencodez.advancedcore.AdvancedCorePlugin.class);
        advancedCore.when(com.bencodez.advancedcore.AdvancedCorePlugin::getInstance).thenReturn(plugin);
        when(plugin.getConfigFile().getPointsOnVote()).thenReturn(POINTS);
        when(plugin.getConfigFile().isAddTotals()).thenReturn(true);
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getUserManager().getProperName("Player")).thenReturn("Player");
        when(plugin.getUserManager().getValidationService().validate("Player", false)).thenReturn(
                new UserValidationResult(ValidationStatus.VALID, "Player", ValidationSource.STORAGE, "fixture", false));
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER, "Player")).thenReturn(user);
        when(plugin.getVoteSiteManager().getVoteSite(SERVICE, true)).thenReturn(site);
        when(site.isEnabled()).thenReturn(true);
        when(site.getKey()).thenReturn("Example");
        when(site.getServiceSite()).thenReturn(SERVICE);
        when(user.getPlayerName()).thenReturn("Player");
        when(user.getUUID()).thenReturn(PLAYER.toString());
        when(user.getJavaUUID()).thenReturn(PLAYER);
        when(user.isOnline()).thenReturn(true);
        when(user.getPoints()).thenAnswer(ignored -> storedPoints.get());
        doAnswer(call -> { storedPoints.set(call.getArgument(0)); return null; })
                .when(user).setPoints(anyInt(), eq(false));
        var field = VotingPluginUser.class.getDeclaredField("plugin");
        field.setAccessible(true);
        field.set(user, plugin);
        doCallRealMethod().when(user).addPoints(anyInt());
        doCallRealMethod().when(user).addPoints(anyInt(), anyBoolean());
        doCallRealMethod().when(user).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
                anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), any(), anyBoolean(), anyBoolean());

        var dispatcher = plugin.getServer().getPluginManager();
        var listener = new PlayerVoteListener(plugin);
        doAnswer(call -> {
            Event event = call.getArgument(0);
            if (event instanceof PlayerVoteEvent vote) {
                accepted.add(vote);
                listener.onplayerVote(vote);
            } else if (event instanceof PlayerPostVoteEvent post) {
                posted.add(post);
            }
            return null;
        }).when(dispatcher).callEvent(any());
        bukkit = mockStatic(Bukkit.class);
        bukkit.when(Bukkit::getPluginManager).thenReturn(dispatcher);
        router = new BackendProxyMessageRouter(plugin, mock(BackendPresenceManager.class),
                mock(BackendGlobalDataSync.class), mock(BackendVotePartySync.class), cache);
        definition("october");
        reloadDefinitions();
    }

    @AfterEach
    void closeBukkit() {
        if (bukkit != null) bukkit.close();
        if (advancedCore != null) advancedCore.close();
    }

    private void definition(String id) {
        String prefix = "DateVoteMilestones." + id + ".";
        config.set(prefix + "Enabled", true);
        config.set(prefix + "Start", "2026-10-01T00:00:00");
        config.set(prefix + "End", "2026-11-01T00:00:00");
        config.set(prefix + "Timezone", "UTC");
        config.set(prefix + "AccountingServer", "owner");
        config.set(prefix + "Milestones.1.Rewards.Messages.Player", "Thanks");
    }

    private void reloadDefinitions() {
        var milestones = new DateVoteMilestones(plugin);
        milestones.reload();
        when(plugin.getDateVoteMilestones()).thenReturn(milestones);
    }

    private DateVoteLedger.Progress progress(String id) throws Exception {
        var event = DateVoteMilestones.parse(id, config.getConfigurationSection("DateVoteMilestones." + id));
        return new DateVoteLedger(root.resolve("date-vote-milestones")).progress(event, PLAYER);
    }

    private JsonEnvelope vote(UUID id, boolean queued) {
        var original = VotingPluginWire.vote("Player", PLAYER.toString(), SERVICE, OCCURRED_AT,
                true, true, "", id, false, false, 1, 1, true);
        var builder = JsonEnvelope.builder(original.getSubChannel()).schema(original.getSchema());
        original.getFields().forEach(builder::put);
        return VotingPluginWire.requestVoteDeliveryAcknowledgement(
                builder.put(VotingPluginWire.K_QUEUED_DELIVERY, queued).build());
    }

    @Test
    void liveAccountingFailureFinishesNormalEffectsBeforeQuarantineAndDoesNotReplayThem() throws Exception {
        assertAccountingFailureFinishesNormalEffects(false);
    }

    @Test
    void queuedAccountingFailureFinishesNormalEffectsBeforeQuarantineAndDoesNotReplayThem() throws Exception {
        assertAccountingFailureFinishesNormalEffects(true);
    }

    private void assertAccountingFailureFinishesNormalEffects(boolean queued) throws Exception {
        definition("sibling");
        reloadDefinitions();
        var event = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        Path corrupt = root.resolve("date-vote-milestones").resolve(event.fileId() + "-" + PLAYER + ".properties");
        Files.createDirectories(corrupt.getParent());
        Files.writeString(corrupt, "corrupt fixture record");
        // A corrupt player record needs its existing definition seal, just as an installed ledger would have.
        var definitions = new java.util.Properties();
        definitions.setProperty("october", event.fingerprint());
        try (var writer = Files.newBufferedWriter(corrupt.getParent().resolve("definitions.properties"))) {
            definitions.store(writer, "fixture");
        }
        UUID id = UUID.randomUUID();
        JsonEnvelope envelope = vote(id, queued);
        var outcomes = new ArrayList<OrderedVoteOutcome>();
        assertThrows(VotingPluginUser.DateMilestoneAccountingException.class,
                () -> router.handleOrderedVote(envelope, outcome -> {
                    // The lane may quarantine only after the remaining normal mutation and observation.
                    assertEquals(44, storedPoints.get());
                    verify(plugin.getServerData()).addServiceSite(SERVICE);
                    outcomes.add(outcome);
                }));
        assertEquals(List.of(OrderedVoteOutcome.QUARANTINE), outcomes);
        assertTrue(accepted.get(0).isDateMilestoneAccountingFailed());
        assertEquals(id, accepted.get(0).getProxyVoteId());
        assertEquals(id, posted.get(0).getProxyVoteId());
        assertEquals(OCCURRED_AT, posted.get(0).getVoteTime());
        assertEquals("corrupt fixture record", Files.readString(corrupt));
        assertEquals(1, progress("sibling").votes());
        assertEquals(java.util.Set.of(1), progress("sibling").submittedAwards());

        router.handleOrderedVote(envelope, outcomes::add);
        assertEquals(List.of(OrderedVoteOutcome.QUARANTINE, OrderedVoteOutcome.QUARANTINE), outcomes);
        assertEquals(44, storedPoints.get());
        assertEquals(1, accepted.size());
        assertEquals(1, posted.size());
        assertFalse(cache.hasCompletedEffects(id));
        verify(cache, never()).complete(id);
        assertNormalEffectsOnce(id, OCCURRED_AT);
        verify(user).addPoints(POINTS);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), anyString(), any());
    }

    private void assertNormalEffectsOnce(UUID id, long time) {
        verify(user).playerVote(site, true, true);
        verify(user).addTotal();
        verify(user).addTotalDaily();
        verify(user).addTotalWeekly();
        verify(user).addPoints();
        verify(user).setTime(site, time);
        verify(plugin.getVoteParty()).vote(user, true, true, true);
        verify(plugin.getVoteMilestonesManager()).handleVote(eq(user), any(), eq(true), eq(id), any());
        verify(plugin.getCoolDownCheck()).vote(user, site);
        verify(plugin.getVoteStreakHandler()).processVote(user, time, id);
        verify(plugin).setUpdate(true);
    }

    @Test
    void missingWireIdKeepsLegacyTotalsCorrelationAndDedupWithoutDateAccounting() throws Exception {
        UUID correlation = UUID.randomUUID();
        // Actual legacy storage layout, including the old ignored milestone-count column.
        String totals = "1//2//3//4//5//0//6//7//8//" + correlation;
        JsonEnvelope envelope = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).schema(VotingPluginWire.SCHEMA_VERSION)
                .put(VotingPluginWire.K_PLAYER, "Player").put(VotingPluginWire.K_UUID, PLAYER)
                .put(VotingPluginWire.K_SERVICE, SERVICE).put(VotingPluginWire.K_TIME, OCCURRED_AT)
                .put(VotingPluginWire.K_WAS_ONLINE, true).put(VotingPluginWire.K_REAL_VOTE, true)
                .put(VotingPluginWire.K_TOTALS, totals).build();
        assertFalse(envelope.getFields().containsKey(VotingPluginWire.K_VOTE_ID));
        assertLegacyCorrelation(envelope, correlation);
    }

    @Test
    void emptyWireIdKeepsVersionedTotalsCorrelationWithoutDateAccounting() throws Exception {
        UUID correlation = UUID.randomUUID();
        JsonEnvelope envelope = VotingPluginWire.vote("Player", PLAYER.toString(), SERVICE, OCCURRED_AT,
                true, true, "v2//1//2//3//4//5//6//7//8//" + correlation, null, false, false, 1, 1);
        assertEquals("", envelope.getFields().get(VotingPluginWire.K_VOTE_ID));
        assertLegacyCorrelation(envelope, correlation);
    }

    private void assertLegacyCorrelation(JsonEnvelope envelope, UUID correlation) throws Exception {
        var outcome = new AtomicReference<OrderedVoteOutcome>();
        router.handleOrderedVote(envelope, outcome::set);
        assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
        router.handleOrderedVote(envelope, outcome::set);
        assertEquals(1, accepted.size());
        assertEquals(1, posted.size());
        assertNull(accepted.get(0).getProxyVoteId());
        assertNull(posted.get(0).getProxyVoteId());
        assertEquals(correlation, posted.get(0).getVoteUUID());
        assertEquals(OCCURRED_AT, posted.get(0).getVoteTime());
        assertEquals(0, progress("october").votes());
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        assertEquals(44, storedPoints.get());
        assertNormalEffectsOnce(correlation, OCCURRED_AT);
        verify(cache, times(2)).reserveWithOutcome(correlation);
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
    }

    @Test
    void targetedWireIdentityRemainsExplicitButDoesNotEnterTheOwnerLedger() throws Exception {
        UUID id = UUID.randomUUID();
        router.handleOrderedVote(VotingPluginWire.voteOnline("Player", PLAYER.toString(), SERVICE, OCCURRED_AT,
                true, true, "", id, false, false, 1, 1), ignored -> { });
        assertTrue(accepted.get(0).isTargetedProxyVote());
        assertEquals(id, posted.get(0).getProxyVoteId());
        assertEquals(OCCURRED_AT, posted.get(0).getVoteTime());
        assertEquals(0, progress("october").votes());
        assertNormalEffectsOnce(id, OCCURRED_AT);
    }

    @Test
    void canonicalTestFlagRemainsExcludedAcrossRouterAndAcceptedListener() throws Exception {
        UUID id = UUID.randomUUID();
        router.handleOrderedVote(VotingPluginWire.vote("Player", PLAYER.toString(), SERVICE, OCCURRED_AT,
                true, false, "", id, false, false, 1, 1), ignored -> { });
        assertFalse(accepted.get(0).isRealVote());
        assertFalse(posted.get(0).isRealVote());
        assertEquals(id, posted.get(0).getProxyVoteId());
        assertEquals(OCCURRED_AT, posted.get(0).getVoteTime());
        assertEquals(0, progress("october").votes());
        verify(user).playerVote(site, true, true);
        verify(user, never()).addTotal();
        verify(user).addPoints(POINTS); // Preserve the existing router's per-server points policy.
    }

    @Test
    void missingOriginalTimeIsNotMadeCanonicalByNormalPostVoteTimestampFallback() throws Exception {
        UUID id = UUID.randomUUID();
        long before = System.currentTimeMillis();
        router.handleOrderedVote(VotingPluginWire.vote("Player", PLAYER.toString(), SERVICE, 0,
                true, true, "", id, false, false, 1, 1), ignored -> { });
        assertEquals(0, accepted.get(0).getTime());
        assertEquals(id, posted.get(0).getProxyVoteId());
        assertTrue(posted.get(0).getVoteTime() >= before);
        assertTrue(posted.get(0).getVoteTime() <= System.currentTimeMillis());
        assertEquals(0, progress("october").votes());
        verify(user).setTime(site);
    }

    @Test
    void explicitWireIdOverridesTotalsCorrelationThroughDateAccountingAndPostVote() throws Exception {
        UUID wireId = UUID.randomUUID(), correlation = UUID.randomUUID();
        JsonEnvelope envelope = VotingPluginWire.requestVoteDeliveryAcknowledgement(VotingPluginWire.vote(
                "Player", PLAYER.toString(), SERVICE, OCCURRED_AT, true, true,
                "v2//1//2//3//4//5//6//7//8//" + correlation, wireId, false, false, 1, 1));
        var outcome = new AtomicReference<OrderedVoteOutcome>();
        router.handleOrderedVote(envelope, outcome::set);
        assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
        router.handleOrderedVote(envelope, outcome::set);
        assertEquals(1, accepted.size());
        assertEquals(1, posted.size());
        assertEquals(wireId, accepted.get(0).getProxyVoteId());
        assertEquals(wireId, posted.get(0).getProxyVoteId());
        assertEquals(wireId, posted.get(0).getVoteUUID());
        assertEquals(OCCURRED_AT, posted.get(0).getVoteTime());
        assertEquals(1, progress("october").votes());
        assertNormalEffectsOnce(wireId, OCCURRED_AT);
        verify(cache, times(2)).reserveWithOutcome(wireId);
        verify(cache, never()).reserveWithOutcome(correlation);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), anyString(), any());
    }
}
