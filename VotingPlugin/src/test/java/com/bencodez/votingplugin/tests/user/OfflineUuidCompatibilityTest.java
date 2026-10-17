package com.bencodez.votingplugin.tests.user;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.user.UserManager;

class OfflineUuidCompatibilityTest {
    private static final String NAME = "Milovannila02";
    private static final UUID SERVER = UUID.fromString("0ed7aeff-acc9-337e-8bcb-9ed9388b16d2");
    private static final UUID STORAGE = UUID.fromString("5fb82a90-e348-3e0a-b7d8-d3f2f8a1cf1b");

    @Test void issue1721ResolvesToExistingPointsWithoutCreatingServerUuidData() {
        Fixture f = new Fixture();
        f.join();
        UserDataCache existing = new UserDataCache(null, STORAGE);
        existing.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(37))));
        var data = f.plugin.getUserManager().getDataManager();
        when(f.plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
        when(data.effectiveStorageType(any())).thenReturn(UserStorage.SQLITE);
        when(data.getCache(STORAGE)).thenReturn(existing);
        when(data.isCached(STORAGE)).thenReturn(true);
        try (var bukkit = mockStatic(Bukkit.class)) {
            var corrected = f.users.getVotingPluginUser(SERVER);
            assertEquals(STORAGE, corrected.getJavaUUID());
            assertEquals(NAME, corrected.getPlayerName());
            assertEquals(37, corrected.getPoints());
            assertEquals(37, f.users.getVotingPluginUser(STORAGE).getPoints());
            bukkit.verifyNoInteractions();
        }
        verify(data, never()).getCache(SERVER);
        verify(f.logger).warning(contains("third-party compatibility"));
        verify(f.logger).warning(contains(SERVER + " -> VotingPlugin UUID: " + STORAGE));
    }

    @Test void matchingAndCanonicalIdentitiesDoNotWarn() {
        Fixture f = new Fixture();
        f.join();
        assertEquals(STORAGE, f.users.getVotingPluginUser(STORAGE).getJavaUUID());
        f.presence.captureOnline(SERVER, f.player);
        assertEquals(SERVER, f.users.getVotingPluginUser(SERVER).getJavaUUID());
        verifyNoInteractions(f.logger);
    }

    @Test void offlineAndUnknownInputsRetainOriginalConstructorContract() {
        Fixture f = new Fixture();
        try (var constructors = mockConstruction(com.bencodez.votingplugin.user.VotingPluginUser.class,
                (user, context) -> assertEquals(List.of(f.plugin, SERVER), context.arguments()))) {
            f.users.getVotingPluginUser(SERVER);
            assertEquals(1, constructors.constructed().size());
        }
        verifyNoInteractions(f.logger);
    }

    @Test void onlineModeRetainsOriginalConstructorEvenWithCapturedMismatch() {
        Fixture f = new Fixture();
        f.join();
        when(f.plugin.getOptions().isOnlineMode()).thenReturn(true);
        try (var constructors = mockConstruction(com.bencodez.votingplugin.user.VotingPluginUser.class,
                (user, context) -> assertEquals(List.of(f.plugin, SERVER), context.arguments()))) {
            f.users.getVotingPluginUser(SERVER);
            assertEquals(1, constructors.constructed().size());
        }
        verifyNoInteractions(f.logger);
    }

    @Test void exactStorageOverloadsRemainExactRegardlessOfLoadName() {
        Fixture f = new Fixture();
        f.join();
        try (var constructors = mockConstruction(com.bencodez.votingplugin.user.VotingPluginUser.class,
                (user, context) -> assertEquals(SERVER, context.arguments().get(1)))) {
            f.users.getVotingPluginUser(SERVER, false);
            f.users.getVotingPluginUser(SERVER, true);
            assertEquals(2, constructors.constructed().size());
        }
        verifyNoInteractions(f.logger);
    }

    @Test void repeatedAndConcurrentAsyncCallsDoNotTouchBukkitOrSpamWarnings() throws Exception {
        Fixture f = new Fixture();
        f.join();
        clearInvocations(f.player);
        var workers = Executors.newFixedThreadPool(8);
        try {
            List<Callable<UUID>> calls = new ArrayList<>();
            for (int i = 0; i < 256; i++) calls.add(() -> {
                try (var bukkit = mockStatic(Bukkit.class)) {
                    UUID id = f.users.getVotingPluginUser(SERVER).getJavaUUID();
                    bukkit.verifyNoInteractions();
                    return id;
                }
            });
            for (var call : workers.invokeAll(calls, 10, TimeUnit.SECONDS)) assertEquals(STORAGE, call.get());
        } finally {
            workers.shutdownNow();
            assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
        }
        verifyNoInteractions(f.player);
        verify(f.logger, times(1)).warning(anyString());
        verifyNoMoreInteractions(f.logger);
    }

    @Test void quitReconnectAndReloadDoNotResetWarningOrResurrectOldOwner() {
        Fixture f = new Fixture();
        f.join();
        f.users.getVotingPluginUser(SERVER);
        assertTrue(f.presence.playerOffline(STORAGE, f.player));
        assertNull(f.presence.liveIdentity(SERVER));
        assertFalse(f.presence.updateStorageUuid(STORAGE, f.player));
        Player replacement = player(SERVER, NAME);
        f.presence.captureOnline(STORAGE, replacement);
        assertFalse(f.presence.playerOffline(STORAGE, f.player));
        assertFalse(f.presence.updateStorageUuid(SERVER, f.player));
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        f.presence.refresh(() -> List.of(replacement), captures::put);
        captures.get(replacement).accept(STORAGE);
        assertEquals(STORAGE, f.users.getVotingPluginUser(SERVER).getJavaUUID());
        verify(f.logger, times(1)).warning(anyString());
        f.presence.clear();
        captures.get(replacement).accept(STORAGE);
        assertNull(f.presence.liveIdentity(SERVER));
    }

    @Test void staleRefreshCannotOverwriteReplacementOrNewerRefresh() {
        Fixture f = new Fixture();
        Map<Player, Consumer<UUID>> old = new java.util.IdentityHashMap<>();
        f.presence.refresh(() -> List.of(f.player), old::put);
        Player replacement = player(SERVER, NAME);
        f.presence.captureOnline(STORAGE, replacement);
        old.get(f.player).accept(SERVER);
        assertSame(replacement, f.presence.schedulerOwner(STORAGE));
        assertEquals(STORAGE, f.presence.liveIdentity(SERVER).storageUuid());
        f.presence.refresh(List::of, (p, capture) -> fail("No capture expected"));
        old.get(f.player).accept(STORAGE);
        assertNull(f.presence.liveIdentity(SERVER));
    }

    @Test void reloadSchedulesPlayerCaptureWithoutReadingPlayersOnCallingThread() {
        Fixture f = new Fixture();
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        clearInvocations(f.player);
        f.presence.refresh(() -> List.of(f.player), captures::put);
        verifyNoInteractions(f.player);
        assertNull(f.presence.liveIdentity(SERVER));
        captures.get(f.player).accept(STORAGE);
        assertEquals(STORAGE, f.presence.liveIdentity(SERVER).storageUuid());
    }

    @Test void ambiguousServerUuidAndExistingExactStorageKeyAreNotGuessed() {
        Fixture f = new Fixture();
        f.join();
        Player second = player(SERVER, "OtherName");
        UUID otherStorage = UUID.randomUUID();
        f.presence.captureOnline(otherStorage, second);
        assertNull(f.presence.liveIdentity(SERVER));
        f.presence.playerOnline(SERVER, player(UUID.randomUUID(), "ExactStorageOwner"));
        assertNull(f.presence.liveIdentity(SERVER), "An exact stored key must not be redirected through another alias");
    }

    @Test void loginRekeysOnlyCapturedCurrentOwnerWithoutReadingBukkit() {
        Fixture f = new Fixture();
        f.presence.captureOnline(SERVER, f.player);
        clearInvocations(f.player);
        assertTrue(f.presence.updateStorageUuid(STORAGE, f.player));
        assertEquals(STORAGE, f.presence.liveIdentity(SERVER).storageUuid());
        verifyNoInteractions(f.player);
    }

    @Test void publicIssueUuidsUseDifferentCaseWithoutChangingAlgorithm() {
        assertEquals(SERVER, UUID.nameUUIDFromBytes(("OfflinePlayer:" + NAME).getBytes(StandardCharsets.UTF_8)));
        assertEquals(STORAGE, UUID.nameUUIDFromBytes(("OfflinePlayer:" + NAME.toLowerCase(java.util.Locale.ROOT))
                .getBytes(StandardCharsets.UTF_8)));
    }

    @Test void coldReloadUsesActualAdvancedCoreLowercaseDerivationWithoutStorage() throws Exception {
        Fixture f = new Fixture();
        var constructor = UuidLookup.class.getDeclaredConstructor(com.bencodez.advancedcore.AdvancedCorePlugin.class);
        constructor.setAccessible(true);
        UuidLookup lookup = constructor.newInstance(f.plugin);
        try (var staticLookup = mockStatic(UuidLookup.class); var bukkit = mockStatic(Bukkit.class)) {
            staticLookup.when(UuidLookup::getInstance).thenReturn(lookup);
            assertEquals("", lookup.getCachedUUID(NAME));
            assertEquals(STORAGE, com.bencodez.votingplugin.tests.VotingPluginMainAccess
                    .placeholderStorageUuid(f.player, false));
            bukkit.verifyNoInteractions();
        }
    }

    @Test void caseChangedReconnectRetiresTheOldServerAlias() {
        Fixture f = new Fixture();
        f.join();
        UUID reconnectedUuid = UUID.randomUUID();
        Player replacement = player(reconnectedUuid, NAME.toUpperCase(java.util.Locale.ROOT));
        f.presence.captureOnline(STORAGE, replacement);
        assertNull(f.presence.liveIdentity(SERVER));
        assertEquals(STORAGE, f.users.getVotingPluginUser(reconnectedUuid).getJavaUUID());
        assertFalse(f.presence.updateStorageUuid(SERVER, f.player));
    }

    @Test void pluginRefreshDefersIdentityReadsToTheEntityScheduler() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        var options = mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class);
        doReturn(options).when(plugin).getOptions();
        var scheduler = mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
        doReturn(scheduler).when(plugin).getBukkitScheduler();
        PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
        var field = VotingPluginMain.class.getDeclaredField("placeholderPlayerPresence");
        field.setAccessible(true);
        field.set(plugin, presence);
        Player player = player(SERVER, NAME);
        when(player.isOnline()).thenReturn(true);
        UuidLookup lookup = mock(UuidLookup.class);
        when(lookup.getUUIDWithoutStorage(NAME)).thenReturn(STORAGE.toString());
        List<Runnable> entityTasks = new ArrayList<>();
        doAnswer(call -> { entityTasks.add(call.getArgument(1)); return null; })
                .when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
        clearInvocations(player);
        try (var bukkit = mockStatic(Bukkit.class); var uuidLookup = mockStatic(UuidLookup.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of(player));
            uuidLookup.when(UuidLookup::getInstance).thenReturn(lookup);
            plugin.refreshPlaceholderPlayerPresence();
            verifyNoInteractions(player);
            assertNull(presence.liveIdentity(SERVER));
            assertEquals(1, entityTasks.size());
            entityTasks.getFirst().run();
            assertEquals(STORAGE, presence.liveIdentity(SERVER).storageUuid());
            UUID authoritativeLoginUuid = UUID.randomUUID();
            assertTrue(presence.updateStorageUuid(authoritativeLoginUuid, player));
            plugin.refreshPlaceholderPlayerPresence();
            entityTasks.get(1).run();
            assertEquals(authoritativeLoginUuid, presence.liveIdentity(SERVER).storageUuid(),
                    "Reload retains the authoritative login storage key");
            presence.clear();
            entityTasks.getFirst().run();
            assertNull(presence.liveIdentity(SERVER), "A callback after disable cannot republish a player");
        }
    }

    @Test void staleCaseChangedRefreshCannotReplaceTheCurrentStorageOwner() {
        Fixture f = new Fixture();
        f.join();
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        f.presence.refresh(() -> List.of(f.player), captures::put);
        Player replacement = player(UUID.randomUUID(), NAME.toUpperCase(java.util.Locale.ROOT));
        f.presence.captureOnline(STORAGE, replacement);
        captures.get(f.player).accept(STORAGE);
        assertSame(replacement, f.presence.schedulerOwner(STORAGE));
        assertNull(f.presence.liveIdentity(SERVER));
    }

    @Test void displacedRefreshCannotResurrectAfterTheSuccessorQuits() {
        Fixture f = new Fixture();
        f.join();
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        f.presence.refresh(() -> List.of(f.player), captures::put);
        Player replacement = player(UUID.randomUUID(), NAME.toUpperCase(java.util.Locale.ROOT));
        f.presence.captureOnline(STORAGE, replacement);
        assertTrue(f.presence.playerOffline(STORAGE, replacement));
        captures.get(f.player).accept(STORAGE);
        assertNull(f.presence.liveIdentity(SERVER));
        assertNull(f.presence.schedulerOwner(STORAGE));
    }

    @Test void retiredUnknownRefreshCannotResurrectAfterAReplacementQuits() {
        Fixture f = new Fixture();
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        f.presence.refresh(() -> List.of(f.player), captures::put);
        Player replacement = player(UUID.randomUUID(), NAME.toUpperCase(java.util.Locale.ROOT));
        f.presence.captureOnline(STORAGE, replacement);
        assertTrue(f.presence.playerOffline(STORAGE, replacement));
        when(f.player.isOnline()).thenReturn(false);
        captures.get(f.player).accept(STORAGE);
        assertNull(f.presence.liveIdentity(SERVER));
        assertNull(f.presence.schedulerOwner(STORAGE));
    }

    @Test void delayedLoginCannotTakeAnotherSessionsStorageUuid() {
        Fixture f = new Fixture();
        f.presence.captureOnline(SERVER, f.player);
        Player replacement = player(UUID.randomUUID(), NAME.toUpperCase(java.util.Locale.ROOT));
        f.presence.captureOnline(STORAGE, replacement);
        clearInvocations(f.player, replacement);
        assertFalse(f.presence.updateStorageUuid(STORAGE, f.player));
        assertSame(replacement, f.presence.schedulerOwner(STORAGE));
        verifyNoInteractions(f.player, replacement);
    }

    @Test void refreshRetiresExactOwnersEvenWhenPlayerEqualityMatchesUuid() {
        PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
        Player old = equivalentPlayer(SERVER, NAME);
        Player current = equivalentPlayer(SERVER, NAME);
        UUID oldStorage = UUID.randomUUID();
        presence.captureOnline(oldStorage, old);
        presence.captureOnline(STORAGE, current);
        assertNull(presence.liveIdentity(SERVER));
        Map<Player, Consumer<UUID>> captures = new java.util.IdentityHashMap<>();
        presence.refresh(() -> List.of(current), captures::put);
        captures.get(current).accept(STORAGE);
        var identity = presence.liveIdentity(SERVER);
        assertNotNull(identity);
        assertEquals(STORAGE, identity.storageUuid());
        assertNull(presence.schedulerOwner(oldStorage));
    }

    private static Player equivalentPlayer(UUID uuid, String name) {
        return (Player) java.lang.reflect.Proxy.newProxyInstance(Player.class.getClassLoader(),
                new Class<?>[] { Player.class }, (proxy, method, args) -> switch (method.getName()) {
                    case "getUniqueId" -> uuid;
                    case "getName" -> name;
                    case "isOnline" -> true;
                    case "equals" -> args[0] instanceof Player other && uuid.equals(other.getUniqueId());
                    case "hashCode" -> uuid.hashCode();
                    case "toString" -> name;
                    default -> throw new AssertionError("Unexpected player access: " + method.getName());
                });
    }

    private static Player player(UUID uuid, String name) {
        Player player = mock(Player.class);
        when(player.getUniqueId()).thenReturn(uuid);
        when(player.getName()).thenReturn(name);
        when(player.isOnline()).thenReturn(true);
        return player;
    }

    private static final class Fixture {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
        final Logger logger = mock(Logger.class);
        final Player player = player(SERVER, NAME);
        final UserManager users = new UserManager(plugin);
        Fixture() {
            when(plugin.getOptions()).thenReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class));
            when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
            when(plugin.getLogger()).thenReturn(logger);
        }
        void join() { presence.captureOnline(STORAGE, player); }
    }
}
