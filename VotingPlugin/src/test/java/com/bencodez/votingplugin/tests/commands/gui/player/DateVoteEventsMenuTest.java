package com.bencodez.votingplugin.tests.commands.gui.player;

import com.bencodez.votingplugin.commands.gui.player.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Availability;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Event;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Milestone;

class DateVoteEventsMenuTest {
    private static final long NOW = 1_800_000_000_000L;

    @Test
    void localProgressAndLiteralTextComeFromSnapshotAndNavigationPreservesListPage() {
        try (Boundary boundary = new Boundary()) {
            String literal = "&a<red>%player% {javascript:throw new Error('must not run')}";
            Event event = event("literal", literal, true, NOW - 1000, NOW + 1000, Availability.OWNED,
                    7, Set.of(1), Set.of(2), Set.of(3), List.of(
                            milestone(1, true), milestone(2, true), milestone(3, true),
                            milestone(5, true), milestone(10, false), milestone(20, true)));
            List<Event> events = new ArrayList<>(IntStream.range(0, 45)
                    .mapToObj(i -> simpleEvent("event" + i, Availability.OWNED)).toList());
            events.add(event);
            boundary.open(events);
            boundary.click(0, 53);
            assertEquals(literal, boundary.name(1, 0));
            assertTrue(boundary.lore(1, 0).contains("Votes: 7"));
            assertTrue(boundary.lore(1, 0).contains("Next milestone: 20 votes"));
            assertTrue(boundary.lore(1, 0).contains("Timezone: America/Regina"));
            assertTrue(boundary.lore(1, 0).contains("Status: active"));
            boundary.click(1, 0);
            assertEquals(literal, boundary.name(2, 48));
            assertTrue(boundary.lore(2, 0).contains("Award state: submitted"));
            assertTrue(boundary.lore(2, 1).contains("Award state: deferred"));
            assertTrue(boundary.lore(2, 2).contains("Award state: pending review"));
            assertTrue(boundary.lore(2, 3).contains("Award state: reached"));
            assertTrue(boundary.lore(2, 4).contains("Award state: not reached"));
            assertTrue(boundary.lore(2, 4).contains("Enabled: false"));
            assertTrue(boundary.lore(2, 5).contains("Reward types: Commands, Items"));
            boundary.click(2, 49);
            assertEquals(literal, boundary.name(3, 0));
            boundary.assertOnlyPresentationAccess();
        }
    }

    @Test
    void all64EventsAndMilestonesAreReachableAcrossTwoBoundedPages() {
        try (Boundary boundary = new Boundary()) {
            List<Milestone> milestones = IntStream.rangeClosed(1, 64).mapToObj(i -> milestone(i, true)).toList();
            List<Event> events = IntStream.range(0, 64).mapToObj(i -> event("e" + i, "Event " + i, true,
                    NOW - 1000, NOW + 1000, Availability.OWNED, 0, Set.of(), Set.of(), Set.of(), milestones)).toList();
            boundary.open(events);
            assertEquals(47, boundary.buttons.get(0).size());
            assertFalse(boundary.buttons.get(0).containsKey(45));
            boundary.click(0, 53);
            assertEquals("Event 45", boundary.name(1, 0));
            assertEquals("Event 63", boundary.name(1, 18));
            assertFalse(boundary.buttons.get(1).containsKey(19));
            assertFalse(boundary.buttons.get(1).containsKey(53));
            boundary.click(1, 18);
            assertEquals("1 votes", boundary.name(2, 0));
            assertEquals("45 votes", boundary.name(2, 44));
            boundary.click(2, 53);
            assertEquals("46 votes", boundary.name(3, 0));
            assertEquals("64 votes", boundary.name(3, 18));
            assertFalse(boundary.buttons.get(3).containsKey(19));
            assertFalse(boundary.buttons.get(3).containsKey(53));
            boundary.click(3, 45);
            assertEquals("1 votes", boundary.name(4, 0));
            boundary.click(4, 49);
            assertEquals("Event 45", boundary.name(5, 0));
            boundary.click(5, 45);
            assertEquals("Event 0", boundary.name(6, 0));
            boundary.inventories.constructed().forEach(inventory -> {
                verify(inventory).runClicksSync();
                verify(inventory).setMaxInvSize(54);
                verify(inventory).setPages(false);
                verify(inventory).openInventory(boundary.player);
            });
            boundary.assertOnlyPresentationAccess();
        }
    }

    @Test
    void remoteOwnerAndUnavailableErrorsNeverFabricateZeroProgress() {
        try (Boundary boundary = new Boundary()) {
            boundary.open(List.of(simpleEvent("remote", Availability.REMOTE),
                    simpleEvent("failed", Availability.UNAVAILABLE)));
            assertTrue(boundary.lore(0, 0).contains("Progress owned by: Exact-Owner"));
            assertTrue(boundary.lore(0, 1).contains("Progress unavailable: accounting read failed."));
            for (int slot = 0; slot < 2; slot++) {
                assertTrue(boundary.lore(0, slot).stream().noneMatch(line -> line.startsWith("Votes:")
                        || line.startsWith("Next milestone:")));
            }
            boundary.click(0, 0);
            assertTrue(boundary.lore(1, 0).contains("Award state: progress unavailable"));
            boundary.click(1, 49);
            boundary.click(2, 1);
            assertTrue(boundary.lore(3, 0).contains("Award state: progress unavailable"));
            boundary.assertOnlyPresentationAccess();
        }
    }

    @Test
    void statusUsesCapturedTimeWithInclusiveStartAndExclusiveEnd() {
        try (Boundary boundary = new Boundary()) {
            boundary.open(List.of(
                    event("future", "Future", true, NOW + 1, NOW + 100, Availability.OWNED, 0, Set.of(), Set.of(), Set.of(), List.of(milestone(1, true))),
                    event("start", "Start", true, NOW, NOW + 100, Availability.OWNED, 0, Set.of(), Set.of(), Set.of(), List.of(milestone(1, true))),
                    event("end", "End", true, NOW - 100, NOW, Availability.OWNED, 0, Set.of(), Set.of(), Set.of(), List.of(milestone(1, true))),
                    event("off", "Off", false, NOW - 100, NOW + 100, Availability.OWNED, 0, Set.of(), Set.of(), Set.of(), List.of(milestone(1, false)))));
            String[] statuses = {"upcoming", "active", "ended", "disabled"};
            for (int i = 0; i < statuses.length; i++)
                assertTrue(boundary.lore(0, i).contains("Status: " + statuses[i]));
        }
    }

    @Test
    void emptyAndAllDisabledStatesAreExplicit() {
        try (Boundary boundary = new Boundary()) {
            boundary.open(List.of());
            assertTrue(boundary.lore(0, 49).contains("No date vote events configured."));
            assertFalse(boundary.buttons.get(0).containsKey(0));
            Event disabled = event("off", "Disabled", false, NOW - 1, NOW + 1, Availability.OWNED,
                    0, Set.of(), Set.of(), Set.of(), List.of(milestone(1, false)));
            boundary.open(List.of(disabled));
            assertTrue(boundary.lore(1, 49).contains("No enabled date vote events."));
            boundary.click(1, 0);
            assertTrue(boundary.lore(2, 0).contains("Enabled: false"));
        }
    }

    @Test
    void previewContainsOnlyTypesAndMilestoneClicksHaveNoRewardEffects() {
        try (Boundary boundary = new Boundary()) {
            Event event = event("preview", "Preview", true, NOW - 1, NOW + 1, Availability.OWNED,
                    1, Set.of(), Set.of(), Set.of(), List.of(new Milestone(1, "one", true,
                            List.of("Commands", "say PRIVATE_SECRET", "{javascript:bad}", "Items"))));
            boundary.open(List.of(event));
            boundary.click(0, 0);
            assertTrue(boundary.lore(1, 0).contains("Reward types: Commands, Other, Items"));
            assertFalse(String.join(" ", boundary.lore(1, 0)).contains("PRIVATE_SECRET"));
            boundary.click(1, 0);
            assertEquals(2, boundary.inventories.constructed().size());
            boundary.assertOnlyPresentationAccess();
        }
    }

    @Test
    void staleOfflineDisabledAndRevokedPermissionsBlockInitialAndLaterNavigation() {
        for (int failure = 0; failure < 4; failure++) {
            try (Boundary boundary = new Boundary()) {
                boundary.open(List.of(simpleEvent("event", Availability.OWNED)));
                switch (failure) {
                    case 0 -> boundary.current.set(false);
                    case 1 -> when(boundary.player.isOnline()).thenReturn(false);
                    case 2 -> when(boundary.plugin.isEnabled()).thenReturn(false);
                    case 3 -> when(boundary.player.hasPermission(anyString())).thenReturn(false);
                    default -> fail();
                }
                boundary.click(0, 0);
                boundary.open(List.of(simpleEvent("other", Availability.OWNED)));
                assertEquals(1, boundary.inventories.constructed().size());
            }
        }
    }

    @Test
    void backAndBothPaginationDirectionsRecheckEligibility() {
        for (int control : new int[] {45, 49, 53}) {
            try (Boundary boundary = new Boundary()) {
                List<Milestone> milestones = IntStream.rangeClosed(1, 64).mapToObj(i -> milestone(i, true)).toList();
                boundary.open(List.of(event("event", "Event", true, NOW - 1, NOW + 1, Availability.OWNED,
                        0, Set.of(), Set.of(), Set.of(), milestones)));
                boundary.click(0, 0);
                int screen = 1;
                if (control == 45) { boundary.click(1, 53); screen = 2; }
                boundary.current.set(false);
                boundary.click(screen, control);
                assertEquals(screen + 1, boundary.inventories.constructed().size());
            }
        }
    }

    @Test
    void playerPermissionIsAcceptedAndSnapshotIsRecheckedImmediatelyBeforeOpen() {
        try (Boundary boundary = new Boundary()) {
            when(boundary.player.hasPermission("VotingPlugin.Commands.Vote.DateEvents")).thenReturn(false);
            when(boundary.player.hasPermission("VotingPlugin.Player")).thenReturn(true);
            boundary.open(List.of());
            verify(boundary.inventories.constructed().get(0)).openInventory(boundary.player);
            AtomicInteger checks = new AtomicInteger();
            DateVoteEventsMenu.open(boundary.plugin, boundary.player, new DateVoteProgressSnapshot(NOW, List.of()),
                    () -> checks.incrementAndGet() == 1);
            verify(boundary.inventories.constructed().get(1), never()).openInventory(any(Player.class));
        }
    }

    private static Milestone milestone(int threshold, boolean enabled) {
        return new Milestone(threshold, "m" + threshold, enabled, List.of("Commands", "Items"));
    }

    private static Event simpleEvent(String id, Availability availability) {
        return event(id, id, true, NOW - 1000, NOW + 1000, availability, 0,
                Set.of(), Set.of(), Set.of(), List.of(milestone(1, true)));
    }

    private static Event event(String id, String name, boolean enabled, long start, long end,
            Availability availability, int votes, Set<Integer> submitted, Set<Integer> deferred,
            Set<Integer> reserved, List<Milestone> milestones) {
        DateVoteEvent definition = new DateVoteEvent(id, name, enabled, start, end, "America/Regina",
                milestones.stream().map(Milestone::threshold).toList(), Set.of(), "Exact-Owner");
        return new Event(definition, availability, votes, submitted, deferred, reserved, milestones);
    }

    /** Mock only platform/item rendering and inventory opening; retain real buttons and callbacks. */
    private static final class Boundary implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final Player player = mock(Player.class);
        final AtomicBoolean current = new AtomicBoolean(true);
        final List<Map<Integer, BInventoryButton>> buttons = new ArrayList<>();
        final MockedConstruction<BInventory> inventories;
        final MockedConstruction<ItemBuilder> builders;

        Boundary() {
            when(plugin.isEnabled()).thenReturn(true);
            when(player.isOnline()).thenReturn(true);
            when(player.hasPermission("VotingPlugin.Commands.Vote.DateEvents")).thenReturn(true);
            inventories = mockConstruction(BInventory.class, (inventory, context) -> {
                Map<Integer, BInventoryButton> entries = new HashMap<>();
                buttons.add(entries);
                doAnswer(call -> { entries.put(call.getArgument(0), call.getArgument(1)); return null; })
                        .when(inventory).addButton(anyInt(), any(BInventoryButton.class));
            });
            builders = mockConstruction(ItemBuilder.class, (builder, context) -> {
                assertEquals(1, context.arguments().size());
                assertInstanceOf(Material.class, context.arguments().get(0));
                ItemStack stack = mock(ItemStack.class);
                ItemMeta meta = mock(ItemMeta.class);
                Map<String, Object> rendered = new HashMap<>();
                when(stack.getItemMeta()).thenReturn(meta);
                when(builder.toItemStack()).thenReturn(stack);
                doAnswer(call -> { rendered.put("name", call.getArgument(0)); return null; })
                        .when(meta).setDisplayName(anyString());
                doAnswer(call -> { rendered.put("lore", call.getArgument(0)); return null; })
                        .when(meta).setLore(anyList());
                when(meta.getDisplayName()).thenAnswer(call -> rendered.get("name"));
                when(meta.getLore()).thenAnswer(call -> rendered.get("lore"));
            });
        }

        void open(List<Event> events) {
            DateVoteEventsMenu.open(plugin, player, new DateVoteProgressSnapshot(NOW, events), current::get);
        }

        void click(int screen, int slot) {
            BInventoryButton button = buttons.get(screen).get(slot);
            assertNotNull(button, "Expected navigation button in slot " + slot);
            button.onClick(null);
        }

        String name(int screen, int slot) {
            return buttons.get(screen).get(slot).getItem(player, new HashMap<>()).getItemMeta().getDisplayName();
        }

        List<String> lore(int screen, int slot) {
            return buttons.get(screen).get(slot).getItem(player).getItemMeta().getLore();
        }

        void assertOnlyPresentationAccess() {
            assertTrue(mockingDetails(plugin).getInvocations().stream()
                    .allMatch(call -> call.getMethod().getName().equals("isEnabled")),
                    "Presentation must not access users, storage, rewards, configuration or schedulers");
            builders.constructed().forEach(builder -> {
                verify(builder, never()).setName(anyString());
                verify(builder, never()).setLore(anyList());
                verify(builder, never()).addPlaceholder(anyString(), anyString());
            });
        }

        @Override public void close() {
            builders.close();
            inventories.close();
        }
    }
}
