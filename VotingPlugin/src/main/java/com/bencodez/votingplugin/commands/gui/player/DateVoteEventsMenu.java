package com.bencodez.votingplugin.commands.gui.player;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.function.BooleanSupplier;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.entity.HumanEntity;

import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Availability;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Event;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot.Milestone;

/** Entity-thread presentation of an immutable storage-worker result. */
public final class DateVoteEventsMenu {
    private static final int PAGE_SIZE = 45;
    private static final int LIMIT = 64;
    private static final String OPEN_GUARD = "DateVoteEventsOpenGuard";
    private record OpenGuard(java.util.UUID owner, BooleanSupplier eligible) { }
    private static final DateTimeFormatter TIME = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss");

    private final VotingPluginMain plugin;
    private final Player player;
    private final DateVoteProgressSnapshot snapshot;
    private final BooleanSupplier current;

    private DateVoteEventsMenu(VotingPluginMain plugin, Player player, DateVoteProgressSnapshot snapshot,
            BooleanSupplier current) {
        this.plugin = plugin;
        this.player = player;
        this.snapshot = snapshot;
        this.current = current;
    }

    public static void open(VotingPluginMain plugin, Player player, DateVoteProgressSnapshot snapshot,
            BooleanSupplier current) {
        new DateVoteEventsMenu(plugin, player, snapshot, current).list(0);
    }

    private boolean eligible() {
        return current.getAsBoolean() && plugin.isEnabled() && player.isOnline()
                && (player.hasPermission("VotingPlugin.Commands.Vote.DateEvents")
                        || player.hasPermission("VotingPlugin.Player"));
    }

    private BInventory inventory(String title) {
        BInventory inventory = new BInventory(title);
        inventory.setMaxInvSize(54);
        inventory.setPages(false);
        inventory.runClicksSync();
        inventory.dontClose();
        inventory.addData(OPEN_GUARD, new OpenGuard(player.getUniqueId(), this::eligible));
        return inventory;
    }

    /** Called at Bukkit's final InventoryOpenEvent, after AdvancedCore's deferred callback. */
    public static boolean permitsOpen(BInventory inventory, HumanEntity viewer) {
        Object value = inventory.getData(OPEN_GUARD);
        return !(value instanceof OpenGuard guard)
                || guard.owner().equals(viewer.getUniqueId()) && guard.eligible().getAsBoolean();
    }

    private void list(int page) {
        if (!eligible()) return;
        List<Event> events = snapshot.events().stream().limit(LIMIT).toList();
        BInventory inventory = inventory("Date vote events " + (page + 1) + "/" + pages(events.size()));
        int end = Math.min(events.size(), (page + 1) * PAGE_SIZE);
        for (int index = page * PAGE_SIZE; index < end; index++) {
            Event event = events.get(index);
            inventory.addButton(index % PAGE_SIZE, button(Material.PAPER,
                    event.definition().displayName(), eventLore(event), () -> detail(event, 0, page)));
        }
        List<String> summary = new ArrayList<>();
        if (events.isEmpty()) summary.add("No date vote events configured.");
        else if (events.stream().noneMatch(event -> event.definition().enabled()))
            summary.add("No enabled date vote events.");
        else summary.add("Select an event to view milestones.");
        summary.add("Snapshot: " + Instant.ofEpochMilli(snapshot.capturedAt()));
        inventory.addButton(49, button(Material.BOOK, "Date vote events", summary, () -> {}));
        if (page > 0) inventory.addButton(45, button(Material.ARROW, "Previous page", List.of(), () -> list(page - 1)));
        if (end < events.size()) inventory.addButton(53, button(Material.ARROW, "Next page", List.of(), () -> list(page + 1)));
        if (eligible()) inventory.openInventory(player);
    }

    private void detail(Event event, int page, int listPage) {
        if (!eligible()) return;
        List<Milestone> milestones = event.milestones().stream().limit(LIMIT).toList();
        BInventory inventory = inventory("Date vote milestones " + (page + 1) + "/" + pages(milestones.size()));
        int end = Math.min(milestones.size(), (page + 1) * PAGE_SIZE);
        for (int index = page * PAGE_SIZE; index < end; index++) {
            Milestone milestone = milestones.get(index);
            List<String> lore = new ArrayList<>();
            lore.add("ID: " + milestone.id());
            lore.add("Enabled: " + milestone.enabled());
            lore.add("Award state: " + event.awardState(milestone.threshold()));
            List<String> types = milestone.rewardTypes().stream().limit(LIMIT)
                    .map(type -> type.matches("[A-Za-z0-9_-]{1,64}") ? type : "Other").distinct().toList();
            lore.add("Reward types: " + (types.isEmpty() ? "none" : String.join(", ", types)));
            inventory.addButton(index % PAGE_SIZE, button(Material.PAPER, milestone.threshold() + " votes", lore, () -> {}));
        }
        List<String> summary = eventLore(event);
        if (milestones.isEmpty()) summary.add("No milestones configured.");
        inventory.addButton(48, button(Material.BOOK, event.definition().displayName(), summary, () -> {}));
        inventory.addButton(49, button(Material.ARROW, "Back to events", List.of(), () -> list(listPage)));
        if (page > 0) inventory.addButton(45, button(Material.ARROW, "Previous page", List.of(), () -> detail(event, page - 1, listPage)));
        if (end < milestones.size()) inventory.addButton(53, button(Material.ARROW, "Next page", List.of(), () -> detail(event, page + 1, listPage)));
        if (eligible()) inventory.openInventory(player);
    }

    private List<String> eventLore(Event event) {
        var definition = event.definition();
        List<String> lore = new ArrayList<>();
        lore.add("ID: " + definition.id());
        lore.add("Status: " + event.status(snapshot.capturedAt()));
        ZoneId zone = ZoneId.of(definition.timezone());
        lore.add("Start: " + TIME.format(Instant.ofEpochMilli(definition.start()).atZone(zone)));
        lore.add("End (exclusive): " + TIME.format(Instant.ofEpochMilli(definition.end()).atZone(zone)));
        lore.add("Timezone: " + definition.timezone());
        if (event.availability() == Availability.OWNED) {
            lore.add("Votes: " + event.votes());
            lore.add("Next milestone: " + (event.nextThreshold().isPresent() ? event.nextThreshold().getAsInt() + " votes" : "none"));
        } else if (event.availability() == Availability.REMOTE) {
            lore.add("Progress owned by: " + definition.accountingServer());
            lore.add("Progress unavailable on this server.");
        } else {
            lore.add("Progress unavailable: accounting read failed.");
        }
        return lore;
    }

    private static int pages(int size) {
        return Math.max(1, (size + PAGE_SIZE - 1) / PAGE_SIZE);
    }

    private BInventoryButton button(Material material, String name, List<String> lore, Runnable action) {
        List<String> lines = List.copyOf(lore);
        return new BInventoryButton(new ItemBuilder(material)) {
            // ItemBuilder evaluates display Javascript and formatting while rendering. Render only
            // the blank material through it, then apply snapshot text literally to Bukkit metadata.
            @Override public ItemStack getItem() {
                ItemStack item = new ItemBuilder(material).toItemStack();
                ItemMeta meta = item.getItemMeta();
                if (meta != null) {
                    meta.setDisplayName(name);
                    meta.setLore(lines);
                    item.setItemMeta(meta);
                }
                return item;
            }
            @Override public ItemStack getItem(Player ignored) { return getItem(); }
            @Override public ItemStack getItem(Player ignored, HashMap<String, String> placeholders) { return getItem(); }
            @Override public void onClick(ClickEvent click) {
                if (eligible()) action.run();
            }
        }.dontClose();
    }
}
