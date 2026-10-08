package com.bencodez.votingplugin.hologram;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Color;
import org.bukkit.Location;
import org.bukkit.entity.Display;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Interaction;
import org.bukkit.entity.TextDisplay;
import org.bukkit.util.Transformation;
import org.bukkit.util.Vector;
import org.joml.AxisAngle4f;
import org.joml.Vector3f;

/** Loaded only after the 1.19.4+ API/visibility probe succeeds. No NMS or external hologram plugin. */
final class NativeHologramRenderer {
    interface Spawned { void accept(Entity entity, HologramVoteMenu.Action action, int siteIndex); }
    private record Row(Location location, String text, HologramVoteMenu.Action action, int index, float width) { }
    static void render(HologramVoteMenu.Session session, HologramMenuScheduler scheduler, Spawned spawned) {
        List<Row> rows = new ArrayList<>();
        Location anchor = session.anchor;
        rows.add(new Row(offset(anchor, 0, 1.18), "\u00a76\u2726 VOTING REWARDS \u2726", null, -1, 0));
        List<HologramVoteModel.Site> page = HologramVoteModel.page(session.sites, session.page,
                session.settings.sitesPerPage());
        for (int i = 0; i < page.size(); i++) {
            rows.add(new Row(offset(anchor, 0, .7 - i * .38), page.get(i).label(),
                    HologramVoteMenu.Action.SITE, session.page * session.settings.sitesPerPage() + i, 2.6f));
        }
        if (page.isEmpty()) rows.add(new Row(offset(anchor, 0, .7), "\u00a77No visible enabled voting sites", null, -1, 0));
        double footer = .58 - Math.max(1, page.size()) * .38;
        int pages = HologramVoteModel.pages(session.sites.size(), session.settings.sitesPerPage());
        rows.add(new Row(offset(anchor, -.95, footer), (session.page > 0 ? "\u00a7e" : "\u00a78") + "[Previous]",
                session.page > 0 ? HologramVoteMenu.Action.PREVIOUS : null, -1, .75f));
        rows.add(new Row(offset(anchor, 0, footer), (session.page < pages - 1 ? "\u00a7e" : "\u00a78") + "[Next]",
                session.page < pages - 1 ? HologramVoteMenu.Action.NEXT : null, -1, .75f));
        rows.add(new Row(offset(anchor, .95, footer), "\u00a7c[Close]", HologramVoteMenu.Action.CLOSE, -1, .75f));
        rows.add(new Row(offset(anchor, 0, footer - .35), "\u00a78Page " + (session.page + 1) + "/" + pages
                + " - Right-click an entry", null, -1, 0));
        // Check all spawn chunks first. Never touch a neighbouring Folia region from this callback.
        for (Row row : rows) if (!scheduler.owns(row.location()))
            throw new IllegalStateException("Menu spans an unowned region");
        for (Row row : rows) {
            TextDisplay display = anchor.getWorld().spawn(row.location(), TextDisplay.class, text -> {
                configure(text);
                text.setText(row.text());
                text.setBillboard(Display.Billboard.FIXED);
                text.setLineWidth(450);
                text.setShadowed(true);
                text.setSeeThrough(false);
                text.setBackgroundColor(Color.fromARGB(185, 12, 16, 25));
                text.setAlignment(TextDisplay.TextAlignment.CENTER);
                text.setTransformation(new Transformation(new Vector3f(), new AxisAngle4f(),
                        new Vector3f(.5f), new AxisAngle4f()));
            });
            spawned.accept(display, null, -1);
            if (row.action() != null) {
                Interaction interaction = anchor.getWorld().spawn(row.location().clone().subtract(0, .06, 0),
                        Interaction.class, hit -> {
                            configure(hit);
                            hit.setInteractionWidth(row.width());
                            hit.setInteractionHeight(.29f);
                            hit.setResponsive(false);
                        });
                spawned.accept(interaction, row.action(), row.index());
            }
        }
    }
    private static void configure(Entity entity) {
        entity.setPersistent(false);
        entity.setGravity(false);
        entity.setInvulnerable(true);
        entity.setVisibleByDefault(false);
    }
    private static Location offset(Location anchor, double sideways, double up) {
        double yaw = Math.toRadians(anchor.getYaw());
        return anchor.clone().add(new Vector(Math.cos(yaw) * sideways, up, Math.sin(yaw) * sideways));
    }
}
