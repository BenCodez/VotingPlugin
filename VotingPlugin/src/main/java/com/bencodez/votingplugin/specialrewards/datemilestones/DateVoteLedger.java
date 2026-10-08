package com.bencodez.votingplugin.specialrewards.datemilestones;

import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import com.bencodez.votingplugin.util.DurableFiles;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;

/** Single local accounting owner. All entry points must run on a persistence worker. */
public final class DateVoteLedger {
    public record Progress(int votes, Set<Integer> reservedAwards, Set<Integer> submittedAwards, Set<Integer> deferredAwards) { }
    private final Path directory;
    private int recordCount = -1;
    public DateVoteLedger(Path directory) { this.directory = directory.toAbsolutePath().normalize(); }

    public synchronized List<Integer> record(DateVoteEvent event, UUID player, UUID occurrence) throws IOException {
        return record(event, player, occurrence, true);
    }
    public synchronized List<Integer> record(DateVoteEvent event, UUID player, UUID occurrence, boolean processRewards) throws IOException {
        seal(event);
        Path file = playerFile(event, player);
        Properties state = read(file);
        String fingerprint = state.getProperty("fingerprint");
        if (fingerprint != null && !fingerprint.equals(event.fingerprint())) throw new IOException("Event accounting changed; use a new event ID");
        validateFingerprint(event, state);
        Set<UUID> seen = occurrences(state);
        boolean changed = !seen.contains(occurrence);
        if (changed && seen.size() >= 4096) throw new IOException("Event occurrence capacity reached (4096); progress retained");
        seen.add(occurrence);
        state.setProperty("fingerprint", event.fingerprint());
        state.setProperty("seen", String.join(",", seen.stream().map(UUID::toString).sorted().toList()));
        List<Integer> awards = new ArrayList<>();
        for (int threshold : event.thresholds()) {
            String key = "award." + threshold;
            if (seen.size() >= threshold && (!state.containsKey(key)
                    || processRewards && "DEFERRED".equals(state.getProperty(key)))) {
                state.setProperty(key, processRewards ? "RESERVED" : "DEFERRED");
                if (processRewards) awards.add(threshold);
                changed = true;
            }
        }
        // Reserve the whole count/award decision before invoking arbitrary external rewards.
        if (changed) write(file, state);
        return List.copyOf(awards);
    }
    public synchronized void submitted(DateVoteEvent event, UUID player, int threshold) throws IOException {
        Path file = playerFile(event, player);
        Properties state = read(file);
        validateFingerprint(event, state);
        if (!"RESERVED".equals(state.getProperty("award." + threshold))) return;
        state.setProperty("award." + threshold, "SUBMITTED");
        write(file, state);
    }
    public synchronized Progress progress(DateVoteEvent event, UUID player) throws IOException {
        Properties definitions = readSealedDefinitions(event);
        String sealed = definitions.getProperty(event.id());
        if (sealed != null && !sealed.equals(event.fingerprint())) throw new IOException("Event accounting changed; use a new ID");
        Properties state = read(playerFile(event, player));
        validateFingerprint(event, state);
        Set<Integer> reserved = new HashSet<>(), submitted = new HashSet<>(), deferred = new HashSet<>();
        for (String key : state.stringPropertyNames()) {
            if (key.startsWith("award.")) {
                int threshold;
                try { threshold = Integer.parseInt(key.substring(6)); }
                catch (NumberFormatException invalid) { throw new IOException("Invalid award state", invalid); }
                if ("RESERVED".equals(state.getProperty(key))) reserved.add(threshold);
                else if ("SUBMITTED".equals(state.getProperty(key))) submitted.add(threshold);
                else if ("DEFERRED".equals(state.getProperty(key))) deferred.add(threshold);
                else throw new IOException("Unknown award state");
            }
        }
        return new Progress(occurrences(state).size(), Set.copyOf(reserved), Set.copyOf(submitted), Set.copyOf(deferred));
    }
    private void validateFingerprint(DateVoteEvent event, Properties state) throws IOException {
        if (state.isEmpty()) return;
        if (!event.fingerprint().equals(state.getProperty("fingerprint")))
            throw new IOException("Event accounting changed or state invalid; use a new ID");
        int votes = occurrences(state).size();
        for (int threshold : event.thresholds()) if (votes >= threshold && !state.containsKey("award." + threshold))
            throw new IOException("Missing reached milestone award state; progress retained for review");
        for (String key : state.stringPropertyNames()) if (key.startsWith("award.")
                && !event.thresholds().contains(Integer.parseInt(key.substring(6))))
            throw new IOException("Award is outside configured milestone contract");
    }
    private void seal(DateVoteEvent event) throws IOException {
        Properties definitions = readSealedDefinitions(event);
        String old = definitions.getProperty(event.id());
        if (old != null && !old.equals(event.fingerprint())) throw new IOException("DateVoteMilestones event " + event.id() + " changed accounting; use a new ID");
        if (old == null) {
            if (definitions.size() >= 1024) throw new IOException("Event definition capacity reached");
            definitions.setProperty(event.id(), event.fingerprint());
            write(directory.resolve("definitions.properties"), definitions);
        }
    }
    /** Missing metadata cannot turn surviving player history into a fresh event. */
    private Properties readSealedDefinitions(DateVoteEvent event) throws IOException {
        if (Files.isSymbolicLink(directory)) throw new IOException("Unsafe milestone directory");
        Properties definitions = read(directory.resolve("definitions.properties"));
        if (!definitions.containsKey(event.id()) && Files.exists(directory, LinkOption.NOFOLLOW_LINKS)) {
            // Bounded accounting directory; this lookup is exclusively on the I/O owner.
            try (var history = Files.newDirectoryStream(directory, event.fileId() + "-*.properties")) {
                if (history.iterator().hasNext())
                    throw new IOException("Missing historical event seal; preserve state and repair metadata before accounting");
            }
        }
        return definitions;
    }
    private Path playerFile(DateVoteEvent event, UUID player) {
        return directory.resolve(event.fileId() + "-" + player + ".properties");
    }
    private Set<UUID> occurrences(Properties state) throws IOException {
        Set<UUID> ids = new HashSet<>();
        String raw = state.getProperty("seen", "");
        if (!raw.isEmpty()) for (String id : raw.split(",", -1)) {
            try { if (!ids.add(UUID.fromString(id))) throw new IOException("Duplicate stored occurrence"); }
            catch (IllegalArgumentException invalid) { throw new IOException("Invalid stored occurrence", invalid); }
            if (ids.size() > 4096) throw new IOException("Oversized event state");
        }
        return ids;
    }
    private Properties read(Path file) throws IOException {
        Properties state = new Properties() {
            @Override public synchronized Object put(Object key, Object value) {
                if (containsKey(key)) throw new IllegalArgumentException("Duplicate stored field");
                return super.put(key, value);
            }
        };
        if (Files.notExists(file, LinkOption.NOFOLLOW_LINKS)) return new Properties();
        if (Files.isSymbolicLink(directory)) throw new IOException("Unsafe milestone directory");
        if (Files.isSymbolicLink(file) || !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) || Files.size(file) > 256 * 1024)
            throw new IOException("Unsafe/oversized date milestone state");
        try { state.load(new StringReader(Files.readString(file))); }
        catch (IllegalArgumentException invalid) { throw new IOException("Malformed milestone state", invalid); }
        if (file.getFileName().toString().equals("definitions.properties")) {
            if (state.isEmpty() || state.size() > 1024) throw new IOException("Empty/oversized definitions; existing seals must not be reset");
            for (String key : state.stringPropertyNames()) if (!key.matches("[A-Za-z0-9_-]{1,64}")
                    || !state.getProperty(key).matches("[a-f0-9]{64}")) throw new IOException("Invalid definition state");
        } else {
            if (!state.containsKey("fingerprint") || !state.getProperty("fingerprint").matches("[a-f0-9]{64}")
                    || !state.containsKey("seen") || state.size() > 66) throw new IOException("Incomplete milestone state");
            Set<UUID> seen = occurrences(state);
            for (String key : state.stringPropertyNames()) if (!key.equals("fingerprint") && !key.equals("seen")) {
                if (!key.matches("award\\.[1-9][0-9]{0,3}")) throw new IOException("Unknown milestone state field");
                int threshold = Integer.parseInt(key.substring(6));
                if (threshold > 4096 || threshold > seen.size()
                        || !(state.getProperty(key).equals("RESERVED") || state.getProperty(key).equals("SUBMITTED") || state.getProperty(key).equals("DEFERRED")))
                    throw new IOException("Invalid award state");
            }
        }
        Properties mutable = new Properties(); mutable.putAll(state);
        return mutable;
    }
    protected void write(Path file, Properties state) throws IOException {
        if (Files.isSymbolicLink(directory)) throw new IOException("Unsafe milestone directory");
        Files.createDirectories(directory);
        if (Files.isSymbolicLink(directory) || Files.isSymbolicLink(file)) throw new IOException("Unsafe milestone state path");
        if (recordCount < 0) {
            recordCount = 0;
            try (var files = Files.newDirectoryStream(directory, "*.properties")) {
                for (Path existing : files) if (!existing.getFileName().toString().equals("definitions.properties")) {
                    if (++recordCount > 100_000) throw new IOException("Milestone record capacity exceeded");
                }
            } catch (IOException failure) { recordCount = -1; throw failure; }
        }
        boolean newRecord = !file.getFileName().toString().equals("definitions.properties") && Files.notExists(file, LinkOption.NOFOLLOW_LINKS);
        if (newRecord && recordCount >= 100_000) throw new IOException("Milestone record capacity reached; existing progress retained");
        StringWriter writer = new StringWriter(); state.store(writer, "DateVoteMilestones v1");
        String text = writer.toString();
        if (text.length() > 256 * 1024) throw new IOException("Oversized milestone state");
        Path staged = Files.createTempFile(directory, "milestone-", ".tmp");
        try { Files.writeString(staged, text); DurableFiles.publishStagedFile(staged, file); if (newRecord) recordCount++; }
        catch (IOException failure) { recordCount = -1; throw failure; }
        finally { Files.deleteIfExists(staged); }
    }
}
