package com.bencodez.votingplugin.core.vote;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessingResult.RewardDisposition;

/**
 * TEST ONLY: atomic file snapshots model the existing user transaction and a
 * separate keyed reward owner. Recorded command/message strings are test effects,
 * not live server commands or an exactly-once external-effect implementation.
 */
class SharedVoteTestStore implements SharedVoteUserServices, SharedVoteRewardServices {
    final Path file;
    final Path rewardFile;
    final int pointsPerVote;
    boolean failBeforeCommit, failAfterCommit, failBeforeMark, failAfterMark;
    boolean failBeforeDelivery, failAfterDelivery;
    CompletableFuture<Void> commitAck = CompletableFuture.completedFuture(null);
    int mutationCount;

    SharedVoteTestStore(Path file, int pointsPerVote) {
        this.file = file;
        this.rewardFile = file.resolveSibling(file.getFileName() + ".rewards");
        this.pointsPerVote = pointsPerVote;
    }

    @Override public CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity user, SharedVoteMutation mutation) {
        return CompletableFuture.failedFuture(new AssertionError("Unsafe mutation-only API was called"));
    }

    @Override public synchronized CompletionStage<SharedVoteUserSnapshot> load(UUID uuid) {
        try { return CompletableFuture.completedFuture(snapshot(read(file), uuid + ".")); }
        catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public synchronized CompletionStage<SharedVoteReceipt> findVote(UUID id) {
        try { return CompletableFuture.completedFuture(receipt(read(file), id)); }
        catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public synchronized CompletionStage<SharedVoteReceipt> persistVoteWithReward(SharedVoteInput input,
            SharedVoteIdentity identity, SharedVoteMutation mutation, boolean execute) {
        try {
            Properties properties = read(file);
            SharedVoteReceipt existing = receipt(properties, input.voteId());
            if (existing != null) {
                existing.requireInput(input);
                if (!existing.identity().uuid().equals(identity.uuid())) throw new IllegalStateException("Conflicting user");
                return CompletableFuture.completedFuture(existing);
            }
            if (failBeforeCommit) { failBeforeCommit = false; throw new IOException("before commit"); }
            String prefix = identity.uuid() + ".";
            SharedVoteUserSnapshot previous = snapshot(properties, prefix);
            int increment = mutation.countTotals() ? 1 : 0;
            SharedVoteUserSnapshot next = new SharedVoteUserSnapshot(previous.allTimeTotal() + increment,
                    previous.monthTotal() + increment, previous.weeklyTotal() + increment, previous.dailyTotal() + increment,
                    previous.points() + (mutation.awardConfiguredPoints() ? pointsPerVote : 0));
            putSnapshot(properties, prefix, next);
            SharedVoteReceipt created = new SharedVoteReceipt(input, identity, mutation, next, execute, null);
            putReceipt(properties, created);
            // A SINGLE replace commits BOTH user state and the pending reward receipt.
            write(file, properties);
            mutationCount++;
            if (failAfterCommit) { failAfterCommit = false; throw new IOException("lost commit acknowledgement"); }
            return commitAck.thenApply(ignored -> created);
        } catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public synchronized CompletionStage<SharedVoteReceipt> markRewardCompleted(UUID id, RewardDisposition disposition) {
        try {
            if (failBeforeMark) { failBeforeMark = false; throw new IOException("before receipt acknowledgement"); }
            Properties properties = read(file);
            SharedVoteReceipt completed = receipt(properties, id).completed(disposition);
            putReceipt(properties, completed);
            write(file, properties);
            if (failAfterMark) { failAfterMark = false; throw new IOException("lost receipt acknowledgement"); }
            return CompletableFuture.completedFuture(completed);
        } catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public synchronized CompletionStage<List<SharedVoteReceipt>> pendingVotes(int limit) {
        try {
            Properties properties = read(file);
            List<SharedVoteReceipt> pending = new ArrayList<>();
            for (String key : properties.stringPropertyNames().stream().sorted().toList()) {
                if (key.startsWith("receipt.") && key.endsWith(".done") && properties.getProperty(key).isEmpty()) {
                    UUID id = UUID.fromString(key.substring("receipt.".length(), key.length() - ".done".length()));
                    pending.add(receipt(properties, id));
                    if (pending.size() == limit) break;
                }
            }
            return CompletableFuture.completedFuture(pending);
        } catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public synchronized CompletionStage<RewardDisposition> deliverOnce(SharedVoteReceipt receipt) {
        try {
            if (failBeforeDelivery) { failBeforeDelivery = false; throw new IOException("reward owner unavailable"); }
            Properties properties = read(rewardFile);
            String prefix = receipt.input().voteId() + ".";
            String origin = receipt.input() + "|" + receipt.identity() + "|" + receipt.mutation()
                    + "|" + receipt.persistedState() + "|" + receipt.executeRewardsNow();
            if (properties.containsKey(prefix + "done")) {
                if (!origin.equals(properties.getProperty(prefix + "origin"))) throw new IllegalStateException("Conflicting reward receipt");
                return CompletableFuture.completedFuture(RewardDisposition.valueOf(properties.getProperty(prefix + "done")));
            }
            RewardDisposition disposition = receipt.executeRewardsNow() ? RewardDisposition.EXECUTED : RewardDisposition.DEFERRED;
            if (receipt.executeRewardsNow()) {
                effect(properties, "command:say Thanks " + receipt.identity().playerName() + ":" + receipt.persistedState().allTimeTotal());
                effect(properties, "message:Thanks " + receipt.identity().playerName() + ":" + receipt.persistedState().points());
            } else {
                effect(properties, "defer:" + receipt.input().serviceSite() + ":" + receipt.persistedState().allTimeTotal());
            }
            properties.setProperty(prefix + "origin", origin);
            properties.setProperty(prefix + "done", disposition.name());
            write(rewardFile, properties);
            if (failAfterDelivery) { failAfterDelivery = false; throw new IOException("lost durable reward acknowledgement"); }
            return CompletableFuture.completedFuture(disposition);
        } catch (Exception failure) { return CompletableFuture.failedFuture(failure); }
    }

    @Override public CompletionStage<Void> executeVoteRewards(SharedVoteInput input, SharedVoteIdentity identity, SharedVoteUserSnapshot snapshot) {
        return CompletableFuture.failedFuture(new AssertionError("Unkeyed execution API was called"));
    }
    @Override public CompletionStage<Void> deferVoteRewards(SharedVoteInput input, SharedVoteIdentity identity, SharedVoteUserSnapshot snapshot) {
        return CompletableFuture.failedFuture(new AssertionError("Unkeyed deferral API was called"));
    }

    synchronized List<String> events() {
        try {
            Properties properties = read(rewardFile);
            List<String> events = new ArrayList<>();
            for (int i = 0; i < integer(properties, "effects"); i++) events.add(properties.getProperty("effect." + i));
            return events;
        } catch (IOException failure) { throw new IllegalStateException(failure); }
    }
    private static void effect(Properties p, String value) {
        int size = integer(p, "effects");
        p.setProperty("effect." + size, value);
        p.setProperty("effects", Integer.toString(size + 1));
    }
    private static Properties read(Path file) throws IOException {
        Properties properties = new Properties();
        if (Files.exists(file)) try (InputStream input = Files.newInputStream(file)) { properties.load(input); }
        return properties;
    }
    private static void write(Path file, Properties properties) throws IOException {
        Files.createDirectories(file.getParent());
        Path temporary = file.resolveSibling(file.getFileName() + ".tmp");
        try (OutputStream out = Files.newOutputStream(temporary)) { properties.store(out, "test snapshot"); }
        Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
    }
    private static int integer(Properties p, String key) { return Integer.parseInt(p.getProperty(key, "0")); }
    private static boolean bool(Properties p, String key) { return Boolean.parseBoolean(p.getProperty(key)); }
    private static SharedVoteUserSnapshot snapshot(Properties p, String prefix) {
        return new SharedVoteUserSnapshot(integer(p, prefix + "all"), integer(p, prefix + "month"), integer(p, prefix + "week"),
                integer(p, prefix + "day"), integer(p, prefix + "points"));
    }
    private static void putSnapshot(Properties p, String prefix, SharedVoteUserSnapshot s) {
        p.setProperty(prefix + "all", Integer.toString(s.allTimeTotal()));
        p.setProperty(prefix + "month", Integer.toString(s.monthTotal()));
        p.setProperty(prefix + "week", Integer.toString(s.weeklyTotal()));
        p.setProperty(prefix + "day", Integer.toString(s.dailyTotal()));
        p.setProperty(prefix + "points", Integer.toString(s.points()));
    }
    private static void putReceipt(Properties p, SharedVoteReceipt r) {
        String k = "receipt." + r.input().voteId() + ".";
        p.setProperty(k + "name", r.input().playerName());
        p.setProperty(k + "site", r.input().serviceSite());
        p.setProperty(k + "time", Long.toString(r.input().voteTime()));
        p.setProperty(k + "real", Boolean.toString(r.input().realVote()));
        p.setProperty(k + "add", Boolean.toString(r.input().addTotals()));
        p.setProperty(k + "proxy", Boolean.toString(r.input().proxyVote()));
        p.setProperty(k + "wasOnline", Boolean.toString(r.input().wasOnline()));
        p.setProperty(k + "uuid", r.identity().uuid().toString());
        p.setProperty(k + "resolvedName", r.identity().playerName());
        p.setProperty(k + "online", Boolean.toString(r.identity().online()));
        p.setProperty(k + "count", Boolean.toString(r.mutation().countTotals()));
        p.setProperty(k + "award", Boolean.toString(r.mutation().awardConfiguredPoints()));
        p.setProperty(k + "execute", Boolean.toString(r.executeRewardsNow()));
        p.setProperty(k + "done", r.pending() ? "" : r.completedDisposition().name());
        putSnapshot(p, k + "state.", r.persistedState());
    }
    private static SharedVoteReceipt receipt(Properties p, UUID id) {
        String k = "receipt." + id + ".";
        if (!p.containsKey(k + "done")) return null;
        SharedVoteInput input = new SharedVoteInput(id, p.getProperty(k + "name"), p.getProperty(k + "site"),
                Long.parseLong(p.getProperty(k + "time")), bool(p, k + "real"), bool(p, k + "add"), bool(p, k + "proxy"), bool(p, k + "wasOnline"));
        SharedVoteIdentity identity = new SharedVoteIdentity(UUID.fromString(p.getProperty(k + "uuid")),
                p.getProperty(k + "resolvedName"), bool(p, k + "online"));
        SharedVoteMutation mutation = new SharedVoteMutation(id, input.serviceSite(), input.voteTime(), bool(p, k + "count"), bool(p, k + "award"));
        String done = p.getProperty(k + "done");
        return new SharedVoteReceipt(input, identity, mutation, snapshot(p, k + "state."), bool(p, k + "execute"),
                done.isEmpty() ? null : RewardDisposition.valueOf(done));
    }
}
