package com.bencodez.votingplugin.data;

import java.io.File;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.plugin.Plugin;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

/**
 * Routes SDK publications through this generation's ownership lease. The SDK's
 * original file/configuration remains authoritative; no YAML snapshot is copied.
 */
public final class GenerationFencedServerDataFile extends com.bencodez.advancedcore.data.ServerData {
    private final com.bencodez.advancedcore.data.ServerData delegate;
    private final ServerDataPublicationFence fence;
    private final VotingPluginMain ownerPlugin;

    public GenerationFencedServerDataFile(VotingPluginMain plugin,
            com.bencodez.advancedcore.data.ServerData delegate, ServerDataPublicationFence fence) {
        super(plugin);
        this.ownerPlugin = java.util.Objects.requireNonNull(plugin);
        this.delegate = java.util.Objects.requireNonNull(delegate);
        this.fence = java.util.Objects.requireNonNull(fence);
    }

    public boolean wraps(com.bencodez.advancedcore.data.ServerData file) { return delegate == file; }

    public boolean belongsTo(ServerDataPublicationFence owner) { return fence == owner; }

    /** Checkpoint callers use the same monitor as native final publication. */
    @Override public Object publicationMonitor() {
        return java.util.Objects.requireNonNull(delegate.publicationMonitor(), "Server-data publication monitor");
    }

    private void withOwner(Runnable operation) {
        try (var lease = fence.acquire()) {
            synchronized (publicationMonitor()) {
                try { operation.run(); }
                catch (RuntimeException | Error failure) {
                    try { refreshVotePartyDisplay(); }
                    catch (RuntimeException | Error observationFailure) {
                        if (observationFailure != failure) failure.addSuppressed(observationFailure);
                    }
                    throw failure;
                }
                refreshVotePartyDisplay();
            }
        }
    }

    private <T> T withOwnedValue(Supplier<T> operation) {
        try (var lease = fence.acquire()) {
            synchronized (publicationMonitor()) { return operation.get(); }
        }
    }

    @Override public FileConfiguration getData() { return delegate.getData(); }
    @Override public File getdFile() { return delegate.getdFile(); }
    @Override public JavaPlugin getPlugin() { return delegate.getPlugin(); }
    @Override public boolean isJustCreated() { return delegate.isJustCreated(); }
    @Override public boolean isFailedToRead() { return delegate.isFailedToRead(); }
    @Override public boolean isIgnoreCase() { return delegate.isIgnoreCase(); }
    @Override public void setIgnoreCase(boolean value) {
        withOwner(() -> delegate.setIgnoreCase(value));
    }
    @Override public void createSection(String path) {
        mutateOrdinaryData(data -> data.createSection(path));
    }
    @Override public void setData(FileConfiguration data) {
        withOwner(() -> delegate.setData(data));
    }
    @Override public void setValue(String path, Object value) {
        mutateOrdinaryData(data -> data.set(path, value));
    }
    @Override public void onFileCreation() {
        withOwner(delegate::onFileCreation);
    }

    @Override public void setup() {
        withOwner(delegate::setup);
    }
    @Override public void loadValues() {
        withOwner(delegate::loadValues);
    }
    @Override public void reloadData() {
        withOwner(delegate::reloadData);
    }
    private void refreshVotePartyDisplay() {
        var wrapper = ownerPlugin.getServerData();
        if (wrapper != null) {
            synchronized (publicationMonitor()) {
                if (delegate.getData() != null) wrapper.refreshVotePartyDisplay();
            }
        }
    }

    @Override public void saveData() {
        withOwner(() -> {
            refreshVotePartyDisplay();
            delegate.saveData();
        });
    }
    @Override public void saveDataChecked() {
        withOwner(() -> {
            refreshVotePartyDisplay();
            delegate.saveDataChecked();
        });
    }
    @Override protected void saveTimeChangeTransitionData() { saveDataChecked(); }

    @Override public void mutateOrdinaryData(Consumer<FileConfiguration> mutation) {
        // Delegate admission, live mutation and saving to the actual SDK owner.
        // The projection is published inside that owner's callback before I/O.
        withOwner(() -> delegate.mutateOrdinaryData(data -> applyOwnedMutation(mutation, data)));
    }

    private void refreshIfLiveMutation(FileConfiguration data) {
        var wrapper = ownerPlugin.getServerData();
        if (wrapper == null) return;
        synchronized (publicationMonitor()) {
            // Native final publication mutates a staged candidate first. Legacy
            // adapters may instead mutate the live owner before their save.
            if (data == delegate.getData()) wrapper.refreshVotePartyDisplay();
        }
    }

    private void applyOwnedMutation(Consumer<FileConfiguration> mutation, FileConfiguration data) {
        try { mutation.accept(data); }
        catch (RuntimeException | Error failure) {
            try { refreshIfLiveMutation(data); }
            catch (RuntimeException | Error observationFailure) {
                if (observationFailure != failure) failure.addSuppressed(observationFailure);
            }
            throw failure;
        }
        refreshIfLiveMutation(data);
    }

    private void finishFinalPublication(ServerDataPublicationFence.Lease lease,
            CompletableFuture<Void> result, Throwable failure) {
        Throwable outcome = failure;
        try {
            // Partial startup may have no display wrapper. In that case, do not
            // introduce a YAML-observation dependency into final-stage drain.
            refreshVotePartyDisplay();
        } catch (Throwable observationFailure) {
            if (outcome == null) outcome = observationFailure;
            else if (outcome != observationFailure) outcome.addSuppressed(observationFailure);
        }
        // Refresh observes only the actual live owner; it never acknowledges a
        // failed/cancelled publication or releases its retained generation.
        if (outcome != null) { result.completeExceptionally(outcome); return; }
        try { lease.close(); result.complete(null); }
        catch (Throwable releaseFailure) { result.completeExceptionally(releaseFailure); }
    }

    /** Retain this handle only for retries of the same accepted mutation. */
    FinalPublication prepareFinalPublication(Consumer<FileConfiguration> mutation) {
        return new FinalPublication(mutation);
    }

    final class FinalPublication {
        private final Consumer<FileConfiguration> mutation;
        private ServerDataPublicationFence.Lease lease;
        private CompletableFuture<Void> attempt;
        private boolean publicationConfirmed;

        private FinalPublication(Consumer<FileConfiguration> mutation) { this.mutation = mutation; }

        CompletionStage<Void> publish() {
            CompletableFuture<Void> result;
            ServerDataPublicationFence.Lease owner;
            boolean refreshOnly;
            synchronized (this) {
                if (attempt != null && !attempt.isCompletedExceptionally())
                    return attempt.minimalCompletionStage();
                if (lease == null) {
                    try { lease = fence.acquire(); }
                    catch (Throwable failure) { return CompletableFuture.failedFuture(failure); }
                }
                owner = lease;
                result = new CompletableFuture<>();
                attempt = result;
                refreshOnly = publicationConfirmed;
            }
            // No operation lock is held across the actual owner's YAML or I/O.
            if (refreshOnly) finish(owner, result, null);
            else {
                try {
                    java.util.Objects.requireNonNull(mutation, "mutation");
                    java.util.Objects.requireNonNull(delegate.mutateFinalData(data -> applyOwnedMutation(mutation, data)))
                            .whenComplete((ignored, failure) -> finish(owner, result, failure));
                } catch (Throwable failure) { finish(owner, result, failure); }
            }
            return result.minimalCompletionStage();
        }

        private void finish(ServerDataPublicationFence.Lease owner,
                CompletableFuture<Void> result, Throwable failure) {
            if (failure == null) {
                synchronized (this) { publicationConfirmed = true; }
            }
            // A retry after a display-observation failure must not repeat a
            // confirmed mutation. Failed publication instead retains this one
            // lease until this same operation has a confirmed successful retry.
            finishFinalPublication(owner, result, failure);
        }
    }

    @Override public CompletionStage<Void> mutateFinalData(Consumer<FileConfiguration> mutation) {
        // Ordinary one-shot callers retain failed ownership conservatively.
        // A caller with an explicit retry contract retains the operation handle.
        return prepareFinalPublication(mutation).publish();
    }

    @Override public void setData(String path, Object value) { mutateOrdinaryData(data -> data.set(path, value)); }
    @Override public void setIgnoreTime(boolean value) { withOwner(() -> delegate.setIgnoreTime(value)); }
    @Override public void setLastUpdated() { withOwner(delegate::setLastUpdated); }
    @Override public void setPluginVersion(Plugin plugin) { withOwner(() -> delegate.setPluginVersion(plugin)); }
    @Override public void setPrevDay(int value) { withOwner(() -> delegate.setPrevDay(value)); }
    @Override public void setPrevMonth(String value) { withOwner(() -> delegate.setPrevMonth(value)); }
    @Override public void setPrevWeekDay(int value) { withOwner(() -> delegate.setPrevWeekDay(value)); }

    @Override public TimeChangeTransitionState beginTimeChangeTransition(TimeType type, String period, String marker) {
        return withOwnedValue(() -> delegate.beginTimeChangeTransition(type, period, marker));
    }
    @Override public TimeChangeTransitionState getPendingTimeChangeTransition(TimeType type) {
        return withOwnedValue(() -> delegate.getPendingTimeChangeTransition(type));
    }
    @Override public void completeTimeChangeTransition(TimeChangeTransitionState transition) {
        withOwner(() -> delegate.completeTimeChangeTransition(transition));
    }
    @Override public void failTimeChangeTransition(TimeChangeTransitionState transition) {
        withOwner(() -> delegate.failTimeChangeTransition(transition));
    }
    @Override public boolean isPendingTimeChangeTransition(TimeChangeTransitionState transition) {
        return withOwnedValue(() -> delegate.isPendingTimeChangeTransition(transition));
    }
}
