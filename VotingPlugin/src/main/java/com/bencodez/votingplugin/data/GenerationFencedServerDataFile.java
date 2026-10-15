package com.bencodez.votingplugin.data;

import java.io.File;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.plugin.java.JavaPlugin;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

/**
 * Routes SDK publications through this generation's ownership lease. The SDK's
 * original file/configuration remains authoritative; no YAML snapshot is copied.
 */
public final class GenerationFencedServerDataFile extends com.bencodez.advancedcore.data.ServerData {
    private final com.bencodez.advancedcore.data.ServerData delegate;
    private final ServerDataPublicationFence fence;

    public GenerationFencedServerDataFile(VotingPluginMain plugin,
            com.bencodez.advancedcore.data.ServerData delegate, ServerDataPublicationFence fence) {
        super(plugin);
        this.delegate = java.util.Objects.requireNonNull(delegate);
        this.fence = java.util.Objects.requireNonNull(fence);
    }

    public boolean wraps(com.bencodez.advancedcore.data.ServerData file) { return delegate == file; }

    public boolean belongsTo(ServerDataPublicationFence owner) { return fence == owner; }

    @Override public FileConfiguration getData() { return delegate.getData(); }
    @Override public File getdFile() { return delegate.getdFile(); }
    @Override public JavaPlugin getPlugin() { return delegate.getPlugin(); }
    @Override public boolean isJustCreated() { return delegate.isJustCreated(); }
    @Override public boolean isFailedToRead() { return delegate.isFailedToRead(); }
    @Override public boolean isIgnoreCase() { return delegate.isIgnoreCase(); }
    @Override public void setIgnoreCase(boolean value) {
        try (var lease = fence.acquire()) { delegate.setIgnoreCase(value); }
    }
    @Override public void createSection(String path) {
        try (var lease = fence.acquire()) { delegate.createSection(path); }
    }
    @Override public void setData(FileConfiguration data) {
        try (var lease = fence.acquire()) { delegate.setData(data); }
    }
    @Override public void setValue(String path, Object value) {
        try (var lease = fence.acquire()) { delegate.setValue(path, value); }
    }
    @Override public void onFileCreation() {
        try (var lease = fence.acquire()) { delegate.onFileCreation(); }
    }

    @Override public synchronized void setup() {
        try (var lease = fence.acquire()) { delegate.setup(); }
    }
    @Override public void loadValues() {
        try (var lease = fence.acquire()) { delegate.loadValues(); }
    }
    @Override public void reloadData() {
        try (var lease = fence.acquire()) { delegate.reloadData(); }
    }
    @Override public void saveData() {
        try (var lease = fence.acquire()) { delegate.saveData(); }
    }
    @Override protected synchronized void saveTimeChangeTransitionData() {
        // Preserve AdvancedCore's checked, atomic transition publication rather
        // than substituting the legacy saveData() IO-error reporting contract.
        try (var lease = fence.acquire()) { super.saveTimeChangeTransitionData(); }
    }
}
