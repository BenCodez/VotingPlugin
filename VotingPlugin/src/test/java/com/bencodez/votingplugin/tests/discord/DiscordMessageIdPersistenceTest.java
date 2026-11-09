package com.bencodez.votingplugin.tests.discord;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Files;
import java.nio.file.Path;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.topvoter.TopVoter;

class DiscordMessageIdPersistenceTest {
    @TempDir Path directory;

    private com.bencodez.advancedcore.data.ServerData writer(Path file) throws Exception {
        var writer = mock(com.bencodez.advancedcore.data.ServerData.class, CALLS_REAL_METHODS);
        var owner = com.bencodez.simpleapi.file.YMLFile.class;
        var data = owner.getDeclaredField("data"); data.setAccessible(true);
        data.set(writer, new YamlConfiguration());
        var path = owner.getDeclaredField("dFile"); path.setAccessible(true); path.set(writer, file.toFile());
        return writer;
    }

    @Test void successfulWritesAndClearsAreConfirmedInTheActualNamespacedFile() throws Exception {
        Path path = directory.resolve("state.yml");
        var plugin = mock(VotingPluginMain.class); var file = writer(path);
        when(plugin.getServerDataFile()).thenReturn(file); var data = new ServerData(plugin);
        assertTrue(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 12L));
        var saved = new YamlConfiguration(); saved.load(path.toFile());
        assertEquals(12L, saved.getLong("VotingPlugin.DiscordSRV.TopVoterMessageId.Monthly"));
        assertTrue(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 0L));
        saved.load(path.toFile()); assertEquals(0L, saved.getLong("VotingPlugin.DiscordSRV.TopVoterMessageId.Monthly"));
    }

    @Test void actualLegacyWriterSwallowsIOExceptionButConfirmationRemainsFalse() throws Exception {
        var plugin = mock(VotingPluginMain.class); var file = writer(directory);
        when(plugin.getServerDataFile()).thenReturn(file); var data = new ServerData(plugin);
        assertFalse(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 12L));
        assertEquals(12L, data.getTopVoterMessageId(TopVoter.Monthly));
        assertTrue(Files.isDirectory(directory));
    }

    @Test void customOrQueuedSaveDoesNotBecomeConfirmationFromMemoryAlone() throws Exception {
        Path path = directory.resolve("state.yml");
        var plugin = mock(VotingPluginMain.class); var file = writer(path);
        when(plugin.getServerDataFile()).thenReturn(file); var data = new ServerData(plugin);
        assertTrue(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 9L));
        doNothing().when(file).saveData();
        assertFalse(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 12L));
        assertEquals(12L, data.getTopVoterMessageId(TopVoter.Monthly));
        var saved = new YamlConfiguration(); saved.load(path.toFile());
        assertEquals(9L, saved.getLong("VotingPlugin.DiscordSRV.TopVoterMessageId.Monthly"));
    }

    @Test void corruptSavedContentIsNotAConfirmedId() throws Exception {
        Path path = directory.resolve("state.yml");
        var plugin = mock(VotingPluginMain.class); var file = writer(path);
        when(plugin.getServerDataFile()).thenReturn(file); var data = new ServerData(plugin);
        doAnswer(call -> { Files.writeString(path, "invalid: [\n"); return null; }).when(file).saveData();
        assertFalse(data.setTopVoterMessageIdConfirmed(TopVoter.Monthly, 12L));
    }
}
