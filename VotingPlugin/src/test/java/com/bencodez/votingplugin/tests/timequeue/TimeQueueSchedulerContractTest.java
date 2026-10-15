package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.folialib.FoliaLib;
import com.bencodez.simpleapi.folialib.impl.ServerImplementation;
import com.bencodez.simpleapi.folialib.wrapper.task.WrappedTask;

class TimeQueueSchedulerContractTest {
    @Test void installedSimpleApiForwardsAsyncDelayAsSecondsToThePlatformImplementation() {
        BukkitScheduler scheduler = mock(BukkitScheduler.class, CALLS_REAL_METHODS);
        FoliaLib bridge = mock(FoliaLib.class);
        ServerImplementation platform = mock(ServerImplementation.class);
        doReturn(bridge).when(scheduler).getFoliaLib();
        when(bridge.getImpl()).thenReturn(platform);
        AtomicBoolean ran = new AtomicBoolean();
        scheduler.runTaskLaterAsynchronously(mock(org.bukkit.plugin.Plugin.class), () -> ran.set(true), 7L);
        @SuppressWarnings("unchecked")
        var callback = (org.mockito.ArgumentCaptor<Consumer<WrappedTask>>) (Object)
                org.mockito.ArgumentCaptor.forClass(Consumer.class);
        verify(platform).runLaterAsync(callback.capture(), eq(7L), eq(TimeUnit.SECONDS));
        assertFalse(ran.get());
        callback.getValue().accept(mock(WrappedTask.class));
        assertTrue(ran.get());
    }
}
