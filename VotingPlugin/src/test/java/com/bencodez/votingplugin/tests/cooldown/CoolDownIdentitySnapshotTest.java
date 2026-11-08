package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.user.VotingPluginUser;

class CoolDownIdentitySnapshotTest {
    private VotingPluginUser user(String cachedName, String explicitName) throws Exception {
        VotingPluginUser user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        AdvancedCorePlugin plugin = mock(AdvancedCorePlugin.class);
        when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
        Field pluginField = AdvancedCoreUser.class.getDeclaredField("plugin");
        pluginField.setAccessible(true);
        pluginField.set(user, plugin);
        Field dataField = AdvancedCoreUser.class.getDeclaredField("data");
        dataField.setAccessible(true);
        UserData data = new UserData(user);
        dataField.set(user, data);
        user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
        HashMap<String, DataValue> cache = new HashMap<>();
        if (cachedName != null) cache.put("PlayerName", new DataValueString(cachedName));
        data.setTempCache(cache);
        if (explicitName != null) user.setPlayerName(explicitName);
        return user;
    }

    private void snapshot(VotingPluginUser user) throws Exception {
        Method capture = CoolDownCheck.class.getDeclaredMethod("capturePlayerName", VotingPluginUser.class);
        capture.setAccessible(true);
        capture.invoke(null, user);
    }

    @Test void delayedRewardRetainsNameAfterTemporaryCacheIsCleared() throws Exception {
        VotingPluginUser user = user("OfflinePlayer", null);
        snapshot(user);
        var admitted = new CountDownLatch(1);
        var resume = new CountDownLatch(1);
        var worker = Executors.newSingleThreadExecutor();
        try {
            var result = worker.submit(() -> {
                admitted.countDown();
                if (!resume.await(5, TimeUnit.SECONDS)) throw new AssertionError("worker not resumed");
                return user.getPlayerName();
            });
            assertTrue(admitted.await(5, TimeUnit.SECONDS));
            user.clearTempCache();
            resume.countDown();
            assertEquals("OfflinePlayer", result.get(5, TimeUnit.SECONDS));
        } finally {
            resume.countDown();
            worker.shutdownNow();
        }
    }

    @Test void missingNameRemainsMissingAndExplicitNameIsPreserved() throws Exception {
        VotingPluginUser missing = user(null, null);
        snapshot(missing);
        missing.clearTempCache();
        assertEquals("", missing.getPlayerName());
        VotingPluginUser online = user("OldName", "OnlineName");
        snapshot(online);
        online.clearTempCache();
        assertEquals("OnlineName", online.getPlayerName());
    }
}
