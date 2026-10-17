package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import com.bencodez.votingplugin.user.SharedMysqlCacheReconciler.*;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.data.DataValue;

/** Typed test-only access to package-private SharedMysqlCacheReconciler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedMysqlCacheReconcilerAccess {
    private SharedMysqlCacheReconcilerAccess() { }
    public static void recordOptimisticPoint(UserDataCache cache, String path, DataValue prediction) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlCacheReconciler", null, "recordOptimisticPoint", new String[]{"UserDataCache", "String", "DataValue"}, new Object[]{cache, path, prediction}); }
}
