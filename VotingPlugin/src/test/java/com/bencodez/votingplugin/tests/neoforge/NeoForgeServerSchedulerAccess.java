package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeServerScheduler.*;
import java.util.concurrent.CompletableFuture;

/** Typed test-only access to package-private NeoForgeServerScheduler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeServerSchedulerAccess {
    private NeoForgeServerSchedulerAccess() { }
    public static CompletableFuture<Void> executeAsync(NeoForgeServerScheduler target, Runnable task) { return (CompletableFuture<Void>) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeServerScheduler", target, "executeAsync", new String[]{"Runnable"}, new Object[]{task}); }
}
