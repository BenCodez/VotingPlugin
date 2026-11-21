package com.bencodez.votingplugin.votelog;

import java.util.function.Supplier;

/** Internal bridge for strict Control reads; legacy table callers retain their fallbacks. */
public final class VoteLogReadScope {
    private static final ThreadLocal<Runnable> FAILURE = new ThreadLocal<>();

    private VoteLogReadScope() { }

    /** Runs synchronously on the inspection worker, restoring any enclosing scope on every exit. */
    public static <T> T read(Supplier<T> action, Runnable unavailable) {
        Runnable previous = FAILURE.get();
        FAILURE.set(unavailable);
        try {
            return action.get();
        } finally {
            if (previous == null) FAILURE.remove();
            else FAILURE.set(previous);
        }
    }

    static void queryFailed() {
        Runnable unavailable = FAILURE.get();
        if (unavailable != null) unavailable.run();
    }
}
