package com.bencodez.votingplugin.core.session;

import java.util.concurrent.atomic.AtomicLong;

/** Process-local total ordering shared by guide creation and vote ingress; never a remote timestamp. */
public final class VoteObservationSequence {
    private static final AtomicLong SEQUENCE = new AtomicLong();
    private VoteObservationSequence() { }
    public static long next() { return SEQUENCE.incrementAndGet(); }
}
