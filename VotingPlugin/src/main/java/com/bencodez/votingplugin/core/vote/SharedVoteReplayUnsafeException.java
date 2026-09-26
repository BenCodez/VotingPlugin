package com.bencodez.votingplugin.core.vote;

/** Signals that a durable delivery may already have executed platform effects. */
public final class SharedVoteReplayUnsafeException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public SharedVoteReplayUnsafeException(String message) {
        super(message);
    }
}
