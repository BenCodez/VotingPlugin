package com.bencodez.votingplugin.core.vote;

/** Signals that durable accounting admission failed before vote side effects began. */
public final class SharedVoteAdmissionException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public SharedVoteAdmissionException(String message) {
        super(message);
    }
}
