package com.bencodez.votingplugin.proxy;

/** Result of atomically admitting a live proxy vote against the current runtime. */
public enum IncomingVoteRuntimeResult {
	/** The current runtime accepted the vote-processing call. */
	PROCESSED,
	/** A temporary runtime replacement is active; retry with the same vote ID. */
	RETRY_AFTER_RELOAD,
	/** No usable runtime exists and the vote must not enter it. */
	RUNTIME_UNAVAILABLE
}
