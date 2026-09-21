package com.bencodez.votingplugin.proxy;

/** Keeps first initialization distinct from replacement of a loaded proxy runtime. */
public final class ProxyRuntimeReplacementLifecycle {
	private ProxyRuntimeReplacementLifecycle() { }

	/** Returns false for first initialization, which has no old runtime to prepare. */
	public static boolean prepare(VotingPluginProxy previous) {
		if (previous == null) return false;
		previous.prepareForRuntimeReplacement();
		return true;
	}

	/** Completes teardown only for a runtime that actually existed before replacement. */
	public static void complete(VotingPluginProxy previous) {
		if (previous != null) previous.completeRuntimeReplacementShutdown();
	}
}
