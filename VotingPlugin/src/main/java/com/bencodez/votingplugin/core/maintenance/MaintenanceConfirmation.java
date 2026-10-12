package com.bencodez.votingplugin.core.maintenance;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.LongSupplier;

/** Bounded, one-use acknowledgement of an identical maintenance request. Not a database lock. */
public final class MaintenanceConfirmation {
	public enum Result { REQUESTED, CONFIRMED, FULL }
	private static final int MAX_ACTORS = 128;
	private static final long TTL_NANOS = TimeUnit.SECONDS.toNanos(30);
	private final Map<String, Pending> pending = new HashMap<>();
	private final LongSupplier clock;

	public MaintenanceConfirmation() { this(System::nanoTime); }
	public MaintenanceConfirmation(LongSupplier clock) { this.clock = clock; }

	/** A different operation replaces the actor's previous prompt. Expired prompts cannot confirm. */
	public synchronized Result request(String actor, String operation) {
		long now = clock.getAsLong();
		pending.entrySet().removeIf(entry -> now - entry.getValue().createdAt() >= TTL_NANOS);
		Pending previous = pending.remove(actor);
		if (previous != null && previous.operation().equals(operation)) return Result.CONFIRMED;
		if (pending.size() >= MAX_ACTORS) return Result.FULL;
		pending.put(actor, new Pending(operation, now));
		return Result.REQUESTED;
	}

	public static String prompt(String command) {
		return "No changes made. Before continuing, stop vote/reward ingress and ALL backend servers sharing this database "
				+ "from writing, and verify any external reward effects. Repeat " + command
				+ " within 30 seconds to confirm. This acknowledges your precautions; it does not lock other servers.";
	}
	private record Pending(String operation, long createdAt) { }
}
