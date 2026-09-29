package com.bencodez.votingplugin.util;

import java.util.concurrent.CompletableFuture;

/**
 * Test statuses for code that intentionally treats Folia's shaded entity-task result
 * as an opaque enum. Keeping tests independent of the relocated FoliaLib enum lets
 * Eclipse resolve the project whether SimpleAPI comes from the workspace or Maven.
 */
public final class EntityTaskResultTestCompat {

	private enum Status {
		SUCCESS,
		ENTITY_RETIRED,
		SCHEDULER_RETIRED
	}

	private EntityTaskResultTestCompat() {
	}

	@SuppressWarnings("rawtypes")
	public static CompletableFuture success() {
		return CompletableFuture.completedFuture(Status.SUCCESS);
	}

	@SuppressWarnings("rawtypes")
	public static CompletableFuture entityRetired() {
		return CompletableFuture.completedFuture(Status.ENTITY_RETIRED);
	}

	@SuppressWarnings("rawtypes")
	public static CompletableFuture schedulerRetired() {
		return CompletableFuture.completedFuture(Status.SCHEDULER_RETIRED);
	}

	@SuppressWarnings("rawtypes")
	public static CompletableFuture pending() {
		return new CompletableFuture();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static void completeSchedulerRetired(CompletableFuture future) {
		future.complete(Status.SCHEDULER_RETIRED);
	}
}
