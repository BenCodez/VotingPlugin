package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.control.BackendControlResultStore.StoredResult;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

class BackendControlConnectorProtocolTest {
	@TempDir Path directory;
	@Test void onlyTheLeaseExpiryConflictRequestsAResultReclaim() {
		assertTrue(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(409,
				"{\"error\":{\"code\":\"TASK_LEASE_EXPIRED\"}}")));
		assertFalse(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(409,
				"{\"error\":{\"code\":\"SESSION_MISMATCH\"}}")));
		assertFalse(BackendControlConnector.taskLeaseExpired(new BackendControlConnector.Response(500, "not-json")));
		assertTrue(BackendControlConnector.operationNotFound(new BackendControlConnector.Response(404,
				"{\"error\":{\"code\":\"OPERATION_NOT_FOUND\"}}")));
		assertFalse(BackendControlConnector.operationNotFound(new BackendControlConnector.Response(404,
				"{\"error\":{\"code\":\"NODE_NOT_FOUND\"}}")));
	}

	@Test void abandonedBackendIntentBecomesATerminalRecoveryResult() {
		JsonObject anticipated = new JsonObject();
		anticipated.addProperty("attemptId", "00000000-0000-0000-0000-000000000199");

		StoredResult recovered = BackendControlConnector.abortedIntent(
				new StoredResult(anticipated, true, false, false));

		assertTrue(recovered.committed());
		assertFalse(recovered.claimRequired());
		assertFalse(recovered.restartConnector());
		assertTrue("RECOVERY_ABORTED".equals(recovered.result().get("code").getAsString()));
	}

	@Test void installedFileIntentRecoveryRebuildsMaskedContent() throws Exception {
		Files.writeString(directory.resolve("Config.yml"), "Database:\n  Password: keep-me\nDebug: true\n");
		BackendConfigurationService configurations = new BackendConfigurationService(directory, () -> { });
		BackendConfigurationService.Document installed = configurations.read("Config.yml");
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", "Config.yml");
		JsonObject intent = new JsonObject();
		intent.addProperty("revision", installed.revision());
		intent.add("configuration", configuration);
		StoredResult recovered = BackendControlConnector.committedInstalledForAttempt(configurations,
				new StoredResult(intent, false, false, false), "00000000-0000-0000-0000-000000000198");

		assertTrue(recovered.committed());
		assertEquals("00000000-0000-0000-0000-000000000198", recovered.result().get("attemptId").getAsString());
		String content = recovered.result().getAsJsonObject("configuration").get("content").getAsString();
		assertFalse(content.contains("keep-me"));
		assertTrue(content.contains(BackendConfigurationService.REDACTED));
	}

	@Test void changedMalformedFilesAbortPendingRecoveryBeforeYamlParsing() throws Exception {
		BackendConfigurationService configurations = new BackendConfigurationService(directory, () -> { });
		for (String fileName : List.of("Config.yml", "Rewards/Daily.yml")) {
			Path file = directory.resolve(fileName);
			Files.createDirectories(file.getParent());
			Files.writeString(file, "Money: 1\n");
			String revision = configurations.read(fileName).revision();
			JsonObject configuration = new JsonObject();
			configuration.addProperty("domain", "file");
			configuration.addProperty("fileName", fileName);
			JsonObject intent = new JsonObject();
			intent.addProperty("attemptId", "00000000-0000-0000-0000-000000000196");
			intent.addProperty("revision", revision);
			intent.add("configuration", configuration);
			StoredResult pending = new StoredResult(intent, false, false, false);
			Files.writeString(file, "Money: [\n");
			assertThrows(IOException.class, () -> configurations.read(fileName));
			assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
			assertTrue(BackendControlConnector.abortedIntent(pending).committed());
			Files.writeString(file, "Money: 1\n");
			assertEquals(revision, BackendControlConnector.committedInstalledForAttempt(configurations,
					pending, "attempt").result().get("revision").getAsString());
		}
	}

	@Test void missingOrUnsafeNamedRewardIntentTerminatesWithoutMaskingOtherIoFailure() throws Exception {
		Path rewards = Files.createDirectory(directory.resolve("Rewards"));
		Path daily = rewards.resolve("Daily.yml");
		Files.writeString(daily, "Money: 1\n");
		BackendConfigurationService configurations = new BackendConfigurationService(directory, () -> { });
		String revision = configurations.read("Rewards/Daily.yml").revision();
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", "Rewards/Daily.yml");
		JsonObject intent = new JsonObject();
		intent.addProperty("attemptId", "00000000-0000-0000-0000-000000000197");
		intent.addProperty("revision", revision);
		intent.add("configuration", configuration);
		StoredResult pending = new StoredResult(intent, false, false, false);
		Files.delete(daily);
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		assertTrue(BackendControlConnector.abortedIntent(pending).committed());
		Files.delete(rewards);
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		Files.createDirectory(rewards);
		Files.createSymbolicLink(daily, directory.resolve("not-a-reward.yml"));
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		Files.delete(daily);
		Files.createDirectory(daily);
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		Files.delete(daily);
		Files.writeString(daily, "x".repeat(BackendConfigurationService.MAX_CONTENT_BYTES + 1));
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		Files.write(daily, new byte[] {(byte) 0xC3, (byte) 0x28});
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		Files.writeString(daily, "Money: 1\n");
		assertEquals(revision, BackendControlConnector.committedInstalledForAttempt(configurations,
				pending, "attempt").result().get("revision").getAsString());
		assertFalse(BackendConfigurationService.namedRewardCannotBeConfirmed(new IOException("transient read failure")));
	}

	@Test void fifoNamedRewardIntentTerminatesWithoutOpeningItsBody() throws Exception {
		Assumptions.assumeTrue(System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("linux"));
		Assumptions.assumeTrue(Files.isExecutable(Path.of("/usr/bin/mkfifo")));
		Path rewards = Files.createDirectory(directory.resolve("Rewards"));
		Path daily = rewards.resolve("Daily.yml");
		Files.writeString(daily, "Money: 1\n");
		BackendConfigurationService configurations = new BackendConfigurationService(directory, () -> { });
		String revision = configurations.read("Rewards/Daily.yml").revision();
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", "Rewards/Daily.yml");
		JsonObject intent = new JsonObject();
		intent.addProperty("revision", revision);
		intent.add("configuration", configuration);
		Files.delete(daily);
		Process create = new ProcessBuilder("/usr/bin/mkfifo", daily.toString()).start();
		assertTrue(create.waitFor(5, TimeUnit.SECONDS));
		assertEquals(0, create.exitValue());
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations,
				new StoredResult(intent, false, false, false), "attempt"));
	}

	@Test void namedRewardRecoveryIgnoresUnrelatedInvalidInventoryEntries() throws Exception {
		Path rewards = Files.createDirectory(directory.resolve("Rewards"));
		Files.writeString(rewards.resolve("Daily.yml"), "Money: 1\n");
		BackendConfigurationService configurations = new BackendConfigurationService(directory, () -> { });
		String revision = configurations.read("Rewards/Daily.yml").revision();
		JsonObject configuration = new JsonObject();
		configuration.addProperty("domain", "file");
		configuration.addProperty("fileName", "Rewards/Daily.yml");
		JsonObject intent = new JsonObject();
		intent.addProperty("attemptId", "00000000-0000-0000-0000-000000000198");
		intent.addProperty("revision", revision);
		intent.add("configuration", configuration);
		StoredResult pending = new StoredResult(intent, false, false, false);

		Path unsafe = rewards.resolve("Unrelated.yml");
		Files.createSymbolicLink(unsafe, directory.resolve("outside.yml"));
		assertThrows(IOException.class, () -> configurations.read("Rewards/Daily.yml"));
		assertEquals(revision, BackendControlConnector.committedInstalledForAttempt(configurations,
				pending, "attempt").result().get("revision").getAsString());
		Files.delete(unsafe);

		Path ambiguous = rewards.resolve("daily.yml");
		Files.writeString(ambiguous, "Money: 2\n");
		assertThrows(IOException.class, () -> configurations.read("Rewards/Daily.yml"));
		assertNull(BackendControlConnector.committedInstalledForAttempt(configurations, pending, "attempt"));
		assertTrue(BackendControlConnector.abortedIntent(pending).committed());
		Files.delete(ambiguous);

		Path oversized = rewards.resolve("Oversized.yml");
		Files.writeString(oversized, "x".repeat(BackendConfigurationService.MAX_CONTENT_BYTES + 1));
		assertThrows(IOException.class, () -> configurations.read("Rewards/Daily.yml"));
		assertEquals(revision, BackendControlConnector.committedInstalledForAttempt(configurations,
				pending, "attempt").result().get("revision").getAsString());
		Files.delete(oversized);

		for (int index = 0; index < BackendConfigurationService.MAX_REWARD_FILES; index++) {
			Files.writeString(rewards.resolve("Extra" + index + ".yml"), "Money: 0\n");
		}
		assertThrows(IOException.class, () -> configurations.read("Rewards/Daily.yml"));
		assertEquals(revision, BackendControlConnector.committedInstalledForAttempt(configurations,
				pending, "attempt").result().get("revision").getAsString());
	}

	@Test void registrationRequiresFileControlButAllowsQuickSetupToRemainOptional() {
		assertThrows(RuntimeException.class, () -> BackendControlConnector.requireFileCapability(false));
		assertDoesNotThrow(() -> BackendControlConnector.requireFileCapability(true));
	}

	@Test void registrationAdvertisesCommentPreservingFilesAsAnOptionalCapability() {
		JsonObject registration = new JsonObject();
		BackendControlConnector.addCapabilities(registration, true);

		JsonArray advertised = registration.getAsJsonArray("capabilities");
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.file-comments.v1".equals(value.getAsString())));
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.vote-sites-sync.v1".equals(value.getAsString())));
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.quick-setup.v2".equals(value.getAsString())));
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "config.reward-files.v1".equals(value.getAsString())));
		assertTrue(advertised.asList().stream()
				.anyMatch(value -> "data.inspect.v1".equals(value.getAsString())));
		JsonArray required = registration.getAsJsonArray("requiredCapabilities");
		assertTrue(required.asList().stream()
				.anyMatch(value -> "config.files.v1".equals(value.getAsString())));
		assertFalse(required.asList().stream()
				.anyMatch(value -> "config.file-comments.v1".equals(value.getAsString())));
		assertFalse(required.asList().stream()
				.anyMatch(value -> "data.inspect.v1".equals(value.getAsString())));
	}

	@Test void unsupportedFilesystemDoesNotAdvertiseNamedRewards() {
		JsonObject registration = new JsonObject();
		BackendControlConnector.addCapabilities(registration, false);
		assertFalse(registration.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "config.reward-files.v1".equals(value.getAsString())));
		assertTrue(registration.getAsJsonArray("capabilities").asList().stream()
				.anyMatch(value -> "config.files.v1".equals(value.getAsString())));
	}

	@Test void heartbeatRetainsOmittedCapabilitiesAndHonorsExplicitReplacement() {
		JsonObject omitted = new JsonObject();
		assertTrue(BackendControlConnector.negotiatedCapability(omitted, "config.files.v1", true));

		JsonObject explicit = new JsonObject();
		JsonArray accepted = new JsonArray();
		accepted.add("config.quick-setup.v1");
		explicit.add("acceptedCapabilities", accepted);
		assertFalse(BackendControlConnector.negotiatedCapability(explicit, "config.files.v1", true));
		assertTrue(BackendControlConnector.negotiatedCapability(explicit, "config.quick-setup.v1", false));
	}

	@Test void repeatedInspectionFailuresUseBoundedExponentialBackoff() {
		assertEquals(0, BackendControlConnector.inspectionRetryDelayMillis(0));
		assertEquals(1000, BackendControlConnector.inspectionRetryDelayMillis(1));
		assertEquals(2000, BackendControlConnector.inspectionRetryDelayMillis(2));
		assertEquals(256000, BackendControlConnector.inspectionRetryDelayMillis(9));
		assertEquals(300000, BackendControlConnector.inspectionRetryDelayMillis(10));
		assertEquals(300000, BackendControlConnector.inspectionRetryDelayMillis(30));
	}

	@Test void voteSitesSyncRequiresBothNegotiatedCapabilities() {
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", true, false, false));
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", false, false, true));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("sync-vote-sites", true, false, true));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("common-settings", true, false, false));
	}

	@Test void votePartyRequiresItsVersionedCapability() {
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", true, false, false));
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", false, true, false));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", true, true, false));
	}

	@Test void legacyVotePartyOptionsUseV1ButEnabledRequiresV2() {
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", true, false, false,
				Map.of("threshold", "10")));
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", true, false, false,
				Map.of("enabled", "true")));
		assertFalse(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", false, true, false,
				Map.of("enabled", "true")));
		assertTrue(BackendControlConnector.quickSetupCapabilityAccepted("vote-party", true, true, false,
				Map.of("enabled", "true")));
		Map<String, String> state = Map.of("enabled", "false", "votesRequired", "20");
		assertEquals(Map.of("votesRequired", "20"),
				BackendControlConnector.resultQuickReadOptions("vote-party", state, Map.of()));
		assertEquals(state, BackendControlConnector.resultQuickReadOptions("vote-party", state,
				Map.of("enabled", "false")));
	}

	@Test void rewardBuilderResultsKeepOnlyTheSafeRecoveryTarget() {
		String proposal = "{\"scope\":\"site\",\"site\":\"PMC\",\"commands\":[\"secret command\"]}";
		Map<String, String> result = BackendControlConnector.resultQuickOptions("reward-builder",
				Map.of("proposal", proposal));

		assertEquals(Map.of("targetFile", "VoteSites.yml"), result);
		assertFalse(result.toString().contains("secret command"));
	}

	@Test void configurationFailureMessagesNeverExposeExceptionDetails() {
		IllegalStateException failure = new IllegalStateException(
				"/srv/private/VoteSites.yml jdbc:mysql://database.internal user=secret");

		assertEquals("Configuration read failed; see the backend log",
				BackendControlConnector.operationFailureMessage("READ", failure));
		assertEquals("Configuration preview failed; see the backend log",
				BackendControlConnector.operationFailureMessage("PREVIEW", failure));
		assertEquals("Configuration apply failed; see the backend log",
				BackendControlConnector.operationFailureMessage("APPLY", failure));
		assertEquals("Configuration reload failed; see the backend log",
				BackendControlConnector.reloadFailureMessage(failure));
		assertFalse(BackendControlConnector.operationFailureMessage("READ", failure).contains("/srv"));
		assertFalse(BackendControlConnector.reloadFailureMessage(failure).contains("secret"));
	}

	@Test void unavailableConfigurationReadsPreserveV1CodeAndUseASafeReason() {
		// Existing config.files.v1 peers retain the negotiated code while the
		// message gives a useful reason without exposing sensitive paths.
		assertEquals("READ_FAILED", BackendControlConnector.operationFailureCode("READ",
				new IOException("/srv/private/Config.yml")));
		assertEquals("Configuration file is unavailable or unreadable",
				BackendControlConnector.operationFailureMessage("READ", new IOException("/srv/private/Config.yml")));
	}

	@Test void namedRewardInventoryRequiresItsExplicitCapability() {
		assertTrue(ControlInspectionService.rewardFileInventoryQuery(JsonParser.parseString(
				"{\"kind\":\"reward-file-inventory\"}").getAsJsonObject()));
		assertFalse(ControlInspectionService.rewardFileInventoryQuery(JsonParser.parseString(
				"{\"kind\":\"overview\"}").getAsJsonObject()));
		BackendControlConnector connector = org.mockito.Mockito.mock(BackendControlConnector.class,
				org.mockito.Mockito.CALLS_REAL_METHODS);
		assertEquals("UNAVAILABLE", connector.executeInspection(JsonParser.parseString(
				"{\"kind\":\"reward-file-inventory\"}").getAsJsonObject()).code());
	}

	@Test void unexpectedInspectionFailureMessagesNeverExposeTheCause() {
		String message = BackendControlConnector.inspectionFailureMessage(new IllegalStateException(
				"jdbc:mysql://database.internal/votes user=secret path=/srv/private"));

		assertEquals("Inspection failed; see the backend log", message);
		assertFalse(message.contains("jdbc"));
		assertFalse(message.contains("secret"));
		assertFalse(message.contains("/srv"));
	}

	@Test void failureMessagesAreSingleLineAndBoundedBeforeSubmission() {
		String message = BackendControlConnector.boundedResultMessage(
				"unsupported field " + "x".repeat(1000) + "\r\nnext line");

		assertTrue(message.length() <= 240);
		assertFalse(message.contains("\r"));
		assertFalse(message.contains("\n"));
		assertTrue(message.endsWith("..."));

		var changes = BackendControlConnector.boundedResultChanges(java.util.stream.IntStream.range(0, 25)
				.mapToObj(index -> "change-" + index + "-" + "y".repeat(1000)).toList());
		assertEquals(20, changes.size());
		assertTrue(changes.stream().allMatch(change -> change.length() <= 240));
		assertEquals("additional changes omitted", changes.get(19));
	}

	@Test void operationFailureCodesMatchTheRequestedAction() {
		assertEquals("READ_FAILED", BackendControlConnector.operationFailureCode("READ"));
		assertEquals("PREVIEW_FAILED", BackendControlConnector.operationFailureCode("PREVIEW"));
		assertEquals("APPLY_FAILED", BackendControlConnector.operationFailureCode("APPLY"));
	}

	@Test void shutdownWaitsForTheClaimedBackendOperation() throws Exception {
		var executor = Executors.newSingleThreadScheduledExecutor();
		CompletableFuture<Void> operation = new CompletableFuture<>();
		CompletableFuture<Void> closing = CompletableFuture.runAsync(
				() -> BackendControlConnector.awaitShutdown(executor, operation));

		assertThrows(java.util.concurrent.TimeoutException.class, () -> closing.get(100, TimeUnit.MILLISECONDS));
		operation.complete(null);
		closing.get(2, TimeUnit.SECONDS);
		assertTrue(executor.isTerminated());
	}

	@Test void inspectionShutdownInterruptsItsIndependentWorker() throws Exception {
		var executor = Executors.newSingleThreadScheduledExecutor();
		CountDownLatch started = new CountDownLatch(1);
		AtomicBoolean interrupted = new AtomicBoolean();
		executor.execute(() -> {
			started.countDown();
			try {
				Thread.sleep(TimeUnit.MINUTES.toMillis(1));
			} catch (InterruptedException expected) {
				interrupted.set(true);
				Thread.currentThread().interrupt();
			}
		});
		assertTrue(started.await(1, TimeUnit.SECONDS));

		assertTrue(BackendControlConnector.awaitInspectionShutdown(executor));
		assertTrue(interrupted.get());
		assertTrue(executor.isTerminated());
	}

	@Test void failedResultAcknowledgementDoesNotTriggerConnectorHandoff() throws Exception {
		AtomicBoolean handedOff = new AtomicBoolean();
		assertThrows(IOException.class, () -> BackendControlConnector.afterResultAcknowledged(
				() -> { throw new IOException("Control result was not acknowledged"); },
				() -> handedOff.set(true)));
		assertFalse(handedOff.get());

		BackendControlConnector.afterResultAcknowledged(() -> { }, () -> handedOff.set(true));
		assertTrue(handedOff.get());
	}
}
