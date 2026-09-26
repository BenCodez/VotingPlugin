package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.votingplugin.proxy.control.ControlConnector;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;

class VotingPluginProxyLifecycleTest {
	@Test
	void standaloneSocketPathUsesCommunicationEnvelopeEncryption(@TempDir Path dataDirectory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.SOCKETS);
		Path keyFile = dataDirectory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		TransportEnvelopeEncryption encryption = TransportEnvelopeEncryption.load(keyFile,
				TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
		Field encryptionField = VotingPluginProxy.class.getDeclaredField("communicationEncryption");
		encryptionField.setAccessible(true);
		encryptionField.set(proxy, encryption);
		ClientHandler client = mock(ClientHandler.class);
		Field handles = VotingPluginProxy.class.getDeclaredField("clientHandles");
		handles.setAccessible(true);
		handles.set(proxy, new HashMap<>(Map.of("lobby", client)));
		JsonEnvelope original = JsonEnvelope.builder("Vote").put("player", "Alex").build();

		assertTrue(proxy.sendProxyBroadcastEnvelopeNow("lobby", original));

		ArgumentCaptor<JsonEnvelope> sent = ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(client).sendEnvelope(sent.capture());
		TransportEnvelopeEncryption.Decryption decrypted = encryption.decrypt(sent.getValue());
		assertTrue(decrypted.accepted());
		assertEquals(original.getSubChannel(), decrypted.envelope().getSubChannel());
		assertEquals(original.getFields(), decrypted.envelope().getFields());
	}

	@Test
	void unsignedRedisPresenceCannotReachProxyPresenceHandling(@TempDir Path dataDirectory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		Path keyFile = dataDirectory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		Field authentication = VotingPluginProxy.class.getDeclaredField("sharedTransportAuthenticator");
		authentication.setAccessible(true);
		authentication.set(proxy, SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED));
		@SuppressWarnings("unchecked")
		Consumer<JsonEnvelope> accepted = mock(Consumer.class);

		((VotingPluginProxy) proxy).acceptSharedTransportEnvelope(VotingPluginWire.login("Alex",
				"00000000-0000-0000-0000-000000000001", "backend-a"), Domain.REDIS_PROXY_BACKEND,
				"VotingPlugin", accepted);

		verifyNoInteractions(accepted);
	}

	@Test
	void softReloadAppliesRequiredSharedTransportAuthentication(@TempDir Path dataDirectory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setDataFolder(dataDirectory.toFile());
		Path keyFile = dataDirectory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		when(proxy.getConfig().getBungeeMethod()).thenReturn("REDIS");
		when(proxy.getConfig().getSharedTransportAuthentication()).thenReturn("REQUIRED");
		Field authentication = VotingPluginProxy.class.getDeclaredField("sharedTransportAuthenticator");
		authentication.setAccessible(true);
		authentication.set(proxy, SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.COMPATIBILITY));

		proxy.reloadFromControl();

		SharedTransportEnvelopeAuthenticator reloaded = (SharedTransportEnvelopeAuthenticator) authentication.get(proxy);
		assertEquals(Mode.REQUIRED, reloaded.mode());
		assertEquals(SharedTransportEnvelopeAuthenticator.Rejection.MISSING,
				reloaded.verify(VotingPluginWire.status("backend-a"), Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
	}

	@Test
	void completionAckTransitionsThroughDurableReceiptRelease(@TempDir Path directory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
		UUID voteId = UUID.randomUUID();
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
		org.junit.jupiter.api.Assertions.assertTrue(outbox.offer("survival", VotingPluginWire.vote(
				"Player", UUID.randomUUID().toString(), "site", 10L, true, true, "", voteId,
				false, false, 1, 1)));
		setField(proxy, "reliableVoteDeliveryOutbox", outbox);
		setField(proxy, "globalMessageProxyHandler", messages);
		@SuppressWarnings("unchecked")
		Set<String> reliable = (Set<String>) field(proxy, "reliableVoteDeliveryServers");
		reliable.add("survival");
		Method completion = VotingPluginProxy.class.getDeclaredMethod(
				"handleVoteDeliveryAcknowledgement", JsonEnvelope.class);
		completion.setAccessible(true);
		completion.invoke(proxy, VotingPluginWire.voteDeliveryAcknowledgement(
				"survival", voteId, VotingPluginWire.SUB_VOTE));

		assertEquals(1, outbox.size());
		org.junit.jupiter.api.Assertions.assertTrue(outbox.snapshot().get(0).awaitingReceiptRelease());
		ArgumentCaptor<JsonEnvelope> release = ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(messages).sendMessage(org.mockito.ArgumentMatchers.eq("survival"),
				org.mockito.ArgumentMatchers.eq(1), release.capture());
		assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE,
				release.getValue().getSubChannel());

		Method released = VotingPluginProxy.class.getDeclaredMethod(
				"handleVoteDeliveryReceiptReleaseAcknowledgement", JsonEnvelope.class);
		released.setAccessible(true);
		released.invoke(proxy, VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(
				"survival", voteId, VotingPluginWire.SUB_VOTE));
		assertEquals(0, outbox.size());
	}

	@Test
	void preservesReceiptReleaseAfterLegacyDowngradeDelivery(@TempDir Path directory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
		org.junit.jupiter.api.Assertions.assertTrue(outbox.offer("survival", vote));
		Field outboxField = VotingPluginProxy.class.getDeclaredField("reliableVoteDeliveryOutbox");
		outboxField.setAccessible(true);
		outboxField.set(proxy, outbox);
		Field messagesField = VotingPluginProxy.class.getDeclaredField("globalMessageProxyHandler");
		messagesField.setAccessible(true);
		messagesField.set(proxy, messages);
		Field legacyServers = VotingPluginProxy.class.getDeclaredField("legacyVoteDeliveryServers");
		legacyServers.setAccessible(true);
		@SuppressWarnings("unchecked")
		Set<String> legacy = (Set<String>) legacyServers.get(proxy);
		Method retry = VotingPluginProxy.class.getDeclaredMethod("retryReliableVoteDeliveries", String.class);
		retry.setAccessible(true);
		retry.invoke(proxy, "survival");
		assertEquals(1, outbox.size());
		org.mockito.Mockito.verifyNoInteractions(messages);

		legacy.add("survival");
		proxy.setPluginMessageDeliveryResult(false);
		retry.invoke(proxy, "survival");
		assertEquals(1, outbox.size());

		proxy.setPluginMessageDeliveryResult(true);
		retry.invoke(proxy, "survival");

		assertEquals(1, outbox.size());
		org.junit.jupiter.api.Assertions.assertTrue(outbox.snapshot().get(0).awaitingReceiptRelease());
	}

	@Test
	void rejectedLegacyVoteRemainsRetryableAfterRestart(@TempDir Path directory) throws Exception {
		Path file = directory.resolve("outbox.dat");
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		proxy.setPluginMessageDeliveryResult(false);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		org.junit.jupiter.api.Assertions.assertTrue(outbox.offer("survival", vote));
		setField(proxy, "reliableVoteDeliveryOutbox", outbox);
		@SuppressWarnings("unchecked")
		Set<String> legacy = (Set<String>) field(proxy, "legacyVoteDeliveryServers");
		legacy.add("survival");
		Method retry = VotingPluginProxy.class.getDeclaredMethod("retryReliableVoteDeliveries", String.class);
		retry.setAccessible(true);

		retry.invoke(proxy, "survival");

		assertEquals(1, proxy.getVoteEnvelopeDeliveryAttempts());
		org.junit.jupiter.api.Assertions.assertTrue(outbox.snapshot().get(0).legacyDeliveryRejected());

		VotingPluginProxyTestImpl restartedProxy = new VotingPluginProxyTestImpl();
		restartedProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		ReliableVoteDeliveryOutbox restartedOutbox = new ReliableVoteDeliveryOutbox(file);
		setField(restartedProxy, "reliableVoteDeliveryOutbox", restartedOutbox);
		@SuppressWarnings("unchecked")
		Set<String> restartedLegacy = (Set<String>) field(restartedProxy, "legacyVoteDeliveryServers");
		restartedLegacy.add("survival");

		retry.invoke(restartedProxy, "survival");

		assertEquals(1, restartedProxy.getVoteEnvelopeDeliveryAttempts());
		org.junit.jupiter.api.Assertions.assertTrue(restartedOutbox.snapshot().get(0).awaitingReceiptRelease());
	}

	@Test
	void acceptedLegacyVoteIsNotResentWhileCompletionPersistenceRetries(@TempDir Path directory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);
		Path file = directory.resolve("outbox.dat");
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(file);
		org.junit.jupiter.api.Assertions.assertTrue(outbox.offer("survival", vote));
		setField(proxy, "reliableVoteDeliveryOutbox", outbox);
		@SuppressWarnings("unchecked")
		Set<String> legacy = (Set<String>) field(proxy, "legacyVoteDeliveryServers");
		legacy.add("survival");
		Method retry = VotingPluginProxy.class.getDeclaredMethod("retryReliableVoteDeliveries", String.class);
		retry.setAccessible(true);
		AtomicReference<byte[]> fencedJournal = new AtomicReference<>();
		proxy.setAcceptedVoteEnvelopeHook(() -> {
			try {
				fencedJournal.set(Files.readAllBytes(file));
				Files.delete(file);
				Files.createDirectory(file);
			} catch (java.io.IOException failure) {
				throw new AssertionError(failure);
			}
		});

		retry.invoke(proxy, "survival");
		retry.invoke(proxy, "survival");

		assertEquals(1, proxy.getVoteEnvelopeDeliveryAttempts());
		org.junit.jupiter.api.Assertions.assertFalse(outbox.snapshot().get(0).awaitingReceiptRelease());
		Files.delete(file);
		Files.write(file, fencedJournal.get());

		VotingPluginProxyTestImpl restartedProxy = new VotingPluginProxyTestImpl();
		restartedProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		ReliableVoteDeliveryOutbox restartedOutbox = new ReliableVoteDeliveryOutbox(file);
		setField(restartedProxy, "reliableVoteDeliveryOutbox", restartedOutbox);
		@SuppressWarnings("unchecked")
		Set<String> restartedLegacy = (Set<String>) field(restartedProxy, "legacyVoteDeliveryServers");
		restartedLegacy.add("survival");
		retry.invoke(restartedProxy, "survival");
		assertEquals(0, restartedProxy.getVoteEnvelopeDeliveryAttempts());
		org.junit.jupiter.api.Assertions.assertTrue(restartedOutbox.snapshot().get(0).awaitingReceiptRelease());
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = VotingPluginProxy.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static Object field(Object target, String name) throws Exception {
		Field field = VotingPluginProxy.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}

	@Test
	void probesPluginMessagingBackendsForDeliveryCapability() throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		proxy.setAvailableServers("survival");
		GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
		Field messagesField = VotingPluginProxy.class.getDeclaredField("globalMessageProxyHandler");
		messagesField.setAccessible(true);
		messagesField.set(proxy, messages);
		Method probe = VotingPluginProxy.class.getDeclaredMethod("probeReliableVoteDeliveryCapabilities");
		probe.setAccessible(true);

		probe.invoke(proxy);

		org.mockito.ArgumentCaptor<JsonEnvelope> envelope = org.mockito.ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(messages).sendMessage(org.mockito.ArgumentMatchers.eq("survival"),
				org.mockito.ArgumentMatchers.eq(1), envelope.capture());
		assertEquals(VotingPluginWire.SUB_STATUS, envelope.getValue().getSubChannel());
		org.junit.jupiter.api.Assertions.assertFalse(envelope.getValue().getFields()
				.get(VotingPluginWire.K_REQUEST_ID).isBlank());
	}

	@Test
	void capableHttpVoteRemainsInOutboxWhenImmediateTransportSendIsRejected(@TempDir Path directory)
			throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.HTTP);
		proxy.setVoteEnvelopeDeliveryResult(false);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
		Field outboxField = VotingPluginProxy.class.getDeclaredField("reliableVoteDeliveryOutbox");
		outboxField.setAccessible(true);
		outboxField.set(proxy, outbox);
		Field reliableServers = VotingPluginProxy.class.getDeclaredField("reliableVoteDeliveryServers");
		reliableServers.setAccessible(true);
		@SuppressWarnings("unchecked")
		Set<String> reliable = (Set<String>) reliableServers.get(proxy);
		reliable.add("survival");
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", UUID.randomUUID(), false, false, 1, 1);

		org.junit.jupiter.api.Assertions.assertTrue(proxy.sendVoteEnvelopeAcceptedForTest("survival", 1, vote));

		assertEquals(1, outbox.size());
		org.junit.jupiter.api.Assertions.assertTrue(
				VotingPluginWire.requestsVoteDeliveryAcknowledgement(proxy.getLastVoteEnvelope()));
	}

	@Test
	void unknownBackendCapabilityJournalsVoteBeforeReportingAcceptance(@TempDir Path directory)
			throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		GlobalMessageProxyHandler messages = mock(GlobalMessageProxyHandler.class);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
		setField(proxy, "globalMessageProxyHandler", messages);
		setField(proxy, "reliableVoteDeliveryOutbox", outbox);
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", UUID.randomUUID(), false, false, 1, 1);

		org.junit.jupiter.api.Assertions.assertTrue(proxy.sendVoteEnvelopeAcceptedForTest("survival", 1, vote));

		assertEquals(1, outbox.size());
		org.mockito.Mockito.verifyNoInteractions(messages);
	}

	@Test
	void reliableHttpRetriesReuseOneStableIdPerDeliveryPhase(@TempDir Path directory) throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.HTTP);
		proxy.setVoteEnvelopeDeliveryResult(true);
		ReliableVoteDeliveryOutbox outbox = new ReliableVoteDeliveryOutbox(directory.resolve("outbox.dat"));
		setField(proxy, "reliableVoteDeliveryOutbox", outbox);
		@SuppressWarnings("unchecked")
		Set<String> reliable = (Set<String>) field(proxy, "reliableVoteDeliveryServers");
		reliable.add("survival");
		UUID voteId = UUID.randomUUID();
		JsonEnvelope vote = VotingPluginWire.vote("Player", UUID.randomUUID().toString(), "site", 10L,
				true, true, "", voteId, false, false, 1, 1);

		org.junit.jupiter.api.Assertions.assertTrue(proxy.sendVoteEnvelopeAcceptedForTest("survival", 1, vote));
		Method retry = VotingPluginProxy.class.getDeclaredMethod("retryReliableVoteDeliveries", String.class);
		retry.setAccessible(true);
		retry.invoke(proxy, "survival");
		retry.invoke(proxy, "survival");

		Method completion = VotingPluginProxy.class.getDeclaredMethod(
				"handleVoteDeliveryAcknowledgement", JsonEnvelope.class);
		completion.setAccessible(true);
		completion.invoke(proxy, VotingPluginWire.voteDeliveryAcknowledgement(
				"survival", voteId, VotingPluginWire.SUB_VOTE));
		retry.invoke(proxy, "survival");

		java.util.List<String> deliveryIds = proxy.getAttemptedVotePartyDeliveryIds();
		assertEquals(5, deliveryIds.size());
		assertEquals(deliveryIds.get(0), deliveryIds.get(1));
		assertEquals(deliveryIds.get(0), deliveryIds.get(2));
		assertEquals(deliveryIds.get(3), deliveryIds.get(4));
		assertNotEquals(deliveryIds.get(0), deliveryIds.get(3));
	}

	@Test
	void retainsConnectorWhenOperationShutdownFails() throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		ControlConnector connector = mock(ControlConnector.class);
		doThrow(new IllegalStateException("operation still running")).when(connector).close();
		Field control = VotingPluginProxy.class.getDeclaredField("controlConnector");
		control.setAccessible(true);
		control.set(proxy, connector);
		Method stop = VotingPluginProxy.class.getDeclaredMethod("stopControlServices", boolean.class);
		stop.setAccessible(true);

		assertThrows(java.lang.reflect.InvocationTargetException.class, () -> stop.invoke(proxy, true));

		assertSame(connector, control.get(proxy));
	}

	@Test
	void retainsHostedManagerWhenBoundedShutdownFails() throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		HostedControlManager manager = mock(HostedControlManager.class);
		doThrow(new IllegalStateException("still running")).when(manager).closeAndWait();
		Field hosted = VotingPluginProxy.class.getDeclaredField("hostedControlManager");
		hosted.setAccessible(true);
		hosted.set(proxy, manager);
		Method stop = VotingPluginProxy.class.getDeclaredMethod("stopControlServices", boolean.class);
		stop.setAccessible(true);

		assertThrows(java.lang.reflect.InvocationTargetException.class, () -> stop.invoke(proxy, true));

		assertSame(manager, hosted.get(proxy));
	}

	@Test
	void finalStopContinuesToHostedManagerAfterConnectorDrainFailure() throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		ControlConnector connector = mock(ControlConnector.class);
		doThrow(new IllegalStateException("operation still running")).when(connector).close();
		HostedControlManager manager = mock(HostedControlManager.class);
		Field control = VotingPluginProxy.class.getDeclaredField("controlConnector");
		control.setAccessible(true);
		control.set(proxy, connector);
		Field hosted = VotingPluginProxy.class.getDeclaredField("hostedControlManager");
		hosted.setAccessible(true);
		hosted.set(proxy, manager);
		Method stop = VotingPluginProxy.class.getDeclaredMethod("stopControlServices", boolean.class);
		stop.setAccessible(true);

		stop.invoke(proxy, false);

		verify(manager).close();
		assertNull(control.get(proxy));
		assertNull(hosted.get(proxy));
	}

	@Test
	void stopsEveryReplacedSocketClientEvenWhenOneStopFails() {
		ClientHandler failing = mock(ClientHandler.class);
		ClientHandler healthy = mock(ClientHandler.class);
		doThrow(new IllegalStateException("already closed")).when(failing).stopConnection();
		Map<String, ClientHandler> clients = new LinkedHashMap<>();
		clients.put("failing", failing);
		clients.put("healthy", healthy);

		VotingPluginProxy.stopSocketClients(clients);

		verify(failing).stopConnection();
		verify(healthy).stopConnection();
	}

	@Test
	void waitsForInFlightSocketSendBeforeClosingClientMap() throws Exception {
		VotingPluginProxyTestImpl proxy = new VotingPluginProxyTestImpl();
		proxy.setMethod(BungeeMethod.SOCKETS);
		ClientHandler client = mock(ClientHandler.class);
		JsonEnvelope envelope = mock(JsonEnvelope.class);
		CountDownLatch sending = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		CountDownLatch closingStarted = new CountDownLatch(1);
		org.mockito.Mockito.doAnswer(ignored -> {
			sending.countDown();
			if (!release.await(5, TimeUnit.SECONDS)) throw new AssertionError("send was not released");
			return null;
		}).when(client).sendEnvelope(envelope);

		Field handles = VotingPluginProxy.class.getDeclaredField("clientHandles");
		handles.setAccessible(true);
		HashMap<String, ClientHandler> clients = new HashMap<>();
		clients.put("lobby", client);
		handles.set(proxy, clients);
		Method close = VotingPluginProxy.class.getDeclaredMethod("closeSocketClients");
		close.setAccessible(true);

		var executor = Executors.newFixedThreadPool(2);
		try {
			var send = executor.submit(() -> proxy.sendProxyBroadcastEnvelopeNow("lobby", envelope));
			org.junit.jupiter.api.Assertions.assertTrue(sending.await(5, TimeUnit.SECONDS));
			var closing = executor.submit(() -> {
				closingStarted.countDown();
				try {
					close.invoke(proxy);
				} catch (ReflectiveOperationException e) {
					throw new RuntimeException(e);
				}
			});
			org.junit.jupiter.api.Assertions.assertTrue(closingStarted.await(5, TimeUnit.SECONDS));
			org.junit.jupiter.api.Assertions.assertThrows(java.util.concurrent.TimeoutException.class,
					() -> closing.get(100, TimeUnit.MILLISECONDS));
			release.countDown();
			org.junit.jupiter.api.Assertions.assertTrue(send.get(5, TimeUnit.SECONDS));
			closing.get(5, TimeUnit.SECONDS);
			verify(client).stopConnection();
		} finally {
			release.countDown();
			executor.shutdownNow();
		}
	}
}
