package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.votingplugin.proxy.control.ControlConnector;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;

class VotingPluginProxyLifecycleTest {
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
