package com.bencodez.votingplugin.proxy.multiproxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

class MultiProxyHandlerLifecycleTest {

	@Test
	void doesNotAcceptAnEnvelopeWhenNoSocketDestinationExists() {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);

		assertFalse(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
	}

	@Test
	void rejectsSocketSendWhenAnyConfiguredDestinationFails() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("backend"));
		ClientHandler failing = mock(ClientHandler.class);
		doThrow(new IllegalStateException("socket unavailable")).when(failing).sendEnvelope(org.mockito.ArgumentMatchers.any());
		java.lang.reflect.Field clients = MultiProxyHandler.class.getDeclaredField("multiproxyClientHandles");
		clients.setAccessible(true);
		Map<String, ClientHandler> configured = new LinkedHashMap<>();
		configured.put("backend", failing);
		clients.set(handler, configured);

		assertFalse(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
	}

	@Test
	void acceptsAQueuedSocketSendWhenTheConfiguredDestinationReturnsNormally() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("backend"));
		ClientHandler healthy = mock(ClientHandler.class);
		java.lang.reflect.Field clients = MultiProxyHandler.class.getDeclaredField("multiproxyClientHandles");
		clients.setAccessible(true);
		Map<String, ClientHandler> configured = new LinkedHashMap<>();
		configured.put("backend", healthy);
		clients.set(handler, configured);

		assertTrue(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
		verify(healthy).sendEnvelope(org.mockito.ArgumentMatchers.any());
	}

	@Test
	void stopsEveryReplacedSocketClientEvenWhenOneStopFails() {
		ClientHandler failing = mock(ClientHandler.class);
		ClientHandler healthy = mock(ClientHandler.class);
		doThrow(new IllegalStateException("already closed")).when(failing).stopConnection();
		Map<String, ClientHandler> clients = new LinkedHashMap<>();
		clients.put("failing", failing);
		clients.put("healthy", healthy);

		MultiProxyHandler.stopSocketClients(clients);

		verify(failing).stopConnection();
		verify(healthy).stopConnection();
	}

	@Test
	void forwardsStableWireVoteIdToDurableTrigger() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope",
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.vote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", voteId,
				false, false, 1, 1));

		verify(handler).triggerVote(org.mockito.ArgumentMatchers.eq("Player"),
				org.mockito.ArgumentMatchers.eq("Service"), org.mockito.ArgumentMatchers.eq(true),
				org.mockito.ArgumentMatchers.eq(true), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class),
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"),
				org.mockito.ArgumentMatchers.eq(voteId));
	}

	@Test
	void routesOnlyTheOriginatingProxyAcknowledgement() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteAck(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteAck(voteId, "Other", "Replica"));

		verify(handler).onMultiProxyVoteAcknowledged(voteId, "Replica");
	}

	@Test
	void fencesOnlyPeersThatAdvertiseDurableAcknowledgements() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Legacy", "Capable"));
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1));

		assertEquals(java.util.Set.of("capable"), handler.getMultiProxyVoteRecipients());
	}
}
