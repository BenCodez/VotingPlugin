package com.bencodez.votingplugin.proxy.multiproxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;

class MultiProxyHandlerLifecycleTest {

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
}
