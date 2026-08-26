package com.bencodez.votingplugin.proxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.proxy.control.ControlConnector;

class VotingPluginProxyLifecycleTest {

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
