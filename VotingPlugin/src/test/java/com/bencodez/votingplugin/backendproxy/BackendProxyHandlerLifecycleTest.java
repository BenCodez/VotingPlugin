package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.MysqlBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.PluginMessagingBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.SocketBackendProxyTransport;
import com.bencodez.votingplugin.proxy.BungeeMethod;

class BackendProxyHandlerLifecycleTest {
	@Test
	void failedPluginMessagePublicationRestoresPreviousSharedState() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage pluginMessages =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		GlobalMessageHandler previousHandler = mock(GlobalMessageHandler.class);
		GlobalMessageHandler replacementHandler = mock(GlobalMessageHandler.class);
		AtomicReference<String> activeChannel = new AtomicReference<>("old:channel");
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getPluginMessaging()).thenReturn(pluginMessages);
		when(plugin.getBungeeChannel()).thenAnswer(ignored -> activeChannel.get());
		org.mockito.Mockito.doAnswer(invocation -> {
			activeChannel.set(invocation.getArgument(0));
			return null;
		}).when(plugin).registerBungeeChannels(org.mockito.ArgumentMatchers.anyString());

		when(settings.getPluginMessagingChannel()).thenReturn("old:channel");
		PluginMessagingBackendProxyTransport previous = new PluginMessagingBackendProxyTransport(plugin);
		previous.start(previousHandler);
		previous.activateAfterPublication();

		when(settings.getPluginMessagingChannel()).thenReturn("new:channel");
		PluginMessagingBackendProxyTransport replacement = new PluginMessagingBackendProxyTransport(plugin);
		replacement.start(replacementHandler);
		replacement.activateAfterPublication();
		replacement.close();
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		try {
			Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
			transportField.setAccessible(true);
			transportField.set(manager, previous);
			manager.restoreAfterFailedReplacement();
		} catch (ReflectiveOperationException failure) {
			throw new AssertionError(failure);
		}

		assertEquals("old:channel", activeChannel.get());
		verify(plugin, times(2)).activateBackendPluginMessageHandler(previousHandler);
		verify(plugin).activateBackendPluginMessageHandler(replacementHandler);
		verify(plugin).deactivateBackendPluginMessageHandler(replacementHandler);
	}

	@Test
	void stagedPluginMessageTransportDefersRelaySwapUntilPublication() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage pluginMessages =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		GlobalMessageHandler replacement = mock(GlobalMessageHandler.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getPluginMessaging()).thenReturn(pluginMessages);
		when(settings.getPluginMessagingChannel()).thenReturn("votingplugin:main");

		PluginMessagingBackendProxyTransport transport = new PluginMessagingBackendProxyTransport(plugin);
		transport.start(replacement);
		verify(plugin, never()).activateBackendPluginMessageHandler(replacement);
		verify(plugin, never()).registerBungeeChannels(org.mockito.ArgumentMatchers.anyString());
		verifyNoInteractions(pluginMessages);
		transport.close();
		verify(plugin, never()).deactivateBackendPluginMessageHandler(replacement);
		verifyNoInteractions(pluginMessages);

		transport = new PluginMessagingBackendProxyTransport(plugin);
		transport.start(replacement);
		transport.activateAfterPublication();
		transport.activateAfterPublication();
		verify(plugin, times(1)).registerBungeeChannels("votingplugin:main");
		verify(pluginMessages).setEncryptionHandler(null);
		verify(pluginMessages).setDebug(false);
		verify(plugin, times(1)).activateBackendPluginMessageHandler(replacement);

		transport.close();
		verify(plugin).deactivateBackendPluginMessageHandler(replacement);
	}

	@Test
	void movingPluginMessageRelayDetachesItFromThePreviousPluginMessage() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class,
				CALLS_REAL_METHODS);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage first =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage second =
				mock(com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage.class);
		java.util.ArrayList<PluginMessageHandler> firstHandlers = new java.util.ArrayList<>();
		java.util.ArrayList<PluginMessageHandler> secondHandlers = new java.util.ArrayList<>();
		GlobalMessageHandler firstTarget = mock(GlobalMessageHandler.class);
		GlobalMessageHandler secondTarget = mock(GlobalMessageHandler.class);
		AtomicReference<com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessage> current =
				new AtomicReference<>(first);
		when(plugin.getPluginMessaging()).thenAnswer(ignored -> current.get());
		when(first.getPluginMessages()).thenReturn(firstHandlers);
		when(second.getPluginMessages()).thenReturn(secondHandlers);
		org.mockito.Mockito.doAnswer(invocation -> firstHandlers.add(invocation.getArgument(0))).when(first)
				.add(org.mockito.ArgumentMatchers.any(PluginMessageHandler.class));
		org.mockito.Mockito.doAnswer(invocation -> secondHandlers.add(invocation.getArgument(0))).when(second)
				.add(org.mockito.ArgumentMatchers.any(PluginMessageHandler.class));
		Field targetField = com.bencodez.votingplugin.VotingPluginMain.class
				.getDeclaredField("backendPluginMessageTarget");
		targetField.setAccessible(true);
		targetField.set(plugin, new AtomicReference<GlobalMessageHandler>());

		plugin.activateBackendPluginMessageHandler(firstTarget);
		current.set(second);
		plugin.activateBackendPluginMessageHandler(secondTarget);

		assertTrue(firstHandlers.isEmpty());
		assertEquals(1, secondHandlers.size());
		secondHandlers.get(0).onReceive(null);
		verify(firstTarget, never()).onMessage(null);
		verify(secondTarget).onMessage(null);
	}

	@Test
	void failedPresenceActivationDoesNotAnnounceReplacementGeneration() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getServer()).thenReturn("lobby");
		when(plugin.getTimer()).thenReturn(timer);
		when(timer.scheduleAtFixedRate(org.mockito.ArgumentMatchers.any(Runnable.class),
				org.mockito.ArgumentMatchers.anyLong(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any())).thenThrow(new java.util.concurrent.RejectedExecutionException());

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		assertThrows(java.util.concurrent.RejectedExecutionException.class, presence::start);

		verifyNoInteractions(messages);
	}

	@Test
	void stagedPresenceStartsOnlyAtExplicitPublication() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		Field field = BackendProxyHandler.class.getDeclaredField("presenceManager");
		field.setAccessible(true);
		field.set(handler, presence);

		verifyNoInteractions(presence);
		handler.activatePresenceReporting();
		handler.activatePresenceReporting();

		verify(presence, times(1)).start();
	}

	@Test
	void failedPresenceStartKeepsTransportBehindPublicationBarrier() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException()).when(presence).start();
		Field presenceField = BackendProxyHandler.class.getDeclaredField("presenceManager");
		presenceField.setAccessible(true);
		presenceField.set(handler, presence);

		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		assertThrows(java.util.concurrent.RejectedExecutionException.class, handler::activatePresenceReporting);

		verify(presence).start();
		verify(transport, never()).activateAfterPublication();
	}

	@Test
	void sharesVoteDeduplicationAcrossHandlerReplacement() {
		ProcessedVoteCache cache = new ProcessedVoteCache();
		BackendProxyHandler previous = new BackendProxyHandler(null, cache);
		BackendProxyHandler replacement = new BackendProxyHandler(null, cache);

		assertSame(previous.getProcessedWireVotes(), replacement.getProcessedWireVotes());
	}

	@Test
	void keepsPluginMessageRelayActiveUntilAtomicTargetSwap() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		Field method = BackendProxyHandler.class.getDeclaredField("method");
		method.setAccessible(true);
		method.set(handler, BungeeMethod.PLUGINMESSAGING);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		handler.prepareForReplacement(BungeeMethod.PLUGINMESSAGING);

		verifyNoInteractions(transport);
	}

	@Test
	void releasesSocketListenerBeforeSamePortReplacement() throws Exception {
		SocketBackendProxyTransport handler = new SocketBackendProxyTransport(null);
		SocketHandler socket = mock(SocketHandler.class);
		Field field = SocketBackendProxyTransport.class.getDeclaredField("socketHandler");
		field.setAccessible(true);
		field.set(handler, socket);

		handler.prepareForReplacement();

		verify(socket).closeConnection();
		assertNull(handler.getSocketHandler());
	}

	@Test
	void keepsRedisSubscriberUntilReplacementIsReady() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		Field method = BackendProxyHandler.class.getDeclaredField("method");
		method.setAccessible(true);
		method.set(handler, BungeeMethod.REDIS);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(null);
		RedisHandler redis = mock(RedisHandler.class);
		Field field = RedisBackendProxyTransport.class.getDeclaredField("redisHandler");
		field.setAccessible(true);
		field.set(transport, redis);
		Field transportField = BackendProxyTransportManager.class.getDeclaredField("transport");
		transportField.setAccessible(true);
		transportField.set(manager, transport);

		handler.prepareForReplacement(BungeeMethod.REDIS);

		verifyNoInteractions(redis);
		assertSame(redis, handler.getRedisHandler());
	}

	@Test
	void releasesMqttSubscriberBeforeSameMethodReplacement() throws Exception {
		MqttBackendProxyTransport handler = new MqttBackendProxyTransport(null);
		MqttHandler mqtt = mock(MqttHandler.class);
		Field field = MqttBackendProxyTransport.class.getDeclaredField("mqttHandler");
		field.setAccessible(true);
		field.set(handler, mqtt);

		handler.prepareForReplacement();

		verify(mqtt).disconnect();
		assertNull(handler.getMqttHandler());
	}

	@Test
	void failedSameSocketReplacementLeavesExistingTransportAvailable() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.SOCKETS);
		SocketHandler socket = mock(SocketHandler.class);
		SocketBackendProxyTransport transport = new SocketBackendProxyTransport(null);
		setField(transport, "socketHandler", socket);
		setTransport(handler, transport);

		assertFalse(handler.prepareForReplacement(BungeeMethod.SOCKETS));

		assertSame(socket, handler.getSocketHandler());
		verifyNoInteractions(socket);
	}

	@Test
	void failedSameMqttReplacementLeavesExistingTransportAvailable() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.MQTT);
		MqttHandler mqtt = mock(MqttHandler.class);
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null);
		setField(transport, "mqttHandler", mqtt);
		setTransport(handler, transport);

		assertFalse(handler.prepareForReplacement(BungeeMethod.MQTT));

		assertSame(mqtt, handler.getMqttHandler());
		verifyNoInteractions(mqtt);
	}

	@Test
	void failedSameMysqlReplacementLeavesExistingTransportAvailable() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.MYSQL);
		MySqlMessenger messenger = mock(MySqlMessenger.class);
		MysqlBackendProxyTransport transport = new MysqlBackendProxyTransport(null);
		setField(transport, "messenger", messenger);
		setTransport(handler, transport);

		assertFalse(handler.prepareForReplacement(BungeeMethod.MYSQL));

		assertSame(messenger, handler.getBackendMysqlMessenger());
		verifyNoInteractions(messenger);
	}

	@Test
	void forwardsMessagesBufferedDuringPreparedHttpReplacement() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport previousTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		setField(previous, "transport", previousTransport);
		setField(replacement, "transport", replacementTransport);
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope duringValidation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("during-validation").build();
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPublication =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-publication").build();

		previous.prepareForReplacement();
		previous.send(duringValidation);
		verifyNoInteractions(replacementTransport);

		previous.completePreparedTransportHandoff(replacement);
		previous.send(afterPublication);

		verify(replacementTransport).send(duringValidation);
		verify(replacementTransport).send(afterPublication);
	}

	@Test
	void releasesMysqlSubscriberBeforeSameMethodReplacement() throws Exception {
		MysqlBackendProxyTransport handler = new MysqlBackendProxyTransport(null);
		MySqlMessenger messenger = mock(MySqlMessenger.class);
		Field field = MysqlBackendProxyTransport.class.getDeclaredField("messenger");
		field.setAccessible(true);
		field.set(handler, messenger);

		handler.prepareForReplacement();

		verify(messenger).shutdown();
		assertNull(handler.getMessenger());
	}

	@Test
	void keepsPromotedRedisReplacementWhenOldListenerRetirementFails() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		java.util.logging.Logger logger = mock(java.util.logging.Logger.class);
		when(plugin.getLogger()).thenReturn(logger);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new IllegalStateException("listener did not stop")).when(oldTransport).closeForHandoff();
		doThrow(new IllegalStateException("connection still closing")).doNothing().when(oldTransport).close();

		previous.completeRedisHandoff(replacement);

		org.mockito.InOrder order = inOrder(newTransport, oldTransport);
		order.verify(newTransport).activateAfterHandoff();
		order.verify(oldTransport).closeForHandoff();
		assertSame(newTransport, transport(replacement));
		assertNull(transport(previous));

		assertDoesNotThrow(previous::close);
		assertDoesNotThrow(previous::close);

		verify(oldTransport, times(2)).close();
	}

	@Test
	void stopsGlobalDataTimerWhenHandlerIsReplaced() throws Exception {
		BackendGlobalDataSync handler = new BackendGlobalDataSync(null, null);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		Field field = BackendGlobalDataSync.class.getDeclaredField("timer");
		field.setAccessible(true);
		field.set(handler, timer);

		handler.close();

		verify(timer).shutdownNow();
		assertNull(handler.getTimer());
	}

	private BackendProxyHandler handlerWithTransport(BungeeMethod method) throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		setField(handler, "method", method);
		return handler;
	}

	private void setTransport(BackendProxyHandler handler, BackendProxyTransport transport) throws Exception {
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager manager = (BackendProxyTransportManager) managerField.get(handler);
		setField(manager, "transport", transport);
	}

	private BackendProxyTransport transport(BackendProxyTransportManager manager) throws Exception {
		Field field = BackendProxyTransportManager.class.getDeclaredField("transport");
		field.setAccessible(true);
		return (BackendProxyTransport) field.get(manager);
	}

	private void setField(Object target, String name, Object value) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
