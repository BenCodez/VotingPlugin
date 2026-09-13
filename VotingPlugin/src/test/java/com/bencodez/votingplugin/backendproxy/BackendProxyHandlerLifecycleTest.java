package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.eclipse.paho.client.mqttv3.MqttException;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.pluginmessage.PluginMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttServerComm;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.MysqlBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.PluginMessagingBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.SocketBackendProxyTransport;
import com.bencodez.votingplugin.proxy.BungeeMethod;

class BackendProxyHandlerLifecycleTest {
	@Test
	void globalDataWakeupMovesOffTheCallingAndPrimaryThreads() {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		handler.activateInboundMessages();
		Runnable localDispatch = mock(Runnable.class);

		handler.dispatchIncomingAfterPublication(
				JsonEnvelope.builder(VotingPluginWire.SUB_BUNGEE_TIME_CHANGE).build(), localDispatch);

		verify(scheduler).runTaskAsynchronously(plugin, localDispatch);
		verify(localDispatch, never()).run();
	}

	@Test
	void stagedInboundWaitsForPublicationAndUsesTheBukkitScheduler() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		BackendProxyHandler handler = new BackendProxyHandler(plugin);
		JsonEnvelope envelope = JsonEnvelope.builder("staged").build();
		Runnable localDispatch = mock(Runnable.class);
		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		CountDownLatch started = new CountDownLatch(1);

		CompletableFuture<Void> callback = CompletableFuture.runAsync(() -> {
			started.countDown();
			handler.dispatchIncomingAfterPublication(envelope, localDispatch);
		});
		assertTrue(started.await(1, TimeUnit.SECONDS));
		assertFalse(callback.isDone());
		verifyNoInteractions(scheduler);

		handler.activateInboundMessages();
		callback.get(1, TimeUnit.SECONDS);
		verify(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(localDispatch, never()).run();
		scheduled.get().run();
		verify(localDispatch).run();
	}

	@Test
	void stagedInboundRoutesThroughRestoredPredecessorOnRollback() throws Exception {
		BackendProxyHandler previous = new BackendProxyHandler(null);
		BackendProxyHandler replacement = new BackendProxyHandler(null);
		GlobalMessageHandler previousMessages = mock(GlobalMessageHandler.class);
		setField(previous, "globalMessageHandler", previousMessages);
		JsonEnvelope envelope = JsonEnvelope.builder("rollback").build();
		Runnable replacementDispatch = mock(Runnable.class);
		CountDownLatch started = new CountDownLatch(1);

		CompletableFuture<Void> callback = CompletableFuture.runAsync(() -> {
			started.countDown();
			replacement.dispatchIncomingAfterPublication(envelope, replacementDispatch);
		});
		assertTrue(started.await(1, TimeUnit.SECONDS));
		assertFalse(callback.isDone());

		replacement.abortStagedInboundTo(previous);
		callback.get(1, TimeUnit.SECONDS);
		verify(previousMessages).onMessage(envelope);
		verify(replacementDispatch, never()).run();
	}

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
	void preparedDisablePropagatesRejectedStoppedPresence() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);
		doThrow(new IllegalStateException("outbound queue full")).when(messages).sendMessage(any());

		assertThrows(IllegalStateException.class, presence::stopForDisable);
		assertFalse(presence.isReporting());
	}

	@Test
	void preparedDisableSchedulesStoppedPresenceOnTheBukkitThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		org.bukkit.Server server = mock(org.bukkit.Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getServer()).thenReturn(server);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(server.isPrimaryThread()).thenReturn(false);
		java.util.concurrent.atomic.AtomicBoolean onScheduledTask = new java.util.concurrent.atomic.AtomicBoolean();
		org.mockito.Mockito.doAnswer(invocation -> {
			onScheduledTask.set(true);
			try {
				Runnable task = invocation.getArgument(1);
				task.run();
			} finally {
				onScheduledTask.set(false);
			}
			return null;
		}).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(invocation -> {
			assertTrue(onScheduledTask.get(), "stopped presence must be sent by the scheduled Bukkit task");
			return null;
		}).when(messages).sendMessage(any());

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.PLUGINMESSAGING, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);

		presence.stopForDisable();

		verify(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(messages).sendMessage(any());
		assertFalse(presence.isReporting());
	}

	@Test
	void preparedDisableKeepsHttpStoppedPresenceOffTheBukkitThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		org.bukkit.Server server = mock(org.bukkit.Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		when(plugin.getServer()).thenReturn(server);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(server.isPrimaryThread()).thenReturn(false);

		BackendPresenceManager presence = new BackendPresenceManager(plugin, BungeeMethod.HTTP, messages);
		setField(presence, "reporting", true);
		setField(presence, "server", "backend-1");
		setField(presence, "incarnationId", java.util.UUID.randomUUID());
		setField(presence, "startedAt", 1L);

		presence.stopForDisable();

		verify(scheduler, never()).executeOrScheduleSync(eq(plugin), any(Runnable.class));
		verify(messages).sendMessage(any());
		assertFalse(presence.isReporting());
	}

	@Test
	void failedPreparedDisableCanRestartPresenceDuringRollback() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		BackendPresenceManager presence = mock(BackendPresenceManager.class);
		setField(handler, "presenceManager", presence);
		setField(handler, "presenceReportingActivated", true);
		doThrow(new IllegalStateException("outbound queue full")).when(presence).stopForDisable();

		assertThrows(IllegalStateException.class, handler::preparePresenceForDisable);
		handler.restorePresenceAfterFailedDisablePreparation();

		org.mockito.InOrder rollback = inOrder(presence);
		rollback.verify(presence).stopForDisable();
		rollback.verify(presence).start();
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
	void rejectsSamePortSocketReplacementUntilThePreviousListenerIsPrepared(@TempDir Path dataFolder)
			throws Exception {
		int port;
		try (ServerSocket allocation = new ServerSocket(0)) {
			port = allocation.getLocalPort();
		}
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(plugin.getName()).thenReturn("socket-lifecycle-test");
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getLogger()).thenReturn(Logger.getLogger("socket-lifecycle-test"));
		when(settings.getBungeeServerHost()).thenReturn("127.0.0.1");
		when(settings.getBungeeServerPort()).thenReturn(1);
		when(settings.getSpigotServerHost()).thenReturn("127.0.0.1");
		when(settings.getSpigotServerPort()).thenReturn(port);

		SocketBackendProxyTransport previous = new SocketBackendProxyTransport(plugin);
		SocketBackendProxyTransport replacement = new SocketBackendProxyTransport(plugin);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		try {
			previous.start(messages);
			assertThrows(IllegalStateException.class, () -> replacement.start(messages));
			assertNull(replacement.getClientHandler());

			previous.prepareForReplacement();
			assertDoesNotThrow(() -> replacement.start(messages));
			replacement.close();
			assertDoesNotThrow(previous::restoreAfterFailedReplacement);
			assertNotNull(previous.getSocketHandler());
		} finally {
			replacement.close();
			previous.close();
		}
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
	void preparesSameSocketReplacementByRetiringTheExistingListener() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.SOCKETS);
		SocketHandler socket = mock(SocketHandler.class);
		SocketBackendProxyTransport transport = new SocketBackendProxyTransport(null);
		setField(transport, "socketHandler", socket);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.SOCKETS));

		assertNull(handler.getSocketHandler());
		verify(socket).closeConnection();
	}

	@Test
	void preparesSameMqttReplacementByRetiringTheExistingClient() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.MQTT);
		MqttHandler mqtt = mock(MqttHandler.class);
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null);
		setField(transport, "mqttHandler", mqtt);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.MQTT));

		assertNull(handler.getMqttHandler());
		verify(mqtt).disconnect();
	}

	@Test
	void restoresPreparedMqttClientWhenReplacementValidationIsAbandoned() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null);
		MqttBackendProxyTransport mqtt = mock(MqttBackendProxyTransport.class);
		setField(manager, "preparedTransport", mqtt);

		manager.restoreAfterFailedReplacement();

		assertSame(mqtt, transport(manager));
		verify(mqtt).restoreAfterFailedReplacement();
	}

	@Test
	void failedMqttDisconnectKeepsAStillConnectedPredecessor() throws Exception {
		BackendProxyTransportManager manager = new BackendProxyTransportManager(null);
		MqttBackendProxyTransport mqtt = mock(MqttBackendProxyTransport.class);
		doThrow(new IllegalStateException("disconnect failed")).when(mqtt).prepareForReplacement();
		when(mqtt.isConnected()).thenReturn(true);
		setField(manager, "transport", mqtt);

		assertThrows(IllegalStateException.class, manager::prepareForReplacement);

		assertSame(mqtt, transport(manager));
		verify(mqtt, never()).restoreAfterFailedReplacement();
	}

	@Test
	void disconnectsPartialMqttClientWhenSubscriptionFails() throws Exception {
		MqttHandler partial = mock(MqttHandler.class);
		doThrow(new MqttException(0)).when(partial).subscribeEnvelopes(any(), any());
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(null) {
			@Override
			protected MqttServerComm createMqttServerComm() {
				return mock(MqttServerComm.class);
			}

			@Override
			protected MqttHandler createMqttHandler(MqttServerComm server) {
				return partial;
			}
		};
		setField(transport, "messageHandler", mock(GlobalMessageHandler.class));
		setField(transport, "clientId", "backend-1");
		setField(transport, "brokerUrl", "tcp://broker.invalid:1883");
		setField(transport, "subscriptionTopic", "votingplugin/servers/backend-1");

		assertThrows(IllegalStateException.class, transport::restoreAfterFailedReplacement);

		assertNull(transport.getMqttHandler());
		verify(partial).disconnect();
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
	void preparesHttpDeliveryQueueWhenSwitchingToAnotherMethod() throws Exception {
		BackendProxyHandler handler = handlerWithTransport(BungeeMethod.HTTP);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setTransport(handler, transport);

		assertTrue(handler.prepareForReplacement(BungeeMethod.REDIS));

		verify(transport).prepareForReplacement();
	}

	@Test
	void redisSendKeepsTheChannelCapturedByTheActiveTransport() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin =
				mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getRedisPrefix()).thenReturn("new:");
		RedisBackendProxyTransport transport = new RedisBackendProxyTransport(plugin);
		RedisHandler redis = mock(RedisHandler.class);
		setField(transport, "redisHandler", redis);
		setField(transport, "publishChannel", "old:VotingPlugin");

		transport.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("vote").build());

		verify(redis).publishEnvelope(eq("old:VotingPlugin"), any());
	}

	@Test
	void mqttSendKeepsTheTopicCapturedByTheActiveTransport() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin =
				mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BungeeSettings settings = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(settings);
		when(settings.getMqttPrefix()).thenReturn("new/");
		MqttBackendProxyTransport transport = new MqttBackendProxyTransport(plugin);
		MqttHandler mqtt = mock(MqttHandler.class);
		setField(transport, "mqttHandler", mqtt);
		setField(transport, "publishTopic", "old/votingplugin/servers/proxy");

		assertTrue(transport.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("vote").build()));

		verify(mqtt).publishEnvelope(eq("old/votingplugin/servers/proxy"), any());
	}

	@Test
	void mqttAndMysqlReportRejectedHandoffDeliveries() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		MqttBackendProxyTransport mqttTransport = new MqttBackendProxyTransport(plugin);
		MqttHandler mqtt = mock(MqttHandler.class);
		setField(mqttTransport, "mqttHandler", mqtt);
		setField(mqttTransport, "publishTopic", "votingplugin/servers/proxy");
		doThrow(new IllegalStateException("publish failed")).when(mqtt).publishEnvelope(any(), any());
		JsonEnvelope mqttEnvelope = JsonEnvelope.builder("mqtt").build();

		assertFalse(mqttTransport.send(mqttEnvelope));

		MysqlBackendProxyTransport mysqlTransport = new MysqlBackendProxyTransport(plugin);
		MySqlMessenger mysql = mock(MySqlMessenger.class);
		setField(mysqlTransport, "messenger", mysql);
		doThrow(new java.sql.SQLException("write failed")).when(mysql).sendToProxy(any());

		assertFalse(mysqlTransport.send(JsonEnvelope.builder("mysql").build()));
	}

	@Test
	void asyncHandoffRetainsARejectedEntryUntilAcceptance() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("retry").build();
		when(replacementTransport.send(envelope)).thenReturn(false, true);
		setField(source, "transport", sourceTransport);
		setField(replacement, "transport", replacementTransport);

		replacement.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(envelope);
		source.completePreparedTransportHandoff(replacement);

		verify(replacementTransport, org.mockito.Mockito.timeout(1500).times(2)).send(envelope);
		replacement.awaitAsyncHandoff(System.nanoTime() + TimeUnit.SECONDS.toNanos(1));
		assertFalse(replacement.hasPendingAsyncHandoff());
	}

	@Test
	void forwardsMessagesBufferedDuringPreparedHttpReplacement() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport previousTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		when(replacementTransport.send(any())).thenReturn(true);
		setField(previous, "transport", previousTransport);
		setField(replacement, "transport", replacementTransport);
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope duringValidation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("during-validation").build();
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPublication =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-publication").build();
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope replacementStarted =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("replacement-started").build();

		replacement.beginPreparedTransportHandoff();
		previous.prepareForReplacement();
		previous.send(duringValidation);
		replacement.send(replacementStarted);
		verifyNoInteractions(replacementTransport);

		previous.completePreparedTransportHandoff(replacement);
		previous.send(afterPublication);

		org.mockito.InOrder order = inOrder(replacementTransport);
		order.verify(replacementTransport, org.mockito.Mockito.timeout(1000)).send(duringValidation);
		order.verify(replacementTransport).send(replacementStarted);
		order.verify(replacementTransport).send(afterPublication);
	}

	@Test
	void crossTransportPreparedHandoffDoesNotPerformNetworkSendOnCallerThread() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		BackendProxyTransport previousTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport replacementTransport = mock(BackendProxyTransport.class);
		setField(previous, "transport", previousTransport);
		setField(replacement, "transport", replacementTransport);
		java.util.concurrent.CountDownLatch sendStarted = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch releaseSend = new java.util.concurrent.CountDownLatch(1);
		doAnswer(invocation -> {
			sendStarted.countDown();
			releaseSend.await(2, java.util.concurrent.TimeUnit.SECONDS);
			return true;
		}).when(replacementTransport).send(any());

		replacement.beginPreparedTransportHandoff();
		previous.prepareForReplacement();
		previous.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		org.junit.jupiter.api.Assertions.assertTimeoutPreemptively(java.time.Duration.ofSeconds(1),
				() -> previous.completePreparedTransportHandoff(replacement));

		org.junit.jupiter.api.Assertions.assertTrue(sendStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
		releaseSend.countDown();
		verify(replacementTransport, org.mockito.Mockito.timeout(1000)).send(any());
	}

	@Test
	void preparedDisableRefusesToDiscardMessagesAcceptedDuringPreparation() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setField(manager, "transport", transport);

		manager.prepareForReplacement();
		manager.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("during-disable").build());

		assertFalse(manager.commitPreparedDisable());
		verify(transport, never()).close();
	}

	@Test
	void preparedDisableRejectsOrdinarySendsButDeliversOnlyOneFinalStoppedPresence() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger(getClass().getName()));
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		BackendProxyTransport transport = mock(BackendProxyTransport.class);
		setField(manager, "transport", transport);
		JsonEnvelope ordinary = JsonEnvelope.builder("ordinary").build();
		JsonEnvelope stopped = com.bencodez.votingplugin.proxy.VotingPluginWire.backendStopped("backend-1");
		when(transport.send(stopped)).thenReturn(true);

		manager.beginPreparedDisable();
		manager.send(ordinary);
		manager.send(stopped);
		manager.send(stopped);

		verify(transport, never()).send(ordinary);
		verify(transport, times(1)).send(stopped);
	}

	@Test
	void preparedHttpDisableDeliversTheFinalStoppedPresenceBeforeClosingForReplacement() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager manager = new BackendProxyTransportManager(plugin);
		HttpBackendProxyTransport transport = mock(HttpBackendProxyTransport.class);
		setField(manager, "transport", transport);
		JsonEnvelope stopped = com.bencodez.votingplugin.proxy.VotingPluginWire.backendStopped("backend-1");
		when(transport.send(stopped)).thenReturn(true);

		manager.beginPreparedDisable();
		manager.send(stopped);
		manager.prepareForReplacement();
		assertTrue(manager.commitPreparedDisable());

		org.mockito.InOrder disable = inOrder(transport);
		disable.verify(transport).send(stopped);
		disable.verify(transport).prepareForReplacement();
	}

	@Test
	void laterReplacementWaitsForAnEarlierAsyncHandoff() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyHandler target = handlerWithTransport(BungeeMethod.REDIS);
		Field managerField = BackendProxyHandler.class.getDeclaredField("transportManager");
		managerField.setAccessible(true);
		BackendProxyTransportManager targetManager = (BackendProxyTransportManager) managerField.get(target);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		BackendProxyTransport targetTransport = mock(BackendProxyTransport.class);
		setField(source, "transport", sourceTransport);
		setField(targetManager, "transport", targetTransport);
		java.util.concurrent.CountDownLatch sendStarted = new java.util.concurrent.CountDownLatch(1);
		java.util.concurrent.CountDownLatch releaseSend = new java.util.concurrent.CountDownLatch(1);
		doAnswer(invocation -> {
			sendStarted.countDown();
			releaseSend.await(2, java.util.concurrent.TimeUnit.SECONDS);
			return true;
		}).when(targetTransport).send(any());

		targetManager.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		source.completePreparedTransportHandoff(targetManager);
		assertTrue(sendStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
		assertTrue(target.requiresPreparationForReplacement());
		java.util.concurrent.CompletableFuture<Boolean> preparation = java.util.concurrent.CompletableFuture.supplyAsync(
				() -> target.prepareForReplacement(BungeeMethod.MQTT,
						System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(2)));
		try {
			Thread.sleep(50);
			assertFalse(preparation.isDone(), "replacement must wait while an admitted send is in flight");
		} finally {
			releaseSend.countDown();
		}
		assertTrue(preparation.get(1, java.util.concurrent.TimeUnit.SECONDS));
		assertFalse(targetManager.hasPendingAsyncHandoff());
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPreparation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-preparation").build();
		targetManager.send(afterPreparation);
		BackendProxyTransportManager next = new BackendProxyTransportManager(plugin);
		BackendProxyTransport nextTransport = mock(BackendProxyTransport.class);
		when(nextTransport.send(any())).thenReturn(true);
		setField(next, "transport", nextTransport);
		next.beginPreparedTransportHandoff();
		targetManager.completePreparedTransportHandoff(next);
		verify(nextTransport, org.mockito.Mockito.timeout(1000)).send(afterPreparation);
	}

	@Test
	void pluginMessageHandoffRunsThroughTheBukkitScheduler() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		java.util.concurrent.atomic.AtomicReference<Runnable> scheduled = new java.util.concurrent.atomic.AtomicReference<>();
		doAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		BackendProxyTransportManager source = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager target = new BackendProxyTransportManager(plugin);
		BackendProxyTransport sourceTransport = mock(BackendProxyTransport.class);
		PluginMessagingBackendProxyTransport targetTransport = mock(PluginMessagingBackendProxyTransport.class);
		when(targetTransport.send(any())).thenReturn(true);
		setField(source, "transport", sourceTransport);
		setField(target, "transport", targetTransport);

		target.beginPreparedTransportHandoff();
		source.prepareForReplacement();
		source.send(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("buffered").build());
		source.completePreparedTransportHandoff(target);

		verify(targetTransport, never()).send(any());
		assertTrue(scheduled.get() != null);
		scheduled.get().run();
		verify(targetTransport).send(any());
		assertFalse(target.hasPendingAsyncHandoff());

		target.prepareAsyncHandoffForReplacement(
				System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(1));
		com.bencodez.simpleapi.servercomm.codec.JsonEnvelope afterPreparation =
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder("after-preparation").build();
		target.send(afterPreparation);
		BackendProxyTransportManager next = new BackendProxyTransportManager(plugin);
		BackendProxyTransport nextTransport = mock(BackendProxyTransport.class);
		when(nextTransport.send(any())).thenReturn(true);
		setField(next, "transport", nextTransport);
		next.beginPreparedTransportHandoff();
		target.completePreparedTransportHandoff(next);
		verify(nextTransport, org.mockito.Mockito.timeout(1000)).send(afterPreparation);
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
	void promotesRedisReplacementAfterOldListenerIsFencedEvenWhenRetirementFails() throws Exception {
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
		order.verify(oldTransport).closeForHandoff();
		order.verify(newTransport).activateAfterHandoff();
		assertSame(newTransport, transport(replacement));
		assertNull(transport(previous));

		assertDoesNotThrow(previous::close);
		assertDoesNotThrow(previous::close);

		verify(oldTransport, timeout(1_000).times(2)).close();
	}

	@Test
	void sameRedisHandoffBuffersOutboundSendsUntilPublicationAdmission() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("outbound-during-redis-handoff").build();
		when(newTransport.send(envelope)).thenReturn(true);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);

		// Validation fences the staged replacement before the old Redis listener is
		// retired. Its send FIFO must be admitted only after publication succeeds.
		replacement.beginPreparedTransportHandoff();
		previous.completeRedisHandoff(replacement);
		previous.send(envelope);

		verify(oldTransport, never()).send(envelope);
		verify(newTransport, never()).send(envelope);

		previous.completePreparedTransportHandoff(replacement);

		verify(newTransport, timeout(1_000).times(1)).send(envelope);
	}

	@Test
	void sameRedisHandoffRollbackDrainsBufferedOutboundSendsThroughRestoredListener() throws Exception {
		com.bencodez.votingplugin.VotingPluginMain plugin = mock(com.bencodez.votingplugin.VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
		BackendProxyTransportManager previous = new BackendProxyTransportManager(plugin);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(plugin);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		JsonEnvelope envelope = JsonEnvelope.builder("outbound-redis-rollback").build();
		when(oldTransport.send(envelope)).thenReturn(true);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);

		replacement.beginPreparedTransportHandoff();
		previous.completeRedisHandoff(replacement);
		previous.send(envelope);
		previous.restoreAfterFailedReplacement(replacement);

		verify(newTransport, never()).send(envelope);
		verify(oldTransport, timeout(1_000).times(1)).send(envelope);
	}

	@Test
	void doesNotPromoteRedisReplacementWhenOldCallbacksDoNotQuiesce() throws Exception {
		BackendProxyTransportManager previous = new BackendProxyTransportManager(null);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(null);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new RedisBackendProxyTransport.HandoffQuiescenceException("busy"))
				.when(oldTransport).closeForHandoff();

		assertThrows(RedisBackendProxyTransport.HandoffQuiescenceException.class,
				() -> previous.completeRedisHandoff(replacement));

		verify(newTransport, never()).activateAfterHandoff();
		assertSame(oldTransport, transport(previous));
		assertSame(newTransport, transport(replacement));
	}

	@Test
	void restoresOldRedisSubscriberWhenStandbyActivationFails() throws Exception {
		BackendProxyTransportManager previous = new BackendProxyTransportManager(null);
		BackendProxyTransportManager replacement = new BackendProxyTransportManager(null);
		RedisBackendProxyTransport oldTransport = mock(RedisBackendProxyTransport.class);
		RedisBackendProxyTransport newTransport = mock(RedisBackendProxyTransport.class);
		setField(previous, "transport", oldTransport);
		setField(replacement, "transport", newTransport);
		doThrow(new IllegalStateException("handoff buffer overflowed")).when(newTransport).activateAfterHandoff();

		assertThrows(IllegalStateException.class, () -> previous.completeRedisHandoff(replacement));

		org.mockito.InOrder order = inOrder(oldTransport, newTransport);
		order.verify(oldTransport).closeForHandoff();
		order.verify(newTransport).activateAfterHandoff();
		order.verify(oldTransport).restoreAfterFailedHandoff(java.util.Collections.emptyList());
		assertSame(oldTransport, transport(previous));
		assertSame(newTransport, transport(replacement));
	}

	@Test
	void redisRollbackForcesAFreshPresenceGeneration() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager presence =
				mock(com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager.class);
		setField(handler, "presenceManager", presence);
		setField(handler, "presenceReportingActivated", true);

		handler.refreshPresenceAfterFailedReplacement();

		org.mockito.InOrder order = inOrder(presence);
		order.verify(presence).stop();
		order.verify(presence).start();
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
		Class<?> type = target.getClass();
		while (type != null) {
			try {
				Field field = type.getDeclaredField(name);
				field.setAccessible(true);
				field.set(target, value);
				return;
			} catch (NoSuchFieldException ignored) {
				type = type.getSuperclass();
			}
		}
		throw new NoSuchFieldException(name);
	}
}
