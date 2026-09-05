package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
}
