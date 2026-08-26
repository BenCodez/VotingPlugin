package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.lang.reflect.Field;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.MysqlBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport;
import com.bencodez.votingplugin.backendproxy.transport.SocketBackendProxyTransport;
import com.bencodez.votingplugin.proxy.BungeeMethod;

class BackendProxyHandlerLifecycleTest {

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
	void releasesRedisSubscriberBeforeSameTopicReplacement() throws Exception {
		RedisBackendProxyTransport handler = new RedisBackendProxyTransport(null);
		RedisHandler redis = mock(RedisHandler.class);
		Field field = RedisBackendProxyTransport.class.getDeclaredField("redisHandler");
		field.setAccessible(true);
		field.set(handler, redis);

		handler.prepareForReplacement();

		verify(redis).close();
		assertNull(handler.getRedisHandler());
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
