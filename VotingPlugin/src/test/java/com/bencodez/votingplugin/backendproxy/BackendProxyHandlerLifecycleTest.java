package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.util.concurrent.ScheduledExecutorService;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;

class BackendProxyHandlerLifecycleTest {

	@Test
	void releasesSocketListenerBeforeSamePortReplacement() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		SocketHandler socket = mock(SocketHandler.class);
		Field field = BackendProxyHandler.class.getDeclaredField("socketHandler");
		field.setAccessible(true);
		field.set(handler, socket);

		handler.releaseSocketListener();

		verify(socket).closeConnection();
		assertNull(handler.getSocketHandler());
	}

	@Test
	void releasesRedisSubscriberBeforeSameTopicReplacement() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		RedisHandler redis = mock(RedisHandler.class);
		Field field = BackendProxyHandler.class.getDeclaredField("redisHandler");
		field.setAccessible(true);
		field.set(handler, redis);

		handler.releaseRedisTransport();

		verify(redis).close();
		assertNull(handler.getRedisHandler());
	}

	@Test
	void releasesMqttSubscriberBeforeSameMethodReplacement() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		MqttHandler mqtt = mock(MqttHandler.class);
		Field field = BackendProxyHandler.class.getDeclaredField("mqttHandler");
		field.setAccessible(true);
		field.set(handler, mqtt);

		handler.releaseMqttTransport();

		verify(mqtt).disconnect();
		assertNull(handler.getMqttHandler());
	}

	@Test
	void releasesMysqlSubscriberBeforeSameMethodReplacement() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		MySqlMessenger messenger = mock(MySqlMessenger.class);
		Field field = BackendProxyHandler.class.getDeclaredField("backendMysqlMessenger");
		field.setAccessible(true);
		field.set(handler, messenger);

		handler.releaseMysqlTransport();

		verify(messenger).shutdown();
		assertNull(handler.getBackendMysqlMessenger());
	}

	@Test
	void stopsGlobalDataTimerWhenHandlerIsReplaced() throws Exception {
		BackendProxyHandler handler = new BackendProxyHandler(null);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		Field field = BackendProxyHandler.class.getDeclaredField("timer");
		field.setAccessible(true);
		field.set(handler, timer);

		handler.releaseGlobalDataTimer();

		verify(timer).shutdownNow();
		assertNull(handler.getTimer());
	}
}
