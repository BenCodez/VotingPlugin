package com.bencodez.votingplugin.backendproxy;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
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
}
