package com.bencodez.votingplugin.proxy.multiproxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

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
	void redisSubsetSendPreservesConfiguredChannelCasingAndAppliesPrefixOnce(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Proxy2"));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		SharedTransportEnvelopeAuthenticator authenticator = authenticator(dataDirectory);
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator);
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		java.lang.reflect.Field connection = MultiProxyHandler.class.getDeclaredField("multiProxyRedis");
		connection.setAccessible(true);
		connection.set(handler, redis);
		JsonEnvelope envelope = JsonEnvelope.builder("vote").build();

		assertEquals(java.util.Set.of("Proxy2"), handler.getConfiguredMultiProxyVoteRecipients());
		assertTrue(handler.sendMultiProxyEnvelopeAccepted(envelope, List.of("proxy2")));

		org.mockito.ArgumentCaptor<JsonEnvelope> sent = org.mockito.ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(redis).publishEnvelope(org.mockito.ArgumentMatchers.eq("network-a:VotingPluginProxy_Proxy2"), sent.capture());
		assertTrue(authenticator.verify(sent.getValue(), Domain.REDIS_MULTI_PROXY, "network-a:VotingPluginProxy_Proxy2").accepted());
	}

	@Test
	void encryptedMultiProxyRedisEnvelopeIsSignedOutsideAndDecryptsToOriginal(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Proxy2"));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		SharedTransportEnvelopeAuthenticator authenticator = authenticator(dataDirectory);
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator);
		TransportEnvelopeEncryption encryption = TransportEnvelopeEncryption.load(
				dataDirectory.resolve("secretkey.key"), TransportEnvelopeEncryption.Domain.MULTI_PROXY, true);
		java.lang.reflect.Field cipher = MultiProxyHandler.class.getDeclaredField("communicationEncryption");
		cipher.setAccessible(true);
		cipher.set(handler, encryption);
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		java.lang.reflect.Field connection = MultiProxyHandler.class.getDeclaredField("multiProxyRedis");
		connection.setAccessible(true);
		connection.set(handler, redis);
		JsonEnvelope original = JsonEnvelope.builder("vote").put("player", "Alex").build();

		assertTrue(handler.sendMultiProxyEnvelopeAccepted(original, List.of("Proxy2")));

		org.mockito.ArgumentCaptor<JsonEnvelope> sent = org.mockito.ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(redis).publishEnvelope(org.mockito.ArgumentMatchers.eq("network-a:VotingPluginProxy_Proxy2"), sent.capture());
		JsonEnvelope authenticated = authenticator.verify(sent.getValue(), Domain.REDIS_MULTI_PROXY, "network-a:VotingPluginProxy_Proxy2").envelope();
		TransportEnvelopeEncryption.Decryption decrypted = encryption.decrypt(authenticated);
		assertTrue(decrypted.accepted());
		assertEquals(original.getSubChannel(), decrypted.envelope().getSubChannel());
		assertEquals(original.getFields(), decrypted.envelope().getFields());
	}

	@Test
	void reusedRedisConnectionSubscribesToTheSameSinglePrefixedChannel(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		com.bencodez.simpleapi.servercomm.redis.RedisListener listener =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisListener.class);
		org.mockito.Mockito.when(handler.getMultiProxySupportEnabled()).thenReturn(true);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getMultiProxyRedisUseExistingConnection()).thenReturn(true);
		org.mockito.Mockito.when(handler.getRedisHandler()).thenReturn(redis);
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of());
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator(dataDirectory));
		org.mockito.Mockito.when(redis.createEnvelopeListener(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any())).thenReturn(listener);
		org.mockito.Mockito.doAnswer(invocation -> {
			((Runnable) invocation.getArgument(0)).run();
			return null;
		}).when(handler).runAsnc(org.mockito.ArgumentMatchers.any());

		handler.loadMultiProxySupport();

		verify(redis).createEnvelopeListener(org.mockito.ArgumentMatchers.eq("network-a:VotingPluginProxy_Proxy1"),
				org.mockito.ArgumentMatchers.any());
		verify(redis).loadListener(listener);
	}

	@Test
	void compatibilityModeBridgesPrefixedAndLegacyMultiProxyChannels(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		com.bencodez.simpleapi.servercomm.redis.RedisListener prefixed =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisListener.class);
		com.bencodez.simpleapi.servercomm.redis.RedisListener legacy =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisListener.class);
		org.mockito.Mockito.when(handler.getMultiProxySupportEnabled()).thenReturn(true);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getMultiProxyRedisUseExistingConnection()).thenReturn(true);
		org.mockito.Mockito.when(handler.getRedisHandler()).thenReturn(redis);
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of());
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator())
				.thenReturn(authenticator(dataDirectory, Mode.COMPATIBILITY));
		org.mockito.Mockito.when(redis.createEnvelopeListener(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any())).thenReturn(prefixed, legacy);
		org.mockito.Mockito.doAnswer(invocation -> {
			((Runnable) invocation.getArgument(0)).run();
			return null;
		}).when(handler).runAsnc(org.mockito.ArgumentMatchers.any());

		handler.loadMultiProxySupport();

		verify(redis).createEnvelopeListener(org.mockito.ArgumentMatchers.eq("network-a:VotingPluginProxy_Proxy1"),
				org.mockito.ArgumentMatchers.any());
		verify(redis).createEnvelopeListener(org.mockito.ArgumentMatchers.eq("VotingPluginProxy_Proxy1"),
				org.mockito.ArgumentMatchers.any());
		verify(redis).loadListener(prefixed);
		verify(redis).loadListener(legacy);
	}

	@Test
	void compatibilityModeStartsBothRedisSubscriptionThreads(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				new com.bencodez.simpleapi.servercomm.redis.RedisHandler("127.0.0.1", 1, "", "", 0) {
					@Override public void debug(String message) { }
				};
		try {
			org.mockito.Mockito.when(handler.getMultiProxySupportEnabled()).thenReturn(true);
			org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
			org.mockito.Mockito.when(handler.getMultiProxyRedisUseExistingConnection()).thenReturn(true);
			org.mockito.Mockito.when(handler.getRedisHandler()).thenReturn(redis);
			org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
			org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
			org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of());
			org.mockito.Mockito.when(handler.getSharedTransportAuthenticator())
					.thenReturn(authenticator(dataDirectory, Mode.COMPATIBILITY));
			org.mockito.Mockito.doAnswer(invocation -> {
				((Runnable) invocation.getArgument(0)).run();
				return null;
			}).when(handler).runAsnc(org.mockito.ArgumentMatchers.any());

			handler.loadMultiProxySupport();

			java.lang.reflect.Field threadsField = redis.getClass().getSuperclass().getDeclaredField("listenerThreads");
			threadsField.setAccessible(true);
			@SuppressWarnings("unchecked")
			Map<com.bencodez.simpleapi.servercomm.redis.RedisListener, Thread> threads =
					(Map<com.bencodez.simpleapi.servercomm.redis.RedisListener, Thread>) threadsField.get(redis);
			assertEquals(java.util.Set.of("network-a:VotingPluginProxy_Proxy1", "VotingPluginProxy_Proxy1"),
					threads.keySet().stream().map(com.bencodez.simpleapi.servercomm.redis.RedisListener::getChannel)
							.collect(java.util.stream.Collectors.toSet()));
			assertTrue(threads.values().stream().allMatch(Thread::isAlive));
		} finally {
			redis.close();
		}
	}

	@Test
	void compatibilityModePublishesIdenticalSignedEnvelopeOnBothChannelNames(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Proxy2"));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy1");
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator())
				.thenReturn(authenticator(dataDirectory, Mode.COMPATIBILITY));
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		java.lang.reflect.Field connection = MultiProxyHandler.class.getDeclaredField("multiProxyRedis");
		connection.setAccessible(true);
		connection.set(handler, redis);

		assertTrue(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build(), List.of("Proxy2")));

		org.mockito.ArgumentCaptor<JsonEnvelope> sent = org.mockito.ArgumentCaptor.forClass(JsonEnvelope.class);
		verify(redis).publishEnvelope(org.mockito.ArgumentMatchers.eq("network-a:VotingPluginProxy_Proxy2"),
				sent.capture());
		verify(redis).publishEnvelope(org.mockito.ArgumentMatchers.eq("VotingPluginProxy_Proxy2"), sent.capture());
		assertEquals(sent.getAllValues().get(0).getFields(), sent.getAllValues().get(1).getFields());
	}

	@Test
	void unsignedBridgeCopiesAreDeduplicatedWithoutSuppressingVoteIdsOrSignedTraffic(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator())
				.thenReturn(authenticator(dataDirectory, Mode.COMPATIBILITY));
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy2");
		String prefixed = "network-a:VotingPluginProxy_Proxy2";
		String legacy = "VotingPluginProxy_Proxy2";
		JsonEnvelope clear = VotingPluginWire.clearVotePrimary("player-uuid", "Player", "Proxy1");

		handler.acceptRedisEnvelope(clear, prefixed);
		handler.acceptRedisEnvelope(clear, legacy);
		verify(handler).clearVote("player-uuid");
		handler.acceptRedisEnvelope(clear, prefixed);
		verify(handler, org.mockito.Mockito.times(2)).clearVote("player-uuid");

		JsonEnvelope withVoteId = clear.toBuilder().put(VotingPluginWire.K_VOTE_ID, UUID.randomUUID().toString()).build();
		handler.acceptRedisEnvelope(withVoteId, prefixed);
		handler.acceptRedisEnvelope(withVoteId, legacy);
		verify(handler, org.mockito.Mockito.times(4)).clearVote("player-uuid");

		SharedTransportEnvelopeAuthenticator signer = authenticator(dataDirectory);
		handler.acceptRedisEnvelope(signer.sign(clear, Domain.REDIS_MULTI_PROXY, "Proxy1", "network-a:VotingPluginProxy_Proxy2"), prefixed);
		handler.acceptRedisEnvelope(signer.sign(clear, Domain.REDIS_MULTI_PROXY, "Proxy1", legacy), legacy);
		verify(handler, org.mockito.Mockito.times(6)).clearVote("player-uuid");
	}

	@Test
	void unsignedBridgeWindowExpiresAndRetainsAtMostItsBound(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator())
				.thenReturn(authenticator(dataDirectory, Mode.COMPATIBILITY));
		org.mockito.Mockito.when(handler.getRedisPrefix()).thenReturn("network-a:");
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Proxy2");
		java.util.concurrent.atomic.AtomicLong now = new java.util.concurrent.atomic.AtomicLong(1L);
		org.mockito.Mockito.doAnswer(ignored -> now.get()).when(handler).unsignedBridgeNowNanos();
		String prefixed = "network-a:VotingPluginProxy_Proxy2";
		String legacy = "VotingPluginProxy_Proxy2";
		JsonEnvelope clear = VotingPluginWire.clearVotePrimary("player-uuid", "Player", "Proxy1");

		handler.acceptRedisEnvelope(clear, prefixed);
		handler.acceptRedisEnvelope(clear, legacy);
		now.addAndGet(java.util.concurrent.TimeUnit.SECONDS.toNanos(3));
		handler.acceptRedisEnvelope(clear, legacy);
		verify(handler, org.mockito.Mockito.times(2)).clearVote("player-uuid");

		for (int index = 0; index < 1100; index++) {
			handler.acceptRedisEnvelope(VotingPluginWire.clearVotePrimary("player-" + index, "Player", "Proxy1"),
					prefixed);
		}
		java.lang.reflect.Field entries = MultiProxyHandler.class.getDeclaredField("unsignedBridgeCopies");
		entries.setAccessible(true);
		assertTrue(((Map<?, ?>) entries.get(handler)).size() <= 1024);
	}

	@Test
	void unsignedMultiProxyVoteAndForgedAcknowledgementCannotMutateState(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator(dataDirectory));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		UUID voteId = UUID.randomUUID();

		handler.acceptRedisEnvelope(VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 1L, true, true, "", voteId,
				true, false, 1, 1, "Replica"));
		handler.acceptRedisEnvelope(VotingPluginWire.multiProxyVoteAck(voteId, "Primary", "Replica"));

		verify(handler, org.mockito.Mockito.never()).triggerVote(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean(),
				org.mockito.ArgumentMatchers.anyBoolean(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any(UUID.class), org.mockito.ArgumentMatchers.anyString());
		verify(handler, org.mockito.Mockito.never()).onMultiProxyVoteAcknowledged(
				org.mockito.ArgumentMatchers.any(UUID.class), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void authenticatedMultiProxyVoteIsAcceptedOnlyOnce(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		SharedTransportEnvelopeAuthenticator authenticator = authenticator(dataDirectory);
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator);
		UUID voteId = UUID.randomUUID();
		JsonEnvelope signed = authenticator.sign(VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 1L, true, true, "", voteId,
				true, false, 1, 1, "Replica"), Domain.REDIS_MULTI_PROXY, "Replica", "network-a:VotingPluginProxy_Proxy2");

		handler.acceptRedisEnvelope(signed, "network-a:VotingPluginProxy_Proxy2");
		handler.acceptRedisEnvelope(signed, "network-a:VotingPluginProxy_Proxy2");

		verify(handler, org.mockito.Mockito.times(1)).triggerVote(org.mockito.ArgumentMatchers.eq("Player"),
				org.mockito.ArgumentMatchers.eq("Service"), org.mockito.ArgumentMatchers.eq(true),
				org.mockito.ArgumentMatchers.eq(true), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class),
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"),
				org.mockito.ArgumentMatchers.eq(voteId), org.mockito.ArgumentMatchers.eq("Replica"));
	}

	@Test
	void copiedMultiProxyMessageCannotAuthenticateOnAnotherRecipientChannel(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		SharedTransportEnvelopeAuthenticator authenticator = authenticator(dataDirectory);
		org.mockito.Mockito.when(handler.getSharedTransportAuthenticator()).thenReturn(authenticator);
		JsonEnvelope signed = authenticator.sign(VotingPluginWire.clearVotePrimary("player-uuid", "Player", "Proxy1"),
				Domain.REDIS_MULTI_PROXY, "Proxy1", "network-a:VotingPluginProxy_ProxyA");

		handler.acceptRedisEnvelope(signed, "network-a:VotingPluginProxy_ProxyB");
		verify(handler, org.mockito.Mockito.never()).clearVote("player-uuid");
		handler.acceptRedisEnvelope(signed, "network-a:VotingPluginProxy_ProxyA");
		verify(handler).clearVote("player-uuid");
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
	void routesStableWireVoteWithoutOriginThroughLegacyTrigger() throws Exception {
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
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"));
		verify(handler, org.mockito.Mockito.never()).triggerVote(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean(),
				org.mockito.ArgumentMatchers.anyBoolean(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void legacyVoteEnvelopeHasNoReliableOriginMetadata() {
		UUID voteId = UUID.randomUUID();
		JsonEnvelope envelope = VotingPluginWire.vote("Player", "00000000-0000-0000-0000-000000000001",
				"Service", 100L, false, true, "totals", voteId, false, false, 1, 1);

		assertEquals(voteId.toString(), envelope.getFields().get(VotingPluginWire.K_VOTE_ID));
		assertFalse(envelope.getFields().containsKey(VotingPluginWire.K_MULTI_PROXY_ORIGIN));
	}

	@Test
	void forwardsOriginAndStableWireVoteIdToDurableTrigger() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", voteId,
				false, false, 1, 1, "Primary"));

		verify(handler).triggerVote(org.mockito.ArgumentMatchers.eq("Player"),
				org.mockito.ArgumentMatchers.eq("Service"), org.mockito.ArgumentMatchers.eq(true),
				org.mockito.ArgumentMatchers.eq(true), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class),
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"),
				org.mockito.ArgumentMatchers.eq(voteId), org.mockito.ArgumentMatchers.eq("Primary"));
	}

	@Test
	void rejectsReliableVoteEnvelopeWithoutStableId() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", null,
				false, false, 1, 1, "Primary"));

		verify(handler, org.mockito.Mockito.never()).triggerVote(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean(),
				org.mockito.ArgumentMatchers.anyBoolean(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
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
	void routesOnlyAuthenticatedTargetedRetirementMessages() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Replica");
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Primary"));
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Other", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Primary", "Other"));

		verify(handler).onMultiProxyVoteRetirementRequested(voteId, "Primary");
	}

	@Test
	void routesOnlyTheOriginatingProxyRetirementAcknowledgement() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetireAck(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetireAck(voteId, "Other", "Replica"));

		verify(handler).onMultiProxyVoteRetirementAcknowledged(voteId, "Replica");
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

		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void durableAcknowledgementCapabilityExpiresAndCanBeRenewed() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Capable"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS - 1,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS,
				2_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void expiredKnownCapabilityWaitsForRenewalInsteadOfBecomingLegacy() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Capable"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal());
	}

	@Test
	void durableCapabilityIdentitySurvivesRestartAndFencesAnOfflinePeer(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler beforeRestart = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handleCapability(beforeRestart, "Capable");
		assertEquals(java.util.Set.of("Capable"), beforeRestart.getMultiProxyVoteRecipients());

		MultiProxyHandler afterRestart = capabilityHandler(dataDirectory, "Capable", "Legacy");
		afterRestart.restoreVoteCapabilityPeers();

		assertTrue(afterRestart.getMultiProxyVoteRecipients().isEmpty());
		assertEquals(java.util.Set.of("Capable"),
				afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal());
		assertFalse(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().contains("Legacy"));
	}

	@Test
	void malformedCapabilityStateBlocksForwardingUntilTheOperatorRepairsItAndRestarts(@TempDir Path dataDirectory)
			throws Exception {
		Files.writeString(dataDirectory.resolve(".multiproxy-capability-peers.json"), "not-json");
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handler.restoreVoteCapabilityPeers();

		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		handleCapability(handler, "Capable");
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());

		// Removing the invalid local state and restarting deterministically returns
		// to fresh-install semantics: Legacy remains legacy and a new handshake
		// classifies only Capable as an ACK peer.
		Files.delete(dataDirectory.resolve(".multiproxy-capability-peers.json"));
		MultiProxyHandler recovered = capabilityHandler(dataDirectory, "Capable", "Legacy");
		recovered.restoreVoteCapabilityPeers();
		assertFalse(recovered.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(recovered.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		handleCapability(recovered, "Capable");
		assertEquals(java.util.Set.of("Capable"), recovered.getMultiProxyVoteRecipients());
		assertFalse(recovered.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().contains("Legacy"));
	}

	@Test
	void nonCanonicalCapabilityPeerBlocksForwardingInsteadOfBecomingLegacy(@TempDir Path dataDirectory)
			throws Exception {
		Files.writeString(dataDirectory.resolve(".multiproxy-capability-peers.json"),
				"{\"version\":1,\"peers\":[\"Capable\"]}");

		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handler.restoreVoteCapabilityPeers();

		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
	}

	@Test
	void freshInstallKeepsLegacyPeersOnTheHistoricalRoute(@TempDir Path dataDirectory) {
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Legacy");
		handler.restoreVoteCapabilityPeers();
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
	}

	@Test
	void newlyConfiguredPeerWaitsForCapabilityAnnouncementBeforeItIsClassified() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Candidate"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L);

		assertEquals(java.util.Set.of("Candidate"),
				handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		handleCapability(handler, "Candidate");

		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void missedCapabilityAnnouncementFallsBackOnlyAfterItsBoundedPersistedWindow(@TempDir Path dataDirectory) {
		MultiProxyHandler beforeRestart = capabilityHandler(dataDirectory, "Legacy");
		org.mockito.Mockito.when(beforeRestart.capabilityNowMillis()).thenReturn(1_000L);
		assertEquals(java.util.Set.of("Legacy"),
				beforeRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());

		MultiProxyHandler afterRestart = capabilityHandler(dataDirectory, "Legacy");
		afterRestart.restoreVoteCapabilityPeers();
		org.mockito.Mockito.when(afterRestart.capabilityNowMillis()).thenReturn(
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS);
		assertTrue(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		// The expired deadline remains a legacy classification rather than beginning
		// another discovery window on every retry/restart.
		assertTrue(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
	}

	@Test
	void expiredDiscoveryPersistsItsFinalObservationOnlyOnce(@TempDir Path dataDirectory) {
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Legacy");
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS + 1L);

		assertEquals(java.util.Set.of("Legacy"),
				handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		assertEquals(1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS,
				assertDoesNotThrow(() -> MultiProxyCapabilityStore.load(dataDirectory)).lastObservedMillis());

		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		assertEquals(1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS,
				assertDoesNotThrow(() -> MultiProxyCapabilityStore.load(dataDirectory)).lastObservedMillis());
	}

	@Test
	void discoveryRenewsBeforeItsBoundedFallbackInsteadOfAtExpiry() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Candidate"));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		org.mockito.Mockito.when(handler.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any())).thenReturn(true);
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L, 1_000L,
				11_000L, 11_000L, 21_000L, 21_000L, 30_000L, 31_000L);
		org.mockito.Mockito.clearInvocations(handler);

		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.renewMultiProxyVoteCapabilityDiscoveryIfDue());
		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.renewMultiProxyVoteCapabilityDiscoveryIfDue());
		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.renewMultiProxyVoteCapabilityDiscoveryIfDue());
		// This reply is asynchronous from the final early advertisement, but it is
		// still accepted before the durable fallback boundary.
		handleCapability(handler, "Candidate");
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipients());
		org.mockito.Mockito.verify(handler, org.mockito.Mockito.times(3))
				.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any());
	}

	@Test
	void clockRollbackExpiresDiscoveryInsteadOfExtendingItsPersistedWindow(@TempDir Path dataDirectory) {
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Candidate");
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L, 500L);

		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());

		MultiProxyCapabilityStore.State persisted = assertDoesNotThrow(() -> MultiProxyCapabilityStore.load(dataDirectory));
		assertEquals(500L, persisted.discoveryDeadlines().get("candidate"));
		assertEquals(500L, persisted.lastObservedMillis());
	}

	@Test
	void removedCapabilityPeerIsPrunedDurablyBeforeSameNameIsReadded(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler original = capabilityHandler(dataDirectory, "Capable");
		handleCapability(original, "Capable");

		MultiProxyHandler removed = capabilityHandler(dataDirectory, "Legacy");
		removed.restoreVoteCapabilityPeers();
		assertTrue(MultiProxyCapabilityStore.load(dataDirectory).peers().isEmpty());

		MultiProxyHandler readded = capabilityHandler(dataDirectory, "Capable");
		readded.restoreVoteCapabilityPeers();
		assertTrue(readded.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		assertEquals(java.util.Set.of("Capable"),
				readded.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
	}

	@Test
	void failedConfigurationPruningBlocksForwardingWithoutInstallingStalePeerState(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyCapabilityStore.save(dataDirectory, java.util.Set.of("capable"), Map.of(), 0L);
		MultiProxyHandler removed = capabilityHandler(dataDirectory, "Legacy");
		try (org.mockito.MockedStatic<MultiProxyCapabilityStore> store = org.mockito.Mockito.mockStatic(
				MultiProxyCapabilityStore.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
			store.when(() -> MultiProxyCapabilityStore.save(org.mockito.ArgumentMatchers.eq(dataDirectory),
					org.mockito.ArgumentMatchers.anyCollection(), org.mockito.ArgumentMatchers.anyMap(),
					org.mockito.ArgumentMatchers.anyLong()))
					.thenThrow(new java.io.IOException("read-only capability state"));
			removed.restoreVoteCapabilityPeers();
		}

		assertTrue(removed.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(removed.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		assertEquals(java.util.Set.of("capable"), MultiProxyCapabilityStore.load(dataDirectory).peers());
	}

	@Test
	void capabilityStoreRejectsSymlinkedState(@TempDir Path dataDirectory) throws Exception {
		Path outside = Files.createTempFile("multiproxy-capability-outside", ".json");
		try {
			Files.createSymbolicLink(dataDirectory.resolve(".multiproxy-capability-peers.json"), outside);
			assertThrows(java.io.IOException.class, () -> MultiProxyCapabilityStore.load(dataDirectory));
			MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable");
			handler.restoreVoteCapabilityPeers();
			handleCapability(handler, "Capable");
			assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
			assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
			assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		} finally {
			Files.deleteIfExists(outside);
		}
	}

	private static MultiProxyHandler capabilityHandler(Path dataDirectory, String... peers) {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of(peers));
		org.mockito.Mockito.when(handler.getPluginDataFolder()).thenReturn(dataDirectory.toFile());
		return handler;
	}

	private static SharedTransportEnvelopeAuthenticator authenticator(Path dataDirectory) throws Exception {
		return authenticator(dataDirectory, Mode.REQUIRED);
	}

	private static SharedTransportEnvelopeAuthenticator authenticator(Path dataDirectory, Mode mode) throws Exception {
		Path keyFile = dataDirectory.resolve("secretkey.key");
		Files.writeString(keyFile, Base64.getEncoder().encodeToString(
				"0123456789abcdef0123456789abcdef".getBytes(StandardCharsets.US_ASCII)));
		return SharedTransportEnvelopeAuthenticator.load(keyFile, mode);
	}

	private static void handleCapability(MultiProxyHandler handler, String peer) throws Exception {
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities(peer, 1, true));
	}

	@Test
	void renewalAdvertisementIsRateLimitedAfterAnInitialHandshake() {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS - 1,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS);
		org.mockito.Mockito.when(handler.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any())).thenReturn(true);
		org.mockito.Mockito.clearInvocations(handler);

		handler.announceMultiProxyVoteCapability();
		assertFalse(handler.renewMultiProxyVoteCapabilityIfDue());
		assertTrue(handler.renewMultiProxyVoteCapabilityIfDue());
		org.mockito.Mockito.verify(handler, org.mockito.Mockito.times(2))
				.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any());
	}
}
