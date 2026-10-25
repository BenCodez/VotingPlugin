package com.bencodez.votingplugin.tests.proxy.security;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.ProxyRuntimeReplacementLifecycle;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;
import com.bencodez.votingplugin.tests.neoforge.NeoForgeProxyOriginFixture;
import com.bencodez.votingplugin.tests.proxy.VotingPluginProxyAccess;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;

class ProxyOriginProducerTest {
    @TempDir Path directory;
    VotingPluginProxyTestImpl proxy;
    ProxyOriginEnvelopeSignature verifier;
    SharedTransportEnvelopeAuthenticator membership;

    @BeforeEach void setup() throws Exception {
        NeoForgeProxyOriginFixture.create(directory);
        verifier = NeoForgeProxyOriginFixture.verifier(directory);
        byte[] key = new byte[32];
        new java.security.SecureRandom().nextBytes(key);
        Files.writeString(directory.resolve("secretkey.key"), Base64.getEncoder().encodeToString(key));
        membership = SharedTransportEnvelopeAuthenticator.load(directory.resolve("secretkey.key"), Mode.REQUIRED);
        proxy = new VotingPluginProxyTestImpl();
        proxy.setDataFolder(directory.toFile());
        when(proxy.getConfig().getSpigotServers()).thenReturn(List.of("backend-test", "legacy"));
        when(proxy.getConfig().getSpigotServerConfiguration("backend-test")).thenReturn(
                Map.of("ProxySigningKeyFile", "synthetic-proxy-private.key"));
        when(proxy.getConfig().getSpigotServerConfiguration("legacy")).thenReturn(Map.of());
        when(proxy.getConfig().getProxyServerName()).thenReturn("proxy-test");
        when(proxy.getConfig().getRedisPrefix()).thenReturn("fixture/");
        when(proxy.getConfig().getMqttPrefix()).thenReturn("fixture/");
        Object captured = ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {BungeeMethod.MYSQL});
        set("proxyOriginSigners", captured);
        set("sharedTransportAuthenticator", membership);
        // Publication must use captured key bytes, not perform file I/O in a send callback.
        Files.delete(directory.resolve("synthetic-proxy-private.key"));
    }

    @Test void actualMysqlPublisherBindsCompleteEncryptedEnvelopeAndKeepsLegacyWire() throws Exception {
        var sql = mock(MySqlMessenger.class);
        set("proxyMysqlMessenger", sql);
        proxy.setMethod(BungeeMethod.MYSQL);
        var encryption = encryption();
        set("communicationEncryption", encryption);
        JsonEnvelope raw = localVote("雪😀\r\nAlex");
        assertTrue(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", raw));
        var capture = ArgumentCaptor.forClass(JsonEnvelope.class);
        verify(sql).sendToBackend(eq("backend-test"), capture.capture());
        JsonEnvelope proof = verifier.verify(roundTrip(capture.getValue()), ProxyOriginEnvelopeSignature.Domain.MYSQL,
                "proxy-test", "backend-test");
        assertNotNull(proof);
        assertEquals(wireFields(raw), encryption.decrypt(proof).envelope().getFields());
        assertNull(verifier.verify(capture.getValue(), ProxyOriginEnvelopeSignature.Domain.MYSQL, "proxy-test", "other"));
        assertTrue(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "legacy", raw));
        var legacy = ArgumentCaptor.forClass(JsonEnvelope.class);
        verify(sql).sendToBackend(eq("legacy"), legacy.capture());
        assertFalse(legacy.getValue().getFields().keySet().stream().anyMatch(k -> k.startsWith("_vpOrigin")));
    }

    private JsonEnvelope localVote(String player) throws Exception {
        var id = java.util.UUID.randomUUID();
        var totals = new VoteTotalsSnapshot();
        ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy, "stampLocalOriginTotals",
                new String[] {"UUID", "String", "String", "String", "long", "boolean", "boolean", "boolean",
                        "boolean", "boolean", "VoteTotalsSnapshot", "boolean"},
                new Object[] {id, player, id.toString(), "fixture-site", 1234L, true, true, false, false, false,
                        totals, true});
        return VotingPluginWire.vote(player, id.toString(), "fixture-site", 1234L, false, true,
                totals.toStorageString(), id, false, false, 1, 1);
    }

    private static Map<String, String> wireFields(JsonEnvelope raw) {
        var fields = new java.util.HashMap<>(raw.getFields());
        var totals = VoteTotalsSnapshot.parseStorage(fields.get("totals"));
        totals.setLocalProxyOrigin(false);
        fields.put("totals", totals.toStorageString());
        return fields;
    }

    @Test void replacementCannotGrantAuthorityToRecoveredUnclassifiedVotes() throws Exception {
        // A fresh runtime has keys but no old instance's shared-ingress flag.
        // This persisted row has the same shape as pre-provenance offline work.
        var recovered = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Alex",
                java.util.UUID.randomUUID().toString(), "fixture-site", 1234L, true,
                "v2//0//0//0//0//0//0//0//0", false);
        var sql = mock(MySqlMessenger.class);
        set("proxyMysqlMessenger", sql);
        proxy.setMethod(BungeeMethod.MYSQL);
        for (boolean online : List.of(false, true)) {
            JsonEnvelope replay = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                    "cachedVoteEnvelope", new String[] {"OfflineBungeeVote", "boolean", "boolean", "int", "int"},
                    new Object[] {recovered, online, false, 1, 1});
            assertFalse(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", replay),
                    "recovery cannot manufacture locally admitted provenance");
        }
        verifyNoInteractions(sql);
        assertFalse(recovered.isRewardDelivered());
    }

    @Test void actualRedisAndMqttPublishersComposeMembershipOriginAndEncryptionInConsumerOrder() throws Exception {
        var encryption = encryption();
        set("communicationEncryption", encryption);
        JsonEnvelope raw = localVote("Alex");
        var mqtt = mock(MqttHandler.class);
        set("mqttHandler", mqtt);
        assertTrue(proxy.sendMqttEnvelopeServer("backend-test", raw));
        var capture = ArgumentCaptor.forClass(JsonEnvelope.class);
        String topic = "fixture/votingplugin/servers/backend-test";
        verify(mqtt).publishEnvelope(eq(topic), capture.capture());
        assertConsumerLayers(raw, roundTrip(capture.getValue()), encryption,
                SharedTransportEnvelopeAuthenticator.Domain.MQTT_PROXY_BACKEND, ProxyOriginEnvelopeSignature.Domain.MQTT, topic);

        var pool = mock(JedisPool.class);
        var jedis = mock(Jedis.class);
        when(pool.getResource()).thenReturn(jedis);
        when(jedis.publish(anyString(), anyString())).thenReturn(1L);
        set("redisPublisherPool", pool);
        assertTrue(proxy.sendRedisEnvelopeServer("backend-test", raw));
        var channel = ArgumentCaptor.forClass(String.class);
        var payload = ArgumentCaptor.forClass(String.class);
        verify(jedis).publish(channel.capture(), payload.capture());
        assertConsumerLayers(raw, JsonEnvelopeCodec.decode(payload.getValue()), encryption,
                SharedTransportEnvelopeAuthenticator.Domain.REDIS_PROXY_BACKEND, ProxyOriginEnvelopeSignature.Domain.REDIS,
                channel.getValue());
    }

    @Test void alteredRecoveryFieldsAndBareFlagsCannotAcquireAuthorityOnAnyPublisher() throws Exception {
        JsonEnvelope local = localVote("Alex");
        var sql = mock(MySqlMessenger.class);
        set("proxyMysqlMessenger", sql);
        proxy.setMethod(BungeeMethod.MYSQL);
        var mqtt = mock(MqttHandler.class);
        set("mqttHandler", mqtt);
        var pool = mock(JedisPool.class);
        var jedis = mock(Jedis.class);
        when(pool.getResource()).thenReturn(jedis);
        when(jedis.publish(anyString(), anyString())).thenReturn(1L);
        set("redisPublisherPool", pool);
        for (String changed : List.of("voteId", "player", "uuid", "service", "time", "realVote",
                "wasOnline", "delayValidated", "totals")) {
            var fields = new java.util.HashMap<>(local.getFields());
            fields.put(changed, switch (changed) {
                case "voteId", "uuid" -> java.util.UUID.randomUUID().toString();
                case "time" -> "1235";
                case "realVote" -> "false";
                case "wasOnline", "delayValidated" -> "true";
                case "totals" -> fields.get("totals").replace("v2//0", "v2//99");
                default -> "changed";
            });
            JsonEnvelope altered = roundTrip(new JsonEnvelope(local.getSubChannel(), local.getSchema(), fields));
            assertFalse(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", altered), changed);
            assertFalse(proxy.sendRedisEnvelopeServer("backend-test", altered), changed);
            assertFalse(proxy.sendMqttEnvelopeServer("backend-test", altered), changed);
        }
        var bare = new VoteTotalsSnapshot(); bare.setLocalProxyOrigin(true);
        var fields = new java.util.HashMap<>(local.getFields()); fields.put("totals", bare.toString());
        assertFalse(proxy.sendMqttEnvelopeServer("backend-test", new JsonEnvelope("Vote", 1, fields)));
        verifyNoInteractions(sql, mqtt);
        verify(jedis, never()).publish(anyString(), anyString());
    }

    @Test void sharedWireCannotCarryEvenAnExistingLocalAdmissionProofIntoRetries() throws Exception {
        JsonEnvelope local = localVote("Alex");
        var authenticated = membership.sign(roundTrip(local),
                SharedTransportEnvelopeAuthenticator.Domain.REDIS_MULTI_PROXY, "claimed-proxy", "fixture/channel");
        var checked = membership.verify(roundTrip(authenticated),
                SharedTransportEnvelopeAuthenticator.Domain.REDIS_MULTI_PROXY, "fixture/channel");
        assertTrue(checked.accepted());
        var vote = VotingPluginWire.readVote(checked.envelope());
        var claimed = VoteTotalsSnapshot.parseStorage(vote.totals);
        assertTrue(claimed.isLocalProxyOrigin());
        set("voteCacheHandler", mock(com.bencodez.votingplugin.proxy.cache.VoteCacheHandler.class));
        set("enabled", false); // Inspect the retained admitted request before its async attempt.
        ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy, "receiveMultiProxyVote",
                new String[] {"String", "String", "boolean", "boolean", "long", "VoteTotalsSnapshot", "String", "UUID"},
                new Object[] {vote.player, vote.service, vote.realVote, false, vote.time, claimed, vote.uuid, vote.voteId});
        var retries = VotingPluginProxy.class.getDeclaredField("multiProxyVoteRetries"); retries.setAccessible(true);
        Object retry = ((Map<?, ?>) retries.get(proxy)).get(vote.voteId);
        assertNotNull(retry);
        var value = retry.getClass().getDeclaredField("totals"); value.setAccessible(true);
        var admitted = (VoteTotalsSnapshot) value.get(retry);
        assertFalse(admitted.isLocalProxyOrigin());
        assertTrue(admitted.getLocalOriginProof().isEmpty());
        assertTrue(claimed.isLocalProxyOrigin(), "untrusted decoded input is cloned, not mutated");
    }

    @Test void legitimateRecoveredVoteKeepsAuthorityAndDoesNotExportTheAdmissionProof() throws Exception {
        var local = localVote("Alex");
        var vote = VotingPluginWire.readVote(local);
        var recovered = new OfflineBungeeVote(vote.voteId, vote.player, vote.uuid, vote.service, vote.time,
                vote.realVote, vote.totals, false);
        recovered.setWasOnline(vote.wasOnline);
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql); proxy.setMethod(BungeeMethod.MYSQL);
        for (boolean online : List.of(false, true)) {
            var replay = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                    "cachedVoteEnvelope", new String[] {"OfflineBungeeVote", "boolean", "boolean", "int", "int"},
                    new Object[] {recovered, online, false, 1, 1});
            assertTrue(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", roundTrip(replay)));
        }
        var sent = ArgumentCaptor.forClass(JsonEnvelope.class); verify(sql, times(2)).sendToBackend(eq("backend-test"), sent.capture());
        for (var envelope : sent.getAllValues()) {
            var verified = verifier.verify(envelope, ProxyOriginEnvelopeSignature.Domain.MYSQL, "proxy-test", "backend-test");
            assertNotNull(verified);
            assertFalse(VoteTotalsSnapshot.parseStorage(verified.getFields().get("totals")).isLocalProxyOrigin());
        }
        assertTrue(VoteTotalsSnapshot.parseStorage(recovered.getText()).isLocalProxyOrigin());
        assertFalse(recovered.isRewardDelivered(), "a low-level publisher cannot retire the cache owner");
    }

    private com.bencodez.votingplugin.proxy.cache.VoteCacheHandler durableCache() {
        var platform = mock(com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee.class);
        when(platform.getDataFolder()).thenReturn(directory.toFile());
        var storage = new com.bencodez.votingplugin.proxy.bungee.BungeeJsonVoteCache(platform);
        return new com.bencodez.votingplugin.proxy.cache.VoteCacheHandler(null, false, false, null, false, storage) {
            @Override public void logInfo1(String message) { }
            @Override public void logSevere1(String message) { }
            @Override public void debug1(Exception failure) { }
            @Override public void debug1(Throwable failure) { }
            @Override public void debug1(String message) { }
        };
    }

    @Test void realLocalAndSharedAdmissionKeepDifferentAuthorityAfterDiskRecovery() throws Exception {
        var cache = durableCache(); set("voteCacheHandler", cache); set("enabled", true);
        proxy.setMethod(BungeeMethod.MYSQL); proxy.setPlayerOnline(false);
        proxy.setStandaloneBroadcastForwarding(false); proxy.setAvailableServers("backend-test");
        proxy.setGlobalMessageProxyHandlerForTest(mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class));
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getBlockedServers()).thenReturn(List.of());
        var localId = java.util.UUID.randomUUID(); String localPlayer = java.util.UUID.randomUUID().toString();
        assertDoesNotThrow(() -> proxy.vote("Alex", "fixture-site", true, false, 1234L, null, localPlayer, localId));
        var sharedId = java.util.UUID.randomUUID(); String sharedPlayer = java.util.UUID.randomUUID().toString();
        var forged = VoteTotalsSnapshot.parseStorage(localVote("Alex").getFields().get("totals"));
        ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy, "receiveMultiProxyVote",
                new String[] {"String", "String", "boolean", "boolean", "long", "VoteTotalsSnapshot", "String", "UUID"},
                new Object[] {"Alex", "fixture-site", true, false, 1234L, forged, sharedPlayer, sharedId});
        var replacementCache = durableCache(); replacementCache.load();
        var recoveredLocal = replacementCache.getOnlineVotes(localPlayer).get(0);
        var recoveredShared = replacementCache.getOnlineVotes(sharedPlayer).get(0);
        assertTrue(VoteTotalsSnapshot.parseStorage(recoveredLocal.getText()).isLocalProxyOrigin());
        assertFalse(VoteTotalsSnapshot.parseStorage(recoveredShared.getText()).isLocalProxyOrigin());
        assertEquals(localId, recoveredLocal.getVoteId());
        assertEquals(localPlayer, recoveredLocal.getUuid());
        assertEquals(1234L, recoveredLocal.getTime());
        assertTrue(recoveredLocal.isWasOnlineKnown());
        assertFalse(recoveredLocal.isWasOnline());
        assertTrue(recoveredLocal.isDelayValidationKnown(), "new rows persist explicit unvalidated classification");
        assertFalse(recoveredLocal.isDelayValidated());
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql);
        for (var vote : List.of(recoveredLocal, recoveredShared)) {
            var replay = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                    "cachedVoteEnvelope", new String[] {"OfflineBungeeVote", "boolean", "boolean", "int", "int"},
                    new Object[] {vote, true, false, 1, 1});
            var parsed = VotingPluginWire.readVote(replay);
            assertEquals(vote.isWasOnlineKnown(), parsed.wasOnlineKnown);
            assertEquals(vote.isWasOnline(), parsed.wasOnline);
            assertEquals(vote.isDelayValidationKnown(), parsed.delayValidationKnown);
            assertEquals(vote == recoveredLocal,
                    VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", roundTrip(replay)));
            assertFalse(vote.isRewardDelivered());
        }
        verify(sql, times(1)).sendToBackend(eq("backend-test"), any(JsonEnvelope.class));
    }

    @Test void copiedSqlCacheAdmissionCannotMintAnotherTargetDelivery() throws Exception {
        var keys = VotingPluginProxy.class.getDeclaredField("proxyOriginSigners"); keys.setAccessible(true);
        var captured = (Map<?, ?>) keys.get(proxy);
        set("proxyOriginSigners", Map.of("backend-test", captured.get("backend-test"),
                "other-backend", captured.get("backend-test")));
        JsonEnvelope copied = roundTrip(localVote("Alex"));
        when(proxy.getConfig().getVoteCacheUseMySQL()).thenReturn(true);
        proxy.setMethod(BungeeMethod.MYSQL);
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql);
        assertFalse(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "other-backend", copied),
                "shared cache bytes cannot prove proxy-owned target selection or unconsumed delivery");
        verifyNoInteractions(sql);
    }

    @Test void signingPreflightRejectsSqlVoteCacheBeforeReplacingRuntime() throws Exception {
        NeoForgeProxyOriginFixture.create(directory);
        when(proxy.getConfig().getVoteCacheUseMySQL()).thenReturn(true);
        for (BungeeMethod method : List.of(BungeeMethod.MYSQL, BungeeMethod.REDIS, BungeeMethod.MQTT)) {
            when(proxy.getConfig().getBungeeMethod()).thenReturn(method.name());
            assertThrows(IllegalStateException.class, proxy::validateReplacementTransportSecurity,
                    "signing requires a proxy-private pending-work owner before any retirement");
        }
        when(proxy.getConfig().getSpigotServerConfiguration("backend-test")).thenReturn(Map.of());
        assertDoesNotThrow(proxy::validateReplacementTransportSecurity,
                "absent signing preserves existing SQL-cache configurations");
        when(proxy.getConfig().getBungeeMethod()).thenReturn("SOCKETS");
        when(proxy.getConfig().getSpigotServerConfiguration("backend-test"))
                .thenReturn(Map.of("ProxySigningKeyFile", "missing-inactive.key"));
        assertDoesNotThrow(proxy::validateReplacementTransportSecurity,
                "inactive signing does not impose new cache restrictions");
    }

    @Test void activeSqlOwnerCannotBeReclassifiedByEditingConfiguration() throws Exception {
        var cache = mock(com.bencodez.votingplugin.proxy.cache.VoteCacheHandler.class);
        when(cache.usesMySQLStorage()).thenReturn(true); set("voteCacheHandler", cache);
        when(proxy.getConfig().getVoteCacheUseMySQL()).thenReturn(false);
        NeoForgeProxyOriginFixture.create(directory);
        when(proxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        assertThrows(IllegalStateException.class, proxy::validateReplacementTransportSecurity);
        proxy.setMethod(BungeeMethod.MYSQL);
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql);
        assertFalse(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test", localVote("Alex")));
        verifyNoInteractions(sql);
    }

    @Test void legitimateSignedLegacyDeliveryReportsActualAcceptance() throws Exception {
        proxy.setMethod(BungeeMethod.MYSQL);
        set("legacyVoteDeliveryServers", new java.util.HashSet<>(List.of("backend-test")));
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql);
        var handler = mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
        proxy.setGlobalMessageProxyHandlerForTest(handler);
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest("backend-test", 2, localVote("Alex")));
        verify(sql).sendToBackend(eq("backend-test"), any(JsonEnvelope.class));
        verifyNoInteractions(handler);
        set("proxyOriginSigners", Map.of());
        when(proxy.getConfig().getVoteCacheUseMySQL()).thenReturn(true);
        assertTrue(proxy.sendVoteEnvelopeAcceptedForTest("backend-test", 2, localVote("Blake")));
        verify(handler).sendMessage(eq("backend-test"), eq(2), any(JsonEnvelope.class));
    }

    @Test void legacySignedDeliveryDoesNotAcceptRejectedPublisher() throws Exception {
        proxy.setMethod(BungeeMethod.MYSQL);
        set("legacyVoteDeliveryServers", new java.util.HashSet<>(List.of("backend-test")));
        var sql = mock(MySqlMessenger.class); set("proxyMysqlMessenger", sql);
        var handler = mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
        proxy.setGlobalMessageProxyHandlerForTest(handler);
        doAnswer(call -> {
            assertFalse(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy,
                    call.getArgument(0), call.getArgument(2)));
            return null;
        }).when(handler).sendMessage(anyString(), anyInt(), any(JsonEnvelope.class));
        var fields = new java.util.HashMap<>(localVote("Alex").getFields());
        var totals = VoteTotalsSnapshot.parseStorage(fields.get("totals"));
        totals.setLocalProxyOrigin(false); fields.put("totals", totals.toStorageString());
        assertFalse(proxy.sendVoteEnvelopeAcceptedForTest("backend-test", 0,
                new JsonEnvelope(VotingPluginWire.SUB_VOTE, 1, fields)),
                "a void adapter must not turn signing refusal into cache retirement");
        verifyNoInteractions(sql);
    }

    @Test void emergencySnapshotRestampsFinalFieldsWhenPartyFailureLeavesNoTotals() throws Exception {
        proxy = spy(proxy);
        set("voteCacheHandler", durableCache()); set("enabled", true);
        proxy.setMethod(BungeeMethod.MYSQL); proxy.setPlayerOnline(false);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getBlockedServers()).thenReturn(List.of());
        doThrow(new IllegalStateException("synthetic party failure")).when(proxy).addVoteParty();
        var id = java.util.UUID.randomUUID(); var uuid = java.util.UUID.randomUUID().toString();
        assertThrows(RuntimeException.class,
                () -> proxy.vote("Alex", "fixture-site", true, false, 1234L, null, uuid, id));
        var saved = proxy.snapshotIncomingVoteForRestart(
                new com.bencodez.votingplugin.proxy.PendingIncomingVote(id, "Alex", "fixture-site", 1234L));
        assertEquals(uuid, saved.getUuid()); assertTrue(saved.isWasOnlineKnown());
        assertEquals(true, ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "hasLocalOriginProof", new String[] {"VoteTimeQueue"}, new Object[] {saved}),
                "proof must cover the final UUID and online state even before totals exist");
        assertTrue(proxy.retainIncomingVoteForRestart(saved));
        var recovered = durableCache(); recovered.load();
        var fromDisk = recovered.getTimeChangeQueue().stream()
                .filter(vote -> id.equals(vote.getVoteId())).findFirst().orElseThrow();
        assertEquals(uuid, fromDisk.getUuid());
        assertEquals(true, ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "hasLocalOriginProof", new String[] {"VoteTimeQueue"}, new Object[] {fromDisk}),
                "the complete emergency storage representation must retain finalized authority");
    }

    @Test void invalidConfiguredKeyCannotReplaceCapturedWorkingAuthority() throws Exception {
        when(proxy.getConfig().getSpigotServerConfiguration("backend-test"))
                .thenReturn(Map.of("ProxySigningKeyFile", "missing.key"));
        assertThrows(IllegalStateException.class, () -> ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {BungeeMethod.MYSQL}));
        // Preparation failure leaves the actual installed signer map intact.
        JsonEnvelope signed = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "signProxyOrigin", new String[] {"JsonEnvelope", "Domain", "String"},
                new Object[] {localVote("Alex"), ProxyOriginEnvelopeSignature.Domain.MYSQL, "backend-test"});
        assertNotNull(verifier.verify(signed, ProxyOriginEnvelopeSignature.Domain.MYSQL, "proxy-test", "backend-test"));
        assertEquals(Map.of(), ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {BungeeMethod.SOCKETS}),
                "inactive signing settings must not alter the established socket contract");
    }

    @Test void proxyOnlySigningCannotBeGrantedToSharedKeyMultiProxyIngress() throws Exception {
        NeoForgeProxyOriginFixture.create(directory);
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
        // REQUIRED membership still proves only possession of the backend-held key.
        JsonEnvelope forged = membership.sign(JsonEnvelope.builder("Vote").put("player", "Alex").build(),
                SharedTransportEnvelopeAuthenticator.Domain.REDIS_MULTI_PROXY, "claimed-proxy", "fixture/channel");
        assertTrue(membership.verify(roundTrip(forged),
                SharedTransportEnvelopeAuthenticator.Domain.REDIS_MULTI_PROXY, "fixture/channel").accepted());
        for (String ingress : List.of("REDIS", "SOCKETS")) {
            when(proxy.getConfig().getMultiProxyMethod()).thenReturn(ingress);
            for (BungeeMethod selected : List.of(BungeeMethod.MYSQL, BungeeMethod.REDIS, BungeeMethod.MQTT)) {
                assertThrows(IllegalStateException.class, () -> ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                        "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {selected}),
                        "a shared-key ingress must not become a proxy-only signing authority");
            }
        }
    }

    @Test void fullReplacementChecksMissingAndMalformedKeysBeforeRetiringHealthyRuntime() throws Exception {
        var previous = spy(proxy);
        when(previous.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        doNothing().when(previous).prepareForRuntimeReplacement();
        for (String file : List.of("missing.key", "malformed.key")) {
            if (file.equals("malformed.key")) Files.writeString(directory.resolve(file), "synthetic invalid key");
            when(previous.getConfig().getSpigotServerConfiguration("backend-test"))
                    .thenReturn(Map.of("ProxySigningKeyFile", file));
            assertThrows(IllegalStateException.class, () -> ProxyRuntimeReplacementLifecycle.prepare(previous));
            verify(previous, never()).prepareForRuntimeReplacement();
            verify(previous, never()).completeRuntimeReplacementShutdown();
            JsonEnvelope retained = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), previous,
                    "signProxyOrigin", new String[] {"JsonEnvelope", "Domain", "String"},
                    new Object[] {localVote("Alex"), ProxyOriginEnvelopeSignature.Domain.MYSQL,
                            "backend-test"});
            assertNotNull(verifier.verify(retained, ProxyOriginEnvelopeSignature.Domain.MYSQL,
                    "proxy-test", "backend-test"));
        }
    }

    @Test void enablingSharedIngressRejectsBeforeClosingExistingHandler() throws Exception {
        var previous = mock(MultiProxyHandler.class);
        set("multiProxyHandler", previous);
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
        for (String ingress : List.of("REDIS", "SOCKETS")) {
            when(proxy.getConfig().getMultiProxyMethod()).thenReturn(ingress);
            assertThrows(IllegalStateException.class, proxy::loadMultiProxySupport);
            verify(previous, never()).close();
        }
    }

    @Test void sharedIngressHistoryRejectsBothSignerPreparationAndLateInstallation() throws Exception {
        NeoForgeProxyOriginFixture.create(directory);
        Object prepared = ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {BungeeMethod.MYSQL});
        set("proxyOriginSigners", Map.of());
        set("sharedMultiProxyIngressSeen", true);
        // A disabled listener does not prove already-admitted asynchronous work retired.
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(false);
        assertThrows(IllegalStateException.class, () -> ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "createProxyOriginSigners", new String[] {"BungeeMethod"}, new Object[] {BungeeMethod.MYSQL}));
        assertThrows(IllegalStateException.class, () -> ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "installTransportSecurity", new String[] {"SharedTransportEnvelopeAuthenticator",
                        "TransportEnvelopeEncryption", "Map"}, new Object[] {membership, encryption(), prepared}));
        var sql = mock(MySqlMessenger.class);
        set("proxyMysqlMessenger", sql);
        proxy.setMethod(BungeeMethod.MYSQL);
        assertTrue(VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(proxy, "backend-test",
                localVote("Alex")));
        var sent = ArgumentCaptor.forClass(JsonEnvelope.class);
        verify(sql).sendToBackend(eq("backend-test"), sent.capture());
        assertNull(verifier.verify(sent.getValue(), ProxyOriginEnvelopeSignature.Domain.MYSQL,
                "proxy-test", "backend-test"), "rejected installation must not grant a signing capability");
    }

    @Test void signingPreflightAllowsSingleProxyAndPreservesCapturedGeneration() throws Exception {
        NeoForgeProxyOriginFixture.create(directory);
        when(proxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        when(proxy.getConfig().getMultiProxyMethod()).thenReturn("REDIS");
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(false);
        assertDoesNotThrow(proxy::validateReplacementTransportSecurity);
        // Validation reads prospective files but never replaces the working captured key.
        JsonEnvelope retained = (JsonEnvelope) ReflectiveAccess.call(VotingPluginProxy.class.getName(), proxy,
                "signProxyOrigin", new String[] {"JsonEnvelope", "Domain", "String"},
                new Object[] {localVote("Alex"), ProxyOriginEnvelopeSignature.Domain.MYSQL,
                        "backend-test"});
        assertNotNull(verifier.verify(retained, ProxyOriginEnvelopeSignature.Domain.MYSQL,
                "proxy-test", "backend-test"));
    }

    @Test void existingSharedMultiProxyWithoutNewSigningOptionRetainsPreflight() throws Exception {
        when(proxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
        when(proxy.getConfig().getMultiProxyMethod()).thenReturn("REDIS");
        when(proxy.getConfig().getSpigotServerConfiguration("backend-test")).thenReturn(Map.of());
        set("proxyOriginSigners", Map.of());
        set("sharedMultiProxyIngressSeen", true);
        for (String ingress : List.of("REDIS", "SOCKETS")) {
            when(proxy.getConfig().getMultiProxyMethod()).thenReturn(ingress);
            assertDoesNotThrow(proxy::validateReplacementTransportSecurity,
                    "the absent new option must preserve established shared-key configurations");
        }
    }

    private void assertConsumerLayers(JsonEnvelope raw, JsonEnvelope wire, TransportEnvelopeEncryption encryption,
            SharedTransportEnvelopeAuthenticator.Domain membershipDomain, ProxyOriginEnvelopeSignature.Domain originDomain,
            String target) {
        var member = membership.verify(wire, membershipDomain, target);
        assertTrue(member.accepted());
        var origin = verifier.verify(member.envelope(), originDomain, "proxy-test", "backend-test");
        assertNotNull(origin);
        var decrypted = encryption.decrypt(origin);
        assertTrue(decrypted.accepted());
        assertEquals(wireFields(raw), decrypted.envelope().getFields());
    }

    private TransportEnvelopeEncryption encryption() throws Exception {
        return TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
    }
    private static JsonEnvelope roundTrip(JsonEnvelope envelope) {
        return JsonEnvelopeCodec.decode(JsonEnvelopeCodec.encode(envelope));
    }
    private void set(String name, Object value) throws Exception {
        var field = VotingPluginProxy.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(proxy, value);
    }
}
