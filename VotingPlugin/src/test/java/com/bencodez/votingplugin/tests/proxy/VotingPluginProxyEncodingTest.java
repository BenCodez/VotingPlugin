package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.proxy.BungeeMethod;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;

class VotingPluginProxyEncodingTest {
    @Test
    void exactActualFrameLimitIsAcceptedAndOneAdditionalByteIsRejected() throws Exception {
        int overhead = frame(message(""), null).length;
        JsonEnvelope exact = message("x".repeat(32767 - overhead));
        assertEquals(32767, frame(exact, null).length);
        CapturingProxy accepted = new CapturingProxy();
        assertTrue(accepted.sendPluginMessageImmediately("backend-a", exact));
        assertEquals(32767, accepted.frames.get(0).length);

        JsonEnvelope oversized = message("x".repeat(32768 - overhead));
        assertEquals(32768, frame(oversized, null).length);
        CapturingProxy rejected = new CapturingProxy();
        assertFalse(rejected.sendPluginMessageImmediately("backend-a", oversized));
        assertTrue(rejected.frames.isEmpty());
    }

    @Test
    void supplementaryUnicodeCannotBypassTheModifiedUtfFrameLimit() throws Exception {
        JsonEnvelope envelope = message("\uD83D\uDE80".repeat(6000));
        assertTrue(JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8).length < 30000);
        assertTrue(frame(envelope, null).length > 32767);
        CapturingProxy proxy = new CapturingProxy();
        assertFalse(proxy.sendPluginMessageImmediately("backend-a", envelope));
        assertTrue(proxy.frames.isEmpty());
    }

    @Test
    void unicodeAndNullCharacterRoundTripPreservesTheDeclaredUtf8Length() throws Exception {
        JsonEnvelope envelope = JsonEnvelope.builder("Synthetic" + (char) 0 + "\u03A9")
                .put("data", "Za\u017C\u00F3\u0142\u0107" + (char) 0 + "\uD83D\uDE80").build();
        CapturingProxy proxy = new CapturingProxy();
        assertTrue(proxy.sendPluginMessageImmediately("backend-a", envelope));
        JsonEnvelope decoded = decode(proxy.frames.get(0), null);
        assertEquals(envelope.getSubChannel(), decoded.getSubChannel());
        assertEquals(envelope.getFields(), decoded.getFields());
    }

    @Test
    void legacyEncryptionExpansionIsIncludedBeforeAnyPlatformHandoff(@TempDir Path directory) throws Exception {
        EncryptionHandler encryption = new EncryptionHandler("Synthetic", syntheticKey(directory));
        JsonEnvelope oversized = message("x".repeat(26000));
        assertTrue(frame(oversized, null).length < 32767);
        assertTrue(frame(oversized, encryption).length > 32767);
        CapturingProxy proxy = new CapturingProxy();
        when(proxy.getConfig().getPluginMessageEncryption()).thenReturn(true);
        VotingPluginProxyPacingTest.setField(proxy, "encryptionHandler", encryption);
        assertFalse(proxy.sendPluginMessageImmediately("backend-a", oversized));
        assertTrue(proxy.frames.isEmpty());

        CapturingProxy small = new CapturingProxy();
        when(small.getConfig().getPluginMessageEncryption()).thenReturn(true);
        VotingPluginProxyPacingTest.setField(small, "encryptionHandler", encryption);
        JsonEnvelope expected = message("Synthetic encrypted payload \u03A9");
        assertTrue(small.sendPluginMessageImmediately("backend-a", expected));
        assertEquals(expected.getFields(), decode(small.frames.get(0), encryption).getFields());
    }

    @Test
    void envelopeEncryptionExpansionIsIncludedAndSmallMessagesStillDecrypt(@TempDir Path directory) throws Exception {
        TransportEnvelopeEncryption encryption = TransportEnvelopeEncryption.load(syntheticKey(directory).toPath(),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
        CapturingProxy proxy = new CapturingProxy();
        VotingPluginProxyPacingTest.setField(proxy, "communicationEncryption", encryption);
        assertFalse(proxy.sendPluginMessageImmediately("backend-a", message("x".repeat(26000))));
        assertTrue(proxy.frames.isEmpty());

        CapturingProxy small = new CapturingProxy();
        VotingPluginProxyPacingTest.setField(small, "communicationEncryption", encryption);
        JsonEnvelope expected = message("Synthetic encrypted payload");
        assertTrue(small.sendPluginMessageImmediately("backend-a", expected));
        TransportEnvelopeEncryption.Decryption decrypted = encryption.decrypt(decode(small.frames.get(0), null));
        assertTrue(decrypted.accepted());
        assertEquals(expected.getSubChannel(), decrypted.envelope().getSubChannel());
        assertEquals(expected.getFields(), decrypted.envelope().getFields());
    }

    @Test
    void encryptionChangeCannotStrandALaterValidControl(@TempDir Path directory) throws Exception {
        CapturingProxy proxy = new CapturingProxy();
        proxy.sendPluginMessageServer("backend-a", 1, message("x".repeat(26000)));
        proxy.sendPluginMessageServer("backend-a", 1, message("Synthetic next control"));
        TransportEnvelopeEncryption encryption = TransportEnvelopeEncryption.load(syntheticKey(directory).toPath(),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
        VotingPluginProxyPacingTest.setField(proxy, "communicationEncryption", encryption);
        proxy.time.advance(TimeUnit.SECONDS.toNanos(3));
        assertEquals(1, proxy.frames.size());
        TransportEnvelopeEncryption.Decryption decrypted = encryption.decrypt(decode(proxy.frames.get(0), null));
        assertTrue(decrypted.accepted());
        assertEquals("Synthetic next control", decrypted.envelope().getFields().get("data"));
    }

    private static java.io.File syntheticKey(Path directory) throws Exception {
        byte[] bytes = new byte[32];
        Arrays.fill(bytes, (byte) 7);
        Path key = directory.resolve("synthetic.key");
        Files.writeString(key, Base64.getEncoder().encodeToString(bytes), StandardCharsets.US_ASCII);
        return key.toFile();
    }

    private static JsonEnvelope message(String text) {
        return JsonEnvelope.builder("Synthetic").put("data", text).build();
    }

    private static byte[] frame(JsonEnvelope envelope, EncryptionHandler encryption) throws Exception {
        String payload = JsonEnvelopeCodec.encode(envelope);
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (DataOutputStream output = new DataOutputStream(bytes)) {
            output.writeUTF(encryption == null ? envelope.getSubChannel() : encryption.encrypt(envelope.getSubChannel()));
            output.writeInt(payload.getBytes(StandardCharsets.UTF_8).length);
            output.writeUTF(encryption == null ? payload : encryption.encrypt(payload));
        }
        return bytes.toByteArray();
    }

    private static JsonEnvelope decode(byte[] frame, EncryptionHandler encryption) throws Exception {
        try (DataInputStream input = new DataInputStream(new ByteArrayInputStream(frame))) {
            String subChannel = input.readUTF();
            int declaredLength = input.readInt();
            String payload = input.readUTF();
            if (encryption != null) {
                subChannel = encryption.decrypt(subChannel);
                payload = encryption.decrypt(payload);
            }
            assertEquals(payload.getBytes(StandardCharsets.UTF_8).length, declaredLength);
            JsonEnvelope decoded = JsonEnvelopeCodec.decode(payload);
            assertEquals(subChannel, decoded.getSubChannel());
            assertEquals(-1, input.read());
            return decoded;
        }
    }

    private static final class CapturingProxy extends VotingPluginProxyTestImpl {
        final VotingPluginProxyPacingTest.ManualTime time = new VotingPluginProxyPacingTest.ManualTime();
        final List<byte[]> frames = new ArrayList<>();

        CapturingProxy() throws Exception {
            setMethod(BungeeMethod.PLUGINMESSAGING);
            setSchedulerForTest(time.executor);
            VotingPluginProxyPacingTest.setField(this, "enabled", true);
        }

        @Override protected long pluginMessageNanoTime() { return time.now; }
        @Override public boolean sendPluginMessageData(String server, String channel, byte[] value, boolean queue) {
            assertFalse(queue);
            frames.add(value.clone());
            return true;
        }
    }
}
