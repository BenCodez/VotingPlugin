package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

class NeoForgeProxyEnvelopeCodecTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeProxyEnvelopeCodec";

    @Test void completeSdkWrappersPreserveUnicodeEscapesAndFieldOrder() throws IOException {
        JsonEnvelope original = JsonEnvelope.builder("status").schema(1)
                .put("server", "backend-test").put("data", "Unicode \u2603 \ud83d\ude00, quotes \" braces {} and newline\n")
                .build();
        JsonEnvelope decoded = decode(JsonEnvelopeCodec.encode(original).getBytes(StandardCharsets.UTF_8));
        assertEquals(original.getSubChannel(), decoded.getSubChannel());
        assertEquals(original.getSchema(), decoded.getSchema());
        assertEquals(original.getFields(), decoded.getFields());
    }

    @Test void rejectsInvalidCompleteWrappers() {
        invalid().forEach(input -> assertThrows(IOException.class,
                () -> decode(input.getBytes(StandardCharsets.UTF_8))));
    }

    static Stream<String> invalid() {
        String prefix = "{\"t\":\"status\",\"v\":1,\"f\":";
        return Stream.of("", "[]", "{}", prefix + "{}}{}", prefix + "{\"server\":1}}",
                prefix + "{\"server\":null}}", prefix + "{\"server\":{\"x\":\"y\"}}}",
                prefix + "{\"server\":[]}}", prefix + "{\"a\":\"x\",\"a\":\"y\"}}",
                prefix + "{\"a\":\"x\",\"\\u0061\":\"y\"}}",
                prefix + "{},\"v\":2}", prefix + "{},\"extra\":true}",
                prefix.replace("\"v\":1", "\"v\":1.0") + "{}}",
                prefix.replace("\"v\":1", "\"v\":\"1\"") + "{}}",
                prefix + "{\"a\":\"" + "x".repeat(65_537) + "\"}}",
                prefix + "{\"" + "x".repeat(129) + "\":\"y\"}}",
                prefix + "[".repeat(2048) + "0" + "]".repeat(2048) + "}");
    }

    @Test void refusesMalformedUtf8AndOversizedByteInputBeforeJsonParsing() {
        assertThrows(IOException.class, () -> decode(new byte[] {(byte) 0xc3, 0x28}));
        assertThrows(IOException.class, () -> decode(new byte[256 * 1024 + 1]));
        assertThrows(IOException.class, () -> decode(null));
    }

    @Test void exactByteLimitUsesUtf8BytesAndAcceptsCompleteCrLfWrappedPayload() throws IOException {
        String content = "\r\n{\"t\":\"status\",\"v\":1,\"f\":{\"unicode\":\"\u2603\ud83d\ude00\"}}\r\n";
        int bytes = content.getBytes(StandardCharsets.UTF_8).length;
        byte[] maximum = (content + " ".repeat(256 * 1024 - bytes)).getBytes(StandardCharsets.UTF_8);
        assertEquals(256 * 1024, maximum.length);
        assertEquals("\u2603\ud83d\ude00", decode(maximum).getFields().get("unicode"));
        assertThrows(IOException.class, () -> decode(java.util.Arrays.copyOf(maximum, maximum.length + 1)));
    }

    @Test void exactFieldAndValueLimitsHaveValidCounterexamples() throws IOException {
        JsonEnvelope.Builder builder = JsonEnvelope.builder("status").schema(1);
        for (int i = 0; i < 128; i++) builder.put("key" + i, "value");
        assertEquals(128, decode(JsonEnvelopeCodec.encode(builder.build())
                .getBytes(StandardCharsets.UTF_8)).getFields().size());
        builder.put("extra", "value");
        assertThrows(IOException.class, () -> decode(JsonEnvelopeCodec.encode(builder.build())
                .getBytes(StandardCharsets.UTF_8)));
        JsonEnvelope largest = JsonEnvelope.builder("status").schema(1).put("value", "x".repeat(65_536)).build();
        assertEquals(65_536, decode(JsonEnvelopeCodec.encode(largest).getBytes(StandardCharsets.UTF_8))
                .getFields().get("value").length());
    }

    @Test void establishedCodecDefaultsRemainCompatibleWithoutInventingData() throws IOException {
        JsonEnvelope unversioned = decode("{\"t\":\"status\",\"f\":{\"server\":\"backend-test\"}}"
                .getBytes(StandardCharsets.UTF_8));
        assertEquals(1, unversioned.getSchema());
        assertEquals("backend-test", unversioned.getFields().get("server"));
        assertEquals(java.util.Map.of(), decode("{\"t\":\"status\"}".getBytes(StandardCharsets.UTF_8)).getFields());
    }

    private static JsonEnvelope decode(byte[] bytes) throws IOException {
        return (JsonEnvelope) ReflectiveAccess.call(OWNER, null, "decode", new String[] {"byte[]"},
                new Object[] {bytes});
    }
}
