package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

/** Bounded broker input, matching the flat SimpleAPI envelope wire representation. */
final class NeoForgeProxyEnvelopeCodec {
    static final int MAX_BYTES = 256 * 1024;
    private NeoForgeProxyEnvelopeCodec() { }

    static JsonEnvelope decode(byte[] payload) throws IOException {
        if (payload == null || payload.length == 0 || payload.length > MAX_BYTES) {
            throw new IOException("Invalid proxy envelope size");
        }
        String json;
        try {
            json = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(payload)).toString();
        } catch (CharacterCodingException invalid) {
            throw new IOException("Invalid proxy envelope encoding");
        }
        NeoForgeJsonStrings.validate(json);
        try (JsonReader reader = new JsonReader(new StringReader(json))) {
            reader.setLenient(false);
            reader.beginObject();
            String subchannel = null;
            // The established codec defaults an omitted version to schema 1 and omitted fields to empty.
            Integer schema = 1;
            Map<String, String> fields = Map.of();
            Set<String> seen = new HashSet<>();
            while (reader.hasNext()) {
                String name = reader.nextName();
                if (!seen.add(name)) throw new IOException("Duplicate envelope member");
                switch (name) {
                    case "t" -> subchannel = text(reader, 64);
                    case "v" -> {
                        if (reader.peek() != JsonToken.NUMBER) throw new IOException("Invalid envelope schema");
                        String number = reader.nextString();
                        if (!number.matches("[0-9]{1,9}")) throw new IOException("Invalid envelope schema");
                        schema = Integer.valueOf(number);
                    }
                    case "f" -> {
                        fields = new LinkedHashMap<>();
                        reader.beginObject();
                        while (reader.hasNext()) {
                            if (fields.size() >= 128) throw new IOException("Too many envelope fields");
                            String key = reader.nextName();
                            if (key.isEmpty() || key.length() > 128 || fields.containsKey(key)) {
                                throw new IOException("Invalid envelope field");
                            }
                            fields.put(key, text(reader, 64 * 1024));
                        }
                        reader.endObject();
                    }
                    default -> throw new IOException("Unknown envelope member");
                }
            }
            reader.endObject();
            if (reader.peek() != JsonToken.END_DOCUMENT || subchannel == null || subchannel.isBlank()
                    || schema == null || fields == null) throw new IOException("Incomplete proxy envelope");
            return new JsonEnvelope(subchannel, schema, fields);
        } catch (IllegalStateException | NumberFormatException invalid) {
            throw new IOException("Invalid proxy envelope");
        }
    }

    private static String text(JsonReader reader, int maximum) throws IOException {
        if (reader.peek() != JsonToken.STRING) throw new IOException("Envelope text must be a string");
        String value = reader.nextString();
        if (value.length() > maximum) throw new IOException("Envelope text is too long");
        return value;
    }
}
