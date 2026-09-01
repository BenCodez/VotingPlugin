package com.bencodez.votingplugin.backendproxy.http;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

/** Strict, versioned HTTP transport envelope.  Payloads remain canonical JsonEnvelopeCodec values. */
final class HttpTransportProtocol {
	static final int VERSION = 1;
	static final int MAX_BODY_BYTES = 256 * 1024;
	static final int MAX_BATCH = 64;
	static final int MAX_ENVELOPE_BYTES = 48 * 1024;
	static final int MAX_QUEUE = 1024;
	static final long MAX_CLOCK_SKEW_MILLIS = 90_000L;

	private HttpTransportProtocol() { }

	static void validateEnvelope(JsonEnvelope envelope) {
		if (envelope == null || JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8).length > MAX_ENVELOPE_BYTES) throw bad();
	}

	static byte[] request(String server, String session, long sequence, Collection<String> acks,
			Collection<Delivery> messages) {
		JsonObject root = base(server, session, sequence);
		root.add("acks", ids(acks));
		root.add("messages", messages(messages));
		byte[] encoded = root.toString().getBytes(StandardCharsets.UTF_8);
		if (encoded.length > MAX_BODY_BYTES) throw bad();
		return encoded;
	}

	static List<Delivery> fittingMessages(String server, String session, long sequence, Collection<String> acks,
			Collection<Delivery> candidates) {
		List<Delivery> output = new ArrayList<>();
		for (Delivery candidate : candidates) {
			if (output.size() == MAX_BATCH) break;
			output.add(candidate);
			try { request(server, session, sequence, acks, output); }
			catch (IllegalArgumentException tooLarge) { output.remove(output.size() - 1); break; }
		}
		return output;
	}

	static byte[] storedDelivery(Delivery delivery) {
		validId(delivery.id());
		validateEnvelope(delivery.envelope());
		JsonObject root = new JsonObject();
		root.addProperty("v", VERSION);
		root.addProperty("id", delivery.id());
		root.addProperty("payload", Base64.getUrlEncoder().withoutPadding().encodeToString(
				JsonEnvelopeCodec.encode(delivery.envelope()).getBytes(StandardCharsets.UTF_8)));
		return root.toString().getBytes(StandardCharsets.UTF_8);
	}

	static Delivery parseStoredDelivery(byte[] body) {
		if (body == null || body.length == 0 || body.length > MAX_ENVELOPE_BYTES * 2) throw bad();
		try {
			JsonObject root = JsonParser.parseString(new String(body, StandardCharsets.UTF_8)).getAsJsonObject();
			requireOnly(root, "v", "id", "payload");
			if (integer(root, "v") != VERSION) throw bad();
			String id = string(root, "id", 64); validId(id);
			byte[] payload = Base64.getUrlDecoder().decode(string(root, "payload", MAX_ENVELOPE_BYTES * 2));
			if (payload.length == 0 || payload.length > MAX_ENVELOPE_BYTES) throw bad();
			return new Delivery(id, JsonEnvelopeCodec.decode(new String(payload, StandardCharsets.UTF_8)));
		} catch (RuntimeException invalid) { throw bad(); }
	}

	static byte[] response(String server, String session, long sequence, Collection<String> acks,
			Collection<Delivery> messages) {
		return request(server, session, sequence, acks, messages);
	}

	static Packet parsePacket(byte[] body) {
		if (body == null || body.length == 0 || body.length > MAX_BODY_BYTES) throw bad();
		try {
			JsonElement parsed = JsonParser.parseString(new String(body, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw bad();
			JsonObject root = parsed.getAsJsonObject();
			requireOnly(root, "v", "server", "session", "sequence", "timestamp", "acks", "messages");
			if (integer(root, "v") != VERSION) throw bad();
			String server = HttpTlsIdentity.canonicalServerId(string(root, "server", 64));
			String session = uuid(root, "session");
			long sequence = nonNegative(root, "sequence");
			long timestamp = integer(root, "timestamp");
			long now = Instant.now().toEpochMilli();
			if (timestamp < now - MAX_CLOCK_SKEW_MILLIS || timestamp > now + MAX_CLOCK_SKEW_MILLIS) throw bad();
			List<String> acks = parseIds(root.get("acks"));
			List<Delivery> messages = parseMessages(root.get("messages"));
			return new Packet(server, session, sequence, acks, messages);
		} catch (RuntimeException invalid) { throw bad(); }
	}

	static byte[] enrollmentResponse(HttpTlsIdentity.IssuedClientCertificate certificate) {
		JsonObject output = new JsonObject();
		byte[] bundle = certificate.pkcs12();
		try { output.addProperty("bundle", Base64.getUrlEncoder().withoutPadding().encodeToString(bundle)); }
		finally { java.util.Arrays.fill(bundle, (byte) 0); }
		char[] password = certificate.password();
		try { output.addProperty("password", new String(password)); }
		finally { java.util.Arrays.fill(password, '\0'); }
		return output.toString().getBytes(StandardCharsets.UTF_8);
	}

	static Enrollment parseEnrollment(byte[] body) {
		if (body == null || body.length == 0 || body.length > 8192) throw bad();
		try {
			JsonElement parsed = JsonParser.parseString(new String(body, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw bad();
			JsonObject root = parsed.getAsJsonObject();
			requireOnly(root, "server", "token");
			String server = HttpTlsIdentity.canonicalServerId(string(root, "server", 64));
			String token = string(root, "token", 128);
			if (!token.matches("[A-Za-z0-9_-]{43,128}")) throw bad();
			return new Enrollment(server, token);
		} catch (RuntimeException invalid) { throw bad(); }
	}

	static byte[] renewalRequest(String server) {
		JsonObject root = new JsonObject();
		root.addProperty("server", HttpTlsIdentity.canonicalServerId(server));
		return root.toString().getBytes(StandardCharsets.UTF_8);
	}

	static String parseRenewal(byte[] body) {
		if (body == null || body.length == 0 || body.length > 1024) throw bad();
		try {
			JsonElement parsed = JsonParser.parseString(new String(body, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw bad();
			JsonObject root = parsed.getAsJsonObject();
			requireOnly(root, "server");
			return HttpTlsIdentity.canonicalServerId(string(root, "server", 64));
		} catch (RuntimeException invalid) { throw bad(); }
	}

	static HttpTlsIdentity.IssuedClientCertificate parseEnrollmentResponse(String server, byte[] body) {
		if (body == null || body.length == 0 || body.length > MAX_BODY_BYTES) throw bad();
		try {
			JsonObject root = JsonParser.parseString(new String(body, StandardCharsets.UTF_8)).getAsJsonObject();
			requireOnly(root, "bundle", "password");
			byte[] bundle = Base64.getUrlDecoder().decode(string(root, "bundle", MAX_BODY_BYTES * 2));
			char[] password = string(root, "password", 128).toCharArray();
			if (bundle.length == 0 || password.length < 40) throw bad();
			try { return new HttpTlsIdentity.IssuedClientCertificate(HttpTlsIdentity.canonicalServerId(server), null, bundle, password); }
			finally { java.util.Arrays.fill(bundle, (byte) 0); java.util.Arrays.fill(password, '\0'); }
		} catch (RuntimeException invalid) { throw bad(); }
	}

	private static JsonObject base(String server, String session, long sequence) {
		JsonObject root = new JsonObject();
		root.addProperty("v", VERSION); root.addProperty("server", server); root.addProperty("session", session);
		root.addProperty("sequence", sequence); root.addProperty("timestamp", Instant.now().toEpochMilli());
		return root;
	}
	private static JsonArray ids(Collection<String> values) {
		if (values == null || values.size() > MAX_BATCH) throw bad();
		JsonArray output = new JsonArray();
		for (String value : values) { validId(value); output.add(value); }
		return output;
	}
	private static JsonArray messages(Collection<Delivery> values) {
		if (values == null || values.size() > MAX_BATCH) throw bad();
		JsonArray output = new JsonArray();
		for (Delivery delivery : values) {
			validId(delivery.id());
			String encoded = JsonEnvelopeCodec.encode(delivery.envelope());
			byte[] bytes = encoded.getBytes(StandardCharsets.UTF_8);
			if (bytes.length > MAX_ENVELOPE_BYTES) throw bad();
			JsonObject item = new JsonObject(); item.addProperty("id", delivery.id());
			item.addProperty("payload", Base64.getUrlEncoder().withoutPadding().encodeToString(bytes)); output.add(item);
		}
		return output;
	}
	private static List<String> parseIds(JsonElement value) {
		if (value == null || !value.isJsonArray() || value.getAsJsonArray().size() > MAX_BATCH) throw bad();
		List<String> output = new ArrayList<>();
		for (JsonElement item : value.getAsJsonArray()) { if (!item.isJsonPrimitive()) throw bad(); String id = item.getAsString(); validId(id); output.add(id); }
		return output;
	}
	private static List<Delivery> parseMessages(JsonElement value) {
		if (value == null || !value.isJsonArray() || value.getAsJsonArray().size() > MAX_BATCH) throw bad();
		List<Delivery> output = new ArrayList<>();
		for (JsonElement item : value.getAsJsonArray()) {
			if (!item.isJsonObject()) throw bad(); JsonObject object = item.getAsJsonObject(); requireOnly(object, "id", "payload");
			String id = string(object, "id", 64); validId(id);
			byte[] payload = Base64.getUrlDecoder().decode(string(object, "payload", MAX_ENVELOPE_BYTES * 2));
			if (payload.length == 0 || payload.length > MAX_ENVELOPE_BYTES) throw bad();
			JsonEnvelope envelope = JsonEnvelopeCodec.decode(new String(payload, StandardCharsets.UTF_8));
			output.add(new Delivery(id, envelope));
		}
		return output;
	}
	private static void requireOnly(JsonObject object, String... names) {
		for (String name : object.keySet()) { boolean found = false; for (String allowed : names) if (allowed.equals(name)) { found = true; break; } if (!found) throw bad(); }
		for (String name : names) if (!object.has(name) || object.get(name).isJsonNull()) throw bad();
	}
	private static String string(JsonObject object, String name, int max) { JsonElement v = object.get(name); if (!v.isJsonPrimitive() || !v.getAsJsonPrimitive().isString()) throw bad(); String value = v.getAsString(); if (value.isEmpty() || value.length() > max) throw bad(); return value; }
	private static long integer(JsonObject object, String name) {
		try {
			JsonElement value = object.get(name);
			if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isNumber()) throw bad();
			String token = value.getAsString();
			if (!token.matches("-?(?:0|[1-9][0-9]*)")) throw bad();
			return Long.parseLong(token);
		} catch (RuntimeException failure) { throw bad(); }
	}
	private static long nonNegative(JsonObject object, String name) { long n = integer(object, name); if (n < 0) throw bad(); return n; }
	private static String uuid(JsonObject object, String name) { return canonicalUuid(string(object, name, 64)); }
	private static void validId(String id) { if (id == null || id.length() > 64) throw bad(); canonicalUuid(id); }
	private static String canonicalUuid(String value) {
		try {
			UUID parsed = UUID.fromString(value);
			if (!parsed.toString().equals(value)) throw bad();
			return value;
		} catch (IllegalArgumentException invalid) { throw bad(); }
	}
	private static IllegalArgumentException bad() { return new IllegalArgumentException("Invalid HTTP transport message"); }

	record Delivery(String id, JsonEnvelope envelope) { }
	record Packet(String server, String session, long sequence, List<String> acks, List<Delivery> messages) { }
	record Enrollment(String server, String token) { }
}
