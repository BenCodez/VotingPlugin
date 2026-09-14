package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Stores only the stable identities of peers that have completed the
 * durable-delivery capability handshake and the bounded discovery deadlines
 * for peers which have not advertised yet. A lease is deliberately not stored:
 * every process must renew a peer's current handshake before publishing a
 * durable outbox retry.
 */
final class MultiProxyCapabilityStore {
	static final String FILE_NAME = ".multiproxy-capability-peers.json";
	private static final int VERSION = 2;
	private static final int LEGACY_VERSION = 1;
	private static final int MAX_BYTES = 64 * 1024;
	private static final int MAX_PEERS = 256;
	private static final int MAX_PEER_LENGTH = 256;

	private MultiProxyCapabilityStore() { }

	static State load(Path dataDirectory) throws IOException {
		Path target = target(dataDirectory);
		if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) return State.empty();
		if (Files.isSymbolicLink(target) || !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)
				|| Files.size(target) > MAX_BYTES) throw new IOException("Multi-proxy capability state is unsafe");
		byte[] bytes;
		try (SeekableByteChannel channel = Files.newByteChannel(target,
				Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
			ByteBuffer buffer = ByteBuffer.allocate((int) Files.size(target) + 1);
			while (channel.read(buffer) >= 0 && buffer.hasRemaining()) { }
			if (!buffer.hasRemaining()) throw new IOException("Multi-proxy capability state is too large");
			buffer.flip();
			bytes = new byte[buffer.remaining()];
			buffer.get(bytes);
		}
		try {
			JsonElement parsed = JsonParser.parseString(new String(bytes, StandardCharsets.UTF_8));
			if (!parsed.isJsonObject()) throw invalid();
			JsonObject root = parsed.getAsJsonObject();
			if (!root.has("version") || (root.get("version").getAsInt() != VERSION
					&& root.get("version").getAsInt() != LEGACY_VERSION)
					|| !root.has("peers") || !root.get("peers").isJsonArray()) throw invalid();
			JsonArray peers = root.getAsJsonArray("peers");
			if (peers.size() > MAX_PEERS) throw invalid();
			Set<String> restored = new LinkedHashSet<>();
			for (JsonElement peer : peers) {
				if (!peer.isJsonPrimitive() || !peer.getAsJsonPrimitive().isString()) throw invalid();
				String name = peer.getAsString();
				if (!validPeer(name) || !restored.add(name)) throw invalid();
			}
			Map<String, Long> discoveryDeadlines = new LinkedHashMap<>();
			if (root.has("discoveryDeadlines")) {
				if (!root.get("discoveryDeadlines").isJsonObject()) throw invalid();
				JsonObject storedDeadlines = root.getAsJsonObject("discoveryDeadlines");
				if (storedDeadlines.size() > MAX_PEERS) throw invalid();
				for (Map.Entry<String, JsonElement> entry : storedDeadlines.entrySet()) {
					if (!validPeer(entry.getKey()) || !entry.getValue().isJsonPrimitive()
							|| !entry.getValue().getAsJsonPrimitive().isNumber()) throw invalid();
					long deadline = entry.getValue().getAsLong();
					if (deadline <= 0L || discoveryDeadlines.put(entry.getKey(), deadline) != null) throw invalid();
				}
			}
			long lastObservedMillis = 0L;
			if (root.has("lastObservedMillis")) {
				if (!root.get("lastObservedMillis").isJsonPrimitive()
						|| !root.get("lastObservedMillis").getAsJsonPrimitive().isNumber()) throw invalid();
				lastObservedMillis = root.get("lastObservedMillis").getAsLong();
				if (lastObservedMillis < 0L) throw invalid();
			}
			return new State(restored, discoveryDeadlines, lastObservedMillis);
		} catch (RuntimeException malformed) {
			throw new IOException("Multi-proxy capability state is malformed", malformed);
		}
	}

	static void save(Path dataDirectory, Collection<String> peers) throws IOException {
		save(dataDirectory, peers, Map.of());
	}

	static void save(Path dataDirectory, Collection<String> peers, Map<String, Long> discoveryDeadlines)
			throws IOException {
		save(dataDirectory, peers, discoveryDeadlines, 0L);
	}

	static void save(Path dataDirectory, Collection<String> peers, Map<String, Long> discoveryDeadlines,
			long lastObservedMillis) throws IOException {
		if (dataDirectory == null || peers == null || discoveryDeadlines == null || peers.size() > MAX_PEERS
				|| discoveryDeadlines.size() > MAX_PEERS || lastObservedMillis < 0L)
			throw new IOException("Invalid capability state");
		Set<String> copy = new LinkedHashSet<>();
		for (String peer : peers) {
			if (!validPeer(peer) || !copy.add(peer)) throw new IOException("Invalid capability peer");
		}
		JsonObject root = new JsonObject();
		root.addProperty("version", VERSION);
		JsonArray listed = new JsonArray();
		copy.stream().sorted().forEach(listed::add);
		root.add("peers", listed);
		JsonObject storedDeadlines = new JsonObject();
		for (String peer : discoveryDeadlines.keySet().stream().sorted().toList()) {
			Long deadline = discoveryDeadlines.get(peer);
			if (!validPeer(peer) || deadline == null || deadline <= 0L) {
				throw new IOException("Invalid capability discovery state");
			}
			storedDeadlines.addProperty(peer, deadline);
		}
		root.add("discoveryDeadlines", storedDeadlines);
		root.addProperty("lastObservedMillis", lastObservedMillis);
		byte[] serialized = root.toString().getBytes(StandardCharsets.UTF_8);
		if (serialized.length > MAX_BYTES) throw new IOException("Multi-proxy capability state is too large");

		Files.createDirectories(dataDirectory);
		Path target = target(dataDirectory);
		if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)
				&& (Files.isSymbolicLink(target) || !Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS))) {
			throw new IOException("Multi-proxy capability state is unsafe");
		}
		Path staging = Files.createTempFile(dataDirectory, ".multiproxy-capabilities-", ".json");
		try {
			Files.write(staging, serialized, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.publishStagedFile(staging, target);
		} finally {
			Files.deleteIfExists(staging);
		}
	}

	private static Path target(Path dataDirectory) {
		if (dataDirectory == null) throw new IllegalArgumentException("dataDirectory");
		return dataDirectory.resolve(FILE_NAME);
	}

	private static boolean validPeer(String peer) {
		return peer != null && !peer.isBlank() && peer.length() <= MAX_PEER_LENGTH
				&& peer.equals(peer.toLowerCase(Locale.ROOT));
	}

	private static IllegalArgumentException invalid() {
		return new IllegalArgumentException("Invalid capability state");
	}

	record State(Set<String> peers, Map<String, Long> discoveryDeadlines, long lastObservedMillis) {
		State {
			peers = Set.copyOf(peers);
			discoveryDeadlines = Map.copyOf(discoveryDeadlines);
		}

		static State empty() {
			return new State(Set.of(), Map.of(), 0L);
		}
	}
}
