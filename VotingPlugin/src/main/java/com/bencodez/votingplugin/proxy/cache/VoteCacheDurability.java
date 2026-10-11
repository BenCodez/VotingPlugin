package com.bencodez.votingplugin.proxy.cache;

import java.nio.file.Path;
import java.nio.file.Files;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.util.Collection;
import java.util.Objects;
import java.util.function.BooleanSupplier;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;

/** Forces and reads back emergency JSON journal entries before they are exposed in memory. */
public final class VoteCacheDurability {
	public static final class ReloadFailedException extends IllegalStateException {
		private static final long serialVersionUID = 1L;

		private ReloadFailedException(Throwable saveFailure, Throwable reloadFailure) {
			super("Unable to restore the JSON vote journal after a failed durable save", saveFailure);
			addSuppressed(reloadFailure);
		}
	}

	private VoteCacheDurability() { }

	/** Reads a complete strict JSON object without silently recovering a partial journal. */
	public static JsonObject readJournal(Path path) throws IOException {
		try (JsonReader reader = new JsonReader(new StrictJsonInput(
				Files.newBufferedReader(path, StandardCharsets.UTF_8)))) {
			reader.setLenient(false);
			JsonElement document = readValue(reader, 0);
			if (!document.isJsonObject() || reader.peek() != JsonToken.END_DOCUMENT)
				throw new IOException("Vote cache must contain one JSON object");
			return document.getAsJsonObject();
		}
	}

	/** Enforces JSON lexical rules which older Gson non-lenient readers leave permissive. */
	private static final class StrictJsonInput extends java.io.FilterReader {
		private boolean quoted;
		private boolean escaped;
		private int unicode;
		private final StringBuilder token = new StringBuilder();

		StrictJsonInput(java.io.Reader input) { super(input); }

		@Override
		public int read(char[] buffer, int offset, int length) throws IOException {
			int count = super.read(buffer, offset, length);
			if (count < 0) finish();
			else for (int i = offset; i < offset + count; i++) validate(buffer[i]);
			return count;
		}

		@Override
		public int read() throws IOException {
			int character = super.read();
			if (character < 0) finish(); else validate((char) character);
			return character;
		}

		private void validate(char character) throws IOException {
			if (quoted) {
				if (unicode > 0) {
					if (Character.digit(character, 16) < 0 || character > 127) invalid();
					unicode--;
				} else if (escaped) {
					escaped = false;
					if (character == 'u') unicode = 4;
					else if ("\"\\/bfnrt".indexOf(character) < 0) invalid();
				} else if (character == '\\') escaped = true;
				else if (character == '"') quoted = false;
				else if (character < 32) invalid();
			} else if (character == '"') {
				finishToken(); quoted = true;
			} else if ("{}[],: \t\r\n".indexOf(character) >= 0) finishToken();
			else {
				if (token.length() >= 4096) invalid();
				token.append(character);
			}
		}

		private void finishToken() throws IOException {
			if (token.length() == 0) return;
			String value = token.toString(); token.setLength(0);
			if (!value.equals("true") && !value.equals("false") && !value.equals("null")
					&& !value.matches("-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?")) invalid();
		}

		private void finish() throws IOException {
			if (quoted || escaped || unicode > 0) invalid();
			finishToken();
		}

		private void invalid() throws IOException { throw new IOException("Invalid vote cache JSON syntax"); }
	}

	private static JsonElement readValue(JsonReader reader, int depth) throws IOException {
		if (depth > 64) throw new IOException("Vote cache JSON nesting exceeds the supported depth");
		switch (reader.peek()) {
		case BEGIN_OBJECT:
			JsonObject object = new JsonObject();
			reader.beginObject();
			while (reader.hasNext()) {
				String name = reader.nextName();
				if (object.has(name)) throw new IOException("Duplicate field in vote cache JSON");
				object.add(name, readValue(reader, depth + 1));
			}
			reader.endObject();
			return object;
		case BEGIN_ARRAY:
			JsonArray array = new JsonArray();
			reader.beginArray();
			while (reader.hasNext()) array.add(readValue(reader, depth + 1));
			reader.endArray();
			return array;
		case STRING: return new JsonPrimitive(reader.nextString());
		case NUMBER: return new JsonPrimitive(new java.math.BigDecimal(reader.nextString()));
		case BOOLEAN: return new JsonPrimitive(reader.nextBoolean());
		case NULL: reader.nextNull(); return JsonNull.INSTANCE;
		default: throw new IOException("Incomplete vote cache JSON");
		}
	}

	/** Commits all replay positions together and verifies their exact journal owners. */
	public static boolean saveAndVerifyCachedBatch(IVoteCache cache, String owner, boolean online,
			Collection<OfflineBungeeVote> expected) {
		return saveAndVerify(cache, () -> {
			for (OfflineBungeeVote vote : expected) {
				DataNode stored = online ? cache.getOnlineVotes(owner, vote.getOnlineVoteCacheJsonKey())
						: cache.getServerVotes(owner, vote.getServerVoteCacheJsonKey());
				if (!matches(stored, vote) || !sameField(stored, "Name", vote.getPlayerName())
						|| !sameField(stored, "Text", vote.getText()) || !stored.has("Real")
						|| stored.get("Real").asBoolean() != vote.isRealVote()
						|| !sameBoolean(stored, "WasOnline", vote.isWasOnline())
						|| !sameBoolean(stored, "WasOnlineKnown", vote.isWasOnlineKnown())
						|| !sameBoolean(stored, "DelayValidated", vote.isDelayValidated())
						|| !sameBoolean(stored, "DelayValidationKnown", vote.isQueueClassificationKnown())
						|| !sameBoolean(stored, "BroadcastForwarded", vote.isBroadcastForwarded())
						|| !sameBoolean(stored, "ProxyBroadcastHandled", vote.isProxyBroadcastHandled())
						|| !sameBoolean(stored, "RewardDelivered", vote.isRewardDelivered())
						|| !sameField(stored, "BroadcastTargets", vote.encodeBroadcastTargets())
						|| !sameField(stored, "BroadcastForwardedServers", vote.encodeBroadcastForwardedServers())
						|| !sameField(stored, "HttpDeliveryIds", vote.encodeHttpDeliveryIds())
						|| !sameField(stored, "HttpBroadcastDeliveryIds", vote.encodeHttpBroadcastDeliveryIds())
						|| !sameInteger(stored, "CachedBatchNumber", vote.getCachedBatchNumber())
						|| !sameInteger(stored, "CachedBatchSize", vote.getCachedBatchSize())
						|| !sameInteger(stored, "CachedBatchSourceRowId", vote.getCachedBatchSourceRowId()))
					return false;
			}
			return true;
		});
	}

	private static boolean sameField(DataNode data, String field, String expected) {
		return data.has(field) && Objects.equals(data.get(field).asString(), expected);
	}

	private static boolean sameBoolean(DataNode data, String field, boolean expected) {
		return data.has(field) && data.get(field).isPrimitive()
				&& data.get(field).asPrimitive() instanceof Boolean && data.get(field).asBoolean() == expected;
	}

	private static boolean sameInteger(DataNode data, String field, int expected) {
		if (!data.has(field) || !data.get(field).isPrimitive()
				|| !(data.get(field).asPrimitive() instanceof Number)) return false;
		try { return new java.math.BigDecimal(data.get(field).asString()).intValueExact() == expected; }
		catch (RuntimeException invalid) { return false; }
	}

	private static boolean saveAndVerify(IVoteCache cache, BooleanSupplier verify) {
		return saveAndVerify(cache, () -> null, ignored -> verify.getAsBoolean());
	}

	public static boolean saveAndVerifyServerVote(IVoteCache cache, String server, int index,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, () -> cache.getServerVotes(server, String.valueOf(index)), expected);
	}

	public static boolean saveAndVerifyOnlineVote(IVoteCache cache, String uuid, int index,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, () -> cache.getOnlineVotes(uuid, String.valueOf(index)), expected);
	}

	public static boolean saveAndVerifyTimeVote(IVoteCache cache, int index, VoteTimeQueue expected) {
		return saveAndVerify(cache, () -> cache.getTimedVoteCache(String.valueOf(index)),
				data -> matches(data, expected));
	}

	/** Publishes a cache deletion and verifies the removed identity remains absent after reload. */
	public static boolean saveAndVerifyRemoval(IVoteCache cache, BooleanSupplier isAbsent) {
		if (isAbsent == null) return false;
		return saveAndVerify(cache, isAbsent);
	}

	private static boolean saveAndVerify(IVoteCache cache, java.util.function.Supplier<DataNode> read,
			OfflineBungeeVote expected) {
		return saveAndVerify(cache, read, data -> matches(data, expected));
	}

	private static boolean saveAndVerify(IVoteCache cache, java.util.function.Supplier<DataNode> read,
			java.util.function.Predicate<DataNode> verify) {
		if (cache == null || verify == null) return false;
		Path path = cache.getStoragePath();
		if (path == null) return false;
		synchronized (cache) {
			try {
				try {
					cache.saveDurably();
				} catch (DurableFiles.PublishedException published) {
					// The new file is already active. Verify and keep its matching in-memory
					// state rather than rolling back and later overwriting the publication.
				}
				cache.reload();
				return verify.test(read.get());
			} catch (RuntimeException | java.io.IOException failure) {
				// addVote* mutates the JSON object before saveDurably runs. Discard that
				// uncommitted mutation so a later retry reuses the same slot instead of
				// eventually publishing both the failed attempt and its retry.
				try {
					cache.reload();
				} catch (RuntimeException reloadFailure) {
					throw new ReloadFailedException(failure, reloadFailure);
				}
				return false;
			}

		}
	}

	private static boolean matches(DataNode data, VoteTimeQueue expected) {
		if (expected == null || data == null || !data.isObject() || !data.has("UUID")
				|| !data.has("Service") || !data.has("Time")) return false;
		if (!expected.getUuid().equals(data.get("UUID").asString())
				|| !expected.getService().equals(data.get("Service").asString())
				|| expected.getTime() != data.get("Time").asLong()) return false;
		if (expected.getVoteId() == null) return true;
		String voteId = data.has("VoteId") ? data.get("VoteId").asString()
				: data.has("VoteID") ? data.get("VoteID").asString() : "";
		return expected.getVoteId().toString().equals(voteId);
	}

	private static boolean matches(DataNode data, OfflineBungeeVote expected) {
		if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service") || !data.has("Time"))
			return false;
		if (!expected.getUuid().equals(data.get("UUID").asString())
				|| !expected.getService().equals(data.get("Service").asString())
				|| expected.getTime() != data.get("Time").asLong()) return false;
		String voteId = data.has("VoteId") ? data.get("VoteId").asString()
				: data.has("VoteID") ? data.get("VoteID").asString() : "";
		return expected.getVoteId() == null ? voteId == null || voteId.isEmpty()
				: expected.getVoteId().toString().equals(voteId);
	}
}
