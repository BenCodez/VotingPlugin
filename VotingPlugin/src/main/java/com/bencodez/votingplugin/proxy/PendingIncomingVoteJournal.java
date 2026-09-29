package com.bencodez.votingplugin.proxy;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/** Atomic, bounded last-resort journal for votes accepted during proxy lifecycle changes. */
public final class PendingIncomingVoteJournal {
	private static final int MAX_ENTRIES = 4096;
	private static final long MAX_BYTES = 4L * 1024L * 1024L;
	private final Path file;

	public PendingIncomingVoteJournal(Path dataDirectory) {
		this.file = dataDirectory.resolve("pending-incoming-votes-v1.json").toAbsolutePath().normalize();
	}

	public synchronized List<VoteTimeQueue> load() throws IOException {
		if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) return new ArrayList<>();
		if (Files.isSymbolicLink(file) || !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)
				|| Files.size(file) > MAX_BYTES) throw new IOException("Pending vote journal is invalid");
		JsonElement parsed = JsonParser.parseString(Files.readString(file, StandardCharsets.UTF_8));
		if (!parsed.isJsonArray() || parsed.getAsJsonArray().size() > MAX_ENTRIES) {
			throw new IOException("Pending vote journal exceeds its bounds");
		}
		List<VoteTimeQueue> votes = new ArrayList<>();
		for (JsonElement element : parsed.getAsJsonArray()) {
			if (!element.isJsonObject()) throw new IOException("Pending vote journal contains an invalid record");
			JsonObject value = element.getAsJsonObject();
			try {
				UUID voteId = UUID.fromString(required(value, "voteId", 36));
				VoteTimeQueue vote = new VoteTimeQueue(voteId, required(value, "player", 100),
						required(value, "service", 100), value.get("acceptedAt").getAsLong());
				vote.setUuid(optional(value, "uuid", 36));
				vote.setRealVote(!value.has("realVote") || value.get("realVote").getAsBoolean());
				if (value.has("wasOnlineKnown") && value.get("wasOnlineKnown").getAsBoolean()) {
					vote.setWasOnline(value.has("wasOnline") && value.get("wasOnline").getAsBoolean());
				}
				vote.setTotals(optional(value, "totals", 4096));
				vote.setVotePartyApplied(value.has("votePartyApplied")
						&& value.get("votePartyApplied").getAsBoolean());
				vote.setTotalsApplied(value.has("totalsApplied") && value.get("totalsApplied").getAsBoolean());
				vote.getBroadcastForwardedServers().addAll(VoteTimeQueue.decodeBroadcastForwardedServers(
						optional(value, "broadcastForwardedServers", 16384)));
				vote.setMultiProxyForwardingHandled(value.has("multiProxyForwardingHandled")
						&& value.get("multiProxyForwardingHandled").getAsBoolean());
				if (value.has("delayValidationKnown") && value.get("delayValidationKnown").getAsBoolean()) {
					vote.setDelayValidated(value.has("delayValidated") && value.get("delayValidated").getAsBoolean());
				}
				votes.add(vote);
			} catch (IllegalArgumentException | NullPointerException failure) {
				throw new IOException("Pending vote journal contains an invalid record", failure);
			}
		}
		return votes;
	}

	public synchronized void merge(Collection<VoteTimeQueue> additions) throws IOException {
		Map<UUID, VoteTimeQueue> merged = new LinkedHashMap<>();
		for (VoteTimeQueue existing : load()) merged.put(existing.getVoteId(), existing);
		for (VoteTimeQueue addition : additions) {
			if (addition != null && addition.getVoteId() != null) merged.put(addition.getVoteId(), addition);
		}
		if (merged.size() > MAX_ENTRIES) throw new IOException("Pending vote journal capacity is exhausted");
		write(merged.values());
	}

	public synchronized void replace(Collection<VoteTimeQueue> remaining) throws IOException {
		if (remaining.isEmpty()) {
			DurableFiles.deleteIfExists(file);
			return;
		}
		write(remaining);
	}

	private void write(Collection<VoteTimeQueue> votes) throws IOException {
		Path parent = file.getParent();
		Files.createDirectories(parent);
		JsonArray root = new JsonArray();
		for (VoteTimeQueue vote : votes) {
			JsonObject value = new JsonObject();
			value.addProperty("voteId", vote.getVoteId().toString());
			value.addProperty("player", vote.getName());
			value.addProperty("service", vote.getService());
			value.addProperty("acceptedAt", vote.getTime());
			value.addProperty("uuid", vote.getUuid());
			value.addProperty("realVote", vote.isRealVote());
			value.addProperty("wasOnline", vote.isWasOnline());
			value.addProperty("wasOnlineKnown", vote.isWasOnlineKnown());
			value.addProperty("totals", vote.getTotals());
			value.addProperty("votePartyApplied", vote.isVotePartyApplied());
			value.addProperty("totalsApplied", vote.isTotalsApplied());
			value.addProperty("broadcastForwardedServers", vote.encodeBroadcastForwardedServers());
			value.addProperty("multiProxyForwardingHandled", vote.isMultiProxyForwardingHandled());
			value.addProperty("delayValidated", vote.isDelayValidated());
			value.addProperty("delayValidationKnown", vote.isDelayValidationKnown());
			root.add(value);
		}
		byte[] bytes = root.toString().getBytes(StandardCharsets.UTF_8);
		if (bytes.length > MAX_BYTES) throw new IOException("Pending vote journal exceeds its byte bound");
		Path staged = Files.createTempFile(parent, file.getFileName().toString(), ".tmp");
		try {
			Files.write(staged, bytes);
			DurableFiles.publishStagedFile(staged, file);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	private static String required(JsonObject value, String key, int maxLength) throws IOException {
		if (!value.has(key)) throw new IOException("Pending vote journal is missing " + key);
		String result = value.get(key).getAsString();
		if (result.isEmpty() || result.length() > maxLength) throw new IOException("Invalid " + key);
		return result;
	}

	private static String optional(JsonObject value, String key, int maxLength) throws IOException {
		String result = value.has(key) ? value.get(key).getAsString() : "";
		if (result.length() > maxLength) throw new IOException("Invalid " + key);
		return result;
	}
}
