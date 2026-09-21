package com.bencodez.votingplugin.proxy;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.util.DurableFiles;

/** Durable proxy outbox for reward-bearing votes awaiting backend completion. */
final class ReliableVoteDeliveryOutbox {
	static final int MAX_ENTRIES = 4096;
	private static final long MAX_FILE_BYTES = 16L * 1024L * 1024L;
	private static final String HEADER = "VP-VOTE-OUTBOX-2";
	private static final String ADD = "A";
	private static final String COMPLETED = "C";
	private static final String REMOVE = "R";

	record Entry(String server, JsonEnvelope envelope, boolean awaitingReceiptRelease) { }

	private final Path file;
	private final LinkedHashMap<String, Entry> entries = new LinkedHashMap<>();
	private int journalRecords;

	ReliableVoteDeliveryOutbox(Path file) throws IOException {
		this.file = file.toAbsolutePath().normalize();
		load();
	}

	synchronized boolean offer(String server, JsonEnvelope envelope) {
		String key = key(server, envelope);
		if (key == null) return false;
		if (entries.containsKey(key)) return true;
		if (entries.size() >= MAX_ENTRIES) return false;
		String record = addRecord(server, envelope);
		if (!prepareAppend(record) || !append(record)) return false;
		entries.put(key, new Entry(server, envelope, false));
		journalRecords++;
		return true;
	}

	synchronized boolean acknowledgeCompletion(String server, UUID voteId, String subChannel) {
		if (voteId == null || server == null || subChannel == null) return false;
		String key = normalized(server) + '|' + subChannel + '|' + voteId;
		Entry entry = entries.get(key);
		if (entry == null) return false;
		if (entry.awaitingReceiptRelease()) return true;
		String record = COMPLETED + '\t' + encode(key) + '\n';
		if (!prepareAppend(record) || !append(record)) return false;
		entries.put(key, new Entry(entry.server(), entry.envelope(), true));
		journalRecords++;
		return true;
	}

	synchronized boolean acknowledgeReceiptRelease(String server, UUID voteId, String subChannel) {
		if (voteId == null || server == null || subChannel == null) return false;
		String key = normalized(server) + '|' + subChannel + '|' + voteId;
		Entry entry = entries.get(key);
		if (entry == null || !entry.awaitingReceiptRelease()) return false;
		return remove(key);
	}

	synchronized boolean acknowledgeLegacyDelivery(String server, UUID voteId, String subChannel) {
		if (voteId == null || server == null || subChannel == null) return false;
		String key = normalized(server) + '|' + subChannel + '|' + voteId;
		Entry entry = entries.get(key);
		if (entry == null || entry.awaitingReceiptRelease()) return false;
		return remove(key);
	}

	private boolean remove(String key) {
		if (entries.size() == 1) {
			try {
				if (!DurableFiles.deleteIfExists(file) && Files.exists(file)) return false;
			} catch (IOException failure) {
				return false;
			}
			entries.clear();
			journalRecords = 0;
			return true;
		}
		String record = REMOVE + '\t' + encode(key) + '\n';
		if (!prepareAppend(record) || !append(record)) return false;
		entries.remove(key);
		journalRecords++;
		return true;
	}

	synchronized List<Entry> snapshot() {
		return new ArrayList<>(entries.values());
	}

	synchronized List<Entry> pendingVotes() {
		return entries.values().stream().filter(entry -> !entry.awaitingReceiptRelease()).toList();
	}

	synchronized List<Entry> pendingReceiptReleases() {
		return entries.values().stream().filter(Entry::awaitingReceiptRelease).toList();
	}

	synchronized int size() {
		return entries.size();
	}

	private void load() throws IOException {
		if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) return;
		if (Files.isSymbolicLink(file) || !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Vote delivery outbox is not a regular file");
		}
		if (Files.size(file) > MAX_FILE_BYTES) throw new IOException("Vote delivery outbox exceeds size limit");
		String content = Files.readString(file, StandardCharsets.UTF_8);
		String[] lines = content.split("\\n", -1);
		if (lines.length == 0 || !HEADER.equals(lines[0])) throw new IOException("Unsupported vote delivery outbox");
		boolean unterminatedTail = !content.endsWith("\n");
		int completeLineLimit = unterminatedTail ? lines.length - 1 : lines.length;
		for (int index = 1; index < completeLineLimit; index++) {
			if (lines[index].isBlank()) continue;
			String[] parts = lines[index].split("\\t", 3);
			try {
				if (parts.length == 3 && ADD.equals(parts[0])) {
					String server = decode(parts[1]);
					JsonEnvelope envelope = JsonEnvelopeCodec.decode(decode(parts[2]));
					String key = key(server, envelope);
					if (key == null) throw new IllegalArgumentException("Invalid vote envelope");
					entries.put(key, new Entry(server, envelope, false));
				} else if (parts.length == 2 && COMPLETED.equals(parts[0])) {
					String key = decode(parts[1]);
					Entry entry = entries.get(key);
					if (entry == null) throw new IllegalArgumentException("Completion without vote");
					entries.put(key, new Entry(entry.server(), entry.envelope(), true));
				} else if (parts.length == 2 && REMOVE.equals(parts[0])) {
					entries.remove(decode(parts[1]));
				} else throw new IllegalArgumentException("Unknown journal record");
			} catch (RuntimeException malformed) {
				throw new IOException("Malformed vote delivery outbox entry", malformed);
			}
			journalRecords++;
			if (entries.size() > MAX_ENTRIES) throw new IOException("Vote delivery outbox exceeds entry limit");
		}
		if (unterminatedTail && !compact()) throw new IOException("Unable to repair vote delivery outbox");
	}

	private boolean prepareAppend(String record) {
		try {
			long projectedBytes = (Files.exists(file) ? Files.size(file) : HEADER.length() + 1L)
					+ record.getBytes(StandardCharsets.UTF_8).length;
			boolean shouldCompact = journalRecords > entries.size() * 2 + 64;
			if ((projectedBytes > MAX_FILE_BYTES || shouldCompact) && compact()) {
				projectedBytes = Files.size(file) + record.getBytes(StandardCharsets.UTF_8).length;
			}
			return projectedBytes <= MAX_FILE_BYTES;
		} catch (IOException failure) {
			return false;
		}
	}

	private boolean append(String record) {
		try {
			Files.createDirectories(file.getParent());
			if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) {
				Path staged = file.resolveSibling(file.getFileName() + ".tmp");
				Files.writeString(staged, HEADER + '\n' + record, StandardCharsets.UTF_8,
						StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
				DurableFiles.publishStagedFile(staged, file);
			} else {
				Files.writeString(file, record, StandardCharsets.UTF_8, StandardOpenOption.APPEND,
						StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS);
				DurableFiles.forceFile(file);
			}
			return true;
		} catch (IOException failure) {
			return false;
		}
	}

	private boolean compact() {
		try {
			StringBuilder text = new StringBuilder(HEADER).append('\n');
			for (Entry entry : entries.values()) {
				text.append(addRecord(entry.server(), entry.envelope()));
				if (entry.awaitingReceiptRelease()) {
					text.append(COMPLETED).append('\t').append(encode(key(entry.server(), entry.envelope()))).append('\n');
				}
			}
			Path staged = file.resolveSibling(file.getFileName() + ".tmp");
			Files.writeString(staged, text, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.publishStagedFile(staged, file);
			journalRecords = entries.size() + (int) entries.values().stream()
					.filter(Entry::awaitingReceiptRelease).count();
			return true;
		} catch (IOException failure) {
			return false;
		}
	}

	private static String addRecord(String server, JsonEnvelope envelope) {
		return ADD + '\t' + encode(server) + '\t' + encode(JsonEnvelopeCodec.encode(envelope)) + '\n';
	}

	private static String key(String server, JsonEnvelope envelope) {
		if (server == null || server.isBlank() || envelope == null) return null;
		String subChannel = envelope.getSubChannel();
		if (!VotingPluginWire.SUB_VOTE.equals(subChannel)
				&& !VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)) return null;
		String voteId = envelope.getFields().get(VotingPluginWire.K_VOTE_ID);
		try {
			return normalized(server) + '|' + subChannel + '|' + UUID.fromString(voteId);
		} catch (RuntimeException invalid) {
			return null;
		}
	}

	private static String normalized(String value) {
		return value.trim().toLowerCase(Locale.ROOT);
	}

	private static String encode(String value) {
		return Base64.getUrlEncoder().withoutPadding().encodeToString(value.getBytes(StandardCharsets.UTF_8));
	}

	private static String decode(String value) {
		return new String(Base64.getUrlDecoder().decode(value), StandardCharsets.UTF_8);
	}
}
