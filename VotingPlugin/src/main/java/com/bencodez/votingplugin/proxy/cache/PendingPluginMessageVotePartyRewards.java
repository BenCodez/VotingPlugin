package com.bencodez.votingplugin.proxy.cache;

import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/** Immutable, bounded ownership of plugin-message vote-party handoffs. */
public record PendingPluginMessageVotePartyRewards(List<Delivery> deliveries) {

	public static final int MAX_DELIVERIES = 1_024;
	public static final int MAX_SERVERS = 256;
	public static final int MAX_PER_SERVER = 64;
	public static final int MAX_SERVER_BYTES = 256;
	public static final int MAX_SERIALIZED_BYTES = 512 * 1_024;
	private static final Gson JSON = new GsonBuilder().setPrettyPrinting().create();
	private static final Set<String> FIELDS = Set.of("batchId", "deliveryId", "server", "state");
	private static final PendingPluginMessageVotePartyRewards EMPTY =
			new PendingPluginMessageVotePartyRewards(List.of());

	public enum State { READY, HANDOFF_FENCED }

	public record Delivery(UUID batchId, UUID deliveryId, String server, State state) {
		public Delivery {
			if (batchId == null || deliveryId == null || state == null || server == null)
				throw new IllegalArgumentException("Missing plugin-message vote-party delivery field");
			// Keep the configured routing name exact; capacity keys are normalized separately.
			if (server.trim().isEmpty() || server.codePoints().anyMatch(Character::isISOControl)
					|| utf8Size(server) > MAX_SERVER_BYTES)
				throw new IllegalArgumentException("Invalid plugin-message vote-party server");
		}
	}

	public PendingPluginMessageVotePartyRewards {
		if (deliveries == null || deliveries.size() > MAX_DELIVERIES)
			throw new IllegalArgumentException("Invalid plugin-message vote-party delivery count");
		List<Delivery> copied = new ArrayList<>(deliveries.size());
		Set<UUID> ids = new HashSet<>();
		Map<String, Integer> servers = new HashMap<>();
		for (Delivery delivery : deliveries) {
			if (delivery == null || !ids.add(delivery.deliveryId()))
				throw new IllegalArgumentException("Duplicate or missing plugin-message vote-party delivery");
			if (servers.merge(delivery.server().toLowerCase(Locale.ROOT), 1, Integer::sum) > MAX_PER_SERVER
					|| servers.size() > MAX_SERVERS)
				throw new IllegalArgumentException("Plugin-message vote-party server backlog exceeds limit");
			copied.add(delivery);
		}
		deliveries = List.copyOf(copied);
		JsonObject party = new JsonObject();
		party.add("PluginMessageRewards", encode(deliveries));
		JsonObject namespace = new JsonObject();
		namespace.add("VoteParty", party);
		if (utf8Size(JSON.toJson(namespace)) > MAX_SERIALIZED_BYTES)
			throw new IllegalArgumentException("Plugin-message vote-party backlog exceeds byte limit");
	}

	public static PendingPluginMessageVotePartyRewards empty() { return EMPTY; }

	public Optional<Delivery> find(UUID deliveryId) {
		if (deliveryId == null) throw new IllegalArgumentException("Missing delivery ID");
		return deliveries.stream().filter(delivery -> delivery.deliveryId().equals(deliveryId)).findFirst();
	}

	/** Replaces only the state of an existing identity, without changing its position. */
	public PendingPluginMessageVotePartyRewards with(Delivery delivery) {
		if (delivery == null) throw new IllegalArgumentException("Missing delivery");
		List<Delivery> changed = new ArrayList<>(deliveries);
		for (int index = 0; index < changed.size(); index++) {
			Delivery existing = changed.get(index);
			if (!existing.deliveryId().equals(delivery.deliveryId())) continue;
			if (!existing.batchId().equals(delivery.batchId()) || !existing.server().equals(delivery.server()))
				throw new IllegalArgumentException("Conflicting plugin-message vote-party delivery identity");
			changed.set(index, delivery);
			return new PendingPluginMessageVotePartyRewards(changed);
		}
		changed.add(delivery);
		return new PendingPluginMessageVotePartyRewards(changed);
	}

	public PendingPluginMessageVotePartyRewards without(UUID deliveryId) {
		if (deliveryId == null) throw new IllegalArgumentException("Missing delivery ID");
		List<Delivery> changed = deliveries.stream()
				.filter(delivery -> !delivery.deliveryId().equals(deliveryId)).toList();
		return changed.size() == deliveries.size() ? this : new PendingPluginMessageVotePartyRewards(changed);
	}

	public PendingPluginMessageVotePartyRewards withState(UUID deliveryId, State state) {
		Delivery delivery = find(deliveryId)
				.orElseThrow(() -> new IllegalArgumentException("Unknown plugin-message vote-party delivery"));
		return with(new Delivery(delivery.batchId(), delivery.deliveryId(), delivery.server(), state));
	}

	/** Returns a fresh JSON array; callers cannot mutate the retained snapshot. */
	public JsonArray toJson() { return encode(deliveries); }

	/** Uses plain JSON-compatible values for the Configurate adapter. */
	public List<Map<String, String>> toStorageValue() {
		List<Map<String, String>> result = new ArrayList<>(deliveries.size());
		for (Delivery delivery : deliveries) {
			Map<String, String> fields = new LinkedHashMap<>();
			fields.put("batchId", delivery.batchId().toString());
			fields.put("deliveryId", delivery.deliveryId().toString());
			fields.put("server", delivery.server());
			fields.put("state", delivery.state().name());
			result.add(fields);
		}
		return result;
	}

	/** A missing namespace is legacy empty state; an explicit null is invalid. */
	public static PendingPluginMessageVotePartyRewards fromJson(JsonElement encoded) {
		if (encoded == null) return empty();
		if (!encoded.isJsonArray() || encoded.getAsJsonArray().size() > MAX_DELIVERIES)
			throw new IllegalArgumentException("Invalid plugin-message vote-party backlog");
		List<Delivery> deliveries = new ArrayList<>();
		for (JsonElement element : encoded.getAsJsonArray()) {
			if (!element.isJsonObject() || !element.getAsJsonObject().keySet().equals(FIELDS))
				throw new IllegalArgumentException("Invalid plugin-message vote-party delivery fields");
			JsonObject fields = element.getAsJsonObject();
			deliveries.add(new Delivery(uuid(fields, "batchId"), uuid(fields, "deliveryId"),
					string(fields, "server"), State.valueOf(string(fields, "state"))));
		}
		return new PendingPluginMessageVotePartyRewards(deliveries);
	}

	/** Validates the new namespace without changing existing cache sections. */
	public static PendingPluginMessageVotePartyRewards fromCacheJson(JsonObject root) {
		if (root == null) throw new IllegalArgumentException("Missing vote cache");
		JsonElement party = root.get("VoteParty");
		if (party == null || party.isJsonNull()) return empty();
		if (!party.isJsonObject()) throw new IllegalArgumentException("Invalid vote-party cache");
		return fromJson(party.getAsJsonObject().get("PluginMessageRewards"));
	}

	/** Converts the Configurate adapter's already-validated JSON-compatible values. */
	public static PendingPluginMessageVotePartyRewards fromStorageValue(Object value) {
		return value == null ? empty() : fromJson(JSON.toJsonTree(value));
	}

	private static UUID uuid(JsonObject object, String field) {
		String value = string(object, field);
		if (value.length() != 36) throw new IllegalArgumentException("Invalid vote-party UUID");
		UUID parsed = UUID.fromString(value);
		if (!parsed.toString().equals(value)) throw new IllegalArgumentException("Noncanonical vote-party UUID");
		return parsed;
	}

	private static String string(JsonObject object, String field) {
		JsonElement value = object.get(field);
		if (value == null || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString())
			throw new IllegalArgumentException("Invalid plugin-message vote-party field type");
		return value.getAsString();
	}

	private static JsonArray encode(List<Delivery> deliveries) {
		JsonArray result = new JsonArray();
		for (Delivery delivery : deliveries) {
			JsonObject fields = new JsonObject();
			fields.addProperty("batchId", delivery.batchId().toString());
			fields.addProperty("deliveryId", delivery.deliveryId().toString());
			fields.addProperty("server", delivery.server());
			fields.addProperty("state", delivery.state().name());
			result.add(fields);
		}
		return result;
	}

	private static int utf8Size(String value) {
		try { return StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(value)).remaining(); }
		catch (CharacterCodingException invalid) {
			throw new IllegalArgumentException("Invalid vote-party Unicode", invalid);
		}
	}
}
