package com.bencodez.votingplugin.control;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

import org.bukkit.Material;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

/** Shared, side-effect-free parser for Control reward simulation and persistence. */
final class ControlRewardProposal {
	static final int MAX_JSON_BYTES = 64 * 1024;
	private static final Pattern SITE_NAME = Pattern.compile("[A-Za-z0-9_-]{1,64}");
	private static final Set<String> FIELDS = Set.of("scope", "site", "commands", "playerMessages",
			"broadcastMessages", "items", "money", "permissions", "chancePercent", "onlineOnly");

	private ControlRewardProposal() { }

	static Parsed parse(String encoded) {
		if (encoded == null || encoded.isBlank() || encoded.indexOf('\0') >= 0
				|| encoded.getBytes(StandardCharsets.UTF_8).length > MAX_JSON_BYTES) {
			throw new IllegalArgumentException("proposal must be a JSON object encoded as text within 64 KiB");
		}
		JsonObject proposal;
		try {
			JsonElement parsed = JsonParser.parseString(encoded);
			if (!parsed.isJsonObject()) throw new IllegalArgumentException();
			proposal = parsed.getAsJsonObject();
		} catch (RuntimeException failure) {
			throw new IllegalArgumentException("proposal must be a JSON object encoded as text");
		}
		rejectUnknown(proposal, FIELDS, "reward proposal");
		String scope = requiredString(proposal, "scope", 32);
		if (!Set.of("site", "every-site", "vote-party").contains(scope)) {
			throw new IllegalArgumentException("reward proposal scope is invalid");
		}
		String site = optionalString(proposal, "site", 64);
		if ("site".equals(scope)) {
			if (!SITE_NAME.matcher(site).matches()) {
				throw new IllegalArgumentException("reward proposal site is invalid");
			}
		} else if (!site.isBlank()) {
			throw new IllegalArgumentException("reward proposal site is only valid for site scope");
		}
		List<String> commands = boundedStrings(proposal, "commands", 20, 500);
		List<String> playerMessages = boundedStrings(proposal, "playerMessages", 20, 500);
		List<String> broadcastMessages = boundedStrings(proposal, "broadcastMessages", 20, 500);
		List<String> permissions = boundedStrings(proposal, "permissions", 20, 200);
		List<Item> items = boundedItems(proposal);
		double money = boundedDouble(proposal, "money", 0, 1_000_000_000D, 0D);
		double chance = boundedDouble(proposal, "chancePercent", 0, 100D, 100D);
		boolean onlineOnly = optionalBoolean(proposal, "onlineOnly", false);
		Parsed parsed = new Parsed(scope, site, commands, playerMessages, broadcastMessages, items, money,
				permissions, chance, onlineOnly);
		if (parsed.actionCount() == 0) {
			throw new IllegalArgumentException("reward proposal contains no actions");
		}
		return parsed;
	}

	private static List<String> boundedStrings(JsonObject object, String name, int maximumItems,
			int maximumLength) {
		JsonArray values = object.has(name) ? requireArray(object, name) : new JsonArray();
		if (values.size() > maximumItems) throw new IllegalArgumentException(name + " has too many entries");
		List<String> result = new ArrayList<>(values.size());
		for (JsonElement value : values) {
			if (!(value instanceof JsonPrimitive primitive) || !primitive.isString()) {
				throw new IllegalArgumentException(name + " must contain text entries");
			}
			String text = primitive.getAsString();
			if (text.isBlank() || text.length() > maximumLength || text.indexOf('\0') >= 0
					|| text.indexOf('\r') >= 0 || text.indexOf('\n') >= 0) {
				throw new IllegalArgumentException(name + " contains an invalid entry");
			}
			result.add(text);
		}
		return List.copyOf(result);
	}

	private static List<Item> boundedItems(JsonObject proposal) {
		JsonArray values = proposal.has("items") ? requireArray(proposal, "items") : new JsonArray();
		if (values.size() > 20) throw new IllegalArgumentException("items has too many entries");
		List<Item> result = new ArrayList<>(values.size());
		for (JsonElement value : values) {
			if (!value.isJsonObject()) throw new IllegalArgumentException("items must contain objects");
			JsonObject item = value.getAsJsonObject();
			rejectUnknown(item, Set.of("material", "amount"), "reward item");
			String materialName = requiredString(item, "material", 80).toUpperCase(Locale.ROOT);
			if (!materialName.matches("[A-Z0-9_]{1,80}")) {
				throw new IllegalArgumentException("item material is invalid");
			}
			Material material = Material.matchMaterial(materialName);
			// Modern Bukkit resolves item-ness through the live registry. The null-server
			// path keeps the shared parser usable in isolated validation tests; production
			// connector calls always have a server and therefore enforce isItem().
			if (material == null || (org.bukkit.Bukkit.getServer() != null && !material.isItem())) {
				throw new IllegalArgumentException("item material is invalid");
			}
			result.add(new Item(material.name(), boundedInt(item, "amount", 1, 1, 64)));
		}
		return List.copyOf(result);
	}

	private static void rejectUnknown(JsonObject object, Set<String> accepted, String label) {
		for (String key : object.keySet()) {
			if (!accepted.contains(key)) {
				throw new IllegalArgumentException(label + " contains unsupported field " + key);
			}
		}
	}

	private static JsonArray requireArray(JsonObject object, String name) {
		JsonElement value = object.get(name);
		if (value == null || !value.isJsonArray()) throw new IllegalArgumentException(name + " must be an array");
		return value.getAsJsonArray();
	}

	private static String requiredString(JsonObject object, String name, int maximumLength) {
		String value = optionalString(object, name, maximumLength);
		if (value.isBlank()) throw new IllegalArgumentException(name + " is required");
		return value;
	}

	private static String optionalString(JsonObject object, String name, int maximumLength) {
		if (!object.has(name) || object.get(name).isJsonNull()) return "";
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isString()) {
			throw new IllegalArgumentException(name + " must be text");
		}
		String text = primitive.getAsString();
		if (text.length() > maximumLength || text.indexOf('\0') >= 0 || text.indexOf('\r') >= 0
				|| text.indexOf('\n') >= 0) throw new IllegalArgumentException(name + " is invalid");
		return text;
	}

	private static boolean optionalBoolean(JsonObject object, String name, boolean defaultValue) {
		if (!object.has(name) || object.get(name).isJsonNull()) return defaultValue;
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isBoolean()) {
			throw new IllegalArgumentException(name + " must be true or false");
		}
		return primitive.getAsBoolean();
	}

	private static int boundedInt(JsonObject object, String name, int defaultValue, int minimum, int maximum) {
		if (!object.has(name) || object.get(name).isJsonNull()) return defaultValue;
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isNumber()) {
			throw new IllegalArgumentException(name + " must be a number");
		}
		try {
			int parsed = new java.math.BigDecimal(primitive.getAsString()).intValueExact();
			if (parsed < minimum || parsed > maximum) throw new ArithmeticException();
			return parsed;
		} catch (ArithmeticException | NumberFormatException failure) {
			throw new IllegalArgumentException(name + " is outside the allowed range");
		}
	}

	private static double boundedDouble(JsonObject object, String name, double minimum, double maximum,
			double defaultValue) {
		if (!object.has(name) || object.get(name).isJsonNull()) return defaultValue;
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isNumber()) {
			throw new IllegalArgumentException(name + " must be a number");
		}
		double parsed = primitive.getAsDouble();
		if (!Double.isFinite(parsed) || parsed < minimum || parsed > maximum) {
			throw new IllegalArgumentException(name + " is outside the allowed range");
		}
		return parsed;
	}

	record Item(String material, int amount) {
		JsonObject json() {
			JsonObject result = new JsonObject();
			result.addProperty("material", material);
			result.addProperty("amount", amount);
			return result;
		}
	}

	record Parsed(String scope, String site, List<String> commands, List<String> playerMessages,
			List<String> broadcastMessages, List<Item> items, double money, List<String> permissions,
			double chancePercent, boolean onlineOnly) {
		int actionCount() {
			return commands.size() + playerMessages.size() + broadcastMessages.size() + items.size()
					+ permissions.size() + (money > 0 ? 1 : 0);
		}

		String fileName() {
			return "vote-party".equals(scope) ? "SpecialRewards.yml" : "VoteSites.yml";
		}

		JsonObject json() {
			JsonObject result = new JsonObject();
			result.addProperty("scope", scope);
			if (!site.isBlank()) result.addProperty("site", site);
			result.add("commands", strings(commands));
			result.add("playerMessages", strings(playerMessages));
			result.add("broadcastMessages", strings(broadcastMessages));
			JsonArray itemValues = new JsonArray();
			items.forEach(item -> itemValues.add(item.json()));
			result.add("items", itemValues);
			result.addProperty("money", money);
			result.add("permissions", strings(permissions));
			result.addProperty("chancePercent", chancePercent);
			result.addProperty("onlineOnly", onlineOnly);
			return result;
		}

		private static JsonArray strings(List<String> values) {
			JsonArray result = new JsonArray();
			values.forEach(result::add);
			return result;
		}
	}
}
