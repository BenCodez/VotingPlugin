package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

/** Exact, bounded, read-only rendering of allow-listed VotingPlugin player fields. */
final class ControlPlayerDataService {
	private static final int MAX_CONTENT_BYTES = 512 * 1024;
	private static final int MAX_VALUE_BYTES = 16 * 1024;
	private static final Set<String> SAFE_STRING_COLUMNS = Set.of("UUID", "PlayerName", "LastOnline",
			"DayVoteStreakLastUpdate", "VoteRemindersLast");
	private static final Set<String> SAFE_BOOLEAN_COLUMNS = Set.of("TopVoterIgnore", "Reminded", "DisableBroadcast");
	private static final Set<String> SAFE_INTEGER_COLUMNS = Set.of("VotePartyVotes", "MonthTotal", "AllTimeTotal",
			"DailyTotal", "WeeklyTotal", "Points", "DayVoteStreak", "BestDayVoteStreak", "WeekVoteStreak",
			"BestWeekVoteStreak", "MonthVoteStreak", "BestMonthVoteStreak", "HighestDailyTotal",
			"HighestMonthlyTotal", "HighestWeeklyTotal", "LastMonthTotal", "LastWeeklyTotal", "LastDailyTotal");
	private static final Pattern SAFE_DYNAMIC_INTEGER_COLUMN = Pattern.compile(
			"(?:MonthTotal-(?:JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER)-[0-9]{4}|VoteShopLimit[A-Za-z0-9_-]{1,64})");
	private final VotingPluginMain plugin;

	ControlPlayerDataService(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	Document readLoaded(VotingPluginUser user) throws IOException {
		java.util.Objects.requireNonNull(user, "user");
		if (user.getUserData() == null || plugin.getStorageType() == null) {
			throw new IOException("player data storage is unavailable");
		}
		Map<String, DataValue> values = user.getUserData().getValues();
		if (values == null) throw new IOException("player data storage is unavailable");
		List<Map.Entry<String, DataValue>> columns = new ArrayList<>(values.entrySet());
		columns.sort(Map.Entry.<String, DataValue>comparingByKey(String.CASE_INSENSITIVE_ORDER)
				.thenComparing(Map.Entry.comparingByKey()));
		JsonObject content = new JsonObject();
		content.addProperty("uuid", user.getUUID());
		content.addProperty("name", user.getPlayerName());
		content.addProperty("storage", plugin.getStorageType().name());
		JsonArray listed = new JsonArray();
		boolean truncated = false;
		for (Map.Entry<String, DataValue> entry : columns) {
			String name = entry.getKey();
			DataValue value = entry.getValue();
			if (name == null || value == null || !safeColumn(name, value)) continue;
			if (listed.size() >= ControlInspectionService.MAX_ROWS) {
				truncated = true;
				break;
			}
			String rendered = render(value);
			if (rendered.getBytes(StandardCharsets.UTF_8).length > MAX_VALUE_BYTES) {
				truncated = true;
				continue;
			}
			JsonObject column = new JsonObject();
			column.addProperty("name", name);
			column.addProperty("type", value.getType().name());
			column.addProperty("value", rendered);
			listed.add(column);
		}
		content.add("columns", listed);
		content.addProperty("columnsTruncated", truncated);
		String json = content.toString();
		if (json.getBytes(StandardCharsets.UTF_8).length > MAX_CONTENT_BYTES) {
			throw new IOException("player data exceeds Control limits");
		}
		return new Document(json);
	}

	private boolean safeColumn(String name, DataValue value) {
		if (SAFE_STRING_COLUMNS.contains(name)) return value.isString();
		if (SAFE_BOOLEAN_COLUMNS.contains(name)) {
			if (value.isBoolean()) return true;
			return value.isString() && value.getString() != null
					&& value.getString().matches("(?i:true|false)");
		}
		if (SAFE_INTEGER_COLUMNS.contains(name)) return value.isInt();
		if (SAFE_DYNAMIC_INTEGER_COLUMN.matcher(name).matches()) return value.isInt();
		if (name.equals(plugin.getVotingPluginUserManager().getCoolDownCheckPath())) return value.isBoolean();
		if (name.equals(plugin.getVotingPluginUserManager().getCoolDownCheckSitePath())) return value.isString();
		return (name.equals(plugin.getVotingPluginUserManager().getGottenAllSitesDayPath())
				|| name.equals(plugin.getVotingPluginUserManager().getGottenAlmostAllSitesDayPath())) && value.isInt();
	}

	private static String render(DataValue value) {
		if (value.isInt()) return Integer.toString(value.getInt());
		if (value.isBoolean()) return Boolean.toString(value.getBoolean());
		String rendered = value.getString();
		return rendered == null ? "" : rendered;
	}

	record Document(String content) { }
}
