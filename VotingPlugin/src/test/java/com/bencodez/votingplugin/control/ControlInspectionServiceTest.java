package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServerCount;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServiceCount;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServiceHealth;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogCounts;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

class ControlInspectionServiceTest {
	@Test void overviewIncludesSafeStorageAndVoteLogReadiness() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		org.bukkit.configuration.file.YamlConfiguration config = new org.bukkit.configuration.file.YamlConfiguration();
		config.set("DataStorage", "MYSQL");
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		when(plugin.getConfigFile().getData()).thenReturn(config);
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getVoteSiteManager().getVoteSites()).thenReturn(new ArrayList<>());
		when(plugin.getVoteLogMysqlTable()).thenReturn(null);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"overview\",\"filters\":{}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertEquals("MYSQL", result.get("dataStorage").getAsString());
		assertFalse(result.get("voteLogAvailable").getAsBoolean());
		assertFalse(result.get("voteLogReadable").getAsBoolean());
	}

	@Test void rewardSimulationNormalizesButNeverExecutesTypedActions() {
		ControlInspectionService service = new ControlInspectionService();
		JsonObject proposal = JsonParser.parseString("""
				{"scope":"site","site":"PMC","commands":["eco give %player% 100"],
				 "playerMessages":["Thanks"],"items":[{"material":"diamond","amount":2}],
				 "chancePercent":25,"onlineOnly":true}
				""").getAsJsonObject();
		JsonObject query = rewardQuery(proposal);

		JsonObject data = service.inspect(query);
		JsonObject result = data.getAsJsonObject("result");
		assertEquals(1, data.get("schemaVersion").getAsInt());
		assertEquals("reward-simulation", data.get("kind").getAsString());
		assertTrue(data.get("generatedAt").getAsJsonPrimitive().isString());
		assertTrue(data.get("generatedAt").getAsString().endsWith("Z"));
		assertTrue(result.get("valid").getAsBoolean());
		assertFalse(result.get("wouldExecute").getAsBoolean());
		assertFalse(result.get("sideEffects").getAsBoolean());
		assertEquals(3, result.get("actionCount").getAsInt());
		assertEquals("DIAMOND", result.getAsJsonObject("normalizedProposal").getAsJsonArray("items")
				.get(0).getAsJsonObject().get("material").getAsString());
		assertTrue(data.toString().getBytes(StandardCharsets.UTF_8).length
				< ControlInspectionService.MAX_DATA_BYTES);
	}

	@Test void inspectionContractRejectsUnknownKindsFieldsAndUnboundedSearches() {
		ControlInspectionService service = new ControlInspectionService();

		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString("{\"kind\":\"raw-sql\"}").getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"overview\",\"filters\":{\"includeSecrets\":true}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-log-search\",\"filters\":{\"player\":\"Ben\",\"service\":\"PMC\"}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-log-search\",\"filters\":{\"days\":\"366\"}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-log-search\",\"filters\":{\"days\":30}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-site-resolution\",\"filters\":{\"serviceSite\":\"x\",\"includeDisabled\":true}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-site-resolution\",\"filters\":{\"serviceSite\":\"[invalid]\"}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"player\",\"filters\":{\"uuid\":\"1-1-1-1-1\"}}")
						.getAsJsonObject()));
		assertThrows(IllegalArgumentException.class,
				() -> service.inspect(JsonParser.parseString(
						"{\"kind\":\"vote-trace\",\"filters\":{\"voteId\":\"1-1-1-1-1\"}}")
						.getAsJsonObject()));
	}

	@Test void rewardProposalRejectsUnknownActionsAndEmptyPlans() {
		ControlInspectionService service = new ControlInspectionService();
		assertThrows(IllegalArgumentException.class, () -> service.inspect(rewardQuery(JsonParser.parseString(
				"{\"scope\":\"site\",\"site\":\"PMC\",\"shell\":\"rm\"}").getAsJsonObject())));
		assertThrows(IllegalArgumentException.class, () -> service.inspect(rewardQuery(JsonParser.parseString(
				"{\"scope\":\"site\",\"site\":\"PMC\"}").getAsJsonObject())));
		assertThrows(IllegalArgumentException.class, () -> service.inspect(rewardQuery(JsonParser.parseString(
				"{\"scope\":\"every-site\",\"items\":[{\"material\":\"NOT_A_REAL_ITEM\",\"amount\":1}]}"
				).getAsJsonObject())));
		assertThrows(IllegalArgumentException.class, () -> service.inspect(JsonParser.parseString(
				"{\"kind\":\"reward-simulation\",\"filters\":{\"proposal\":\"not-json\"}}")
				.getAsJsonObject()));
	}

	@Test void productionRewardSimulationRejectsAnUnconfiguredSite() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		when(plugin.getConfigVoteSites().getData())
				.thenReturn(new org.bukkit.configuration.file.YamlConfiguration());
		ControlInspectionService service = new ControlInspectionService(plugin);

		assertThrows(IllegalArgumentException.class, () -> service.inspect(rewardQuery(JsonParser.parseString(
				"{\"scope\":\"site\",\"site\":\"Missing\",\"commands\":[\"say test\"]}"
				).getAsJsonObject())));
	}

	@Test void disabledVoteLoggingIsUnavailableInsteadOfAnAuthoritativeEmptyResult() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable staleTable = mock(VoteLogMysqlTable.class);
		when(plugin.getVoteLogMysqlTable()).thenReturn(staleTable);
		ControlInspectionService service = new ControlInspectionService(plugin);
		JsonObject query = JsonParser.parseString(
				"{\"kind\":\"vote-log-summary\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject();

		assertThrows(ControlInspectionService.InspectionUnavailableException.class,
				() -> service.inspect(query));
		verify(staleTable, never()).isReadable();
	}

	@Test void unreadableVoteLoggingIsUnavailableInsteadOfAnAuthoritativeEmptyResult() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(false);
		ControlInspectionService service = new ControlInspectionService(plugin);
		JsonObject query = JsonParser.parseString(
				"{\"kind\":\"vote-log-summary\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject();

		assertThrows(ControlInspectionService.InspectionUnavailableException.class,
				() -> service.inspect(query));
		verify(table, never()).getCounts(30);
	}

	@Test void voteLogTopListsBreakCountTiesByName() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(true);
		when(table.getCounts(30)).thenReturn(new VoteLogCounts(12, 10, 2));
		when(table.getUniqueVoters(30)).thenReturn(4L);
		when(table.getTopServices(30, 20)).thenReturn(List.of(
				new ServiceCount("Zulu", 3), new ServiceCount("alpha", 3), new ServiceCount("Middle", 6)));
		when(table.getTopServers(30, 20)).thenReturn(List.of(
				new ServerCount("survival", 2), new ServerCount("Creative", 2), new ServerCount("hub", 8)));
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-log-summary\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");

		assertEquals(List.of("Middle", "alpha", "Zulu"), result.getAsJsonArray("topServices").asList().stream()
				.map(row -> row.getAsJsonObject().get("service").getAsString()).toList());
		assertEquals(List.of("hub", "Creative", "survival"), result.getAsJsonArray("topServers").asList().stream()
				.map(row -> row.getAsJsonObject().get("server").getAsString()).toList());
	}

	@Test void exactPlayerMissDoesNotLoadOrEnumerateUsers() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		when(plugin.getUserManager().userExist("ExactName")).thenReturn(false);
		ControlInspectionService service = new ControlInspectionService(plugin);
		JsonObject query = JsonParser.parseString(
				"{\"kind\":\"player\",\"filters\":{\"name\":\"ExactName\"}}")
				.getAsJsonObject();

		JsonObject result = service.inspect(query).getAsJsonObject("result");
		assertFalse(result.get("found").getAsBoolean());
		verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser("ExactName");
	}

	@Test void playerInspectionReturnsBoundedDeterministicPerSiteLastVotes() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getUserManager().userExist("ExactName")).thenReturn(true);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser("ExactName")).thenReturn(user);
		when(user.getUUID()).thenReturn("3b0c76c1-b7ef-4a2c-a565-b7bc662531f9");
		when(user.getPlayerName()).thenReturn("ExactName");
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>());
		VoteSite later = voteSite("Zulu", "Zulu display", "zulu.example");
		VoteSite earlier = voteSite("alpha", "Alpha display", "alpha.example");
		HashMap<VoteSite, Long> lastVotes = new HashMap<>();
		lastVotes.put(later, 200L);
		lastVotes.put(earlier, 100L);
		when(user.getLastVotes()).thenReturn(lastVotes);
		when(user.getLastVoteTime()).thenReturn(200L);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"player\",\"filters\":{\"name\":\"ExactName\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertEquals("alpha", result.getAsJsonArray("lastVotes").get(0).getAsJsonObject()
				.get("siteKey").getAsString());
		assertEquals("Zulu", result.getAsJsonArray("lastVotes").get(1).getAsJsonObject()
				.get("siteKey").getAsString());
		assertFalse(result.get("lastVotesTruncated").getAsBoolean());
		assertFalse(result.get("storageRowAvailable").getAsBoolean());
		assertTrue(result.getAsJsonArray("columns").isEmpty());
	}

	@Test void playerInspectionShowsExactAllowListedStorageValuesWithoutInternalPayloads() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getUserManager().userExist("ExactName")).thenReturn(true);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser("ExactName")).thenReturn(user);
		when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
		when(user.getUUID()).thenReturn("3b0c76c1-b7ef-4a2c-a565-b7bc662531f9");
		when(user.getPlayerName()).thenReturn("ExactName");
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>());
		when(user.getLastVotes()).thenReturn(new HashMap<>());
		HashMap<String, DataValue> stored = new HashMap<>();
		stored.put("Points", new DataValueInt(42));
		stored.put("MonthTotal-JANUARY-2025", new DataValueInt(11));
		stored.put("MonthTotal-DECEMBER-2026", new DataValueInt(12));
		stored.put("VoteShopLimitKeys", new DataValueInt(3));
		stored.put("TopVoterIgnore", new DataValueString("true"));
		stored.put("VoteShopLimitInjected", new DataValueString("must not leave through a dynamic field"));
		stored.put("DailyTotal", new DataValueString("must not leave through an integer field"));
		stored.put("Reminded", new DataValueString("must not leave through a boolean field"));
		stored.put("OfflineVotes", new DataValueString("private serialized vote payload"));
		stored.put("FuturePluginSecret", new DataValueString("must not leave the backend"));
		stored.put("MonthTotal_2025_1", new DataValueInt(91));
		stored.put("MonthTotal-JANUARY-25", new DataValueInt(92));
		stored.put("MonthTotal-JANUARY-2025-extra", new DataValueInt(93));
		stored.put("MonthTotal-january-2025", new DataValueInt(94));
		stored.put("MonthTotal-SMARCH-2025", new DataValueInt(95));
		stored.put("MonthTotal-FEBRUARY-2025", new DataValueString("wrong type"));
		when(user.getUserData().getValues()).thenReturn(stored);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"player\",\"filters\":{\"name\":\"ExactName\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertTrue(result.get("storageRowAvailable").getAsBoolean());
		assertEquals("SQLITE", result.get("storage").getAsString());
		assertEquals(List.of("MonthTotal-DECEMBER-2026", "MonthTotal-JANUARY-2025", "Points", "TopVoterIgnore", "VoteShopLimitKeys"), result.getAsJsonArray("columns").asList().stream()
				.map(value -> value.getAsJsonObject().get("name").getAsString()).toList());
		assertEquals(List.of("12", "11", "42", "true", "3"), result.getAsJsonArray("columns").asList().stream()
				.map(value -> value.getAsJsonObject().get("value").getAsString()).toList());
		assertFalse(result.toString().contains("private serialized vote payload"));
		assertFalse(result.toString().contains("must not leave through a dynamic field"));
		assertFalse(result.toString().contains("must not leave through an integer field"));
		assertFalse(result.toString().contains("must not leave through a boolean field"));
		assertFalse(result.toString().contains("must not leave the backend"));
		assertFalse(result.toString().contains("MonthTotal_2025_1"));
		assertFalse(result.toString().contains("MonthTotal-JANUARY-25"));
		assertFalse(result.toString().contains("MonthTotal-JANUARY-2025-extra"));
		assertFalse(result.toString().contains("MonthTotal-january-2025"));
		assertFalse(result.toString().contains("MonthTotal-SMARCH-2025"));
		assertFalse(result.toString().contains("MonthTotal-FEBRUARY-2025"));
	}

	@Test void playerInspectionBoundsHistoricalMonthTotalsDeterministically() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getUserManager().userExist("ExactName")).thenReturn(true);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser("ExactName")).thenReturn(user);
		when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
		when(user.getUUID()).thenReturn("3b0c76c1-b7ef-4a2c-a565-b7bc662531f9");
		when(user.getPlayerName()).thenReturn("ExactName");
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>());
		when(user.getLastVotes()).thenReturn(new HashMap<>());
		HashMap<String, DataValue> stored = new HashMap<>();
		List<String> expected = new ArrayList<>();
		for (int index = 0; index <= ControlInspectionService.MAX_ROWS; index++) {
			String name = "MonthTotal-" + java.time.Month.of(index % 12 + 1).name() + "-" + (2000 + index / 12);
			stored.put(name, new DataValueInt(index));
			expected.add(name);
		}
		expected.sort(String.CASE_INSENSITIVE_ORDER.thenComparing(java.util.Comparator.naturalOrder()));
		when(user.getUserData().getValues()).thenReturn(stored);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"player\",\"filters\":{\"name\":\"ExactName\"}}")
				.getAsJsonObject()).getAsJsonObject("result");

		assertEquals(expected.subList(0, ControlInspectionService.MAX_ROWS),
				result.getAsJsonArray("columns").asList().stream()
						.map(value -> value.getAsJsonObject().get("name").getAsString()).toList());
		assertTrue(result.get("columnsTruncated").getAsBoolean());
	}

	@Test void playerInspectionBoundsAllowListedStorageColumnsAtTheSharedRowLimit() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getUserManager().userExist("ExactName")).thenReturn(true);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser("ExactName")).thenReturn(user);
		when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
		when(user.getUUID()).thenReturn("3b0c76c1-b7ef-4a2c-a565-b7bc662531f9");
		when(user.getPlayerName()).thenReturn("ExactName");
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>());
		when(user.getLastVotes()).thenReturn(new HashMap<>());
		HashMap<String, DataValue> stored = new HashMap<>();
		for (int index = 0; index <= ControlInspectionService.MAX_ROWS; index++) {
			stored.put(String.format("VoteShopLimit%03d", index), new DataValueInt(index));
		}
		when(user.getUserData().getValues()).thenReturn(stored);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"player\",\"filters\":{\"name\":\"ExactName\"}}")
				.getAsJsonObject()).getAsJsonObject("result");

		assertEquals(ControlInspectionService.MAX_ROWS, result.getAsJsonArray("columns").size());
		assertEquals("VoteShopLimit000", result.getAsJsonArray("columns").get(0).getAsJsonObject()
				.get("name").getAsString());
		assertEquals("VoteShopLimit099", result.getAsJsonArray("columns").get(99).getAsJsonObject()
				.get("name").getAsString());
		assertTrue(result.get("columnsTruncated").getAsBoolean());
	}

	@Test void diagnosticsBoundsAndReportsTruncatedPluginInventory() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		org.bukkit.configuration.file.YamlConfiguration config = new org.bukkit.configuration.file.YamlConfiguration();
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		when(plugin.getConfigFile().getData()).thenReturn(config);
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getVoteSiteManager().getVoteSites()).thenReturn(new ArrayList<>());
		when(plugin.getVoteLogMysqlTable()).thenReturn(null);
		org.bukkit.plugin.Plugin[] installed = new org.bukkit.plugin.Plugin[ControlInspectionService.MAX_ROWS + 1];
		for (int index = 0; index < installed.length; index++) {
			installed[index] = mock(org.bukkit.plugin.Plugin.class, RETURNS_DEEP_STUBS);
			when(installed[index].getDescription().getName()).thenReturn(String.format("Plugin%03d", index));
		}
		when(installed[99].getDescription().getName()).thenReturn("plugina");
		when(installed[100].getDescription().getName()).thenReturn("PluginA");
		when(plugin.getServer().getPluginManager().getPlugins()).thenReturn(installed);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"diagnostics\",\"filters\":{}}")
				.getAsJsonObject()).getAsJsonObject("result");

		assertEquals(ControlInspectionService.MAX_ROWS, result.getAsJsonArray("detectedPlugins").size());
		assertEquals("Plugin000", result.getAsJsonArray("detectedPlugins").get(0).getAsString());
		assertEquals("PluginA", result.getAsJsonArray("detectedPlugins").get(99).getAsString());
		assertTrue(result.get("detectedPluginsTruncated").getAsBoolean());
	}

	@Test void voteSiteHealthIncludesPersistedDetectedInboxWithoutVoteLogging() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		voteSites.set("VoteSites.PMC.ServiceSite", "configured.example");
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getVoteLogMysqlTable()).thenReturn(null);
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(java.util.List.of(
				"NEW.example", "new.EXAMPLE", "configured.example"));
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertEquals(1, result.getAsJsonArray("detectedUnconfiguredServices").size());
		assertEquals("NEW.example", result.getAsJsonArray("detectedUnconfiguredServices").get(0).getAsString());
		assertFalse(result.get("detectedUnconfiguredServicesTruncated").getAsBoolean());
		assertFalse(result.get("voteLoggingAvailable").getAsBoolean());
		assertFalse(result.get("voteLogReadable").getAsBoolean());
		assertEquals("VOTE_LOG_UNAVAILABLE", result.getAsJsonArray("sites").get(0).getAsJsonObject()
				.get("status").getAsString());
		assertFalse(result.getAsJsonArray("sites").get(0).getAsJsonObject().has("loggedVotes"));
	}

	@Test void voteSiteHealthSkipsAggregatesWhenVoteLogIsUnreadable() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		voteSites.set("VoteSites.PMC.ServiceSite", "configured.example");
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(false);
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(java.util.List.of());
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		JsonObject site = result.getAsJsonArray("sites").get(0).getAsJsonObject();
		assertEquals("VOTE_LOG_UNREADABLE", site.get("status").getAsString());
		assertFalse(result.get("voteLogReadable").getAsBoolean());
		assertFalse(site.has("loggedVotes"));
		assertFalse(site.has("lastVoteTime"));
		verify(table, never()).getServiceHealth(30, 100);
	}

	@Test void voteSiteHealthQueriesConfiguredServicesOutsideTheRecentAggregateWindow() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		voteSites.set("VoteSites.PMC.ServiceSite", "configured.example");
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(true);
		List<ServiceHealth> recent = new ArrayList<>();
		for (int index = 0; index < 100; index++) {
			recent.add(new ServiceHealth("other-" + index, 1, 1000 - index, 1, 0));
		}
		when(table.getServiceHealth(30, 100)).thenReturn(recent);
		when(table.getServiceHealthForServices(30, List.of("configured.example"))).thenReturn(List.of(
				new ServiceHealth("configured.example", 7, 500, 6, 1)));
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(List.of());
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		JsonObject site = result.getAsJsonArray("sites").get(0).getAsJsonObject();

		assertEquals("ACTIVE", site.get("status").getAsString());
		assertEquals(7, site.get("loggedVotes").getAsLong());
		assertTrue(result.get("truncated").getAsBoolean());
	}

	@Test void voteSiteHealthMatchesFullServiceNamesBeforeTruncatingOutput() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		String fullService = "long-service-" + "x".repeat(100);
		String normalized = fullService.toLowerCase(Locale.ROOT);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		voteSites.set("VoteSites.Long.ServiceSite", fullService);
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(true);
		when(table.getServiceHealth(30, 100)).thenReturn(List.of());
		when(table.getServiceHealthForServices(30, List.of(normalized))).thenReturn(List.of(
				new ServiceHealth(fullService.toUpperCase(Locale.ROOT), 5, 900, 4, 1)));
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(List.of(
				fullService.toUpperCase(Locale.ROOT)));
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		JsonObject site = result.getAsJsonArray("sites").get(0).getAsJsonObject();

		assertEquals("ACTIVE", site.get("status").getAsString());
		assertEquals(5, site.get("loggedVotes").getAsLong());
		assertEquals(64, site.get("serviceSite").getAsString().length());
		assertEquals(0, result.getAsJsonArray("detectedUnconfiguredServices").size());
		verify(table).getServiceHealthForServices(30, List.of(normalized));
	}

	@Test void voteSiteHealthExcludesConfiguredSitesBeyondTheDisplayedWindowFromUnmatchedRows() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		List<String> displayed = new ArrayList<>();
		for (int index = 0; index <= 100; index++) {
			String serviceName = String.format("configured-%03d.example", index);
			voteSites.set(String.format("VoteSites.Site%03d.ServiceSite", index), serviceName);
			if (index < 100) displayed.add(serviceName);
		}
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(true);
		when(table.getServiceHealth(30, 100)).thenReturn(List.of(
				new ServiceHealth("configured-100.example", 3, 800, 3, 0)));
		when(table.getServiceHealthForServices(30, displayed)).thenReturn(List.of());
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(List.of());
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");

		assertEquals(100, result.getAsJsonArray("sites").size());
		assertEquals(0, result.getAsJsonArray("unmatchedLoggedServices").size());
		assertTrue(result.get("truncated").getAsBoolean());
	}

	@Test void voteSiteHealthMergesCaseVariantsInsteadOfOverwritingAggregates() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteLogMysqlTable table = mock(VoteLogMysqlTable.class);
		org.bukkit.configuration.file.YamlConfiguration voteSites = new org.bukkit.configuration.file.YamlConfiguration();
		voteSites.set("VoteSites.Mixed.ServiceSite", "Example.COM");
		when(plugin.getConfigVoteSites().getData()).thenReturn(voteSites);
		when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
		when(plugin.getVoteLogMysqlTable()).thenReturn(table);
		when(table.isReadable()).thenReturn(true);
		when(table.getServiceHealth(30, 100)).thenReturn(List.of(
				new ServiceHealth("Example.com", 3, 700, 2, 1),
				new ServiceHealth("example.COM", 4, 900, 3, 1)));
		when(table.getServiceHealthForServices(30, List.of("example.com"))).thenReturn(List.of());
		when(plugin.getServerData().getServiceSitesReadOnly()).thenReturn(List.of());
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-health\",\"filters\":{\"days\":\"30\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		JsonObject site = result.getAsJsonArray("sites").get(0).getAsJsonObject();

		assertEquals("ACTIVE", site.get("status").getAsString());
		assertEquals(7, site.get("loggedVotes").getAsLong());
		assertEquals(900, site.get("lastVoteTime").getAsLong());
		assertEquals(5, site.get("immediateVotes").getAsLong());
		assertEquals(2, site.get("cachedVotes").getAsLong());
		assertEquals(0, result.getAsJsonArray("unmatchedLoggedServices").size());
	}

	@Test void voteSiteResolutionUsesOnlyNonCreatingPaths() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		when(plugin.getVoteSiteManager().getResolver().getConfiguredVoteSiteName("new.example"))
				.thenReturn(null);
		when(plugin.getVoteSiteManager().resolveVoteSite("new.example", true)).thenReturn(null);
		when(plugin.getConfigFile().isAutoCreateVoteSites()).thenReturn(true);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-resolution\",\"filters\":{\"serviceSite\":\"new.example\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertTrue(result.get("wouldAutoCreate").getAsBoolean());
		assertFalse(result.get("sideEffects").getAsBoolean());
		verify(plugin.getConfigVoteSites(), never()).tryGenerateVoteSite("new.example");
		verify(plugin.getConfigVoteSites(), never()).tryAutoGenerateVoteSite("new.example");
	}

	@Test void voteSiteResolutionUsesTheInboundAdvancedServiceAlias() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		VoteSite site = voteSite("PMC", "Planet Minecraft", "PlanetMinecraft.com");
		when(plugin.getConfigFile().isAdvancedServiceSiteHandling()).thenReturn(true);
		when(plugin.getServiceSiteHandler().matchReverse("planetminecraft.com")).thenReturn("PlanetMinecraft");
		when(plugin.getVoteSiteManager().getResolver()
				.getConfiguredVoteSiteName("planetminecraft.com", "PlanetMinecraft")).thenReturn("PMC");
		when(plugin.getVoteSiteManager().getVoteSiteName(true, "planetminecraft.com", "PlanetMinecraft"))
				.thenReturn("PMC");
		when(plugin.getVoteSiteManager().getVoteSiteName(false, "planetminecraft.com", "PlanetMinecraft"))
				.thenReturn("PMC");
		when(plugin.getVoteSiteManager().resolveVoteSite("PMC", true)).thenReturn(site);
		when(plugin.getVoteSiteManager().hasVoteSite("PMC")).thenReturn(true);
		when(plugin.getConfigFile().isAutoCreateVoteSites()).thenReturn(true);
		ControlInspectionService service = new ControlInspectionService(plugin);

		JsonObject result = service.inspect(JsonParser.parseString(
				"{\"kind\":\"vote-site-resolution\",\"filters\":{\"serviceSite\":\"planetminecraft.com\"}}")
				.getAsJsonObject()).getAsJsonObject("result");
		assertTrue(result.get("matched").getAsBoolean());
		assertEquals("PMC", result.get("key").getAsString());
		assertFalse(result.get("wouldAutoCreate").getAsBoolean());
		assertFalse(result.get("sideEffects").getAsBoolean());
		verify(plugin.getConfigVoteSites(), never()).tryGenerateVoteSite("planetminecraft.com");
		verify(plugin.getConfigVoteSites(), never()).tryAutoGenerateVoteSite("planetminecraft.com");
	}

	private static JsonObject rewardQuery(JsonObject proposal) {
		JsonObject filters = new JsonObject();
		filters.addProperty("proposal", proposal.toString());
		JsonObject query = new JsonObject();
		query.addProperty("kind", "reward-simulation");
		query.add("filters", filters);
		return query;
	}

	private static VoteSite voteSite(String key, String displayName, String serviceSite) {
		VoteSite site = mock(VoteSite.class);
		when(site.getKey()).thenReturn(key);
		when(site.getDisplayName()).thenReturn(displayName);
		when(site.getServiceSite()).thenReturn(serviceSite);
		return site;
	}
}
