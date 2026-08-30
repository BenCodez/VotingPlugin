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

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
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
