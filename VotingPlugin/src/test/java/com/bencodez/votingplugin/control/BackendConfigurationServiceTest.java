package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.util.DurableFiles;

class BackendConfigurationServiceTest {
	@TempDir Path directory;

	@Test void masksSecretsPreservesThemAndAppliesARevisionedReload() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Database:\n  Password: keep-me\nFeature: false\n");
		AtomicInteger reloads = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory, reloads::incrementAndGet);

		BackendConfigurationService.Document read = service.read("Config.yml");
		assertFalse(read.content().contains("keep-me"));
		assertTrue(read.content().contains(BackendConfigurationService.REDACTED));
		String proposal = read.content().replace("Feature: false", "Feature: true");
		BackendConfigurationService.Preview preview = service.preview("Config.yml", proposal);
		assertTrue(preview.changes().contains("changed Feature"));

		BackendConfigurationService.ApplyResult result = service.apply("Config.yml", proposal, read.revision());
		assertEquals(1, reloads.get());
		YamlConfiguration applied = new YamlConfiguration();
		applied.loadFromString(Files.readString(config));
		assertEquals("keep-me", applied.getString("Database.Password"));
		assertTrue(applied.getBoolean("Feature"));
		assertFalse(result.document().content().contains("keep-me"));
		assertTrue(Files.isRegularFile(directory.resolve("Config.yml.control-backup")));
	}

	@Test void fullFileEditingPreservesCommentsWhileSecretsRemainRedacted() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "# VotingPlugin owner notes\n"
				+ "Database:\n"
				+ "  # Never expose this value\n"
				+ "  Password: keep-me # database credential\n"
				+ "Feature: false # toggle from Control\n"
				+ "# End of owner notes\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		BackendConfigurationService.Document read = service.read("Config.yml");
		assertFalse(read.content().contains("keep-me"));
		assertTrue(read.content().contains(BackendConfigurationService.REDACTED));
		assertTrue(read.content().contains("# VotingPlugin owner notes"));
		assertTrue(read.content().contains("# Never expose this value"));
		assertTrue(read.content().contains("# database credential"));
		assertTrue(read.content().contains("# toggle from Control"));
		assertTrue(read.content().contains("# End of owner notes"));

		String proposal = read.content().replace("Feature: false", "Feature: true");
		BackendConfigurationService.Preview preview = service.preview("Config.yml", proposal);
		assertTrue(preview.resolvedContent().contains("# VotingPlugin owner notes"));
		assertTrue(preview.resolvedContent().contains("# database credential"));
		assertFalse(preview.resolvedContent().contains(BackendConfigurationService.REDACTED));

		service.apply("Config.yml", proposal, read.revision());
		String applied = Files.readString(config);
		assertTrue(applied.contains("Password: keep-me"));
		assertTrue(applied.contains("Feature: true"));
		assertTrue(applied.contains("# VotingPlugin owner notes"));
		assertTrue(applied.contains("# Never expose this value"));
		assertTrue(applied.contains("# database credential"));
		assertTrue(applied.contains("# toggle from Control"));
		assertTrue(applied.contains("# End of owner notes"));
	}

	@Test void redactsSecretsRepeatedOrDefinedInsideComments() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "# Password: commented-secret\n"
				+ "Database:\n"
				+ "  Password: active-secret # rotate active-secret soon\n"
				+ "Feature: true # Token=comment-token\n"
				+ "Other: true # Authorization: Bearer old-token\n"
				+ "Hook: true # WebhookURL: https://example.invalid/private hook\n"
				+ "Quoted: true # DatabasePassword: \"two words\"\n");
		BackendConfigurationService.Document read = new BackendConfigurationService(directory, () -> { })
				.read("Config.yml");

		assertFalse(read.content().contains("commented-secret"));
		assertFalse(read.content().contains("active-secret"));
		assertFalse(read.content().contains("comment-token"));
		assertFalse(read.content().contains("Bearer old-token"));
		assertFalse(read.content().contains("example.invalid"));
		assertFalse(read.content().contains("two words"));
		assertTrue(read.content().contains("# Password: " + BackendConfigurationService.REDACTED));
		assertTrue(read.content().contains("# rotate " + BackendConfigurationService.REDACTED + " soon"));
	}

	@Test void doesNotRewriteHashTextInsideBlockScalars() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Message: |\n  # Password: this is message text\nFeature: false\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		BackendConfigurationService.Document read = service.read("Config.yml");
		assertTrue(read.content().contains("# Password: this is message text"));
		service.apply("Config.yml", read.content().replace("Feature: false", "Feature: true"), read.revision());
		assertTrue(Files.readString(config).contains("# Password: this is message text"));
	}

	@Test void redactsWebhookUrlCommentsWithoutReplacingShortSecretsInProse() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Password: true\nDiscordWebhook:\n  URL: '' # URL: https://old-secret.invalid/hook\n"
				+ "Feature: false # This is true when enabled\n");
		BackendConfigurationService.Document read = new BackendConfigurationService(directory, () -> { })
				.read("Config.yml");

		assertFalse(read.content().contains("old-secret.invalid"));
		assertTrue(read.content().contains("# This is true when enabled"));
	}

	@Test void rejectsStaleInvalidAndUnmanagedWrites() throws Exception {
		Files.writeString(directory.resolve("Config.yml"), "Feature: false\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });
		String revision = service.read("Config.yml").revision();
		Files.writeString(directory.resolve("Config.yml"), "Feature: manual\n");
		assertThrows(BackendConfigurationService.StaleRevisionException.class,
				() -> service.apply("Config.yml", "Feature: true\n", revision));
		assertThrows(IllegalArgumentException.class, () -> service.preview("../server.properties", "x: y"));
		assertThrows(IllegalArgumentException.class, () -> service.preview("Config.yml", "bad: [yaml"));
	}

	@Test void rejectsManagedFileAndDirectorySymlinkEscapes() throws Exception {
		Path root = Files.createDirectory(directory.resolve("plugin-data"));
		Path outside = Files.createDirectory(directory.resolve("outside"));
		Path externalConfig = outside.resolve("Config.yml");
		Files.writeString(externalConfig, "Feature: external\n");
		Files.createSymbolicLink(root.resolve("Config.yml"), externalConfig);
		BackendConfigurationService service = new BackendConfigurationService(root, () -> { });

		assertThrows(java.io.IOException.class, () -> service.read("Config.yml"));
		Files.createDirectory(outside.resolve("sites"));
		Files.writeString(outside.resolve("sites/External.yml"), "VoteSites: {}\n");
		Files.createSymbolicLink(root.resolve("VoteSites"), outside.resolve("sites"));
		assertThrows(java.io.IOException.class, () -> service.read("VoteSites/External.yml"));
	}

	@Test void rejectsSymlinkedBackupSidecarsWithoutTouchingTheirTargets() throws Exception {
		Path config = directory.resolve("Config.yml");
		Path external = directory.resolve("external-backup.yml");
		Files.writeString(config, "Feature: false\n");
		Files.writeString(external, "external: preserved\n");
		Files.createSymbolicLink(directory.resolve("Config.yml.control-backup"), external);
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });
		String revision = service.read("Config.yml").revision();

		assertThrows(java.io.IOException.class,
				() -> service.apply("Config.yml", "Feature: true\n", revision));
		assertEquals("external: preserved\n", Files.readString(external));
		assertEquals("Feature: false\n", Files.readString(config));
	}

	@Test void transportValidationFailureRollsBackBeforeApplyReturns() throws Exception {
		Path settings = directory.resolve("BungeeSettings.yml");
		Files.writeString(settings, "UseBungeecord: false\nBungeeMethod: PLUGINMESSAGING\n");
		AtomicInteger reloads = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory,
				(BackendConfigurationService.ApplyAction) fileName -> {
					assertEquals("BungeeSettings.yml", fileName);
					if (reloads.incrementAndGet() == 1) throw new IllegalStateException("transport unavailable");
				});
		BackendConfigurationService.Document before = service.read("BungeeSettings.yml");
		BackendConfigurationService.ApplyFailureException failure = assertThrows(
				BackendConfigurationService.ApplyFailureException.class,
				() -> service.apply("BungeeSettings.yml",
						"UseBungeecord: true\nBungeeMethod: MQTT\n", before.revision()));

		assertTrue(failure.rolledBack());
		assertEquals(2, reloads.get());
		assertTrue(service.read("BungeeSettings.yml").content().contains("PLUGINMESSAGING"));
	}

	@Test void publishedRollbackStillReloadsTheRestoredConfiguration() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Feature: before\n");
		AtomicInteger reloads = new AtomicInteger();
		AtomicInteger moves = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory,
				(BackendConfigurationService.ApplyAction) fileName -> {
					if (reloads.incrementAndGet() == 1) throw new IOException("reload failed");
				}, (source, target) -> {
					Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
					if (moves.incrementAndGet() == 3) {
						throw new DurableFiles.PublishedException(new IOException("directory force failed"));
					}
				});
		BackendConfigurationService.Document before = service.read("Config.yml");

		BackendConfigurationService.ApplyFailureException failure = assertThrows(
				BackendConfigurationService.ApplyFailureException.class,
				() -> service.apply("Config.yml", "Feature: proposed\n", before.revision()));

		assertTrue(failure.rolledBack());
		assertEquals(2, reloads.get());
		assertEquals("Feature: before\n", Files.readString(config));
		assertTrue(java.util.Arrays.stream(failure.getCause().getSuppressed())
				.anyMatch(DurableFiles.PublishedException.class::isInstance));
	}

	@Test void failedReloadDoesNotOverwriteAConcurrentManualEdit() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Feature: before\n");
		AtomicInteger reloads = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory,
				(BackendConfigurationService.ApplyAction) fileName -> {
					reloads.incrementAndGet();
					Files.writeString(config, "Feature: manual\n");
					throw new IllegalStateException("reload failed");
				});
		BackendConfigurationService.Document before = service.read("Config.yml");

		BackendConfigurationService.ApplyFailureException failure = assertThrows(
				BackendConfigurationService.ApplyFailureException.class,
				() -> service.apply("Config.yml", "Feature: proposed\n", before.revision()));

		assertFalse(failure.rolledBack());
		assertEquals(1, reloads.get());
		assertEquals("Feature: manual\n", Files.readString(config));
	}

	@Test void successfulReloadDoesNotReportAConcurrentManualEditAsApplied() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Feature: before\n");
		AtomicInteger reloads = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory,
				(BackendConfigurationService.ApplyAction) fileName -> {
					if (reloads.incrementAndGet() == 1) Files.writeString(config, "Feature: manual\n");
				});
		BackendConfigurationService.Document before = service.read("Config.yml");

		assertThrows(BackendConfigurationService.StaleRevisionException.class,
				() -> service.apply("Config.yml", "Feature: proposed\n", before.revision()));
		assertEquals(2, reloads.get());
		assertEquals("Feature: manual\n", Files.readString(config));
	}

	@Test void reconciliationReloadRetriesWhenAnotherManualEditRacesIt() throws Exception {
		Path config = directory.resolve("Config.yml");
		Files.writeString(config, "Feature: before\n");
		AtomicInteger reloads = new AtomicInteger();
		BackendConfigurationService service = new BackendConfigurationService(directory,
				(BackendConfigurationService.ApplyAction) fileName -> {
					int reload = reloads.incrementAndGet();
					if (reload <= 2) Files.writeString(config, "Feature: manual-" + reload + "\n");
				});
		BackendConfigurationService.Document before = service.read("Config.yml");

		assertThrows(BackendConfigurationService.StaleRevisionException.class,
				() -> service.apply("Config.yml", "Feature: proposed\n", before.revision()));
		assertEquals(3, reloads.get());
		assertEquals("Feature: manual-2\n", Files.readString(config));
	}

	@Test void quickSetupsProduceReviewableBackendAndVoteSiteChanges() throws Exception {
		Files.writeString(directory.resolve("BungeeSettings.yml"), "UseBungeecord: false\nServer: PleaseSet\n");
		Files.writeString(directory.resolve("VoteSites.yml"), "VoteSites: {}\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		BackendConfigurationService.QuickPreview proxy = service.previewQuickSetup("proxy-backend",
				Map.of("server", "lobby", "method", "PLUGINMESSAGING"));
		assertTrue(proxy.proposal().content().contains("UseBungeecord: true"));
		assertTrue(proxy.proposal().content().contains("Server: lobby"));

		BackendConfigurationService.QuickPreview site = service.previewQuickSetup("vote-site", Map.of(
				"name", "PMC", "serviceSite", "PlanetMinecraft.com", "voteUrl", "https://example.com/vote",
				"voteDelay", "24h"));
		assertTrue(site.proposal().content().contains("PMC:"));
		assertTrue(site.proposal().content().contains("PlanetMinecraft.com"));

		BackendConfigurationService.QuickPreview reward = service.previewQuickSetup("easy-reward", Map.of(
				"scope", "site", "name", "PMC", "command", "eco give %player% 100",
				"message", "&aThanks for voting!"));
		assertTrue(reward.proposal().content().contains("eco give %player% 100"));

		Files.writeString(directory.resolve("Config.yml"), "ProcessRewards: false\nAutoCreateVoteSites: false\n"
				+ "ExtraAllSitesCheck: false\nCountFakeVotes: false\nDisableNoServiceSiteMessage: false\n"
				+ "DisableUpdateChecking: false\n");
		BackendConfigurationService.QuickPreview common = service.previewQuickSetup("common-settings", Map.of(
				"processRewards", "true", "autoCreateVoteSites", "true", "extraAllSitesCheck", "true",
				"countFakeVotes", "true", "disableNoServiceSiteMessage", "false",
				"disableUpdateChecking", "false"));
		assertTrue(common.proposal().content().contains("ExtraAllSitesCheck: true"));

		Files.writeString(directory.resolve("SpecialRewards.yml"), "VoteParty:\n  Enabled: false\n");
		BackendConfigurationService.QuickPreview party = service.previewQuickSetup("vote-party", Map.of(
				"votesRequired", "25", "command", "give %player% diamond 1", "broadcast", "Party!",
				"giveAllPlayers", "false", "onlineOnly", "true"));
		assertTrue(party.proposal().content().contains("VotesRequired: 25"));
	}

	@Test void proxyBackendQuickSetupRejectsUnknownTransportMethods() throws Exception {
		Files.writeString(directory.resolve("BungeeSettings.yml"),
				"UseBungeecord: false\nServer: PleaseSet\nBungeeMethod: PLUGINMESSAGING\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		assertThrows(IllegalArgumentException.class, () -> service.previewQuickSetup("proxy-backend",
				Map.of("server", "lobby", "method", "NOT_A_TRANSPORT")));
		BackendConfigurationService.QuickPreview mqtt = service.previewQuickSetup("proxy-backend",
				Map.of("server", "lobby", "method", "mqtt"));
		assertTrue(mqtt.proposal().content().contains("BungeeMethod: MQTT"));
	}

	@Test void voteSitesSyncAddsAndUpdatesDefinitionsWithoutTouchingRewardsOrTargetOnlySites() throws Exception {
		Path voteSites = directory.resolve("VoteSites.yml");
		Files.writeString(voteSites, "VoteSites:\n"
				+ "  PMC:\n"
				+ "    Name: Target name\n"
				+ "    VoteURL: https://target.example/vote\n"
				+ "    Rewards:\n"
				+ "      Commands: ['target reward']\n"
				+ "    WaitUntilVoteDelayRewards:\n"
				+ "      Commands: ['target rejected reward']\n"
				+ "  TargetOnly:\n"
				+ "    Name: Keep me\n"
				+ "EverySiteReward:\n"
				+ "  Commands: ['target every-site reward']\n");
		String source = "# Source network sites\n"
				+ "VoteSites:\n"
				+ "  # Planet Minecraft settings\n"
				+ "  PMC:\n"
				+ "    Name: Source name # synchronized field\n"
				+ "    VoteURL: https://source.example/vote\n"
				+ "    DisplayItem:\n"
				+ "      Material: EMERALD\n"
				+ "    Rewards:\n"
				+ "      Commands: ['source reward']\n"
				+ "    WaitUntilVoteDelayRewards:\n"
				+ "      Commands: ['source rejected reward']\n"
				+ "  NewSite:\n"
				+ "    Name: Added site\n"
				+ "    VoteURL: https://new.example/vote\n"
				+ "    Rewards:\n"
				+ "      Commands: ['source new reward']\n"
				+ "EverySiteReward:\n"
				+ "  Commands: ['source every-site reward']\n";
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });
		BackendConfigurationService.Document before = service.read("VoteSites.yml");

		BackendConfigurationService.QuickPreview preview = service.previewQuickSetup("sync-vote-sites",
				Map.of("sourceContent", source));
		YamlConfiguration proposal = new YamlConfiguration();
		proposal.options().parseComments(true);
		proposal.loadFromString(preview.proposal().content());
		assertEquals("Source name", proposal.getString("VoteSites.PMC.Name"));
		assertEquals("EMERALD", proposal.getString("VoteSites.PMC.DisplayItem.Material"));
		assertEquals(List.of("target reward"), proposal.getStringList("VoteSites.PMC.Rewards.Commands"));
		assertEquals(List.of("target rejected reward"),
				proposal.getStringList("VoteSites.PMC.WaitUntilVoteDelayRewards.Commands"));
		assertEquals("Keep me", proposal.getString("VoteSites.TargetOnly.Name"));
		assertEquals("Added site", proposal.getString("VoteSites.NewSite.Name"));
		assertFalse(proposal.contains("VoteSites.NewSite.Rewards"));
		assertEquals(List.of("target every-site reward"), proposal.getStringList("EverySiteReward.Commands"));
		assertTrue(preview.proposal().content().contains("# Planet Minecraft settings"));
		assertTrue(preview.proposal().content().contains("# synchronized field"));
		assertTrue(preview.changes().stream().noneMatch(change -> change.toLowerCase().contains("reward")));

		service.applyQuickSetup("sync-vote-sites", Map.of("sourceContent", source), before.revision());
		String applied = Files.readString(voteSites);
		assertTrue(applied.contains("target reward"));
		assertFalse(applied.contains("source reward"));
		assertTrue(applied.contains("TargetOnly"));
		assertTrue(applied.contains("NewSite"));
		assertTrue(applied.contains("# Planet Minecraft settings"));
	}

	@Test void voteSitesSyncMatchesRootSitesAndFieldsCaseInsensitively() throws Exception {
		Files.writeString(directory.resolve("Config.yml"), "CaseInsensitiveYMLFiles: true\n");
		Files.writeString(directory.resolve("VoteSites.yml"), "votesites:\n  pmc:\n    name: Target\n    Rewards:\n      Commands: ['keep']\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		BackendConfigurationService.QuickPreview preview = service.previewQuickSetup("sync-vote-sites", Map.of(
				"sourceContent", "VOTESITES:\n  PMC:\n    Name: Source\n    VoteURL: https://example.invalid/vote\n"));
		YamlConfiguration proposal = new YamlConfiguration();
		proposal.loadFromString(preview.proposal().content());

		assertEquals("Source", proposal.getString("votesites.pmc.name"));
		assertEquals("https://example.invalid/vote", proposal.getString("votesites.pmc.VoteURL"));
		assertEquals(List.of("keep"), proposal.getStringList("votesites.pmc.Rewards.Commands"));
		assertEquals(1, proposal.getKeys(false).size());
		assertEquals(1, proposal.getConfigurationSection("votesites").getKeys(false).size());
	}

	@Test void voteSitesSyncKeepsDistinctKeysWhenCaseInsensitiveFilesAreDisabled() throws Exception {
		Files.writeString(directory.resolve("Config.yml"), "caseinsensitiveymlfiles: false\n");
		Files.writeString(directory.resolve("VoteSites.yml"), "VoteSites:\n  PMC:\n    Name: Upper target\n"
				+ "  pmc:\n    Name: Lower target\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		BackendConfigurationService.QuickPreview preview = service.previewQuickSetup("sync-vote-sites", Map.of(
				"sourceContent", "votesites:\n  PMC:\n    Name: Updated upper\n"));
		YamlConfiguration proposal = new YamlConfiguration();
		proposal.loadFromString(preview.proposal().content());

		assertEquals("Updated upper", proposal.getString("VoteSites.PMC.Name"));
		assertEquals("Lower target", proposal.getString("VoteSites.pmc.Name"));
	}

	@Test void voteSitesSyncRejectsMalformedOrMissingSourceSections() throws Exception {
		Files.writeString(directory.resolve("VoteSites.yml"), "VoteSites: {}\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		assertThrows(IllegalArgumentException.class, () -> service.previewQuickSetup("sync-vote-sites",
				Map.of("sourceContent", "not: [yaml")));
		assertThrows(IllegalArgumentException.class, () -> service.previewQuickSetup("sync-vote-sites",
				Map.of("sourceContent", "Other: value\n")));
	}

	@Test void fullBungeeSettingsRejectsUnknownTransportMethods() throws Exception {
		Files.writeString(directory.resolve("BungeeSettings.yml"),
				"UseBungeecord: true\nServer: lobby\nBungeeMethod: PLUGINMESSAGING\n");
		BackendConfigurationService service = new BackendConfigurationService(directory, () -> { });

		assertThrows(IllegalArgumentException.class, () -> service.preview("BungeeSettings.yml",
				"UseBungeecord: true\nServer: lobby\nBungeeMethod: NOT_A_TRANSPORT\n"));
		BackendConfigurationService.Preview mqtt = service.preview("BungeeSettings.yml",
				"UseBungeecord: true\nServer: lobby\nBungeeMethod: mqtt\n");
		assertTrue(mqtt.resolvedContent().contains("BungeeMethod: MQTT"));
	}
}
