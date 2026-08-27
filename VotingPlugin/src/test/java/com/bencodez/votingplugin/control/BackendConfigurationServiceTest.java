package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

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
