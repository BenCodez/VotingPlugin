package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfiguration;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfiguration;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfigurationService;

class VelocityConfigControlTest {
	@TempDir Path directory;

	@Test
	void omittedSendVotesSettingUsesRuntimeDefaultDuringRevisionCheck() throws Exception {
		Path file = directory.resolve("velocity.yml");
		Files.writeString(file, "BlockedServers: []\n");
		VelocityConfig config = new VelocityConfig(file.toFile());
		config.loadControlConfiguration();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(config.getSendVotesToAllServers(),
				config.getBlockedServers());

		assertTrue(config.getSendVotesToAllServers());
		config.persistControlProxyRouting(false, List.of("lobby"), current.revision());
		config.loadControlConfiguration();

		assertFalse(config.getSendVotesToAllServers());
		assertTrue(config.getBlockedServers().contains("lobby"));
	}

	@Test
	void rollbackRejectsASymbolicBackupSidecar() throws Exception {
		Path file = directory.resolve("velocity.yml");
		Files.writeString(file, "SendVotesToAllServers: true\nBlockedServers: []\n");
		VelocityConfig config = new VelocityConfig(file.toFile());
		config.loadControlConfiguration();
		ProxyRoutingConfiguration current = new ProxyRoutingConfiguration(config.getSendVotesToAllServers(),
				config.getBlockedServers());
		config.persistControlProxyRouting(false, List.of("lobby"), current.revision());
		Path backup = file.resolveSibling(file.getFileName() + ".control-backup");
		Path external = directory.resolve("external.yml");
		Files.writeString(external, "external: preserved\n");
		Files.delete(backup);
		Files.createSymbolicLink(backup, external);

		assertThrows(java.io.IOException.class, config::rollbackControlProxyRouting);
		assertTrue(Files.readString(file).contains("lobby"));
		assertTrue(Files.readString(external).contains("preserved"));
	}

	@Test
	void controlPersistsOnlyTheRevisionedProxyMethod() throws Exception {
		Path file = directory.resolve("velocity.yml");
		Files.writeString(file, "BungeeMethod: PLUGINMESSAGING\nRedis:\n  Host: localhost\n  Port: 6379\n");
		VelocityConfig config = new VelocityConfig(file.toFile());
		config.loadControlConfiguration();
		ProxyMethodConfiguration current = new ProxyMethodConfiguration(
				ProxyMethodConfigurationService.canonical(config.getBungeeMethod()));

		config.persistControlProxyMethod("REDIS", current.revision());
		config.loadControlConfiguration();

		assertEquals("REDIS", config.getBungeeMethod());
		assertTrue(Files.readString(file).contains("Host: localhost"));
	}
}
