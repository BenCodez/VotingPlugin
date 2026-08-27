package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfiguration;

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
}
