package com.bencodez.votingplugin.presets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VoteSitePresetSafetyTest {

	@Test
	void filtersAndBoundsPresetPaths() throws Exception {
		String json = "["
				+ "{\"type\":\"file\",\"path\":\"presets/votesites/crafty-gg.meta.json\"},"
				+ "{\"type\":\"file\",\"path\":\"presets/rewards/not-allowed.meta.json\"}"
				+ "]";

		assertEquals(Collections.singletonList("presets/votesites/crafty-gg.meta.json"),
				GitHubVoteSitePresetLoader.parsePresetPaths(json.getBytes(StandardCharsets.UTF_8)));
		assertFalse(GitHubVoteSitePresetLoader.isPresetPathAllowed("presets/votesites/../secret.meta.json"));

		StringBuilder tooMany = new StringBuilder("[");
		for (int i = 0; i <= GitHubVoteSitePresetLoader.MAX_PRESET_COUNT; i++) {
			if (i > 0) tooMany.append(',');
			tooMany.append("{\"type\":\"file\",\"path\":\"presets/votesites/site-")
					.append(i).append(".meta.json\"}");
		}
		tooMany.append(']');
		assertThrows(IOException.class, () -> GitHubVoteSitePresetLoader
				.parsePresetPaths(tooMany.toString().getBytes(StandardCharsets.UTF_8)));
	}

	@Test
	void handsPlayerUiBackToPlayerLane() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player player = mock(Player.class);
		GitHubVoteSitePresetLoader loader = mock(GitHubVoteSitePresetLoader.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(loader.listAllVoteSitePresets()).thenReturn(Collections.emptyList());

		new VoteSitePresetSetupHandler(plugin, loader).startSetup(player);

		verify(loader).listAllVoteSitePresets();
		verify(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
	}
}
