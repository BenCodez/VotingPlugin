package com.bencodez.votingplugin.tests.presets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.presets.DisplayInfo;
import com.bencodez.votingplugin.presets.GitHubVoteSitePresetLoader;
import com.bencodez.votingplugin.presets.MatchInfo;
import com.bencodez.votingplugin.presets.PlaceholderDef;
import com.bencodez.votingplugin.presets.VoteSitePreset;

/**
 * Tests for {@link GitHubVoteSitePresetLoader}.
 */
public class GitHubVoteSitePresetLoaderTest {

	/**
	 * Simple test loader that avoids live GitHub calls.
	 */
	private static class TestGitHubVoteSitePresetLoader extends GitHubVoteSitePresetLoader {

		private final List<VoteSitePreset> presets;

		/**
		 * Creates the loader.
		 *
		 * @param presets presets to return
		 */
		public TestGitHubVoteSitePresetLoader(List<VoteSitePreset> presets) {
			super("BenCodez", "VotingPlugin-Presets", "main");
			this.presets = presets;
		}

		@Override
		public List<VoteSitePreset> listAllVoteSitePresets() throws IOException {
			return presets;
		}
	}

	/**
	 * Verifies an exact domain match returns the correct preset.
	 *
	 * @throws IOException if loading fails
	 * @throws InterruptedException 
	 */
	@Test
	public void testFindVoteSitePresetForURLExactMatch() throws IOException, InterruptedException {
		VoteSitePreset mcServers = createPreset("votesite:mc-servers-com", "mc-servers.com",
				Arrays.asList("mc-servers.com", "www.mc-servers.com"));

		VoteSitePreset bestMinecraftServers = createPreset("votesite:best-minecraft-servers", "best-minecraft-servers.co",
				Arrays.asList("best-minecraft-servers.co", "www.best-minecraft-servers.co"));

		TestGitHubVoteSitePresetLoader loader = new TestGitHubVoteSitePresetLoader(
				Arrays.asList(bestMinecraftServers, mcServers));

		VoteSitePreset result = loader.findVoteSitePresetForURL("https://www.mc-servers.com/vote");

		assertEquals("votesite:mc-servers-com", result.getId());
	}

	/**
	 * Verifies a subdomain match returns the correct preset.
	 *
	 * @throws IOException if loading fails
	 * @throws InterruptedException 
	 */
	@Test
	public void testFindVoteSitePresetForURLSubdomainMatch() throws IOException, InterruptedException {
		VoteSitePreset preset = createPreset("votesite:mc-servers-com", "mc-servers.com",
				Arrays.asList("mc-servers.com"));

		TestGitHubVoteSitePresetLoader loader = new TestGitHubVoteSitePresetLoader(
				Arrays.asList(preset));

		VoteSitePreset result = loader.findVoteSitePresetForURL("https://vote.mc-servers.com/server/test");

		assertEquals("votesite:mc-servers-com", result.getId());
	}

	/**
	 * Verifies that a similar URL does not incorrectly match another preset.
	 *
	 * @throws IOException if loading fails
	 * @throws InterruptedException 
	 */
	@Test
	public void testFindVoteSitePresetForURLDoesNotMatchWrongPreset() throws IOException, InterruptedException {
		VoteSitePreset mcServers = createPreset("votesite:mc-servers-com", "MC-Servers.com",
				Arrays.asList("mc-servers.com", "www.mc-servers.com"));

		VoteSitePreset bestMinecraftServers = createPreset("votesite:best-minecraft-servers", "Best Minecraft Servers",
				Arrays.asList("best-minecraft-servers.co", "www.best-minecraft-servers.co"));

		TestGitHubVoteSitePresetLoader loader = new TestGitHubVoteSitePresetLoader(
				Arrays.asList(mcServers, bestMinecraftServers));

		VoteSitePreset result = loader.findVoteSitePresetForURL("https://best-minecraft-servers.co/server/example/vote");

		assertEquals("votesite:best-minecraft-servers", result.getId());
	}

	/**
	 * Verifies that an unmatched URL returns null.
	 *
	 * @throws IOException if loading fails
	 * @throws InterruptedException 
	 */
	@Test
	public void testFindVoteSitePresetForURLNoMatch() throws IOException, InterruptedException {
		VoteSitePreset preset = createPreset("votesite:mc-servers-com", "MC-Servers.com",
				Arrays.asList("mc-servers.com"));

		TestGitHubVoteSitePresetLoader loader = new TestGitHubVoteSitePresetLoader(
				Arrays.asList(preset));

		VoteSitePreset result = loader.findVoteSitePresetForURL("https://example.com/vote");

		assertNull(result);
	}

	/**
	 * Creates a simple preset.
	 *
	 * @param id id
	 * @param serviceSite service site
	 * @param domains domains
	 * @return preset
	 */
	private VoteSitePreset createPreset(String id, String serviceSite, List<String> domains) {
		VoteSitePreset preset = new VoteSitePreset();
		preset.setId(id);

		DisplayInfo display = new DisplayInfo();
		display.setName(id);
		preset.setDisplay(display);

		MatchInfo matchInfo = new MatchInfo();
		matchInfo.setDomains(new ArrayList<String>(domains));
		preset.setMatch(matchInfo);

		PlaceholderDef serviceSiteDef = new PlaceholderDef();
		serviceSiteDef.setDefaultValue(serviceSite);

		java.util.Map<String, PlaceholderDef> placeholders = new java.util.HashMap<String, PlaceholderDef>();
		placeholders.put("serviceSite", serviceSiteDef);
		preset.setPlaceholders(placeholders);

		return preset;
	}
}