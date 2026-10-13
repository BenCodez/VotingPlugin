package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.concurrent.TimeUnit;

import org.bukkit.ChatColor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

class ReleaseSiteFormattingTest {

	private VotingPluginMain plugin;
	private ConfigVoteSites config;

	@BeforeEach
	void setup() {
		plugin = mock(VotingPluginMain.class);
		config = mock(ConfigVoteSites.class);
		when(plugin.getConfigVoteSites()).thenReturn(config);
		when(plugin.getConfigFile()).thenReturn(mock(Config.class));
		when(config.getDisplayName(anyString())).thenReturn("");
		when(config.getVoteDelay(anyString())).thenReturn(ParsedDuration.parse("12h", TimeUnit.HOURS));
		when(config.getServiceSite(anyString())).thenReturn("Example-Site.com");
		when(config.getVoteURL(anyString())).thenReturn("https://example.com/vote");
	}

	@Test
	void fallbackSiteUsesOnlyConfiguredTemplateSpacingInFinalVisibleText() {
		VoteSite site = new VoteSite(plugin, "Minecraft Menu");
		for (String[] row : new String[][] {
				{ "&7Site: &e%SiteName%", "Site: Minecraft Menu" },
				{ "[%SiteName%]", "[Minecraft Menu]" },
				{ "Vote on %SiteName%!", "Vote on Minecraft Menu!" },
				{ "Site:%SiteName%", "Site:Minecraft Menu" },
				{ "&a%SiteName%&7 - Vote now", "Minecraft Menu - Vote now" } }) {
			assertEquals(row[1], visible(row[0].replace("%SiteName%", site.getDisplayNameForFormatting())));
		}
	}

	@Test
	void serviceIdentifierHasReleasePunctuationWithoutAddedSpaces() {
		VoteSite site = new VoteSite(plugin, "example");
		assertEquals("Example-Site.com", site.getServiceSite());
		assertEquals("[Example-Site.com]", visible("[" + site.getServiceSiteForFormatting() + "]"));
	}

	@Test
	void explicitConfiguredFormattingAndWhitespaceRemainExact() {
		when(config.getDisplayName("example")).thenReturn("  &aConfigured Name  ");
		VoteSite site = new VoteSite(plugin, "example");
		assertEquals("  &aConfigured Name  ", site.getDisplayNameForFormatting());
		assertEquals("[  Configured Name  ]", visible("[" + site.getDisplayNameForFormatting() + "]"));
		assertEquals("  " + ChatColor.GREEN + "Configured Name  ",
				MessageAPI.colorize(site.getDisplayNameForFormatting()));
	}

	@Test
	void legacyDisplayValuesMatchTheReleaseWithoutSubstitutions() {
		for (String value : new String[] { "R&D", "&aConfigured", "%player_name%", "{RED}", "  Name  " }) {
			VoteSite site = new VoteSite(plugin, value);
			assertEquals(value, site.getDisplayNameForFormatting());
		}
	}

	@Test
	void repeatedReadsDoNotAccumulateWhitespaceOrInvisibleMarkers() {
		VoteSite site = new VoteSite(plugin, "Minecraft Menu");
		for (int refresh = 0; refresh < 5; refresh++) {
			assertEquals("Minecraft Menu", site.getDisplayNameForFormatting());
			assertFalse(site.getDisplayNameForFormatting().contains("\u2060"));
		}
	}

	@Test
	void realVoteLastChatLineUsesTheReleaseSiteValue() throws Exception {
		VoteSite site = new VoteSite(plugin, "Minecraft Menu");
		VotingPluginUser user = messageUser(site);
		when(plugin.getConfigFile().getFormatCommandsVoteLastLine()).thenReturn("&7Site: &e%SiteName%");
		assertEquals("Site: Minecraft Menu", visible(user.voteCommandLastLine(site)));
	}

	@Test
	void realVoteLastGuiLineUsesTheReleaseSiteValue() throws Exception {
		VoteSite site = new VoteSite(plugin, "Minecraft Menu");
		VotingPluginUser user = messageUser(site);
		GUI gui = mock(GUI.class);
		when(plugin.getGui()).thenReturn(gui);
		when(gui.getChestVoteLastLine()).thenReturn("&a[%SiteName%]");
		assertEquals("[Minecraft Menu]", visible(user.voteCommandLastGUILine(site)));
	}

	private VotingPluginUser messageUser(VoteSite site) throws Exception {
		VotingPluginUser user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
		java.lang.reflect.Field pluginField = VotingPluginUser.class.getDeclaredField("plugin");
		pluginField.setAccessible(true);
		pluginField.set(user, plugin);
		doReturn("yesterday").when(user).voteCommandLastDate(site);
		doReturn("one day").when(user).voteCommandLastDuration(site);
		return user;
	}

	private static String visible(String configuredText) {
		return ChatColor.stripColor(PlaceholderUtils.parseJson(MessageAPI.colorize(configuredText)).toPlainText());
	}
}
