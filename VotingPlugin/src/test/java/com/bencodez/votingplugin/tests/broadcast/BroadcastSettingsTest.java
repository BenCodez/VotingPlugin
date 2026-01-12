package com.bencodez.votingplugin.tests.broadcast;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

import org.bukkit.configuration.ConfigurationSection;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.bencodez.votingplugin.broadcast.BroadcastSettings;
import com.bencodez.votingplugin.broadcast.VoteBroadcastType;

public class BroadcastSettingsTest {

	@Test
	public void fromConfig_defaults_when_missing_keys() {
	    ConfigurationSection cfg = Mockito.mock(ConfigurationSection.class);

	    // Let implementation defaults apply
	    Mockito.when(cfg.getString(eq("Type"), anyString())).thenReturn(null);
	    Mockito.when(cfg.getString(eq("Duration"), anyString())).thenReturn(null);
	    Mockito.when(cfg.getInt(eq("MaxSitesListed"), anyInt())).thenReturn(5);
	    Mockito.when(cfg.getConfigurationSection("Format")).thenReturn(null);

	    BroadcastSettings s = BroadcastSettings.load(cfg);

	    assertEquals(VoteBroadcastType.NONE, s.getType());
	    assertNotNull(s.getDuration());
	    assertEquals(5, s.getMaxSitesListed());
	    assertNotNull(s.getFormat());
	}


	@Test
	public void fromConfig_reads_format_overrides() {
		ConfigurationSection cfg = Mockito.mock(ConfigurationSection.class);
		ConfigurationSection fmt = Mockito.mock(ConfigurationSection.class);

		Mockito.when(cfg.getString(Mockito.eq("Type"), Mockito.anyString())).thenReturn("EVERY_VOTE");
		Mockito.when(cfg.getString(Mockito.eq("Duration"), Mockito.anyString())).thenReturn("10m");
		Mockito.when(cfg.getInt(Mockito.eq("MaxSitesListed"), Mockito.anyInt())).thenReturn(2);
		Mockito.when(cfg.getConfigurationSection("Format")).thenReturn(fmt);

		Mockito.when(fmt.getString(Mockito.eq("BroadcastMsg"), Mockito.anyString())).thenReturn("BM %player% %site%");
		Mockito.when(fmt.getString(Mockito.eq("Header"), Mockito.anyString())).thenReturn("H %player% %sites_count%");
		Mockito.when(fmt.getString(Mockito.eq("ListLine"), Mockito.anyString())).thenReturn("L %site%");

		BroadcastSettings s = BroadcastSettings.load(cfg);

		assertEquals(2, s.getMaxSitesListed());
		assertEquals("BM Ben SiteA", s.getFormat().render("Ben", java.util.Arrays.asList("SiteA"), "vote").get(0));
	}
}
