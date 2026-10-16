package com.bencodez.votingplugin.tests.proxy.bungee;

import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import java.util.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
class NetworkHealthInventoryTest {
 @Test void bungeeReportsPluginDescriptionNamesWithInventoryBound() {
  net.md_5.bungee.api.plugin.Plugin plugin = mock(net.md_5.bungee.api.plugin.Plugin.class);
  var description = mock(net.md_5.bungee.api.plugin.PluginDescription.class);
  when(plugin.getDescription()).thenReturn(description); when(description.getName()).thenReturn("VotifierPlus");
  assertEquals(List.of("VotifierPlus"), VotingPluginBungeeAccess.installedPluginNames(List.of(plugin)));
  assertEquals(128, VotingPluginBungeeAccess.installedPluginNames(Collections.nCopies(129, plugin)).size());
 }
}
