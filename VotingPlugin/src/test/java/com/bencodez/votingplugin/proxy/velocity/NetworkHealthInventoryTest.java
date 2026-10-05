package com.bencodez.votingplugin.proxy.velocity;
import com.velocitypowered.api.plugin.PluginContainer;
import java.util.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
class NetworkHealthInventoryTest {
 @Test void optionalProvidersAreActualInstancesRatherThanContainers() {
  PluginContainer installed = mock(PluginContainer.class), absent = mock(PluginContainer.class);
  Object provider = new Object();
  doReturn(Optional.of(provider)).when(installed).getInstance();
  doReturn(Optional.empty()).when(absent).getInstance();
  assertEquals(List.of(provider), VotingPluginVelocity.diagnosticProviders(List.of(installed, absent)));
 }
 @Test void velocityReportsDeclaredNamesWithStableIdFallback() {
  PluginContainer installed = mock(PluginContainer.class); var description = mock(com.velocitypowered.api.plugin.PluginDescription.class);
  when(installed.getDescription()).thenReturn(description);
  when(description.getName()).thenReturn(Optional.of("VotifierPlus"));
  assertEquals(List.of("VotifierPlus"), VotingPluginVelocity.installedPluginNames(List.of(installed)));
  when(description.getName()).thenReturn(Optional.empty()); when(description.getId()).thenReturn("votifierplus");
  assertEquals(List.of("votifierplus"), VotingPluginVelocity.installedPluginNames(List.of(installed)));
 }

 @Test void diagnosticProvidersRetainProvidersBeyondDisplayInventoryLimit() {
  List<PluginContainer> plugins = new ArrayList<>();
  for (int i = 0; i < 129; i++) {
   PluginContainer plugin = mock(PluginContainer.class);
   doReturn(Optional.of(new Object())).when(plugin).getInstance();
   plugins.add(plugin);
  }
  assertEquals(129, VotingPluginVelocity.diagnosticProviders(plugins).size());
 }

}
