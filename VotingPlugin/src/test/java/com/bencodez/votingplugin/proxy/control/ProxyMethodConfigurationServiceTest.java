package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

class ProxyMethodConfigurationServiceTest {
	private VotingPluginProxy proxy;
	private VotingPluginProxyConfig config;
	private ProxyMethodConfigurationService service;

	@BeforeEach
	void setUp() {
		proxy = mock(VotingPluginProxy.class);
		config = mock(VotingPluginProxyConfig.class);
		when(proxy.getConfig()).thenReturn(config);
		when(proxy.getAllConfiguredServers()).thenReturn(Set.of("lobby"));
		service = new ProxyMethodConfigurationService(proxy);
	}

	@Test
	void validatesRequiredSettingsForEveryTransport() {
		assertThrows(IllegalArgumentException.class,
				() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.PLUGINMESSAGING)));
		when(config.getPluginMessageChannel()).thenReturn("vp:vp");
		assertDoesNotThrow(() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.PLUGINMESSAGING)));

		assertThrows(IllegalArgumentException.class,
				() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.REDIS)));
		when(config.getRedisHost()).thenReturn("localhost");
		when(config.getRedisPort()).thenReturn(6379);
		assertDoesNotThrow(() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.REDIS)));

		when(config.getMqttBrokerURL()).thenReturn("not-a-broker");
		assertThrows(IllegalArgumentException.class,
				() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.MQTT)));
		when(config.getMqttBrokerURL()).thenReturn("tcp://localhost:1883");
		when(config.getMqttClientID()).thenReturn("proxy");
		assertDoesNotThrow(() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.MQTT)));

		when(config.getBungeePort()).thenReturn(1297);
		when(proxy.getAllConfiguredServers()).thenReturn(Set.of("lobby", "blocked"));
		when(config.getBlockedServers()).thenReturn(List.of("blocked"));
		when(config.getSpigotServerConfiguration("lobby")).thenReturn(Map.of("Host", "localhost"));
		assertDoesNotThrow(() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.SOCKETS)));

		assertThrows(IllegalArgumentException.class,
				() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.MYSQL)));
		when(config.hasDatabaseConfigured()).thenReturn(true);
		assertDoesNotThrow(() -> service.validate(new ProxyMethodConfiguration(BungeeMethod.MYSQL)));
	}

	@Test
	void rejectsUnknownMethodNamesInsteadOfFallingBack() {
		assertThrows(IllegalArgumentException.class, () -> ProxyMethodConfigurationService.canonical("unknown"));
	}

	@Test
	void readsUnknownPersistedMethodsUsingTheRuntimeFallback() {
		when(config.getBungeeMethod()).thenReturn("unknown");
		assertEquals(BungeeMethod.PLUGINMESSAGING, service.read().method());
	}

	@Test
	void applyValidatesTheFreshlyLoadedPersistedSnapshot() throws Exception {
		when(config.getBungeeMethod()).thenReturn("PLUGINMESSAGING");
		when(config.getRedisHost()).thenReturn("localhost");
		when(config.getRedisPort()).thenReturn(6379);
		VotingPluginProxyConfig fresh = mock(VotingPluginProxyConfig.class);
		doAnswer(invocation -> {
			VotingPluginProxyConfig.ControlProxyMethodValidator validator = invocation.getArgument(2);
			validator.validate(fresh);
			return null;
		}).when(config).persistControlProxyMethod(org.mockito.ArgumentMatchers.eq("REDIS"),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any());

		ProxyMethodConfiguration proposal = new ProxyMethodConfiguration(BungeeMethod.REDIS);
		assertThrows(IllegalArgumentException.class, () -> service.apply(proposal, service.read().revision()));
	}

	@Test
	void applyDoesNotRejectAValidFreshSnapshotUsingStaleCachedSettings() throws Exception {
		when(config.getBungeeMethod()).thenReturn("PLUGINMESSAGING");
		VotingPluginProxyConfig fresh = mock(VotingPluginProxyConfig.class);
		when(fresh.getRedisHost()).thenReturn("localhost");
		when(fresh.getRedisPort()).thenReturn(6379);
		doAnswer(invocation -> {
			VotingPluginProxyConfig.ControlProxyMethodValidator validator = invocation.getArgument(2);
			validator.validate(fresh);
			return null;
		}).when(config).persistControlProxyMethod(org.mockito.ArgumentMatchers.eq("REDIS"),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any());

		ProxyMethodConfiguration proposal = new ProxyMethodConfiguration(BungeeMethod.REDIS);
		assertDoesNotThrow(() -> service.apply(proposal, service.read().revision()));
	}
}
