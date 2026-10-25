package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.proxy.PendingIncomingVote;
import com.bencodez.votingplugin.proxy.PendingIncomingVoteQueue;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.times;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.nio.file.Path;
import java.util.UUID;
import java.util.ArrayList;
import org.mockito.ArgumentCaptor;
import java.util.logging.Logger;

import org.bstats.velocity.Metrics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.velocitypowered.api.proxy.ProxyServer;

class ServiceSiteVoteLoggingRegressionTest {
	@Test
	void bungeeVoteProcessingUsesServiceSiteSanitizerForTheActualReceiptLog() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class, CALLS_REAL_METHODS);
		VotingPluginProxy runtime = mock(VotingPluginProxy.class);
		Logger logger = mock(Logger.class);
		when(plugin.getLogger()).thenReturn(logger);
		prepareBungee(plugin, runtime);
		String[] services = { "Minecraft-MP.com", "PlanetMinecraft.com", "Minecraft Servers", "R&D",
				"Top100Servers.net", "Safe\nName", "Safe\u2060Name", null };
		for (String service : services) {
			processBungeeVote(plugin, service, "player.name");
			verify(runtime).vote(eq("player.name"), eq(service), eq(true), eq(true), eq(0L), eq(null), eq(null),
					any(UUID.class));

		}
		String longService = "x".repeat(140);
		processBungeeVote(plugin, longService, "example_player");
		verify(runtime).vote(eq("example_player"), eq(longService), eq(true), eq(true), eq(0L), eq(null), eq(null),
				any(UUID.class));
		assertReceiptLogs(logger, services);
	}

	@Test
	void velocityVoteProcessingUsesServiceSiteSanitizerForTheActualReceiptLog(@TempDir Path dataDirectory)
				throws Exception {
		org.slf4j.Logger logger = mock(org.slf4j.Logger.class);
		VotingPluginVelocity plugin = new VotingPluginVelocity(mock(ProxyServer.class), logger,
				mock(Metrics.Factory.class), dataDirectory);
		try {
			VotingPluginProxy runtime = mock(VotingPluginProxy.class);
			prepareVelocity(plugin, runtime);
			String[] services = { "Minecraft-MP.com", "PlanetMinecraft.com", "Minecraft Servers", "R&D",
					"Top100Servers.net", "Safe\nName", "Safe\u2060Name", null };
			for (String service : services) {
				processVelocityVote(plugin, service, "player.name");
				verify(runtime).vote(eq("player.name"), eq(service), eq(true), eq(false), eq(0L), eq(null), eq(null),
					any(UUID.class));

			}
			String longService = "x".repeat(140);
			processVelocityVote(plugin, longService, "example_player");
			verify(runtime).vote(eq("example_player"), eq(longService), eq(true), eq(false), eq(0L), eq(null), eq(null),
					any(UUID.class));
			ArgumentCaptor<String> logs = ArgumentCaptor.forClass(String.class);
			verify(logger, times(services.length + 1)).info(logs.capture());
			assertEquals(expectedReceiptLogs(services), logs.getAllValues());
		} finally {
			plugin.getTimer().shutdownNow();
		}
	}

	@Test
	void serviceSiteLogSanitizationPreservesPunctuationAndBlocksControlCharacters() {
		assertEquals("Minecraft-MP.com Servers", ServiceSiteValidator.sanitizeForLog("Minecraft-MP.com Servers"));
		assertEquals("R&D", ServiceSiteValidator.sanitizeForLog("R&D"));
		assertEquals("Safe?Name", ServiceSiteValidator.sanitizeForLog("Safe\nName"));
	}

	private static void prepareBungee(VotingPluginBungee plugin, VotingPluginProxy runtime) throws Exception {
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
	}

	private static void prepareVelocity(VotingPluginVelocity plugin, VotingPluginProxy runtime) throws Exception {
		setField(plugin, "reloadLock", new Object());
		setField(plugin, "pendingIncomingVotes", new PendingIncomingVoteQueue());
		setField(plugin, "votingPluginProxy", runtime);
		setField(plugin, "runtimeOperational", true);
	}

	private static void processBungeeVote(VotingPluginBungee plugin, String service, String player) throws Exception {
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.admit(player, service);
		invokePendingProcessor(plugin, pending);
	}

	private static void processVelocityVote(VotingPluginVelocity plugin, String service, String player) throws Exception {
		PendingIncomingVote pending = ((PendingIncomingVoteQueue) getField(plugin, "pendingIncomingVotes"))
				.admit(player, service);
		invokePendingProcessor(plugin, pending);
	}

	private static void assertReceiptLogs(Logger logger, String[] services) {
		ArgumentCaptor<String> logs = ArgumentCaptor.forClass(String.class);
		verify(logger, times(services.length + 1)).info(logs.capture());
		assertEquals(expectedReceiptLogs(services), logs.getAllValues());
	}

	private static ArrayList<String> expectedReceiptLogs(String[] services) {
		ArrayList<String> logs = new ArrayList<>();
		for (String service : services) logs.add("Vote received player?name from service site " + expectedServiceLog(service));
		logs.add("Vote received example_player from service site " + "x".repeat(128) + "...");
		return logs;
	}

	private static String expectedServiceLog(String service) {
		return service == null ? "<null>" : service.replace("\n", "?").replace("\u2060", "?");
	}

	private static void invokePendingProcessor(Object plugin, PendingIncomingVote pending) throws Exception {
		java.lang.reflect.Method method = findMethod(plugin.getClass(), "processPendingIncomingVote",
				PendingIncomingVote.class);
		method.setAccessible(true);
		ReflectiveAccess.invoke(method, plugin, new Object[]{pending});
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		java.lang.reflect.Field field = findField(target.getClass(), name);
		field.setAccessible(true);
		ReflectiveAccess.set(field, target, value);
	}

	private static Object getField(Object target, String name) throws Exception {
		java.lang.reflect.Field field = findField(target.getClass(), name);
		field.setAccessible(true);
		return field.get(target);
	}

	private static java.lang.reflect.Field findField(Class<?> type, String name) throws NoSuchFieldException {
		for (Class<?> current = type; current != null; current = current.getSuperclass()) {
			try {
				return current.getDeclaredField(name);
			} catch (NoSuchFieldException ignored) {
				// Continue through Mockito subclasses and the concrete plugin class.
			}
		}
		throw new NoSuchFieldException(name);
	}

	private static java.lang.reflect.Method findMethod(Class<?> type, String name, Class<?> parameter)
				throws NoSuchMethodException {
		for (Class<?> current = type; current != null; current = current.getSuperclass()) {
			try {
				return current.getDeclaredMethod(name, parameter);
			} catch (NoSuchMethodException ignored) {
				// Continue through Mockito subclasses and the concrete plugin class.
			}
		}
		throw new NoSuchMethodException(name);
	}
}
