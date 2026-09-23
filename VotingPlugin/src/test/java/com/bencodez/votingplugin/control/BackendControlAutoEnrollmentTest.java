package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.timeout;

import java.nio.file.Path;
import java.nio.file.Files;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.bukkit.Server;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.scheduler.BukkitScheduler;
import org.bukkit.scheduler.BukkitTask;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;

class BackendControlAutoEnrollmentTest {
	@TempDir
	Path directory;

	@TestFactory
	Stream<DynamicTest> submitsEnrollmentThroughEveryBackendTransport() {
		return Stream.of(BungeeMethod.values()).map(method -> DynamicTest.dynamicTest(method.name(),
				() -> submitEnrollment(method)));
	}

	private void submitEnrollment(BungeeMethod method) throws Exception {
		Path methodDirectory = directory.resolve(method.name());
		Files.createDirectories(methodDirectory);
		Files.writeString(methodDirectory.resolve("secretkey.key"),
				Base64.getEncoder().encodeToString(new byte[32]));
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		BungeeSettings bungee = mock(BungeeSettings.class);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		Server server = mock(Server.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		BukkitTask task = mock(BukkitTask.class);
		BackendProxyHandler handler = mock(BackendProxyHandler.class);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		AtomicReference<String> sendingThread = new AtomicReference<>();
		org.mockito.Mockito.doAnswer(invocation -> {
			sendingThread.compareAndSet(null, Thread.currentThread().getName());
			return null;
		}).when(messages).sendMessage(org.mockito.ArgumentMatchers.any());
		YamlConfiguration data = new YamlConfiguration();
		data.set("Control.Backend.Enabled", true);
		data.set("Control.Backend.Endpoint", "http://control.example.test:2150");
		data.set("Control.Backend.NodeId", "backend-a");
		data.set("Control.Backend.CredentialFile", "control/control-credential.txt");
		when(plugin.getConfigFile()).thenReturn(config);
		when(config.getData()).thenReturn(data);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		when(bungee.isUseBungeecoord()).thenReturn(true);
		when(bungee.getBungeeMethod()).thenReturn(method.name());
		when(plugin.getOptions()).thenReturn(options);
		when(options.getServer()).thenReturn("backend-a");
		when(plugin.getDataFolder()).thenReturn(methodDirectory.toFile());
		when(plugin.getServer()).thenReturn(server);
		when(server.getScheduler()).thenReturn(scheduler);
		ArgumentCaptor<Runnable> scheduled = ArgumentCaptor.forClass(Runnable.class);
		when(scheduler.runTaskTimer(eq(plugin), scheduled.capture(), anyLong(), anyLong())).thenReturn(task);
		when(plugin.getBackendProxyHandler()).thenReturn(handler);
		when(handler.getGlobalMessageHandler()).thenReturn(messages);

		HostConfiguration hosted = new HostConfiguration(false, false, false, "", "", "", "",
				"127.0.0.1", 0, 1, 1);
		try (BackendControlAutoEnrollment enrollment = BackendControlAutoEnrollment.create(plugin, hosted)) {
			assertNotNull(enrollment);
			assertTrue(enrollment.isAwaitingCredential());
			enrollment.start();
			if (method == BungeeMethod.PLUGINMESSAGING) scheduled.getValue().run();
			else verify(messages, timeout(1000)).sendMessage(org.mockito.ArgumentMatchers.any());
			if (method != BungeeMethod.PLUGINMESSAGING) {
				assertTrue(sendingThread.get().startsWith("VotingPlugin-ControlEnrollment"));
			}

			ArgumentCaptor<JsonEnvelope> initialRequest = ArgumentCaptor.forClass(JsonEnvelope.class);
			verify(messages).sendMessage(initialRequest.capture());
			JsonEnvelope initial = initialRequest.getValue();
			assertEquals(VotingPluginWire.SUB_CONTROL_ENROLLMENT_REQUEST, initial.getSubChannel());
			assertEquals("backend-a", initial.getFields().get(VotingPluginWire.K_SERVER));
			java.util.UUID requestId = java.util.UUID.fromString(
					initial.getFields().get(VotingPluginWire.K_REQUEST_ID));
			String challenge = java.util.UUID.randomUUID().toString();
			enrollment.handle(VotingPluginWire.controlEnrollmentResult("backend-a", requestId, false, challenge));
			verify(plugin).startBackendControlConnectorForEnrollment(enrollment);
			org.mockito.Mockito.clearInvocations(messages);
			enrollment.send();

			ArgumentCaptor<JsonEnvelope> provedRequest = ArgumentCaptor.forClass(JsonEnvelope.class);
			verify(messages).sendMessage(provedRequest.capture());
			JsonEnvelope proved = provedRequest.getValue();
			VotingPluginWire.ControlEnrollmentRequest parsed =
					VotingPluginWire.readControlEnrollmentRequest(proved);
			assertTrue(parsed.valid);
			assertEquals(challenge, parsed.challenge);
			assertEquals(64, parsed.verifier.length());
			if (method == BungeeMethod.PLUGINMESSAGING || method == BungeeMethod.HTTP) {
				assertEquals("", parsed.authenticator);
			} else {
				assertEquals(64, parsed.authenticator.length());
			}

			// A repeated challenge retries connector startup without replacing this enrollment,
			// its request correlation, or the verifier that was already written.
			enrollment.handle(VotingPluginWire.controlEnrollmentResult("backend-a", requestId, false, challenge));
			verify(plugin, times(2)).startBackendControlConnectorForEnrollment(enrollment);
			org.mockito.Mockito.clearInvocations(messages);
			enrollment.send();
			ArgumentCaptor<JsonEnvelope> retriedRequest = ArgumentCaptor.forClass(JsonEnvelope.class);
			verify(messages).sendMessage(retriedRequest.capture());
			VotingPluginWire.ControlEnrollmentRequest retried =
					VotingPluginWire.readControlEnrollmentRequest(retriedRequest.getValue());
			assertEquals(requestId, retried.requestId);
			assertEquals(challenge, retried.challenge);
			assertEquals(parsed.verifier, retried.verifier);
			if (method != BungeeMethod.PLUGINMESSAGING) verify(scheduler, never())
					.runTaskTimer(eq(plugin), org.mockito.ArgumentMatchers.any(Runnable.class), anyLong(), anyLong());
		}
	}
}
