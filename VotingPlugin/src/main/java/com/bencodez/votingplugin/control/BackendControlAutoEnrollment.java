package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.scheduler.BukkitTask;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.control.HostedControlManager;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.bencodez.votingplugin.util.ControlCredentialFile.PendingAutoEnrollment;

/**
 * Enrolls a Bukkit node through its authenticated proxy server connection while
 * keeping the generated bearer credential on the Bukkit node.
 */
public final class BackendControlAutoEnrollment implements AutoCloseable {
	private static final long RETRY_TICKS = 20L * 15L;

	private final VotingPluginMain plugin;
	private final PendingAutoEnrollment enrollment;
	private final UUID requestId = UUID.randomUUID();
	private final AtomicBoolean closed = new AtomicBoolean();
	private volatile BukkitTask retryTask;

	private BackendControlAutoEnrollment(VotingPluginMain plugin, PendingAutoEnrollment enrollment) {
		this.plugin = plugin;
		this.enrollment = enrollment;
	}

	/** Prepares a verifier that this Bukkit process's own hosted Control will install. */
	public static PendingAutoEnrollment prepareLocal(VotingPluginMain plugin, HostConfiguration hosted)
			throws IOException {
		ConfigurationSection control = control(plugin);
		if (control == null || !control.getBoolean("Enabled", false)
				|| !HostedControlManager.isDirectLocalEndpoint(control.getString("Endpoint", ""), hosted)) {
			return null;
		}
		return prepare(plugin, control);
	}

	/** Prepares proxy-mediated enrollment only when source-bound plugin messaging is active. */
	public static BackendControlAutoEnrollment create(VotingPluginMain plugin, HostConfiguration hosted)
			throws IOException {
		ConfigurationSection control = control(plugin);
		if (control == null || !control.getBoolean("Enabled", false)
				|| HostedControlManager.isDirectLocalEndpoint(control.getString("Endpoint", ""), hosted)
				|| !plugin.getBungeeSettings().isUseBungeecord()
				|| BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod()) != BungeeMethod.PLUGINMESSAGING) {
			return null;
		}
		String serverName = plugin.getOptions().getServer();
		String nodeId = nodeId(plugin, control);
		if (!nodeId.equals(serverName) || "pleaseset".equalsIgnoreCase(nodeId)) {
			return null;
		}
		PendingAutoEnrollment enrollment = prepare(plugin, control);
		return enrollment == null ? null : new BackendControlAutoEnrollment(plugin, enrollment);
	}

	public static String configuredNodeId(VotingPluginMain plugin, ConfigurationSection control) {
		return nodeId(plugin, control);
	}

	private static PendingAutoEnrollment prepare(VotingPluginMain plugin, ConfigurationSection control)
			throws IOException {
		String credentialFile = control.getString("CredentialFile", "control/control-credential.txt");
		return ControlCredentialFile.prepareAutoEnrollment(plugin.getDataFolder().toPath(), credentialFile,
				nodeId(plugin, control));
	}

	private static ConfigurationSection control(VotingPluginMain plugin) {
		return plugin.getConfigFile().getData().getConfigurationSection("Control.Backend");
	}

	private static String nodeId(VotingPluginMain plugin, ConfigurationSection control) {
		String configured = control.getString("NodeId", "");
		return configured == null || configured.isBlank() ? plugin.getOptions().getServer() : configured.trim();
	}

	public void start() {
		if (closed.get() || retryTask != null) return;
		try {
			retryTask = plugin.getServer().getScheduler().runTaskTimer(plugin, this::send, 1L, RETRY_TICKS);
		} catch (RuntimeException failure) {
			closed.set(true);
			plugin.getLogger().warning("[Control] Automatic backend enrollment could not be scheduled");
		}
	}

	private void send() {
		if (closed.get()) return;
		BackendProxyHandler handler = plugin.getBackendProxyHandler();
		if (handler == null || handler.getMethod() != BungeeMethod.PLUGINMESSAGING
				|| handler.getGlobalMessageHandler() == null) return;
		handler.getGlobalMessageHandler().sendMessage(VotingPluginWire.controlEnrollmentRequest(
				enrollment.nodeId(), enrollment.verifier(), requestId));
	}

	public void handle(JsonEnvelope envelope) {
		VotingPluginWire.ControlEnrollmentResult result = VotingPluginWire.readControlEnrollmentResult(envelope);
		if (closed.get() || !result.valid || !result.success || !requestId.equals(result.requestId)
				|| !enrollment.nodeId().equals(result.nodeId)) return;
		try {
			ControlCredentialFile.completeAutoEnrollment(enrollment);
			plugin.getLogger().info("[Control] Automatic credential enrollment completed for " + enrollment.nodeId());
			close();
		} catch (IOException e) {
			plugin.getLogger().warning("[Control] Automatic credential enrollment could not be finalized; it will retry");
		}
	}

	@Override
	public void close() {
		if (!closed.compareAndSet(false, true)) return;
		BukkitTask task = retryTask;
		if (task != null) task.cancel();
		retryTask = null;
	}
}
