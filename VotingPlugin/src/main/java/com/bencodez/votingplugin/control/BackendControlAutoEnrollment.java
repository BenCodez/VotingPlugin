package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
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
import com.bencodez.votingplugin.util.ControlCredentialFile.AutoEnrollmentInspection;
import com.bencodez.votingplugin.util.ControlCredentialFile.PendingAutoEnrollment;

/**
 * Enrolls a Bukkit node through its authenticated proxy server connection while
 * keeping the generated bearer credential on the Bukkit node.
 */
public final class BackendControlAutoEnrollment implements AutoCloseable {
	private static final long RETRY_TICKS = 20L * 15L;
	private static final long VERIFIER_REFRESH_NANOS = TimeUnit.SECONDS.toNanos(60);

	private final VotingPluginMain plugin;
	private final String nodeId;
	private final String credentialFile;
	private final String endpoint;
	private final UUID requestId = UUID.randomUUID();
	private final AtomicBoolean closed = new AtomicBoolean();
	private volatile BukkitTask retryTask;
	private PendingAutoEnrollment enrollment;
	private boolean verifierInstalled;
	private boolean connectorAuthenticated;
	private long verifierAcknowledgedAt;

	private BackendControlAutoEnrollment(VotingPluginMain plugin, String nodeId, String credentialFile,
			String endpoint, PendingAutoEnrollment enrollment) {
		this.plugin = plugin;
		this.nodeId = nodeId;
		this.credentialFile = credentialFile;
		this.endpoint = endpoint;
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
				|| !plugin.getBungeeSettings().isUseBungeecoord()
				|| BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod()) != BungeeMethod.PLUGINMESSAGING) {
			return null;
		}
		String serverName = plugin.getOptions().getServer();
		String nodeId = nodeId(plugin, control);
		if (!nodeId.equals(serverName) || "pleaseset".equalsIgnoreCase(nodeId)) {
			return null;
		}
		String endpoint = control.getString("Endpoint", "");
		String credentialFile = control.getString("CredentialFile", "control/control-credential.txt");
		AutoEnrollmentInspection inspection = ControlCredentialFile.inspectAutoEnrollment(
				plugin.getDataFolder().toPath(), credentialFile);
		PendingAutoEnrollment pending = inspection.pending();
		if (pending == null && inspection.credentialPresent()) return null;
		if (pending != null && !nodeId.equals(pending.nodeId())) pending = null;
		return new BackendControlAutoEnrollment(plugin, nodeId, credentialFile,
				endpoint == null ? "" : endpoint.trim(), pending);
	}

	public static String configuredNodeId(VotingPluginMain plugin, ConfigurationSection control) {
		return nodeId(plugin, control);
	}

	private static PendingAutoEnrollment prepare(VotingPluginMain plugin, ConfigurationSection control)
			throws IOException {
		String credentialFile = control.getString("CredentialFile", "control/control-credential.txt");
		return prepare(plugin, credentialFile, nodeId(plugin, control));
	}

	private static PendingAutoEnrollment prepare(VotingPluginMain plugin, String credentialFile, String nodeId)
			throws IOException {
		return ControlCredentialFile.prepareAutoEnrollment(plugin.getDataFolder().toPath(), credentialFile, nodeId);
	}

	private static ConfigurationSection control(VotingPluginMain plugin) {
		return plugin.getConfigFile().getData().getConfigurationSection("Control.Backend");
	}

	private static String nodeId(VotingPluginMain plugin, ConfigurationSection control) {
		String configured = control.getString("NodeId", "");
		return configured == null || configured.isBlank() ? plugin.getOptions().getServer() : configured.trim();
	}

	public synchronized void start() {
		if (closed.get() || retryTask != null) return;
		try {
			retryTask = plugin.getServer().getScheduler().runTaskTimer(plugin, this::send, 1L, RETRY_TICKS);
		} catch (RuntimeException failure) {
			closed.set(true);
			plugin.getLogger().warning("[Control] Automatic backend enrollment could not be scheduled");
		}
	}

	/** True while the proxy must confirm that this route reaches its hosted Control. */
	public synchronized boolean isAwaitingCredential() {
		return enrollment == null;
	}

	private void send() {
		PendingAutoEnrollment pending;
		synchronized (this) {
			if (closed.get() || (verifierInstalled
					&& System.nanoTime() - verifierAcknowledgedAt < VERIFIER_REFRESH_NANOS)) return;
			pending = enrollment;
		}
		BackendProxyHandler handler = plugin.getBackendProxyHandler();
		if (handler == null || handler.getMethod() != BungeeMethod.PLUGINMESSAGING
				|| handler.getGlobalMessageHandler() == null) return;
		handler.getGlobalMessageHandler().sendMessage(VotingPluginWire.controlEnrollmentRequest(
				nodeId, pending == null ? "" : pending.verifier(), endpoint, requestId));
	}

	public void handle(JsonEnvelope envelope) {
		VotingPluginWire.ControlEnrollmentResult result = VotingPluginWire.readControlEnrollmentResult(envelope);
		boolean restartConnector = false;
		synchronized (this) {
			if (closed.get() || !result.valid || !result.success || !requestId.equals(result.requestId)
					|| !nodeId.equals(result.nodeId)) return;
			if (enrollment == null) {
				try {
					enrollment = prepare(plugin, credentialFile, nodeId);
					if (enrollment == null) close();
					restartConnector = true;
				} catch (IOException e) {
					plugin.getLogger().warning(
							"[Control] Automatic backend credential could not be prepared; it will retry");
				}
			} else {
				verifierInstalled = true;
				verifierAcknowledgedAt = System.nanoTime();
				completeIfConnected();
			}
		}
		if (restartConnector) plugin.restartBackendControlConnector();
	}

	/** Completes durable enrollment only after this credential authenticated to the configured endpoint. */
	public synchronized void connectorAuthenticated(String nodeId, String credentialFile, String endpoint,
			String verifier) {
		PendingAutoEnrollment pending = enrollment;
		if (closed.get() || pending == null || !pending.nodeId().equals(nodeId)
				|| !pending.configuredPath().equals(credentialFile) || !this.endpoint.equals(endpoint)
				|| !pending.verifier().equals(verifier)) return;
		connectorAuthenticated = true;
		completeIfConnected();
	}

	private void completeIfConnected() {
		if (!verifierInstalled || !connectorAuthenticated) return;
		try {
			PendingAutoEnrollment pending = enrollment;
			if (pending == null) return;
			ControlCredentialFile.completeAutoEnrollment(pending);
			plugin.getLogger().info("[Control] Automatic credential enrollment completed for " + pending.nodeId());
			close();
		} catch (IOException e) {
			verifierInstalled = false;
			plugin.getLogger().warning("[Control] Automatic credential enrollment could not be finalized; it will retry");
		}
	}

	@Override
	public synchronized void close() {
		if (!closed.compareAndSet(false, true)) return;
		BukkitTask task = retryTask;
		if (task != null) task.cancel();
		retryTask = null;
	}
}
