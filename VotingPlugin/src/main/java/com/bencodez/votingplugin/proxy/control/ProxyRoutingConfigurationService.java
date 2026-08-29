package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.util.DurableFiles;

/** Validates, persists, reloads, and rolls back the bounded Control configuration domain. */
public final class ProxyRoutingConfigurationService {
	private final Platform platform;

	public ProxyRoutingConfigurationService(VotingPluginProxy proxy) {
		this(new Platform() {
			@Override public ProxyRoutingConfiguration read() {
				return new ProxyRoutingConfiguration(proxy.getConfig().getSendVotesToAllServers(),
						proxy.getConfig().getBlockedServers());
			}
			@Override public Set<String> configuredServers() { return proxy.getAllConfiguredServers(); }
			@Override public void persist(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException {
				proxy.getConfig().persistControlProxyRouting(proposal.sendVotesToAllServers(),
						proposal.blockedServers(), expectedRevision);
			}
			@Override public void rollback() throws IOException { proxy.getConfig().rollbackControlProxyRouting(); }
			@Override public void reload() throws Exception { proxy.reloadControlConfiguration(); }
			@Override public void verifyInstalled() throws IOException {
				proxy.getConfig().verifyControlProxyRoutingInstalled();
			}
			@Override public byte[] captureInstalledSnapshot() throws IOException {
				return proxy.getConfig().captureControlProxyRoutingSnapshot();
			}
			@Override public void verifyInstalledSnapshot(byte[] snapshot) throws IOException {
				proxy.getConfig().verifyControlProxyRoutingSnapshot(snapshot);
			}
		});
	}

	ProxyRoutingConfigurationService(Platform platform) {
		this.platform = platform;
	}

	public ProxyRoutingConfiguration read() {
		return platform.read();
	}

	public void validate(ProxyRoutingConfiguration proposal) {
		Set<String> available = new HashSet<>(platform.configuredServers());
		for (String server : proposal.blockedServers()) {
			if (!available.contains(server)) {
				throw new IllegalArgumentException("unknown backend: " + server);
			}
		}
	}

	public void apply(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException {
		validate(proposal);
		if (expectedRevision == null || !read().revision().equals(expectedRevision)) {
			throw new StaleRevisionException();
		}
		try {
			platform.persist(proposal, expectedRevision);
		} catch (com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException e) {
			throw new StaleRevisionException();
		} catch (DurableFiles.PublishedException published) {
			throw rollbackAfterFailure(published);
		}
		try {
			platform.reload();
			try {
				platform.verifyInstalled();
			} catch (com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException stale) {
				reconcileConcurrentEdit();
				throw new StaleRevisionException();
			}
		} catch (StaleRevisionException stale) {
			throw stale;
		} catch (Exception e) {
			throw rollbackAfterFailure(e);
		}
	}

	private ApplyFailureException rollbackAfterFailure(Exception failure) {
		boolean rolledBack = false;
		DurableFiles.PublishedException publicationFailure = null;
		try {
			try {
				platform.rollback();
			} catch (DurableFiles.PublishedException published) {
				// The restored file is already active. Continue the reload so runtime
				// state follows disk, while retaining the durability warning.
				publicationFailure = published;
			}
			platform.reload();
			rolledBack = true;
		} catch (Exception rollbackFailure) {
			failure.addSuppressed(rollbackFailure);
		}
		if (publicationFailure != null) failure.addSuppressed(publicationFailure);
		return new ApplyFailureException(rolledBack, failure);
	}

	private void reconcileConcurrentEdit() throws Exception {
		for (int attempt = 0; attempt < 3; attempt++) {
			byte[] snapshot = platform.captureInstalledSnapshot();
			platform.reload();
			try {
				platform.verifyInstalledSnapshot(snapshot);
				return;
			} catch (com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.StaleControlRevisionException stale) {
				if (attempt == 2) throw new StaleRevisionException();
			}
		}
	}

	interface Platform {
		ProxyRoutingConfiguration read();
		Set<String> configuredServers();
		void persist(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException;
		void rollback() throws IOException;
		void reload() throws Exception;
		default void verifyInstalled() throws IOException { }
		default byte[] captureInstalledSnapshot() throws IOException { return new byte[0]; }
		default void verifyInstalledSnapshot(byte[] snapshot) throws IOException { }
	}

	@SuppressWarnings("serial")
	public static final class StaleRevisionException extends RuntimeException { }
	@SuppressWarnings("serial")
	public static final class ApplyFailureException extends RuntimeException {
		private final boolean rolledBack;
		private ApplyFailureException(boolean rolledBack, Throwable cause) { super(cause); this.rolledBack = rolledBack; }
		public boolean rolledBack() { return rolledBack; }
	}
}
