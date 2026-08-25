package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;

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
			@Override public void persist(ProxyRoutingConfiguration proposal) throws IOException {
				proxy.getConfig().persistControlProxyRouting(proposal.sendVotesToAllServers(),
						proposal.blockedServers());
			}
			@Override public void rollback() throws IOException { proxy.getConfig().rollbackControlProxyRouting(); }
			@Override public void reload() throws Exception { proxy.reloadControlConfiguration(); }
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
		platform.persist(proposal);
		try {
			platform.reload();
		} catch (Exception e) {
			boolean rolledBack = false;
			try {
				platform.rollback();
				platform.reload();
				rolledBack = true;
			} catch (Exception rollbackFailure) {
				e.addSuppressed(rollbackFailure);
			}
			throw new ApplyFailureException(rolledBack, e);
		}
	}

	interface Platform {
		ProxyRoutingConfiguration read();
		Set<String> configuredServers();
		void persist(ProxyRoutingConfiguration proposal) throws IOException;
		void rollback() throws IOException;
		void reload() throws Exception;
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
