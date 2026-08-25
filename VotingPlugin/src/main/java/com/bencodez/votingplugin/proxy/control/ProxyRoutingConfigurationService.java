package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;

/** Validates, persists, reloads, and rolls back the bounded Control configuration domain. */
public final class ProxyRoutingConfigurationService {
	private final VotingPluginProxy proxy;

	public ProxyRoutingConfigurationService(VotingPluginProxy proxy) {
		this.proxy = proxy;
	}

	public ProxyRoutingConfiguration read() {
		return new ProxyRoutingConfiguration(proxy.getConfig().getSendVotesToAllServers(),
				proxy.getConfig().getBlockedServers());
	}

	public void validate(ProxyRoutingConfiguration proposal) {
		Set<String> available = new HashSet<>();
		proxy.getAllAvailableServers().forEach(server -> available.add(server.toLowerCase(Locale.ROOT)));
		for (String server : proposal.blockedServers()) {
			if (!available.contains(server.toLowerCase(Locale.ROOT))) {
				throw new IllegalArgumentException("unknown backend: " + server);
			}
		}
	}

	public void apply(ProxyRoutingConfiguration proposal, String expectedRevision) throws IOException {
		validate(proposal);
		if (expectedRevision == null || !read().revision().equals(expectedRevision)) {
			throw new StaleRevisionException();
		}
		proxy.getConfig().persistControlProxyRouting(proposal.sendVotesToAllServers(), proposal.blockedServers());
		try {
			proxy.reloadCore(false);
		} catch (RuntimeException e) {
			boolean rolledBack = false;
			try {
				proxy.getConfig().rollbackControlProxyRouting();
				proxy.reloadCore(false);
				rolledBack = true;
			} catch (Exception rollbackFailure) {
				e.addSuppressed(rollbackFailure);
			}
			throw new ApplyFailureException(rolledBack, e);
		}
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
