package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.net.URI;
import java.util.Map;
import java.util.Set;

import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.util.DurableFiles;

/** Preflights and atomically persists proxy transport-method changes. */
public final class ProxyMethodConfigurationService {
	private final VotingPluginProxy proxy;

	public ProxyMethodConfigurationService(VotingPluginProxy proxy) {
		this.proxy = proxy;
	}

	public ProxyMethodConfiguration read() {
		return new ProxyMethodConfiguration(BungeeMethod.getByName(proxy.getConfig().getBungeeMethod()));
	}

	public void validate(ProxyMethodConfiguration proposal) {
		validate(proposal, proxy.getConfig());
	}

	private void validate(ProxyMethodConfiguration proposal, VotingPluginProxyConfig config) {
		switch (proposal.method()) {
		case PLUGINMESSAGING:
			if (blank(config.getPluginMessageChannel())) {
				throw new IllegalArgumentException("PluginMessageChannel must be set");
			}
			break;
		case REDIS:
			validHostPort(config.getRedisHost(), config.getRedisPort(), "Redis");
			break;
		case MQTT:
			validBroker(config.getMqttBrokerURL());
			if (blank(config.getMqttClientID())) throw new IllegalArgumentException("MQTT.ClientID must be set");
			break;
		case SOCKETS:
			if (config.getBungeePort() < 1 || config.getBungeePort() > 65535) {
				throw new IllegalArgumentException("BungeeServer.Port must be set");
			}
			List<String> blockedServers = config.getBlockedServers();
			for (String server : proxy.getAllConfiguredServers()) {
				if (blockedServers != null && blockedServers.contains(server)) continue;
				Map<String, Object> backend = config.getSpigotServerConfiguration(server);
				Object host = backend.get("Host");
				Object port = backend.get("Port");
				int socketPort = port == null ? 1298 : port instanceof Number ? ((Number) port).intValue() : 0;
				if (!(host instanceof String) || blank((String) host)
						|| socketPort < 1 || socketPort > 65535) {
					throw new IllegalArgumentException("SpigotServers." + server + " Host and Port must be set for SOCKETS");
				}
			}
			break;
		case MYSQL:
			if (!config.hasDatabaseConfigured()) {
				throw new IllegalArgumentException("The proxy database Host must be configured for MYSQL");
			}
			break;
		default:
			throw new IllegalArgumentException("proxy method is unsupported");
		}
	}

	public void apply(ProxyMethodConfiguration proposal, String expectedRevision) throws IOException {
		validate(proposal);
		if (expectedRevision == null || !read().revision().equals(expectedRevision)) {
			throw new StaleRevisionException();
		}
		try {
			proxy.getConfig().persistControlProxyMethod(proposal.method().name(), expectedRevision,
					latest -> validate(proposal, latest));
			proxy.getConfig().verifyControlProxyRoutingInstalled();
		} catch (VotingPluginProxyConfig.StaleControlRevisionException e) {
			throw new StaleRevisionException();
		} catch (IllegalArgumentException validation) {
			throw validation;
		} catch (DurableFiles.PublishedException published) {
			throw rollbackAfterFailure(published);
		} catch (IOException failure) {
			throw failure;
		} catch (RuntimeException failure) {
			throw rollbackAfterFailure(failure);
		}
	}

	private ApplyFailureException rollbackAfterFailure(Exception failure) {
		boolean rolledBack = false;
		try {
			proxy.getConfig().rollbackControlProxyRouting();
			rolledBack = true;
		} catch (Exception rollbackFailure) {
			failure.addSuppressed(rollbackFailure);
		}
		return new ApplyFailureException(rolledBack, failure);
	}

	public static BungeeMethod canonical(String value) {
		if (value == null) throw new IllegalArgumentException("proxy method is required");
		try {
			return BungeeMethod.valueOf(value.trim().toUpperCase(java.util.Locale.ROOT));
		} catch (IllegalArgumentException e) {
			throw new IllegalArgumentException("proxy method is unsupported");
		}
	}

	private static void validHostPort(String host, int port, String label) {
		if (blank(host) || port < 1 || port > 65535) {
			throw new IllegalArgumentException(label + " host and port must be set");
		}
	}

	private static void validBroker(String value) {
		try {
			URI broker = URI.create(value == null ? "" : value.trim());
			if (!Set.of("tcp", "ssl", "ws", "wss").contains(broker.getScheme()) || broker.getHost() == null) {
				throw new IllegalArgumentException();
			}
		} catch (RuntimeException e) {
			throw new IllegalArgumentException("MQTT.BrokerURL must be a valid tcp, ssl, ws, or wss endpoint");
		}
	}

	private static boolean blank(String value) {
		return value == null || value.isBlank();
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
