package com.bencodez.votingplugin;

import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;

/**
 * Compatibility alias for the renamed backend/proxy handler.
 *
 * @deprecated Use {@link BackendProxyHandler} and
 *             {@link VotingPluginMain#getBackendProxyHandler()} instead.
 */
@Deprecated
public class BungeeHandler extends BackendProxyHandler {

	public BungeeHandler(VotingPluginMain plugin) {
		super(plugin);
	}
}
