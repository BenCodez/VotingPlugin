package com.bencodez.votingplugin;

import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;

/**
 * @deprecated Use {@link BackendProxyHandler}.
 */
@Deprecated
public class BungeeHandler extends BackendProxyHandler {

	public BungeeHandler(VotingPluginMain plugin) {
		super(plugin);
	}
}
