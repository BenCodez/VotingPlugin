package com.bencodez.votingplugin.proxy.multiproxy;

import org.spongepowered.configurate.ConfigurationNode;

public class MultiProxyServerSocketConfigurationVelocity implements MultiProxyServerSocketConfiguration {

	private String server;
	private String host;
	private int port;

	public MultiProxyServerSocketConfigurationVelocity(String s, ConfigurationNode config) {
		server = s;
		host = config.node("Host").getString("0.0.0.0");
		port = config.node("Port").getInt(1298);

	}

	@Override
	public String getHost() {
		return host;
	}

	@Override
	public int getPort() {
		return port;
	}

	@Override
	public String getServerName() {
		return server;
	}

}
