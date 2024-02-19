package com.bencodez.votingplugin.bungee.global.multiproxy;

import ninja.leaping.configurate.ConfigurationNode;

public class MultiProxyServerSocketConfigurationVelocity implements MultiProxyServerSocketConfiguration {

	private String server;
	private String host;
	private int port;

	public MultiProxyServerSocketConfigurationVelocity(String s, ConfigurationNode config) {
		server = s;
		host = config.getNode("Host").getString("0.0.0.0");
		port = config.getNode("Port").getInt(1298);

	}

	@Override
	public String getServerName() {
		return server;
	}

	@Override
	public String getHost() {
		return host;
	}

	@Override
	public int getPort() {
		return port;
	}

}
