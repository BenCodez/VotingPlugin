package com.bencodez.votingplugin.bungee.global.multiproxy;

import net.md_5.bungee.config.Configuration;

public class MultiProxyServerSocketConfigurationBungee implements MultiProxyServerSocketConfiguration {
	private String server;
	private String host;
	private int port;

	public MultiProxyServerSocketConfigurationBungee(String s, Configuration config) {
		server = s;
		host = config.getString("Host", "");
		port = config.getInt("Port", 1234);
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
