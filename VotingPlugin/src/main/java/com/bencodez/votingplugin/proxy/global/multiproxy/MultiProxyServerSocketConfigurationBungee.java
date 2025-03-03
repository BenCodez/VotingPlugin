package com.bencodez.votingplugin.proxy.global.multiproxy;

import java.util.Map;

public class MultiProxyServerSocketConfigurationBungee implements MultiProxyServerSocketConfiguration {
	private String server;
	private String host = "";
	private int port;

	public MultiProxyServerSocketConfigurationBungee(String s, Map<String, Object> config) {
		server = s;
		if (config.containsKey("Host")) {
			host = (String) config.get("Host");
		}
		if (config.containsKey("Port")) {
			port = (int) config.get("Port");
		} else {
			port = 1298;
		}
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
