package com.bencodez.votingplugin.proxy;

public enum BungeeMethod {
	MYSQL, PLUGINMESSAGING, SOCKETS, REDIS, MQTT;
	
	public boolean requiresPlayerOnline() {
		return this == PLUGINMESSAGING;
	}

	public static BungeeMethod getByName(String str) {
		for (BungeeMethod method : values()) {
			if (method.toString().equalsIgnoreCase(str)) {
				return method;
			}
		}
		return PLUGINMESSAGING;
	}
}
