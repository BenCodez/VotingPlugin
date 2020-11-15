package com.bencodez.votingplugin.bungee;

public enum BungeeMethod {
	SOCKETS, PLUGINMESSAGING, MYSQL;

	public static BungeeMethod getByName(String str) {
		for (BungeeMethod method : values()) {
			if (method.toString().equalsIgnoreCase(str)) {
				return method;
			}
		}
		return SOCKETS;
	}
}
