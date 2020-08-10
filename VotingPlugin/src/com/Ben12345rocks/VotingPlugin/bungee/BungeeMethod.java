package com.Ben12345rocks.VotingPlugin.bungee;

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
