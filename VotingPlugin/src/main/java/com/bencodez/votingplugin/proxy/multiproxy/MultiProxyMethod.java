package com.bencodez.votingplugin.proxy.multiproxy;

public enum MultiProxyMethod {
	SOCKETS, REDIS;

	public static MultiProxyMethod getByName(String str) {
		for (MultiProxyMethod method : values()) {
			if (method.toString().equalsIgnoreCase(str)) {
				return method;
			}
		}
		return MultiProxyMethod.SOCKETS;
	}
}
