package com.bencodez.votingplugin.proxy.multiproxy;

/**
 * Multi-proxy communication methods.
 */
public enum MultiProxyMethod {
	/** Socket-based communication. */
	SOCKETS,
	/** Redis-based communication. */
	REDIS;

	/**
	 * Gets the multi-proxy method by name.
	 *
	 * @param str the method name
	 * @return the multi-proxy method
	 */
	public static MultiProxyMethod getByName(String str) {
		for (MultiProxyMethod method : values()) {
			if (method.toString().equalsIgnoreCase(str)) {
				return method;
			}
		}
		return MultiProxyMethod.SOCKETS;
	}
}
