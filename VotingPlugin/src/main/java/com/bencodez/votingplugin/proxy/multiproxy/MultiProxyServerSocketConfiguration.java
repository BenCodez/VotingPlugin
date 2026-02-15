package com.bencodez.votingplugin.proxy.multiproxy;

/**
 * Configuration for a multi-proxy server socket.
 */
public interface MultiProxyServerSocketConfiguration {
	/**
	 * Gets the host.
	 *
	 * @return the host
	 */
	String getHost();

	/**
	 * Gets the port.
	 *
	 * @return the port
	 */
	int getPort();

	/**
	 * Gets the server name.
	 *
	 * @return the server name
	 */
	String getServerName();
}
