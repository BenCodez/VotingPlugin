package com.bencodez.votingplugin.proxy;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Configuration interface for proxy server integration.
 */
public interface VotingPluginProxyConfig {
	/** Atomically persists the small non-secret configuration domain exposed by Control. */
	default void persistControlProxyRouting(boolean sendVotesToAllServers, List<String> blockedServers)
			throws IOException {
		throw new UnsupportedOperationException("Control configuration writes are unavailable");
	}

	/** Persists only if the same routing snapshot is still active immediately before replacement. */
	default void persistControlProxyRouting(boolean sendVotesToAllServers, List<String> blockedServers,
			String expectedRevision) throws IOException {
		persistControlProxyRouting(sendVotesToAllServers, blockedServers);
	}

	/** Restores the most recent Control-created backup. */
	default void rollbackControlProxyRouting() throws IOException {
		throw new UnsupportedOperationException("Control configuration rollback is unavailable");
	}

	/** Verifies that the exact Control-installed routing snapshot is still on disk. */
	default void verifyControlProxyRoutingInstalled() throws IOException {
	}

	/** Captures the exact proxy configuration file that the next strict reload should consume. */
	default byte[] captureControlProxyRoutingSnapshot() throws IOException {
		throw new UnsupportedOperationException("Control configuration snapshot verification is unavailable");
	}

	/** Verifies that the proxy configuration file still equals a previously captured snapshot. */
	default void verifyControlProxyRoutingSnapshot(byte[] snapshot) throws IOException {
		throw new UnsupportedOperationException("Control configuration snapshot verification is unavailable");
	}

	@SuppressWarnings("serial")
	final class StaleControlRevisionException extends IOException {
		public StaleControlRevisionException() { super("Control proxy routing revision is stale"); }
	}
	/** Whether the optional local VotingPlugin Control connector is enabled. */
	default boolean getControlEnabled() {
		return false;
	}

	/** Base HTTP(S) endpoint for VotingPlugin Control. */
	default String getControlEndpoint() {
		return "http://127.0.0.1:8080";
	}

	/** Stable enrolled identity; blank reuses ProxyServerName. */
	default String getControlNodeId() {
		return "";
	}

	/** Relative file in the plugin data folder containing the one-time enrollment credential. */
	default String getControlCredentialFile() {
		return "control-credential.txt";
	}

	default int getControlHeartbeatSeconds() {
		return 30;
	}

	default int getControlConnectTimeoutMillis() {
		return 3000;
	}

	default int getControlRequestTimeoutMillis() {
		return 5000;
	}

	/** Whether this proxy should provision and supervise a separate Control JVM. */
	default boolean getControlHostedEnabled() {
		return false;
	}

	/** Download the pinned artifact when it is not installed. */
	default boolean getControlHostedAutoDownload() {
		return true;
	}

	/** Follow newer official releases, or replace an installed artifact when a manual pin changes. */
	default boolean getControlHostedAutoUpdate() {
		return true;
	}

	default String getControlHostedDownloadUrl() {
		return "";
	}

	default String getControlHostedSha256() {
		return "";
	}

	default String getControlHostedJarFile() {
		return "control/votingplugin-control.jar";
	}

	default String getControlHostedDataDirectory() {
		return "control/data";
	}

	default String getControlHostedHost() {
		return "127.0.0.1";
	}

	default int getControlHostedPort() {
		return 8080;
	}

	default int getControlHostedStartupTimeoutSeconds() {
		return 30;
	}

	default int getControlHostedDownloadTimeoutSeconds() {
		return 60;
	}

	/**
	 * Gets whether proxy broadcast is enabled.
	 *
	 * @return true if proxy broadcast is enabled
	 */
	default boolean getProxyBroadcastEnabled() {
		return false;
	}

	/**
	 * Routing scope mode.
	 *
	 * Expected values: PLAYER_SERVER | ALL_SERVERS | SERVERS | ALL_EXCEPT
	 *
	 * @return the scope mode
	 */
	default String getProxyBroadcastScopeMode() {
		return "ALL_SERVERS";
	}

	/**
	 * Optional server list used by scope modes SERVERS and ALL_EXCEPT.
	 *
	 * @return the list of scope servers
	 */
	default List<String> getProxyBroadcastScopeServers() {
		return Collections.emptyList();
	}

	/**
	 * What to do when the voting player is offline and scope is PLAYER_SERVER.
	 *
	 * Expected values: NONE | QUEUE | FORWARD
	 *
	 * @return the offline mode
	 */
	default String getProxyBroadcastOfflineMode() {
		return "QUEUE";
	}

	/**
	 * Backend server list used when OfflineMode is FORWARD and Scope.Mode is
	 * PLAYER_SERVER.
	 *
	 * Ignored when Scope.Mode is ALL_SERVERS or ALL_EXCEPT.
	 *
	 * @return the list of offline forward servers
	 */
	default List<String> getProxyBroadcastOfflineForwardServers() {
		return Collections.emptyList();
	}

	/**
	 * Gets whether unjoined players are allowed.
	 *
	 * @return true if unjoined players are allowed
	 */
	public boolean getAllowUnJoined();

	/**
	 * Gets the Bedrock player prefix.
	 *
	 * @return the Bedrock player prefix
	 */
	public String getBedrockPlayerPrefix();

	/**
	 * Gets the list of blocked servers.
	 *
	 * @return the list of blocked servers
	 */
	public List<String> getBlockedServers();

	/**
	 * Gets the Bungee host.
	 *
	 * @return the Bungee host
	 */
	public String getBungeeHost();

	/**
	 * Gets whether Bungee manages totals.
	 *
	 * @return true if Bungee manages totals
	 */
	public boolean getBungeeManageTotals();

	/**
	 * Gets the Bungee method.
	 *
	 * @return the Bungee method
	 */
	public String getBungeeMethod();

	/**
	 * Gets the Bungee port.
	 *
	 * @return the Bungee port
	 */
	public int getBungeePort();

	/**
	 * Gets the plugin message channel.
	 *
	 * @return the plugin message channel
	 */
	public String getPluginMessageChannel();

	/**
	 * Gets whether plugin message encryption is enabled.
	 *
	 * @return true if plugin message encryption is enabled
	 */
	public boolean getPluginMessageEncryption();

	/**
	 * Gets whether debug mode is enabled.
	 *
	 * @return true if debug mode is enabled
	 */
	public boolean getDebug();

	/**
	 * Gets whether vote cache uses MySQL.
	 *
	 * @return true if vote cache uses MySQL
	 */
	public boolean getVoteCacheUseMySQL();

	/**
	 * Gets whether vote cache uses main MySQL.
	 *
	 * @return true if vote cache uses main MySQL
	 */
	public boolean getVoteCacheUseMainMySQL();

	/**
	 * Gets whether non-voted cache uses MySQL.
	 *
	 * @return true if non-voted cache uses MySQL
	 */
	public boolean getNonVotedCacheUseMySQL();

	/**
	 * Gets whether non-voted cache uses main MySQL.
	 *
	 * @return true if non-voted cache uses main MySQL
	 */
	public boolean getNonVotedCacheUseMainMySQL();

	/**
	 * Gets the MQTT client ID.
	 *
	 * @return the MQTT client ID
	 */
	public String getMqttClientID();

	/**
	 * Gets the MQTT broker URL.
	 *
	 * @return the MQTT broker URL
	 */
	public String getMqttBrokerURL();

	/**
	 * Gets the MQTT username.
	 *
	 * @return the MQTT username
	 */
	public String getMqttUsername();

	/**
	 * Gets the MQTT password.
	 *
	 * @return the MQTT password
	 */
	public String getMqttPassword();

	/**
	 * Gets the MQTT prefix.
	 *
	 * @return the MQTT prefix
	 */
	public String getMqttPrefix();

	/**
	 * Gets the fallback server.
	 *
	 * @return the fallback server
	 */
	public String getFallBack();

	/**
	 * Gets whether global data is enabled.
	 *
	 * @return true if global data is enabled
	 */
	public boolean getGlobalDataEnabled();

	/**
	 * Gets whether global data uses main MySQL.
	 *
	 * @return true if global data uses main MySQL
	 */
	public boolean getGlobalDataUseMainMySQL();

	/**
	 * Gets the limit for vote points.
	 *
	 * @return the vote points limit
	 */
	public int getLimitVotePoints();

	/**
	 * Gets the maximum amount of votes per day.
	 *
	 * @return the maximum votes per day
	 */
	public int getMaxAmountOfVotesPerDay();

	/**
	 * Gets the multi-proxy method.
	 *
	 * @return the multi-proxy method
	 */
	public String getMultiProxyMethod();

	/**
	 * Gets whether multi-proxy uses one global reward.
	 *
	 * @return true if multi-proxy uses one global reward
	 */
	public boolean getMultiProxyOneGlobalReward();

	/**
	 * Gets the multi-proxy Redis host.
	 *
	 * @return the Redis host
	 */
	public String getMultiProxyRedisHost();

	/**
	 * Gets the multi-proxy Redis password.
	 *
	 * @return the Redis password
	 */
	public String getMultiProxyRedisPassword();

	/**
	 * Gets the multi-proxy Redis port.
	 *
	 * @return the Redis port
	 */
	public int getMultiProxyRedisPort();

	/**
	 * Gets whether the multi-proxy Redis connection uses SSL/TLS.
	 *
	 * @return true if SSL/TLS is enabled
	 */
	default boolean getMultiProxyRedisSsl() {
		return false;
	}

	/**
	 * Gets whether multi-proxy Redis uses existing connection.
	 *
	 * @return true if using existing connection
	 */
	public boolean getMultiProxyRedisUseExistingConnection();

	/**
	 * Gets the multi-proxy Redis username.
	 *
	 * @return the Redis username
	 */
	public String getMultiProxyRedisUsername();

	/**
	 * Gets the collection of multi-proxy servers.
	 *
	 * @return the collection of server names
	 */
	public Collection<String> getMultiProxyServers();

	/**
	 * Gets the configuration for a specific multi-proxy server.
	 *
	 * @param s the server name
	 * @return the server configuration as a map
	 */
	public Map<String, Object> getMultiProxyServersConfiguration(String s);

	/**
	 * Gets the multi-proxy socket host.
	 *
	 * @return the socket host
	 */
	public String getMultiProxySocketHostHost();

	/**
	 * Gets the multi-proxy socket host port.
	 *
	 * @return the socket port
	 */
	public int getMultiProxySocketHostPort();

	/**
	 * Gets whether multi-proxy support is enabled.
	 *
	 * @return true if multi-proxy support is enabled
	 */
	public boolean getMultiProxySupport();

	/**
	 * Gets whether online mode is enabled.
	 *
	 * @return true if online mode is enabled
	 */
	public boolean getOnlineMode();

	/**
	 * Gets the points awarded per vote.
	 *
	 * @return the points per vote
	 */
	public int getPointsOnVote();

	/**
	 * Gets whether this is the primary server.
	 *
	 * @return true if this is the primary server
	 */
	public boolean getPrimaryServer();

	/**
	 * Gets the proxy server name.
	 *
	 * @return the proxy server name
	 */
	public String getProxyServerName();

	/**
	 * Gets the list of proxy servers.
	 *
	 * @return the list of proxy servers
	 */
	public List<String> getProxyServers();

	/**
	 * Gets the Redis host.
	 *
	 * @return the Redis host
	 */
	public String getRedisHost();

	/**
	 * Gets the Redis password.
	 *
	 * @return the Redis password
	 */
	public String getRedisPassword();

	/**
	 * Gets the Redis port.
	 *
	 * @return the Redis port
	 */
	public int getRedisPort();

	/**
	 * Gets whether the Redis connection uses SSL/TLS.
	 *
	 * @return true if SSL/TLS is enabled
	 */
	default boolean getRedisSsl() {
		return false;
	}

	/**
	 * Gets the Redis database index.
	 *
	 * @return the Redis database index
	 */
	public int getRedisDbIndex();

	/**
	 * Gets the Redis prefix.
	 *
	 * @return the Redis prefix
	 */
	public String getRedisPrefix();

	/**
	 * Gets the Redis username.
	 *
	 * @return the Redis username
	 */
	public String getRedisUsername();

	/**
	 * Gets whether votes should be sent to all servers.
	 *
	 * @return true if votes should be sent to all servers
	 */
	public boolean getSendVotesToAllServers();

	/**
	 * Gets whether this is the dedicated voting proxy for a multi-proxy network.
	 *
	 * @return true when backend-reported presence should drive vote routing
	 */
	public boolean getDedicatedVotingProxy();

	/**
	 * Gets the configuration for a specific Spigot server.
	 *
	 * @param s the server name
	 * @return the server configuration as a map
	 */
	public Map<String, Object> getSpigotServerConfiguration(String s);

	/**
	 * Gets the collection of Spigot servers.
	 *
	 * @return the collection of server names
	 */
	public Collection<String> getSpigotServers();

	/**
	 * Gets whether to store month totals with date.
	 *
	 * @return true if month totals are stored with date
	 */
	public boolean getStoreMonthTotalsWithDate();

	/**
	 * Gets whether time change fail-safe bypass is enabled.
	 *
	 * @return true if time change fail-safe bypass is enabled
	 */
	public boolean getTimeChangeFailSafeBypass();

	/**
	 * Gets the time hour offset.
	 *
	 * @return the hour offset
	 */
	public int getTimeHourOffSet();

	/**
	 * Gets the time week offset.
	 *
	 * @return the week offset
	 */
	public int getTimeWeekOffSet();

	/**
	 * Gets the time zone.
	 *
	 * @return the time zone
	 */
	public String getTimeZone();

	/**
	 * Gets whether to use month date totals as primary total.
	 *
	 * @return true if month date totals are used as primary total
	 */
	public boolean getUseMonthDateTotalsAsPrimaryTotal();

	/**
	 * Gets whether UUID lookup is enabled.
	 *
	 * @return true if UUID lookup is enabled
	 */
	public boolean getUUIDLookup();

	/**
	 * Gets the vote cache time.
	 *
	 * @return the vote cache time
	 */
	public int getVoteCacheTime();

	/**
	 * Gets the vote party broadcast message.
	 *
	 * @return the vote party broadcast message
	 */
	public String getVotePartyBroadcast();

	/**
	 * Gets the list of vote party Bungee commands.
	 *
	 * @return the list of commands
	 */
	public List<String> getVotePartyBungeeCommands();

	/**
	 * Gets whether vote party is enabled.
	 *
	 * @return true if vote party is enabled
	 */
	public boolean getVotePartyEnabled();

	/**
	 * Gets the vote party increase votes required.
	 *
	 * @return the increase in votes required
	 */
	public int getVotePartyIncreaseVotesRequired();

	/**
	 * Gets whether to send vote party to all servers.
	 *
	 * @return true if vote party is sent to all servers
	 */
	public boolean getVotePartySendToAllServers();

	/**
	 * Gets the list of servers to send vote party to.
	 *
	 * @return the list of servers
	 */
	public List<String> getVotePartyServersToSend();

	/**
	 * Gets the number of votes required for vote party.
	 *
	 * @return the votes required
	 */
	public int getVotePartyVotesRequired();

	/**
	 * Gets whether to wait for user to be online.
	 *
	 * @return true if waiting for user to be online
	 */
	public boolean getWaitForUserOnline();

	/**
	 * Gets the list of whitelisted servers.
	 *
	 * @return the list of whitelisted servers
	 */
	public List<String> getWhiteListedServers();

	/**
	 * Gets the collection of sites with vote delay configured.
	 *
	 * @return the collection of site names
	 */
	public Collection<String> getWaitUntilVoteDelaySites();

	/**
	 * Gets the vote delay service for a specific site.
	 *
	 * @param site the site name
	 * @return the service name
	 */
	public String getWaitUntilVoteDelayService(String site);

	/**
	 * Gets the vote delay in seconds for a specific site.
	 *
	 * @param site the site name
	 * @return the vote delay in seconds
	 */
	public int getWaitUntilVoteDelayVoteDelay(String site);

	/**
	 * Gets the minimum vote delay for a specific site.
	 *
	 * @param site the site name
	 * @return the minimum vote delay
	 */
	public int getWaitUntilVoteDelayVoteDelayMin(String site);

	/**
	 * Gets the hourly vote delay for a specific site.
	 *
	 * @param site the site name
	 * @return the hourly vote delay
	 */
	public int getWaitUntilVoteDelayVoteDelayHour(String site);

	/**
	 * Gets whether the vote delay is daily for a specific site.
	 *
	 * @param site the site name
	 * @return true if the delay is daily
	 */
	public boolean getWaitUntilVoteDelayVoteDelayDaily(String site);

	/**
	 * Loads the configuration.
	 */
	public void load();

	/**
	 * Saves the configuration.
	 */
	public void save();

	/**
	 * Gets whether vote logging is enabled.
	 *
	 * @return true if vote logging is enabled
	 */
	public boolean getVoteLoggingEnabled();

	/**
	 * Gets the number of days to keep vote logs.
	 *
	 * @return the purge days
	 */
	public int getVoteLoggingPurgeDays();

	/**
	 * Gets whether vote logging uses main MySQL.
	 *
	 * @return true if vote logging uses main MySQL
	 */
	public boolean getVoteLoggingUseMainMySQL();

	/**
	 * Gets whether a database is configured.
	 *
	 * @return true if a database is configured
	 */
	public boolean hasDatabaseConfigured();

	/**
	 * Gets the multi-proxy Redis database index.
	 *
	 * @return the database index
	 */
	public int getMultiProxyRedisDbIndex();

}
