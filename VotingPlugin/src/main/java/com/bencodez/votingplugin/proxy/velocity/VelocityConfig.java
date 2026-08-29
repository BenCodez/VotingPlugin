package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.checkerframework.checker.nullness.qual.NonNull;

import com.bencodez.simpleapi.file.velocity.VelocityYMLFile;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfiguration;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfigurationService;
import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfiguration;
import com.bencodez.votingplugin.util.DurableFiles;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

/**
 * Configuration file handler for Velocity proxy.
 */
public class VelocityConfig extends VelocityYMLFile implements VotingPluginProxyConfig {
	private final File configurationFile;
	private byte[] controlInstalledSnapshot;

	/**
	 * Constructs a new Velocity configuration.
	 * @param file the configuration file
	 */
	public VelocityConfig(File file) {
		super(file);
		configurationFile = file;
	}

	@Override
	public synchronized void persistControlProxyRouting(boolean sendVotesToAllServers, List<String> blockedServers,
			String expectedRevision) throws IOException {
		Path target = configurationFile.toPath();
		Path stage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-stage");
		Path backupStage = null;
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		YamlConfigurationLoader sourceLoader = YamlConfigurationLoader.builder().path(target).build();
		try {
			backupStage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-backup-stage");
			byte[] sourceSnapshot = Files.readAllBytes(target);
			ConfigurationNode latest = sourceLoader.load();
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))
					|| !routing(latest).revision().equals(expectedRevision)) {
				throw new StaleControlRevisionException();
			}
			latest.node("SendVotesToAllServers").set(sendVotesToAllServers);
			latest.node("BlockedServers").setList(String.class, List.copyOf(blockedServers));
			YamlConfigurationLoader.builder().path(stage).build().save(latest);
			byte[] installedSnapshot = Files.readAllBytes(stage);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) {
				throw new StaleControlRevisionException();
			}
			Files.write(backupStage, sourceSnapshot);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) {
				throw new StaleControlRevisionException();
			}
			atomicReplace(backupStage, backup);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) {
				throw new StaleControlRevisionException();
			}
			controlInstalledSnapshot = installedSnapshot;
			try {
				atomicReplace(stage, target);
			} catch (IOException failure) {
				if (!(failure instanceof DurableFiles.PublishedException)) controlInstalledSnapshot = null;
				throw failure;
			}
		} finally {
			Files.deleteIfExists(stage);
			if (backupStage != null) Files.deleteIfExists(backupStage);
		}
	}

	private static ProxyRoutingConfiguration routing(ConfigurationNode configuration) throws IOException {
		return new ProxyRoutingConfiguration(configuration.node("SendVotesToAllServers").getBoolean(true),
				configuration.node("BlockedServers").getList(String.class, List.of()));
	}

	@Override
	public synchronized void persistControlProxyMethod(String method, String expectedRevision) throws IOException {
		Path target = configurationFile.toPath();
		Path stage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-stage");
		Path backupStage = null;
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		YamlConfigurationLoader sourceLoader = YamlConfigurationLoader.builder().path(target).build();
		try {
			backupStage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-backup-stage");
			byte[] sourceSnapshot = Files.readAllBytes(target);
			ConfigurationNode latest = sourceLoader.load();
			ProxyMethodConfiguration current = new ProxyMethodConfiguration(ProxyMethodConfigurationService.canonical(
					latest.node("BungeeMethod").getString("PLUGINMESSAGING")));
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))
					|| !current.revision().equals(expectedRevision)) throw new StaleControlRevisionException();
			latest.node("BungeeMethod").set(method);
			YamlConfigurationLoader.builder().path(stage).build().save(latest);
			byte[] installedSnapshot = Files.readAllBytes(stage);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) throw new StaleControlRevisionException();
			Files.write(backupStage, sourceSnapshot);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) throw new StaleControlRevisionException();
			atomicReplace(backupStage, backup);
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))) throw new StaleControlRevisionException();
			controlInstalledSnapshot = installedSnapshot;
			try {
				atomicReplace(stage, target);
			} catch (IOException failure) {
				if (!(failure instanceof DurableFiles.PublishedException)) controlInstalledSnapshot = null;
				throw failure;
			}
		} finally {
			Files.deleteIfExists(stage);
			if (backupStage != null) Files.deleteIfExists(backupStage);
		}
	}

	@Override
	public synchronized void rollbackControlProxyRouting() throws IOException {
		Path target = configurationFile.toPath();
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		if (!Files.isRegularFile(backup, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control backup is unavailable or unsafe");
		}
		if (controlInstalledSnapshot == null
				|| !java.util.Arrays.equals(controlInstalledSnapshot, Files.readAllBytes(target))) {
			throw new StaleControlRevisionException();
		}
		Path stage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-rollback");
		try {
			try (SeekableByteChannel source = Files.newByteChannel(backup,
					Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
				Files.copy(Channels.newInputStream(source), stage, StandardCopyOption.REPLACE_EXISTING);
			}
			if (!java.util.Arrays.equals(controlInstalledSnapshot, Files.readAllBytes(target))) {
				throw new StaleControlRevisionException();
			}
			try {
				atomicReplace(stage, target);
			} catch (DurableFiles.PublishedException published) {
				controlInstalledSnapshot = null;
				throw published;
			}
		} finally {
			Files.deleteIfExists(stage);
		}
		controlInstalledSnapshot = null;
		loadControlConfiguration();
	}

	@Override
	public synchronized void verifyControlProxyRoutingInstalled() throws IOException {
		Path target = configurationFile.toPath();
		if (controlInstalledSnapshot == null
				|| !java.util.Arrays.equals(controlInstalledSnapshot, Files.readAllBytes(target))) {
			throw new StaleControlRevisionException();
		}
	}

	@Override
	public synchronized byte[] captureControlProxyRoutingSnapshot() throws IOException {
		return Files.readAllBytes(configurationFile.toPath());
	}

	@Override
	public synchronized void verifyControlProxyRoutingSnapshot(byte[] snapshot) throws IOException {
		if (!java.util.Arrays.equals(snapshot, Files.readAllBytes(configurationFile.toPath()))) {
			throw new StaleControlRevisionException();
		}
	}

	/** Loads the active file without the superclass's empty-config fallback. */
	public synchronized void loadControlConfiguration() throws IOException {
		ConfigurationNode loaded = YamlConfigurationLoader.builder().path(configurationFile.toPath()).build().load();
		setConf(loaded);
	}

	private static void atomicReplace(Path source, Path target) throws IOException {
		try {
			DurableFiles.forceFile(source);
			Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			DurableFiles.forceMoveDirectories(source, target);
		} catch (java.nio.file.AtomicMoveNotSupportedException e) {
			throw new IOException("Atomic Control configuration activation is unsupported", e);
		}
	}

	@Override
	public boolean getControlEnabled() {
		return getBoolean(getNode("Control", "Enabled"), false);
	}

	@Override
	public String getControlEndpoint() {
		return getString(getNode("Control", "Endpoint"), "http://127.0.0.1:8080");
	}

	@Override
	public String getControlNodeId() {
		return getString(getNode("Control", "NodeId"), "");
	}

	@Override
	public String getControlCredentialFile() {
		return getString(getNode("Control", "CredentialFile"), "control/control-credential.txt");
	}

	@Override
	public int getControlHeartbeatSeconds() {
		return getInt(getNode("Control", "HeartbeatSeconds"), 30);
	}

	@Override
	public int getControlConnectTimeoutMillis() {
		return getInt(getNode("Control", "ConnectTimeoutMillis"), 3000);
	}

	@Override
	public int getControlRequestTimeoutMillis() {
		return getInt(getNode("Control", "RequestTimeoutMillis"), 5000);
	}

	@Override
	public boolean getControlHostedEnabled() {
		return getBoolean(getNode("Control", "Hosted", "Enabled"), false);
	}

	@Override
	public boolean getControlHostedAutoDownload() {
		return getBoolean(getNode("Control", "Hosted", "AutoDownload"), true);
	}

	@Override
	public boolean getControlHostedAutoUpdate() {
		return getBoolean(getNode("Control", "Hosted", "AutoUpdate"), true);
	}

	@Override
	public String getControlHostedDownloadUrl() {
		return getString(getNode("Control", "Hosted", "DownloadUrl"), "");
	}

	@Override
	public String getControlHostedSha256() {
		return getString(getNode("Control", "Hosted", "Sha256"), "");
	}

	@Override
	public String getControlHostedJarFile() {
		return getString(getNode("Control", "Hosted", "JarFile"), "control/votingplugin-control.jar");
	}

	@Override
	public String getControlHostedDataDirectory() {
		return getString(getNode("Control", "Hosted", "DataDirectory"), "control/data");
	}

	@Override
	public String getControlHostedHost() {
		return getString(getNode("Control", "Hosted", "Host"), "127.0.0.1");
	}

	@Override
	public int getControlHostedPort() {
		return getInt(getNode("Control", "Hosted", "Port"), 8080);
	}

	@Override
	public int getControlHostedStartupTimeoutSeconds() {
		return getInt(getNode("Control", "Hosted", "StartupTimeoutSeconds"), 30);
	}

	@Override
	public int getControlHostedDownloadTimeoutSeconds() {
		return getInt(getNode("Control", "Hosted", "DownloadTimeoutSeconds"), 60);
	}

	@Override
	public boolean getAllowUnJoined() {
		return getBoolean(getNode("AllowUnJoined"), false);
	}

	@Override
	public String getBedrockPlayerPrefix() {
		return getString(getNode("BedrockPlayerPrefix"), ".");
	}

	@Override

	public List<String> getBlockedServers() {
		return getStringList(getNode("BlockedServers"), new ArrayList<>());
	}

	@Override
	public boolean getProxyBroadcastEnabled() {
		return getBoolean(getNode("ProxyBroadcast", "Enabled"), false);
	}

	@Override
	public String getProxyBroadcastScopeMode() {
		return getString(getNode("ProxyBroadcast", "Scope", "Mode"), "ALL_SERVERS");
	}

	@Override
	public List<String> getProxyBroadcastScopeServers() {
		List<String> list = getStringList(getNode("ProxyBroadcast", "Scope", "Servers"), new ArrayList<>());
		return list != null ? list : Collections.emptyList();
	}

	@Override
	public String getProxyBroadcastOfflineMode() {
		return getString(getNode("ProxyBroadcast", "OfflineMode"), "QUEUE");
	}

	@Override
	public List<String> getProxyBroadcastOfflineForwardServers() {
		List<String> list = getStringList(getNode("ProxyBroadcast", "OfflineForward", "Servers"), new ArrayList<>());
		return list != null ? list : Collections.emptyList();
	}

	@Override
	public String getBungeeHost() {
		return getString(getNode("BungeeServer", "Host"), "");
	}

	@Override
	public boolean getBungeeManageTotals() {
		return getBoolean(getNode("BungeeManageTotals"), true);
	}

	@Override
	public String getBungeeMethod() {
		return getString(getNode("BungeeMethod"), "PLUGINMESSAGING");
	}

	@Override
	public int getBungeePort() {
		return getInt(getNode("BungeeServer", "Port"), 1297);
	}

	@Override
	public boolean getDebug() {
		return getBoolean(getNode("Debug"), false);
	}

	@Override
	public String getFallBack() {
		return getString(getNode("FallBackServer"), "");
	}

	@Override
	public boolean getGlobalDataEnabled() {
		return getBoolean(getNode("GlobalData", "Enabled"), false);
	}

	@Override
	public boolean getGlobalDataUseMainMySQL() {
		return getBoolean(getNode("GlobalData", "UseMainMySQL"), true);
	}

	@Override
	public int getLimitVotePoints() {
		return getInt(getNode("LimitVotePoints"), -1);
	}

	@Override
	public int getMaxAmountOfVotesPerDay() {
		return getInt(getNode("MaxAmountOfVotesPerDay"), -1);
	}

	@Override
	public String getMultiProxyMethod() {
		return getString(getNode("MultiProxyMethod"), "SOCKET");
	}

	@Override
	public boolean getMultiProxyOneGlobalReward() {
		return getBoolean(getNode("MultiProxyOneGlobalReward"), false);
	}

	@Override
	public String getMultiProxyRedisHost() {
		return getString(getNode("MultiProxyRedis", "Host"), "");
	}

	@Override
	public String getMultiProxyRedisPassword() {
		return getString(getNode("MultiProxyRedis", "Password"), "");
	}

	@Override
	public int getMultiProxyRedisPort() {
		return getInt(getNode("MultiProxyRedis", "Port"), 6379);
	}

	@Override
	public boolean getMultiProxyRedisSsl() {
		return getBoolean(getNode("MultiProxyRedis", "SSL"), false);
	}

	@Override
	public int getMultiProxyRedisDbIndex() {
		return getInt(getNode("MultiProxyRedis", "Db-Index"), 0);
	}

	@Override
	public boolean getMultiProxyRedisUseExistingConnection() {
		return getBoolean(getNode("MultiProxyRedis", "UseExistingConnection"), false);
	}

	@Override
	public String getMultiProxyRedisUsername() {
		return getString(getNode("MultiProxyRedis", "Username"), "");
	}

	@Override
	public @NonNull List<String> getMultiProxyServers() {
		return getChildrenAsList(getNode("MultiProxyServers"));
	}

	/**
	 * Gets children of a configuration node as a list.
	 * @param config the configuration node
	 * @return list of child keys
	 */
	public List<String> getChildrenAsList(ConfigurationNode config) {
		List<String> children = new ArrayList<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.childrenMap().entrySet()) {
				children.add(entry.getKey().toString());
			}
		}
		return children;
	}

	@Override
	public Map<String, Object> getMultiProxyServersConfiguration(String s) {
		return configToMap(getNode("MultiProxyServers", s));
	}

	@Override
	public String getMultiProxySocketHostHost() {
		return getString(getNode("MultiProxySocketHost", "Host"), "");
	}

	@Override
	public int getMultiProxySocketHostPort() {
		return getInt(getNode("MultiProxySocketHost", "Port"), 1297);
	}

	@Override
	public boolean getMultiProxySupport() {
		return getBoolean(getNode("MultiProxySupport"), false);
	}

	/**
	 * Gets the MySQL configuration node.
	 * @return the MySQL configuration node
	 */
	public ConfigurationNode getMysqlNode() {
		return getNode("MySQL");
	}

	@Override
	public boolean getOnlineMode() {
		return getBoolean(getNode("OnlineMode"), true);
	}

	@Override
	public int getPointsOnVote() {
		return getInt(getNode("PointsOnVote"), 1);
	}

	@Override
	public boolean getPrimaryServer() {
		return getBoolean(getNode("PrimaryServer"), false);
	}

	@Override
	public String getProxyServerName() {
		return getString(getNode("ProxyServerName"), "SOCKET");
	}

	@Override
	public List<String> getProxyServers() {
		return getStringList(getNode("ProxyServers"), new ArrayList<>());
	}

	@Override
	public String getRedisHost() {
		return getString(getNode("Redis", "Host"), "");
	}

	@Override
	public String getRedisPassword() {
		return getString(getNode("Redis", "Password"), "");
	}

	@Override
	public int getRedisPort() {
		return getInt(getNode("Redis", "Port"), 6379);
	}

	@Override
	public boolean getRedisSsl() {
		return getBoolean(getNode("Redis", "SSL"), false);
	}

	@Override
	public int getRedisDbIndex() {
		return getInt(getNode("Redis", "Db-Index"), 0);
	}

	@Override
	public String getRedisPrefix() {
		return getString(getNode("Redis", "Prefix"), "");
	}

	@Override
	public String getRedisUsername() {
		return getString(getNode("Redis", "Username"), "");
	}

	@Override
	public boolean getSendVotesToAllServers() {
		return getBoolean(getNode("SendVotesToAllServers"), true);
	}

	@Override
	public boolean getDedicatedVotingProxy() {
		return getBoolean(getNode("DedicatedVotingProxy"), false);
	}

	@Override
	public Map<String, Object> getSpigotServerConfiguration(String s) {
		return configToMap(getNode("SpigotServers", s));
	}

	@Override
	public String getMqttClientID() {
		return getString(getNode("MQTT", "ClientID"), "proxy");
	}

	@Override
	public String getMqttBrokerURL() {
		return getString(getNode("MQTT", "BrokerURL"), "tcp://localhost:1883");
	}

	@Override
	public String getMqttUsername() {
		return getString(getNode("MQTT", "Username"), "");
	}

	@Override
	public String getMqttPassword() {
		return getString(getNode("MQTT", "Password"), "");
	}

	@Override
	public String getMqttPrefix() {
		return getString(getNode("MQTT", "Prefix"), "");
	}

	/**
	 * Converts a configuration node to a map.
	 * @param config the configuration node
	 * @return map of configuration values
	 */
	public Map<String, Object> configToMap(ConfigurationNode config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.childrenMap().entrySet()) {
				map.put(entry.getKey().toString(), entry.getValue().raw());
			}
		}
		return map;
	}

	@Override
	public @NonNull List<String> getSpigotServers() {
		return getChildrenAsList(getNode("SpigotServers"));
	}

	@Override
	public boolean getStoreMonthTotalsWithDate() {
		return getBoolean(getNode("StoreMonthTotalsWithDate"), false);
	}

	@Override
	public boolean getTimeChangeFailSafeBypass() {
		return getBoolean(getNode("TimeChangeFailSafeBypass"), false);
	}

	@Override
	public boolean getUseMonthDateTotalsAsPrimaryTotal() {
		return getBoolean(getNode("UseMonthDateTotalsAsPrimaryTotal"), false);
	}

	@Override
	public boolean getUUIDLookup() {
		return getBoolean(getNode("UUIDLookup"), true);
	}

	@Override
	public int getVoteCacheTime() {
		return getInt(getNode("VoteCacheTime"), -1);
	}

	@Override
	public String getVotePartyBroadcast() {
		return getString(getVotePartyNode().node("Broadcast"), "");
	}

	@Override
	public List<String> getVotePartyBungeeCommands() {
		return getStringList(getVotePartyNode().node("BungeeCommands"), new ArrayList<>());
	}

	@Override
	public boolean getVotePartyEnabled() {
		return getBoolean(getVotePartyNode().node("Enabled"), false);
	}

	@Override
	public int getVotePartyIncreaseVotesRequired() {
		return getInt(getVotePartyNode().node("IncreaseVotesRequired"), 0);
	}

	/**
	 * Gets the vote party configuration node.
	 * @return the vote party configuration node
	 */
	public ConfigurationNode getVotePartyNode() {
		return getNode("VoteParty");
	}

	@Override
	public boolean getVotePartySendToAllServers() {
		return getBoolean(getVotePartyNode().node("SendToAllServers"), false);
	}

	@Override
	public List<String> getVotePartyServersToSend() {
		return getStringList(getVotePartyNode().node("ServersToSend"), new ArrayList<>());
	}

	@Override
	public int getVotePartyVotesRequired() {
		return getInt(getVotePartyNode().node("VotesRequired"), 100);
	}

	@Override
	public boolean getWaitForUserOnline() {
		return getBoolean(getNode("WaitForUserOnline"), true);
	}

	@Override
	public List<String> getWhiteListedServers() {
		return getStringList(getNode("WhiteListedServers"), new ArrayList<>());
	}

	@Override
	public int getTimeHourOffSet() {
		return getInt(getNode("TimeHourOffSet"), 0);
	}

	@Override
	public String getTimeZone() {
		return getString(getNode("TimeZone"), "");
	}

	@Override
	public void load() {
		// TODO Auto-generated method stub

	}

	@Override
	public String getPluginMessageChannel() {
		return getString(getNode("PluginMessageChannel"), "vp:vp");
	}

	@Override
	public boolean getPluginMessageEncryption() {
		return getBoolean(getNode("PluginMessageEncryption"), false);
	}

	@Override
	public Collection<String> getWaitUntilVoteDelaySites() {
		return getChildrenAsList(getNode("WaitUntilVoteDelay"));
	}

	@Override
	public String getWaitUntilVoteDelayService(String site) {
		return getString(getNode("WaitUntilVoteDelay", site, "ServiceSite"), "");
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelay(String site) {
		return getInt(getNode("WaitUntilVoteDelay", site, "VoteDelay"), 24);
	}

	@Override
	public boolean getWaitUntilVoteDelayVoteDelayDaily(String site) {
		return getBoolean(getNode("WaitUntilVoteDelay", site, "VoteDelayDaily"), false);
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelayHour(String site) {
		return getInt(getNode("WaitUntilVoteDelay", site, "VoteDelayDailyHour"), 0);
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelayMin(String site) {
		return getInt(getNode("WaitUntilVoteDelay", site, "VoteDelayMin"), 0);
	}

	@Override
	public boolean getVoteCacheUseMySQL() {
		return getBoolean(getNode("VoteCache", "UseMySQL"), false);
	}

	@Override
	public boolean getVoteCacheUseMainMySQL() {
		return getBoolean(getNode("VoteCache", "UseMainMySQL"), true);
	}

	@Override
	public int getTimeWeekOffSet() {
		return getInt(getNode("TimeWeekOffSet"), 0);
	}

	@Override
	public boolean getNonVotedCacheUseMySQL() {
		return getBoolean(getNode("NonVotedCache", "UseMySQL"), false);
	}

	@Override
	public boolean getNonVotedCacheUseMainMySQL() {
		return getBoolean(getNode("NonVotedCache", "UseMainMySQL"), true);
	}

	@Override
	public boolean getVoteLoggingEnabled() {
		return getBoolean(getNode("VoteLogging", "Enabled"), false);
	}

	@Override
	public int getVoteLoggingPurgeDays() {
		return getInt(getNode("VoteLogging", "PurgeDays"), 30);
	}

	@Override
	public boolean getVoteLoggingUseMainMySQL() {
		return getBoolean(getNode("VoteLogging", "UseMainMySQL"), true);
	}

	/**
	 * Checks if a node is a configuration section.
	 * @param node the node to check
	 * @return true if the node is a section
	 */
	private boolean isSection(ConfigurationNode node) {
		return node != null && node.raw() instanceof Map;
	}

	@Override
	public boolean hasDatabaseConfigured() {
		// New-style: Database: { Host: ... }
		ConfigurationNode db = getNode("Database");
		if (isSection(db)) {
			String host = getString(getNode("Database", "Host"), "");
			return host != null && !host.isEmpty();
		}

		// Legacy-style: Host at root
		String host = getString(getNode("Host"), "");
		if (host != null && !host.isEmpty()) {
			return true;
		}

		// Optional older style some setups used: MySQL.Host
		ConfigurationNode mysql = getNode("MySQL");
		if (isSection(mysql)) {
			String mysqlHost = getString(getNode("MySQL", "Host"), "");
			return mysqlHost != null && !mysqlHost.isEmpty();
		}

		return false;
	}

}
