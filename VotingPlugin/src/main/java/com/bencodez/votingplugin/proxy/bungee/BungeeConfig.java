package com.bencodez.votingplugin.proxy.bungee;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.Channels;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.control.ProxyMethodConfiguration;
import com.bencodez.votingplugin.proxy.control.ProxyRoutingConfiguration;
import com.bencodez.votingplugin.util.DurableFiles;

import lombok.Getter;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.YamlConfiguration;

/**
 * Configuration implementation for Bungee proxy server.
 */
public class BungeeConfig implements VotingPluginProxyConfig {
	private VotingPluginBungee bungee;
	private byte[] controlInstalledSnapshot;
	@Getter
	private Configuration data;

	/**
	 * Constructs a new BungeeConfig.
	 *
	 * @param bungee the VotingPluginBungee instance
	 */
	public BungeeConfig(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	@Override
	public boolean getControlEnabled() {
		return getData().getBoolean("Control.Enabled", false);
	}

	@Override
	public String getControlEndpoint() {
		return getData().getString("Control.Endpoint", "http://127.0.0.1:8080");
	}

	@Override
	public String getControlNodeId() {
		return getData().getString("Control.NodeId", "");
	}

	@Override
	public String getControlCredentialFile() {
		return getData().getString("Control.CredentialFile", "control/control-credential.txt");
	}

	@Override
	public int getControlHeartbeatSeconds() {
		return getData().getInt("Control.HeartbeatSeconds", 30);
	}

	@Override
	public int getControlConnectTimeoutMillis() {
		return getData().getInt("Control.ConnectTimeoutMillis", 3000);
	}

	@Override
	public int getControlRequestTimeoutMillis() {
		return getData().getInt("Control.RequestTimeoutMillis", 5000);
	}

	@Override
	public boolean getControlHostedEnabled() {
		return getData().getBoolean("Control.Hosted.Enabled", false);
	}

	@Override
	public boolean getControlHostedAutoDownload() {
		return getData().getBoolean("Control.Hosted.AutoDownload", true);
	}

	@Override
	public boolean getControlHostedAutoUpdate() {
		return getData().getBoolean("Control.Hosted.AutoUpdate", true);
	}

	@Override
	public String getControlHostedDownloadUrl() {
		return getData().getString("Control.Hosted.DownloadUrl", "");
	}

	@Override
	public String getControlHostedSha256() {
		return getData().getString("Control.Hosted.Sha256", "");
	}

	@Override
	public String getControlHostedJarFile() {
		return getData().getString("Control.Hosted.JarFile", "control/votingplugin-control.jar");
	}

	@Override
	public String getControlHostedDataDirectory() {
		return getData().getString("Control.Hosted.DataDirectory", "control/data");
	}

	@Override
	public String getControlHostedHost() {
		return getData().getString("Control.Hosted.Host", "127.0.0.1");
	}

	@Override
	public int getControlHostedPort() {
		return getData().getInt("Control.Hosted.Port", 8080);
	}

	@Override
	public int getControlHostedStartupTimeoutSeconds() {
		return getData().getInt("Control.Hosted.StartupTimeoutSeconds", 30);
	}

	@Override
	public int getControlHostedDownloadTimeoutSeconds() {
		return getData().getInt("Control.Hosted.DownloadTimeoutSeconds", 60);
	}

	/**
	 * Converts a Configuration object to a Map.
	 *
	 * @param config the configuration to convert
	 * @return a map representation of the configuration
	 */
	public Map<String, Object> configToMap(Configuration config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			config.getKeys().forEach(key -> map.put(key, config.get(key)));
		}
		return map;
	}

	@Override
	public boolean getProxyBroadcastEnabled() {
		return getData().getBoolean("ProxyBroadcast.Enabled", false);
	}

	@Override
	public String getProxyBroadcastScopeMode() {
		return getData().getString("ProxyBroadcast.Scope.Mode", "ALL_SERVERS");
	}

	@Override
	public List<String> getProxyBroadcastScopeServers() {
		List<String> list = getData().getStringList("ProxyBroadcast.Scope.Servers");
		return list != null ? list : Collections.emptyList();
	}

	@Override
	public String getProxyBroadcastOfflineMode() {
		return getData().getString("ProxyBroadcast.OfflineMode", "QUEUE");
	}

	@Override
	public List<String> getProxyBroadcastOfflineForwardServers() {
		List<String> list = getData().getStringList("ProxyBroadcast.OfflineForward.Servers");
		return list != null ? list : Collections.emptyList();
	}


	@Override
	public boolean getAllowUnJoined() {
		return getData().getBoolean("AllowUnJoined", false);
	}

	@Override
	public String getBedrockPlayerPrefix() {
		return getData().getString("BedrockPlayerPrefix", ".");
	}

	@Override
	public List<String> getBlockedServers() {
		return getData().getStringList("BlockedServers");
	}

	@Override
	public String getBungeeHost() {
		return getData().getString("BungeeServer.Host", "");
	}

	@Override
	public boolean getBungeeManageTotals() {
		return getData().getBoolean("BungeeManageTotals", true);
	}

	@Override
	public String getBungeeMethod() {
		return getData().getString("BungeeMethod", "SOCKETS");
	}

	@Override
	public int getBungeePort() {
		return getData().getInt("BungeeServer.Port", 1297);
	}

	@Override
	public boolean getDebug() {
		return getData().getBoolean("Debug", false);
	}

	@Override
	public String getFallBack() {
		return getData().getString("FallBackServer", "");
	}

	@Override
	public boolean getGlobalDataEnabled() {
		return getData().getBoolean("GlobalData.Enabled", false);
	}

	@Override
	public boolean getGlobalDataUseMainMySQL() {
		return getData().getBoolean("GlobalData.UseMainMySQL", true);
	}

	@Override
	public int getLimitVotePoints() {
		return getData().getInt("LimitVotePoints", -1);
	}

	@Override
	public int getMaxAmountOfVotesPerDay() {
		return getData().getInt("MaxAmountOfVotesPerDay", -1);
	}

	@Override
	public String getMultiProxyMethod() {
		return getData().getString("MultiProxyMethod", "SOCKET");
	}

	@Override
	public boolean getMultiProxyOneGlobalReward() {
		return getData().getBoolean("MultiProxyOneGlobalReward", false);
	}

	@Override
	public String getMultiProxyRedisHost() {
		return getData().getString("MultiProxyRedis.Host", "");
	}

	@Override
	public String getMultiProxyRedisPassword() {
		return getData().getString("MultiProxyRedis.Password", "");
	}

	@Override
	public int getMultiProxyRedisPort() {
		return getData().getInt("MultiProxyRedis.Port", 6379);
	}

	@Override
	public boolean getMultiProxyRedisSsl() {
		return getData().getBoolean("MultiProxyRedis.SSL", false);
	}

	@Override
	public int getMultiProxyRedisDbIndex() {
		return getData().getInt("MultiProxyRedis.Db-Index", 0);
	}

	@Override
	public boolean getMultiProxyRedisUseExistingConnection() {
		return getData().getBoolean("MultiProxyRedis.UseExistingConnection", false);
	}

	@Override
	public String getMultiProxyRedisUsername() {
		return getData().getString("MultiProxyRedis.Username", "");
	}

	@Override
	public Collection<String> getMultiProxyServers() {
		return getData().getSection("MultiProxyServers").getKeys();
	}

	@Override
	public Map<String, Object> getMultiProxyServersConfiguration(String s) {
		return configToMap(getData().getSection("MultiProxyServers." + s));
	}

	@Override
	public String getMultiProxySocketHostHost() {
		return getData().getString("MultiProxySocketHost.Host", "");
	}

	@Override
	public int getMultiProxySocketHostPort() {
		return getData().getInt("MultiProxySocketHost.Port", 1297);
	}

	@Override
	public boolean getMultiProxySupport() {
		return getData().getBoolean("MultiProxySupport", false);
	}

	@Override
	public boolean getOnlineMode() {
		return getData().getBoolean("OnlineMode", true);
	}

	@Override
	public int getPointsOnVote() {
		return getData().getInt("PointsOnVote", 1);
	}

	@Override
	public boolean getPrimaryServer() {
		return getData().getBoolean("PrimaryServer", false);
	}

	@Override
	public String getProxyServerName() {
		return getData().getString("ProxyServerName", "SOCKET");
	}

	@Override
	public List<String> getProxyServers() {
		return getData().getStringList("ProxyServers");
	}

	@Override
	public String getRedisHost() {
		return getData().getString("Redis.Host", "");
	}

	@Override
	public String getRedisPassword() {
		return getData().getString("Redis.Password", "");
	}

	@Override
	public int getRedisPort() {
		return getData().getInt("Redis.Port", 6379);
	}

	@Override
	public boolean getRedisSsl() {
		return getData().getBoolean("Redis.SSL", false);
	}

	@Override
	public int getRedisDbIndex() {
		return getData().getInt("Redis.Db-Index", 0);
	}

	@Override
	public String getRedisPrefix() {
		return getData().getString("Redis.Prefix", "");
	}

	@Override
	public String getRedisUsername() {
		return getData().getString("Redis.Username", "");
	}

	@Override
	public boolean getSendVotesToAllServers() {
		return getData().getBoolean("SendVotesToAllServers");
	}

	@Override
	public boolean getDedicatedVotingProxy() {
		return getData().getBoolean("DedicatedVotingProxy", false);
	}

	@Override
	public Map<String, Object> getSpigotServerConfiguration(String s) {
		return configToMap(getData().getSection("SpigotServers." + s));
	}

	@Override
	public Collection<String> getSpigotServers() {
		return getData().getSection("SpigotServers").getKeys();
	}

	@Override
	public boolean getStoreMonthTotalsWithDate() {
		return getData().getBoolean("StoreMonthTotalsWithDate", false);
	}

	@Override
	public boolean getTimeChangeFailSafeBypass() {
		return getData().getBoolean("TimeChangeFailSafeBypass", false);
	}

	@Override
	public int getTimeHourOffSet() {
		return getData().getInt("TimeHourOffSet");
	}

	@Override
	public String getTimeZone() {
		return getData().getString("TimeZone", "");
	}

	@Override
	public boolean getUseMonthDateTotalsAsPrimaryTotal() {
		return getData().getBoolean("UseMonthDateTotalsAsPrimaryTotal", false);
	}

	@Override
	public boolean getUUIDLookup() {
		return getData().getBoolean("UUIDLookup", true);
	}

	@Override
	public int getVoteCacheTime() {
		return getData().getInt("VoteCacheTime", -1);
	}

	@Override
	public String getVotePartyBroadcast() {
		return getData().getString("VoteParty.Broadcast", "");
	}

	@Override
	public List<String> getVotePartyBungeeCommands() {
		return getData().getStringList("VoteParty.BungeeCommands");
	}

	@Override
	public boolean getVotePartyEnabled() {
		return getData().getBoolean("VoteParty.Enabled", false);
	}

	@Override
	public int getVotePartyIncreaseVotesRequired() {
		return getData().getInt("VoteParty.IncreaseVotesRequired", 0);
	}

	@Override
	public boolean getVotePartySendToAllServers() {
		return getData().getBoolean("VoteParty.SendToAllServers", true);
	}

	@Override
	public List<String> getVotePartyServersToSend() {
		return getData().getStringList("VoteParty.ServersToSend");
	}

	@Override
	public int getVotePartyVotesRequired() {
		return getData().getInt("VoteParty.VotesRequired", 100);
	}

	@Override
	public boolean getWaitForUserOnline() {
		return getData().getBoolean("WaitForUserOnline", true);
	}

	@Override
	public List<String> getWhiteListedServers() {
		return getData().getStringList("WhiteListedServers");
	}

	@Override
	public String getMqttClientID() {
		return getData().getString("MQTT.ClientID", "proxy");
	}

	@Override
	public String getMqttBrokerURL() {
		return getData().getString("MQTT.BrokerURL", "tcp://localhost:1883");
	}

	@Override
	public String getMqttUsername() {
		return getData().getString("MQTT.Username", "");
	}

	@Override
	public String getMqttPassword() {
		return getData().getString("MQTT.Password", "");
	}

	@Override
	public String getMqttPrefix() {
		return getData().getString("MQTT.Prefix", "");
	}

	public void load() {
		try {
			loadControlConfiguration();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/** Loads the configuration while propagating failures to Control's rollback path. */
	public void loadControlConfiguration() throws IOException {
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}

		File file = new File(bungee.getDataFolder(), "bungeeconfig.yml");

		if (!file.exists()) {
			try (InputStream in = bungee.getResourceAsStream("bungeeconfig.yml")) {
				Files.copy(in, file.toPath());
			}
		}
		data = ConfigurationProvider.getProvider(YamlConfiguration.class)
				.load(new File(bungee.getDataFolder(), "bungeeconfig.yml"));
	}

	public void save() {
		try {
			ConfigurationProvider.getProvider(YamlConfiguration.class).save(data,
					new File(bungee.getDataFolder(), "bungeeconfig.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public synchronized void persistControlProxyRouting(boolean sendVotesToAllServers, List<String> blockedServers,
			String expectedRevision) throws IOException {
		Path target = new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath();
		Path stage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-stage");
		Path backupStage = null;
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		try {
			backupStage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-backup-stage");
			byte[] sourceSnapshot = Files.readAllBytes(target);
			Configuration latest = ConfigurationProvider.getProvider(YamlConfiguration.class).load(target.toFile());
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))
					|| !routing(latest).revision().equals(expectedRevision)) {
				throw new StaleControlRevisionException();
			}
			latest.set("SendVotesToAllServers", sendVotesToAllServers);
			latest.set("BlockedServers", List.copyOf(blockedServers));
			ConfigurationProvider.getProvider(YamlConfiguration.class).save(latest, stage.toFile());
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
			data = latest;
		} finally {
			Files.deleteIfExists(stage);
			if (backupStage != null) Files.deleteIfExists(backupStage);
		}
	}

	private static ProxyRoutingConfiguration routing(Configuration configuration) {
		return new ProxyRoutingConfiguration(configuration.getBoolean("SendVotesToAllServers", false),
				configuration.getStringList("BlockedServers"));
	}

	@Override
	public synchronized void persistControlProxyMethod(String method, String expectedRevision) throws IOException {
		Path target = new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath();
		Path stage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-stage");
		Path backupStage = null;
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		try {
			backupStage = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".control-backup-stage");
			byte[] sourceSnapshot = Files.readAllBytes(target);
			Configuration latest = ConfigurationProvider.getProvider(YamlConfiguration.class).load(target.toFile());
			ProxyMethodConfiguration current = new ProxyMethodConfiguration(
					com.bencodez.votingplugin.proxy.control.ProxyMethodConfigurationService.canonical(
							latest.getString("BungeeMethod", "PLUGINMESSAGING")));
			if (!java.util.Arrays.equals(sourceSnapshot, Files.readAllBytes(target))
					|| !current.revision().equals(expectedRevision)) throw new StaleControlRevisionException();
			latest.set("BungeeMethod", method);
			ConfigurationProvider.getProvider(YamlConfiguration.class).save(latest, stage.toFile());
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
			data = latest;
		} finally {
			Files.deleteIfExists(stage);
			if (backupStage != null) Files.deleteIfExists(backupStage);
		}
	}

	@Override
	public synchronized void rollbackControlProxyRouting() throws IOException {
		Path target = new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath();
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
		data = ConfigurationProvider.getProvider(YamlConfiguration.class).load(target.toFile());
	}

	@Override
	public synchronized void verifyControlProxyRoutingInstalled() throws IOException {
		Path target = new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath();
		if (controlInstalledSnapshot == null
				|| !java.util.Arrays.equals(controlInstalledSnapshot, Files.readAllBytes(target))) {
			throw new StaleControlRevisionException();
		}
	}

	@Override
	public synchronized byte[] captureControlProxyRoutingSnapshot() throws IOException {
		return Files.readAllBytes(new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath());
	}

	@Override
	public synchronized void verifyControlProxyRoutingSnapshot(byte[] snapshot) throws IOException {
		Path target = new File(bungee.getDataFolder(), "bungeeconfig.yml").toPath();
		if (!java.util.Arrays.equals(snapshot, Files.readAllBytes(target))) {
			throw new StaleControlRevisionException();
		}
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
	public String getPluginMessageChannel() {
		return getData().getString("PluginMessageChannel", "vp:vp");
	}

	@Override
	public boolean getPluginMessageEncryption() {
		return getData().getBoolean("PluginMessageEncryption", false);
	}

	@Override
	public Collection<String> getWaitUntilVoteDelaySites() {
		return getData().getSection("WaitUntilVoteDelay").getKeys();
	}

	@Override
	public String getWaitUntilVoteDelayService(String site) {
		return getData().getString("WaitUntilVoteDelay." + site + ".ServiceSite", "");
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelay(String site) {
		return getData().getInt("WaitUntilVoteDelay." + site + ".VoteDelay", 24);
	}

	@Override
	public boolean getWaitUntilVoteDelayVoteDelayDaily(String site) {
		return getData().getBoolean("WaitUntilVoteDelay." + site + ".VoteDelayDaily", false);
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelayHour(String site) {
		return getData().getInt("WaitUntilVoteDelay." + site + ".VoteDelayDailyHour", 0);
	}

	@Override
	public int getWaitUntilVoteDelayVoteDelayMin(String site) {
		return getData().getInt("WaitUntilVoteDelay." + site + ".VoteDelayMin", 0);
	}

	@Override
	public boolean getVoteCacheUseMySQL() {
		return getData().getBoolean("VoteCache.UseMySQL", false);
	}

	@Override
	public boolean getVoteCacheUseMainMySQL() {
		return getData().getBoolean("VoteCache.UseMainMySQL", true);
	}

	@Override
	public int getTimeWeekOffSet() {
		return getData().getInt("TimeWeekOffSet");
	}

	@Override
	public boolean getNonVotedCacheUseMySQL() {
		return getData().getBoolean("NonVotedCache.UseMySQL", false);
	}

	@Override
	public boolean getNonVotedCacheUseMainMySQL() {
		return getData().getBoolean("NonVotedCache.UseMainMySQL", true);
	}

	@Override
	public boolean getVoteLoggingEnabled() {
		return getData().getBoolean("VoteLogging.Enabled", false);
	}

	@Override
	public int getVoteLoggingPurgeDays() {
		return getData().getInt("VoteLogging.PurgeDays", 30);
	}

	@Override
	public boolean getVoteLoggingUseMainMySQL() {
		return getData().getBoolean("VoteLogging.UseMainMySQL", true);
	}

	/**
	 * Returns a configuration section if the key exists, otherwise null.
	 *
	 * @param root the root configuration
	 * @param key the key to check
	 * @return the configuration section or null
	 */
	public Configuration sectionOrNull(Configuration root, String key) {
		Object v = root.get(key);
		return (v instanceof Configuration) ? (Configuration) v : null;
	}

	@Override
	public boolean hasDatabaseConfigured() {
		// New-style: Database: { Host: ..., ... }
		Configuration db = sectionOrNull(data, "Database");
		if (db != null) {
			return db.getString("Host", "") != null && !db.getString("Host", "").isEmpty();
		}

		// Legacy-style: Host/Port/... at root
		String host = data.getString("Host", "");
		return host != null && !host.isEmpty();
	}

}
