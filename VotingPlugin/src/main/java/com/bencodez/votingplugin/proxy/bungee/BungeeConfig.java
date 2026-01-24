package com.bencodez.votingplugin.proxy.bungee;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

import lombok.Getter;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.YamlConfiguration;

public class BungeeConfig implements VotingPluginProxyConfig {
	private VotingPluginBungee bungee;
	@Getter
	private Configuration data;

	public BungeeConfig(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	public Map<String, Object> configToMap(Configuration config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			config.getKeys().forEach(key -> map.put(key, config.get(key)));
		}
		return map;
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
	public boolean getBroadcast() {
		return getData().getBoolean("Broadcast", false);
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
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}

		File file = new File(bungee.getDataFolder(), "bungeeconfig.yml");

		if (!file.exists()) {
			try (InputStream in = bungee.getResourceAsStream("bungeeconfig.yml")) {
				Files.copy(in, file.toPath());
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		try {
			data = ConfigurationProvider.getProvider(YamlConfiguration.class)
					.load(new File(bungee.getDataFolder(), "bungeeconfig.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
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
