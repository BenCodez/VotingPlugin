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

public class Config implements VotingPluginProxyConfig {
	private VotingPluginBungee bungee;
	@Getter
	private Configuration data;

	public Config(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	public Map<String, Object> configToMap(Configuration config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			config.getKeys().forEach(key -> map.put(key, config.get(key)));
		}
		return map;
	}

	public boolean getAllowUnJoined() {
		return getData().getBoolean("AllowUnJoined", false);
	}

	public String getBedrockPlayerPrefix() {
		return getData().getString("BedrockPlayerPrefix", ".");
	}

	public List<String> getBlockedServers() {
		return getData().getStringList("BlockedServers");
	}

	public boolean getBroadcast() {
		return getData().getBoolean("Broadcast", false);
	}

	public String getBungeeHost() {
		return getData().getString("BungeeServer.Host", "");
	}

	public boolean getBungeeManageTotals() {
		return getData().getBoolean("BungeeManageTotals", true);
	}

	public String getBungeeMethod() {
		return getData().getString("BungeeMethod", "SOCKETS");
	}

	public int getBungeePort() {
		return getData().getInt("BungeeServer.Port", 1297);
	}

	public boolean getDebug() {
		return getData().getBoolean("Debug", false);
	}

	public String getFallBack() {
		return getData().getString("FallBackServer", "");
	}

	public boolean getGlobalDataEnabled() {
		return getData().getBoolean("GlobalData.Enabled", false);
	}

	public boolean getGlobalDataUseMainMySQL() {
		return getData().getBoolean("GlobalData.UseMainMySQL", true);
	}

	public int getLimitVotePoints() {
		return getData().getInt("LimitVotePoints", -1);
	}

	public int getMaxAmountOfVotesPerDay() {
		return getData().getInt("MaxAmountOfVotesPerDay", -1);
	}

	public String getMultiProxyMethod() {
		return getData().getString("MultiProxyMethod", "SOCKET");
	}

	public boolean getMultiProxyOneGlobalReward() {
		return getData().getBoolean("MultiProxyOneGlobalReward", false);
	}

	public String getMultiProxyRedisHost() {
		return getData().getString("MultiProxyRedis.Host", "");
	}

	public String getMultiProxyRedisPassword() {
		return getData().getString("MultiProxyRedis.Password", "");
	}

	public int getMultiProxyRedisPort() {
		return getData().getInt("MultiProxyRedis.Port", 6379);
	}

	public boolean getMultiProxyRedisUseExistingConnection() {
		return getData().getBoolean("MultiProxyRedis.UseExistingConnection", false);
	}

	public String getMultiProxyRedisUsername() {
		return getData().getString("MultiProxyRedis.Username", "");
	}

	public Collection<String> getMultiProxyServers() {
		return getData().getSection("MultiProxyServers").getKeys();
	}

	public Map<String, Object> getMultiProxyServersConfiguration(String s) {
		return configToMap(getData().getSection("MultiProxyServers." + s));
	}

	public String getMultiProxySocketHostHost() {
		return getData().getString("MultiProxySocketHost.Host", "");
	}

	public int getMultiProxySocketHostPort() {
		return getData().getInt("MultiProxySocketHost.Port", 1297);
	}

	public boolean getMultiProxySupport() {
		return getData().getBoolean("MultiProxySupport", false);
	}

	public boolean getOnlineMode() {
		return getData().getBoolean("OnlineMode", true);
	}

	public int getPointsOnVote() {
		return getData().getInt("PointsOnVote", 1);
	}

	public boolean getPrimaryServer() {
		return getData().getBoolean("PrimaryServer", false);
	}

	public String getProxyServerName() {
		return getData().getString("ProxyServerName", "SOCKET");
	}

	public List<String> getProxyServers() {
		return getData().getStringList("ProxyServers");
	}

	public String getRedisHost() {
		return getData().getString("Redis.Host", "");
	}

	public String getRedisPassword() {
		return getData().getString("Redis.Password", "");
	}

	public int getRedisPort() {
		return getData().getInt("Redis.Port", 6379);
	}

	public String getRedisPrefix() {
		return getData().getString("Redis.Prefix", "");
	}

	public String getRedisUsername() {
		return getData().getString("Redis.Username", "");
	}

	public boolean getSendVotesToAllServers() {
		return getData().getBoolean("SendVotesToAllServers");
	}

	public Map<String, Object> getSpigotServerConfiguration(String s) {
		return configToMap(getData().getSection("SpigotServers." + s));
	}

	public Collection<String> getSpigotServers() {
		return getData().getSection("SpigotServers").getKeys();
	}

	public boolean getStoreMonthTotalsWithDate() {
		return getData().getBoolean("StoreMonthTotalsWithDate", false);
	}

	public boolean getTimeChangeFailSafeBypass() {
		return getData().getBoolean("TimeChangeFailSafeBypass", false);
	}

	public int getTimeHourOffSet() {
		return getData().getInt("TimeHourOffSet");
	}

	public String getTimeZone() {
		return getData().getString("TimeZone", "");
	}

	public boolean getUseMonthDateTotalsAsPrimaryTotal() {
		return getData().getBoolean("UseMonthDateTotalsAsPrimaryTotal", false);
	}

	public boolean getUUIDLookup() {
		return getData().getBoolean("UUIDLookup", true);
	}

	public int getVoteCacheTime() {
		return getData().getInt("VoteCacheTime", -1);
	}

	public String getVotePartyBroadcast() {
		return getData().getString("VoteParty.Broadcast", "");
	}

	public List<String> getVotePartyBungeeCommands() {
		return getData().getStringList("VoteParty.BungeeCommands");
	}

	public boolean getVotePartyEnabled() {
		return getData().getBoolean("VoteParty.Enabled", false);
	}

	public int getVotePartyIncreaseVotesRequired() {
		return getData().getInt("VoteParty.IncreaseVotesRequired", 0);
	}

	public boolean getVotePartySendToAllServers() {
		return getData().getBoolean("VoteParty.SendToAllServers", true);
	}

	public List<String> getVotePartyServersToSend() {
		return getData().getStringList("VoteParty.ServersToSend");
	}

	public int getVotePartyVotesRequired() {
		return getData().getInt("VoteParty.VotesRequired", 100);
	}

	public boolean getWaitForUserOnline() {
		return getData().getBoolean("WaitForUserOnline", false);
	}

	public List<String> getWhiteListedServers() {
		return getData().getStringList("WhiteListedServers");
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

}
