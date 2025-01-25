package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.checkerframework.checker.nullness.qual.NonNull;

import com.bencodez.simpleapi.file.velocity.VelocityYMLFile;
import com.bencodez.votingplugin.bungee.proxy.VotingPluginProxyConfig;

import ninja.leaping.configurate.ConfigurationNode;

public class Config extends VelocityYMLFile implements VotingPluginProxyConfig {

	public Config(File file) {
		super(file);
	}

	public boolean getAllowUnJoined() {
		return getBoolean(getNode("AllowUnJoined"), false);
	}

	public String getBedrockPlayerPrefix() {
		return getString(getNode("BedrockPlayerPrefix"), ".");
	}

	public List<String> getBlockedServers() {
		return getStringList(getNode("BlockedServers"), new ArrayList<>());
	}

	public boolean getBroadcast() {
		return getBoolean(getNode("Broadcast"), false);
	}

	public String getBungeeHost() {
		return getString(getNode("BungeeServer", "Host"), "");
	}

	public boolean getBungeeManageTotals() {
		return getBoolean(getNode("BungeeManageTotals"), true);
	}

	public String getBungeeMethod() {
		return getString(getNode("BungeeMethod"), "PLUGINMESSAGING");
	}

	public int getBungeePort() {
		return getInt(getNode("BungeeServer", "Port"), 1297);
	}

	public boolean getDebug() {
		return getBoolean(getNode("Debug"), false);
	}

	public String getFallBack() {
		return getString(getNode("FallBackServer"), "");
	}

	public boolean getGlobalDataEnabled() {
		return getBoolean(getNode("GlobalData", "Enabled"), false);
	}

	public boolean getGlobalDataUseMainMySQL() {
		return getBoolean(getNode("GlobalData", "UseMainMySQL"), true);
	}

	public int getLimitVotePoints() {
		return getInt(getNode("LimitVotePoints"), -1);
	}

	public int getMaxAmountOfVotesPerDay() {
		return getInt(getNode("MaxAmountOfVotesPerDay"), -1);
	}

	public String getMultiProxyMethod() {
		return getString(getNode("MultiProxyMethod"), "SOCKET");
	}

	public boolean getMultiProxyOneGlobalReward() {
		return getBoolean(getNode("MultiProxyOneGlobalReward"), false);
	}

	public String getMultiProxyRedisHost() {
		return getString(getNode("MultiProxyRedis", "Host"), "");
	}

	public String getMultiProxyRedisPassword() {
		return getString(getNode("MultiProxyRedis", "Password"), "");
	}

	public int getMultiProxyRedisPort() {
		return getInt(getNode("MultiProxyRedis", "Port"), 6379);
	}

	public boolean getMultiProxyRedisUseExistingConnection() {
		return getBoolean(getNode("MultiProxyRedis", "UseExistingConnection"), false);
	}

	public String getMultiProxyRedisUsername() {
		return getString(getNode("MultiProxyRedis", "Username"), "");
	}

	public @NonNull List<String> getMultiProxyServers() {
		return getChildrenAsList(getNode("MultiProxyServers"));
	}

	public List<String> getChildrenAsList(ConfigurationNode config) {
		List<String> children = new ArrayList<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.getChildrenMap().entrySet()) {
				children.add(entry.getKey().toString());
			}
		}
		return children;
	}

	public Map<String, Object> getMultiProxyServersConfiguration(String s) {
		return configToMap(getNode("MultiProxyServers", s));
	}

	public String getMultiProxySocketHostHost() {
		return getString(getNode("MultiProxySocketHost", "Host"), "");
	}

	public int getMultiProxySocketHostPort() {
		return getInt(getNode("MultiProxySocketHost", "Port"), 1297);
	}

	public boolean getMultiProxySupport() {
		return getBoolean(getNode("MultiProxySupport"), false);
	}

	public ConfigurationNode getMysqlNode() {
		return getNode("MySQL");
	}

	public boolean getOnlineMode() {
		return getBoolean(getNode("OnlineMode"), true);
	}

	public int getPointsOnVote() {
		return getInt(getNode("PointsOnVote"), 1);
	}

	public boolean getPrimaryServer() {
		return getBoolean(getNode("PrimaryServer"), false);
	}

	public String getProxyServerName() {
		return getString(getNode("ProxyServerName"), "SOCKET");
	}

	public List<String> getProxyServers() {
		return getStringList(getNode("ProxyServers"), new ArrayList<>());
	}

	public String getRedisHost() {
		return getString(getNode("Redis", "Host"), "");
	}

	public String getRedisPassword() {
		return getString(getNode("Redis", "Password"), "");
	}

	public int getRedisPort() {
		return getInt(getNode("Redis", "Port"), 6379);
	}

	public String getRedisPrefix() {
		return getString(getNode("Redis", "Prefix"), "");
	}

	public String getRedisUsername() {
		return getString(getNode("Redis", "Username"), "");
	}

	public boolean getSendVotesToAllServers() {
		return getBoolean(getNode("SendVotesToAllServers"), true);
	}

	public Map<String, Object> getSpigotServerConfiguration(String s) {
		return configToMap(getNode("SpigotServers", s));
	}

	public Map<String, Object> configToMap(ConfigurationNode config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.getChildrenMap().entrySet()) {
				map.put(entry.getKey().toString(), entry.getValue().getValue());
			}
		}
		return map;
	}

	public @NonNull List<String> getSpigotServers() {
		return getChildrenAsList(getNode("SpigotServers"));
	}

	public boolean getStoreMonthTotalsWithDate() {
		return getBoolean(getNode("StoreMonthTotalsWithDate"), false);
	}

	public boolean getTimeChangeFailSafeBypass() {
		return getBoolean(getNode("TimeChangeFailSafeBypass"), false);
	}

	public boolean getUseMonthDateTotalsAsPrimaryTotal() {
		return getBoolean(getNode("UseMonthDateTotalsAsPrimaryTotal"), false);
	}

	public boolean getUUIDLookup() {
		return getBoolean(getNode("UUIDLookup"), true);
	}

	public int getVoteCacheTime() {
		return getInt(getNode("VoteCacheTime"), -1);
	}

	public String getVotePartyBroadcast() {
		return getString(getVotePartyNode().getNode("Broadcast"), "");
	}

	public List<String> getVotePartyBungeeCommands() {
		return getStringList(getVotePartyNode().getNode("BungeeCommands"), new ArrayList<>());
	}

	public boolean getVotePartyEnabled() {
		return getBoolean(getVotePartyNode().getNode("Enabled"), false);
	}

	public int getVotePartyIncreaseVotesRequired() {
		return getInt(getVotePartyNode().getNode("IncreaseVotesRequired"), 0);
	}

	public ConfigurationNode getVotePartyNode() {
		return getNode("VoteParty");
	}

	public boolean getVotePartySendToAllServers() {
		return getBoolean(getVotePartyNode().getNode("SendToAllServers"), false);
	}

	public List<String> getVotePartyServersToSend() {
		return getStringList(getVotePartyNode().getNode("ServersToSend"), new ArrayList<>());
	}

	public int getVotePartyVotesRequired() {
		return getInt(getVotePartyNode().getNode("VotesRequired"), 100);
	}

	public boolean getWaitForUserOnline() {
		return getBoolean(getNode("WaitForUserOnline"), false);
	}

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

}
