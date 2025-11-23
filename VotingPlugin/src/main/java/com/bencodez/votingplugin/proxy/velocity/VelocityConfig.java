package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.checkerframework.checker.nullness.qual.NonNull;

import com.bencodez.simpleapi.file.velocity.VelocityYMLFile;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

import ninja.leaping.configurate.ConfigurationNode;

public class VelocityConfig extends VelocityYMLFile implements VotingPluginProxyConfig {

	public VelocityConfig(File file) {
		super(file);
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
	public boolean getBroadcast() {
		return getBoolean(getNode("Broadcast"), false);
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

	public List<String> getChildrenAsList(ConfigurationNode config) {
		List<String> children = new ArrayList<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.getChildrenMap().entrySet()) {
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

	public Map<String, Object> configToMap(ConfigurationNode config) {
		Map<String, Object> map = new HashMap<>();
		if (config != null) {
			for (Map.Entry<Object, ? extends ConfigurationNode> entry : config.getChildrenMap().entrySet()) {
				map.put(entry.getKey().toString(), entry.getValue().getValue());
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
		return getString(getVotePartyNode().getNode("Broadcast"), "");
	}

	@Override
	public List<String> getVotePartyBungeeCommands() {
		return getStringList(getVotePartyNode().getNode("BungeeCommands"), new ArrayList<>());
	}

	@Override
	public boolean getVotePartyEnabled() {
		return getBoolean(getVotePartyNode().getNode("Enabled"), false);
	}

	@Override
	public int getVotePartyIncreaseVotesRequired() {
		return getInt(getVotePartyNode().getNode("IncreaseVotesRequired"), 0);
	}

	public ConfigurationNode getVotePartyNode() {
		return getNode("VoteParty");
	}

	@Override
	public boolean getVotePartySendToAllServers() {
		return getBoolean(getVotePartyNode().getNode("SendToAllServers"), false);
	}

	@Override
	public List<String> getVotePartyServersToSend() {
		return getStringList(getVotePartyNode().getNode("ServersToSend"), new ArrayList<>());
	}

	@Override
	public int getVotePartyVotesRequired() {
		return getInt(getVotePartyNode().getNode("VotesRequired"), 100);
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
	public boolean getResetMilestonesMonthly() {
		return getBoolean(getNode("ResetMilestonesMonthly"), false);
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

}
