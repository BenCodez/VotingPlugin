package com.bencodez.votingplugin.bungee.proxy;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public interface VotingPluginProxyConfig {

	public boolean getAllowUnJoined();

	public String getBedrockPlayerPrefix();

	public List<String> getBlockedServers();

	public boolean getBroadcast();

	public String getBungeeHost();

	public boolean getBungeeManageTotals();

	public String getBungeeMethod();

	public int getBungeePort();

	public boolean getDebug();

	public String getFallBack();

	public boolean getGlobalDataEnabled();

	public boolean getGlobalDataUseMainMySQL();

	public int getLimitVotePoints();

	public int getMaxAmountOfVotesPerDay();

	public String getMultiProxyMethod();

	public boolean getMultiProxyOneGlobalReward();

	public String getTimeZone();

	public int getTimeHourOffSet();

	public String getMultiProxyRedisHost();

	public String getMultiProxyRedisPassword();

	public int getMultiProxyRedisPort();

	public boolean getMultiProxyRedisUseExistingConnection();

	public String getMultiProxyRedisUsername();

	public Collection<String> getMultiProxyServers();

	public Map<String, Object> getMultiProxyServersConfiguration(String s);

	public String getMultiProxySocketHostHost();

	public int getMultiProxySocketHostPort();

	public boolean getMultiProxySupport();

	public boolean getOnlineMode();

	public int getPointsOnVote();

	public boolean getPrimaryServer();

	public String getProxyServerName();

	public List<String> getProxyServers();

	public String getRedisHost();

	public String getRedisPassword();

	public int getRedisPort();

	public String getRedisPrefix();

	public String getRedisUsername();

	public boolean getSendVotesToAllServers();

	public Map<String, Object> getSpigotServerConfiguration(String s);

	public Collection<String> getSpigotServers();

	public boolean getStoreMonthTotalsWithDate();

	public boolean getTimeChangeFailSafeBypass();

	public boolean getUseMonthDateTotalsAsPrimaryTotal();

	public boolean getUUIDLookup();

	public int getVoteCacheTime();

	public String getVotePartyBroadcast();

	public List<String> getVotePartyBungeeCommands();

	public boolean getVotePartyEnabled();

	public int getVotePartyIncreaseVotesRequired();

	public boolean getVotePartySendToAllServers();

	public List<String> getVotePartyServersToSend();

	public int getVotePartyVotesRequired();

	public boolean getWaitForUserOnline();

	public List<String> getWhiteListedServers();

	public void load();

	public void save();

}
