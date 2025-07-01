package com.bencodez.votingplugin.proxy;

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

	public String getPluginMessageChannel();

	public boolean getPluginMessageEncryption();

	public boolean getDebug();

	public String getMqttClientID();

	public String getMqttBrokerURL();

	public String getMqttUsername();

	public String getMqttPassword();

	public String getMqttPrefix();

	public String getFallBack();

	public boolean getGlobalDataEnabled();

	public boolean getGlobalDataUseMainMySQL();

	public int getLimitVotePoints();

	public int getMaxAmountOfVotesPerDay();

	public String getMultiProxyMethod();

	public boolean getMultiProxyOneGlobalReward();

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

	public int getTimeHourOffSet();

	public String getTimeZone();

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

	public Collection<String> getWaitUntilVoteDelaySites();

	public String getWaitUntilVoteDelayService(String site);

	public int getWaitUntilVoteDelayVoteDelay(String site);

	public int getWaitUntilVoteDelayVoteDelayMin(String site);
	
	public boolean getResetMilestonesMonthly();

	public int getWaitUntilVoteDelayVoteDelayHour(String site);

	public boolean getWaitUntilVoteDelayVoteDelayDaily(String site);

	public void load();

	public void save();

}
