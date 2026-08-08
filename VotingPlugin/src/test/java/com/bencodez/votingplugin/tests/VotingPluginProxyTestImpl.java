package com.bencodez.votingplugin.tests;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;

import org.mockito.Mockito;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;

public class VotingPluginProxyTestImpl extends VotingPluginProxy {
	private final List<String> warnings = new ArrayList<>();
	private VotingPluginProxyConfig config;
	private boolean pluginMessageDeliveryResult = true;
	private boolean playerOnline = true;

	public List<String> getWarnings() {
		return warnings;
	}

	@Override
	public void addNonVotedPlayer(String uuid, String playerName) {
		// Implementation for testing purposes
	}

	@Override
	public void broadcast(String message) {
		// Implementation for testing purposes
	}

	@Override
	public Set<String> getAllAvailableServers() {
		return new HashSet<>(Arrays.asList("Server1", "Server2"));
	}

	@Override
	public VotingPluginProxyConfig getConfig() {
		if (config == null) {
			config = Mockito.mock(VotingPluginProxyConfig.class);
			Mockito.when(config.getPluginMessageEncryption()).thenReturn(false);
			Mockito.when(config.getPluginMessageChannel()).thenReturn("votingplugin:main");
			Mockito.when(config.getDebug()).thenReturn(false);
		}
		return config;
	}

	@Override
	public String getCurrentPlayerServer(String player) {
		return "Server1";
	}

	@Override
	public File getDataFolderPlugin() {
		return new File(".");
	}

	@Override
	public String getProperName(String uuid, String playerName) {
		return playerName;
	}

	@Override
	public String getUUID(String playerName) {
		return UUID.randomUUID().toString();
	}

	@Override
	public String getPluginVersion() {
		return "1.0.0";
	}

	@Override
	public int getVoteCacheCurrentVotePartyVotes() {
		return 0;
	}

	@Override
	public long getVoteCacheLastUpdated() {
		return System.currentTimeMillis();
	}

	@Override
	public int getVoteCachePrevDay() {
		return 1;
	}

	@Override
	public String getVoteCachePrevMonth() {
		return "January-2023";
	}

	@Override
	public int getVoteCachePrevWeek() {
		return 1;
	}

	@Override
	public int getVoteCacheVotePartyIncreaseVotesRequired() {
		return 10;
	}

	@Override
	public boolean isPlayerOnline(String playerName) {
		return playerOnline;
	}

	public void setPlayerOnline(boolean playerOnline) {
		this.playerOnline = playerOnline;
	}

	@Override
	public boolean isServerValid(String server) {
		return true;
	}

	@Override
	public boolean isSomeoneOnlineServer(String server) {
		return true;
	}

	@Override
	public boolean isVoteCacheIgnoreTime() {
		return false;
	}

	@Override
	public void runAsync(Runnable run) {
		run.run();
	}

	@Override
	public void runConsoleCommand(String command) {
		// Mocked for testing
	}

	@Override
	public void saveVoteCacheFile() {
		// Mocked for testing
	}

	@Override
	public void reloadCore(boolean mysql) {
		// Mocked for testing
	}

	@Override
	public void log(String message) {
		// For testing, simply print the message
		System.out.println("LOG: " + message);
	}

	@Override
	public void logSevere(String message) {
		// For testing, simply print the severe message
		System.err.println("SEVERE: " + message);
	}

	@Override
	public void warn(String message) {
		warnings.add(message);
	}

	@Override
	public void debug(String str) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean sendPluginMessageData(String server, String channel, byte[] data, boolean queue) {
		return pluginMessageDeliveryResult;
	}

	public void setPluginMessageDeliveryResult(boolean pluginMessageDeliveryResult) {
		this.pluginMessageDeliveryResult = pluginMessageDeliveryResult;
	}

	public boolean sendPluginMessageImmediately(String server, JsonEnvelope envelope) {
		return sendPluginMessageServerNow(server, envelope);
	}

	public boolean sendProxyBroadcastImmediately(String server, JsonEnvelope envelope) {
		return sendProxyBroadcastEnvelopeNow(server, envelope);
	}

	public void retryPendingOnlineBroadcastsForTest(String server) {
		retryPendingOnlineBroadcasts(server);
	}

	@Override
	public void setVoteCacheLastUpdated() {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCachePrevDay(int day) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCachePrevMonth(String text) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCachePrevWeek(int week) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCacheVoteCacheIgnoreTime(boolean ignore) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCacheVotePartyCurrentVotes(int votes) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setVoteCacheVotePartyIncreaseVotesRequired(int votes) {
		// TODO Auto-generated method stub

	}

	@Override
	public ScheduledExecutorService getScheduler() {
		return null;
	}

	@Override
	public MysqlConfig getVoteCacheMySQLConfig() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MysqlConfig getNonVotedCacheMySQLConfig() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MysqlConfig getVoteLoggingMySQLConfig() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void loadTaskTimer(Runnable runnable, long delaySeconds, long repeatSeconds) {
		// TODO Auto-generated method stub
		
	}
}
