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
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VotingPluginProxyTestImpl extends VotingPluginProxy {
	private final List<String> warnings = new ArrayList<>();
	private VotingPluginProxyConfig config;
	private boolean pluginMessageDeliveryResult = true;
	private boolean playerOnline = true;
	private ScheduledExecutorService scheduler;

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
	public Set<String> getAllConfiguredServers() {
		return getAllAvailableServers();
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
	public String getProxyPlatform() {
		return "VELOCITY";
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
	public void reloadControlConfiguration() throws Exception {
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

	public void handleLoginMessageForTest(JsonEnvelope envelope) {
		handleLoginMessage(envelope);
	}

	public void setGlobalMessageProxyHandlerForTest(GlobalMessageProxyHandler handler) {
		try {
			java.lang.reflect.Field field = VotingPluginProxy.class.getDeclaredField("globalMessageProxyHandler");
			field.setAccessible(true);
			field.set(this, handler);
		} catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	public void retryPendingPresenceHandoffsForTest(long now) {
		retryPendingPresenceHandoffs(now);
	}

	public int getPendingPresenceHandoffCountForTest() {
		return getPendingPresenceHandoffCount();
	}

	public void scheduleBackendPresenceStartupResyncForTest() {
		scheduleBackendPresenceStartupResync();
	}

	public void setSchedulerForTest(ScheduledExecutorService scheduler) {
		this.scheduler = scheduler;
	}

	public void retryPendingOnlineBroadcastsForTest(String server) {
		retryPendingOnlineBroadcasts(server);
	}

	public void retryPendingTimeBroadcastsForTest(String server) {
		retryPendingTimeBroadcasts(server);
	}

	public int[] getProjectedVotePartyStateForTest(int acceptedVotes) {
		return getProjectedVotePartyState(acceptedVotes);
	}

	public boolean persistTimeVoteDeliveryForTest(VoteTimeQueue vote) {
		return persistTimeVoteDelivery(vote);
	}

	public boolean persistServerVoteDeliveryForTest(String server, OfflineBungeeVote vote) {
		return persistServerVoteDelivery(server, vote);
	}

	public boolean persistOnlineVoteDeliveryForTest(String uuid, OfflineBungeeVote vote) {
		return persistOnlineVoteDelivery(uuid, vote);
	}

	public boolean canForwardStandaloneBroadcastForTest(boolean managesTotals) {
		return canForwardStandaloneBroadcast(managesTotals);
	}

	public boolean isPlayerOnlineForVoteRoutingForTest(String player) {
		return isPlayerOnlineForVoteRouting(player);
	}

	public String getCurrentPlayerServerForVoteRoutingForTest(String player) {
		return getCurrentPlayerServerForVoteRouting(player);
	}

	public boolean isSomeoneOnlineServerForVoteRoutingForTest(String server) {
		return isSomeoneOnlineServerForVoteRouting(server);
	}

	public void processDedicatedSnapshotLoginsForTest(String server, Set<UUID> handoffPlayers) {
		processDedicatedSnapshotLogins(server, handoffPlayers);
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
		return scheduler;
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
