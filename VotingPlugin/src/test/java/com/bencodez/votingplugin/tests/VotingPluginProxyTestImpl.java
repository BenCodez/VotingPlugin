package com.bencodez.votingplugin.tests;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Collection;
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
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
import com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VotingPluginProxyTestImpl extends VotingPluginProxy {
	private final List<String> warnings = new ArrayList<>();
	private VotingPluginProxyConfig config;
	private boolean pluginMessageDeliveryResult = true;
	private boolean voteEnvelopeDeliveryResult = true;
	private Boolean stableHttpDeliveryResult;
	private final java.util.ArrayDeque<Boolean> stableHttpDeliveryResults = new java.util.ArrayDeque<>();
	private JsonEnvelope lastVoteEnvelope;
	private boolean communicationTestDeliveryResult = true;
	private JsonEnvelope lastCommunicationTestEnvelope;
	private boolean playerOnline = true;
	private Set<String> availableServers = new HashSet<>(Arrays.asList("Server1", "Server2"));
	private Boolean standaloneBroadcastForwarding;
	private ScheduledExecutorService scheduler;
	private boolean failNextVoteCacheSave;
	private final java.util.Map<String, java.util.Set<String>> pendingVotePartyRewards = new HashMap<>();
	private final List<String> attemptedVotePartyDeliveryIds = new ArrayList<>();
	private boolean failNextGeneratedHttpSend;
	private boolean failNextStableHttpSend;
	private String generatedHttpRetryId;
	private PendingVotePartyProxyEffects pendingVotePartyProxyEffects = PendingVotePartyProxyEffects.empty();
	private PendingVotePartyProxyEffects quarantinedVotePartyProxyEffects = PendingVotePartyProxyEffects.empty();
	private final List<String> broadcasts = new ArrayList<>();
	private final List<String> consoleCommands = new ArrayList<>();
	private boolean failNextBroadcast;
	private String failConsoleCommand;
	private boolean failSaveAfterNextBroadcast;
	private java.util.concurrent.CompletableFuture<Void> nextVotePartyCommandCompletion;
	private boolean declineNextVotePartyCommand;
	private Runnable votePartyProxyCommandTimeout;
	private Boolean pendingHttpTransportDeliveries;
	private volatile int reloadCoreCalls;
	private File dataFolder = new File(".");

	public List<String> getWarnings() {
		return warnings;
	}

	public void setPendingHttpTransportDeliveries(Boolean pending) {
		pendingHttpTransportDeliveries = pending;
	}

	@Override
	protected boolean httpTransportHasPendingDeliveries(HttpProxyTransportServer transport) {
		return pendingHttpTransportDeliveries != null ? pendingHttpTransportDeliveries
				: super.httpTransportHasPendingDeliveries(transport);
	}

	@Override
	public void addNonVotedPlayer(String uuid, String playerName) {
		// Implementation for testing purposes
	}

	@Override
	public void broadcast(String message) {
		if (failNextBroadcast) {
			failNextBroadcast = false;
			throw new IllegalStateException("broadcast failed");
		}
		broadcasts.add(message);
		if (failSaveAfterNextBroadcast) {
			failSaveAfterNextBroadcast = false;
			failNextVoteCacheSave = true;
		}
	}

	@Override
	public Set<String> getAllAvailableServers() {
		return new HashSet<>(availableServers);
	}

	public void setAvailableServers(String... servers) {
		availableServers = new HashSet<>(Arrays.asList(servers));
	}

	public void setStandaloneBroadcastForwarding(Boolean enabled) {
		standaloneBroadcastForwarding = enabled;
	}

	@Override
	protected boolean canForwardStandaloneBroadcast(boolean managesTotals) {
		return standaloneBroadcastForwarding == null
				? super.canForwardStandaloneBroadcast(managesTotals) : standaloneBroadcastForwarding.booleanValue();
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
		return dataFolder;
	}

	public void setDataFolder(File dataFolder) {
		this.dataFolder = dataFolder;
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
	public Collection<String> getVoteCachePendingVotePartyServers() {
		return new HashSet<>(pendingVotePartyRewards.keySet());
	}

	@Override
	public Collection<String> getVoteCachePendingVotePartyRewardIds(String server) {
		return new HashSet<>(pendingVotePartyRewards.getOrDefault(server.toLowerCase(java.util.Locale.ROOT),
				java.util.Set.of()));
	}

	@Override
	public PendingVotePartyProxyEffects getVoteCachePendingVotePartyProxyEffects() {
		return pendingVotePartyProxyEffects;
	}

	@Override
	public PendingVotePartyProxyEffects getVoteCacheQuarantinedVotePartyProxyEffects() {
		return quarantinedVotePartyProxyEffects;
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
		if (command.equals(failConsoleCommand)) {
			failConsoleCommand = null;
			throw new IllegalStateException("command failed");
		}
		consoleCommands.add(command);
	}

	@Override
	protected java.util.concurrent.CompletableFuture<Void> runVotePartyConsoleCommand(String command) {
		java.util.concurrent.CompletableFuture<Void> completion = nextVotePartyCommandCompletion;
		nextVotePartyCommandCompletion = null;
		if (declineNextVotePartyCommand) {
			declineNextVotePartyCommand = false;
			return java.util.concurrent.CompletableFuture.failedFuture(
					new IllegalStateException("proxy declined the vote-party command"));
		}
		runConsoleCommand(command);
		return completion == null ? java.util.concurrent.CompletableFuture.completedFuture(null) : completion;
	}

	@Override
	protected void scheduleVotePartyProxyCommandTimeout(Runnable timeout) {
		votePartyProxyCommandTimeout = timeout;
	}

	@Override
	protected void awaitInFlightVotePartyProxyCommand() {
		// Keep shutdown-path tests deterministic; production performs the bounded wait.
	}

	@Override
	public void saveVoteCacheFile() {
		if (failNextVoteCacheSave) {
			failNextVoteCacheSave = false;
			throw new IllegalStateException("vote cache save failed");
		}
	}

	@Override
	public void saveVotePartyStateDurably() {
		saveVoteCacheFile();
	}

	public void failNextVoteCacheSave() {
		failNextVoteCacheSave = true;
	}

	public void failNextBroadcast() {
		failNextBroadcast = true;
	}

	public void failConsoleCommand(String command) {
		failConsoleCommand = command;
	}

	public void declineNextVotePartyCommand() {
		declineNextVotePartyCommand = true;
	}

	public void failSaveAfterNextBroadcast() {
		failSaveAfterNextBroadcast = true;
	}

	public List<String> getBroadcasts() {
		return List.copyOf(broadcasts);
	}

	public List<String> getConsoleCommands() {
		return List.copyOf(consoleCommands);
	}

	public java.util.concurrent.CompletableFuture<Void> delayNextVotePartyCommandCompletion() {
		nextVotePartyCommandCompletion = new java.util.concurrent.CompletableFuture<>();
		return nextVotePartyCommandCompletion;
	}

	public void runVotePartyProxyCommandTimeoutForTest() {
		Runnable timeout = votePartyProxyCommandTimeout;
		votePartyProxyCommandTimeout = null;
		if (timeout != null) timeout.run();
	}

	@Override
	public void reloadCore(boolean mysql) {
		reloadCoreCalls++;
	}

	public int getReloadCoreCalls() {
		return reloadCoreCalls;
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

	@Override
	protected boolean sendVoteEnvelopeAccepted(String server, int delay, JsonEnvelope envelope) {
		lastVoteEnvelope = envelope;
		if (getMethod() == com.bencodez.votingplugin.proxy.BungeeMethod.HTTP) {
			return voteEnvelopeDeliveryResult;
		}
		return super.sendVoteEnvelopeAccepted(server, delay, envelope);
	}

	public void setVoteEnvelopeDeliveryResult(boolean voteEnvelopeDeliveryResult) {
		this.voteEnvelopeDeliveryResult = voteEnvelopeDeliveryResult;
	}

	@Override
	protected boolean sendHttpEnvelope(String server, JsonEnvelope envelope) {
		lastVoteEnvelope = envelope;
		if (failNextGeneratedHttpSend) {
			failNextGeneratedHttpSend = false;
			try {
				java.lang.reflect.Constructor<HttpProxyTransportServer.DeliveryRetryException> constructor =
						HttpProxyTransportServer.DeliveryRetryException.class
								.getDeclaredConstructor(String.class, Throwable.class);
				constructor.setAccessible(true);
				throw constructor.newInstance(generatedHttpRetryId, new IllegalStateException("publication ambiguous"));
			} catch (ReflectiveOperationException failure) {
				throw new AssertionError(failure);
			}
		}
		return voteEnvelopeDeliveryResult;
	}

	@Override
	protected boolean sendHttpEnvelope(String server, String deliveryId, JsonEnvelope envelope) {
		lastVoteEnvelope = envelope;
		attemptedVotePartyDeliveryIds.add(deliveryId);
		if (failNextStableHttpSend) {
			failNextStableHttpSend = false;
			throw deliveryRetryException(generatedHttpRetryId);
		}
		if (!stableHttpDeliveryResults.isEmpty()) return stableHttpDeliveryResults.removeFirst();
		return stableHttpDeliveryResult == null ? voteEnvelopeDeliveryResult : stableHttpDeliveryResult;
	}

	public void setStableHttpDeliveryResult(Boolean stableHttpDeliveryResult) {
		this.stableHttpDeliveryResult = stableHttpDeliveryResult;
	}

	public void setStableHttpDeliveryResults(Boolean... results) {
		stableHttpDeliveryResults.clear();
		java.util.Collections.addAll(stableHttpDeliveryResults, results);
	}

	public JsonEnvelope getLastVoteEnvelope() {
		return lastVoteEnvelope;
	}

	public List<String> getAttemptedVotePartyDeliveryIds() {
		return attemptedVotePartyDeliveryIds;
	}

	public void failNextGeneratedHttpSend(String deliveryId) {
		failNextGeneratedHttpSend = true;
		generatedHttpRetryId = deliveryId;
	}

	public void failNextStableHttpSend(String deliveryId) {
		failNextStableHttpSend = true;
		generatedHttpRetryId = deliveryId;
	}

	private static RuntimeException deliveryRetryException(String deliveryId) {
		try {
			java.lang.reflect.Constructor<HttpProxyTransportServer.DeliveryRetryException> constructor =
					HttpProxyTransportServer.DeliveryRetryException.class
							.getDeclaredConstructor(String.class, Throwable.class);
			constructor.setAccessible(true);
			return constructor.newInstance(deliveryId, new IllegalStateException("publication ambiguous"));
		} catch (ReflectiveOperationException failure) {
			throw new AssertionError(failure);
		}
	}

	public boolean sendHttpVoteEnvelopeWithRecoveryForTest(String server, JsonEnvelope envelope) {
		return sendHttpEnvelopeWithRecovery(server, envelope);
	}

	public boolean sendGenericHttpEnvelopeForTest(String server, JsonEnvelope envelope) {
		return sendGenericHttpEnvelope(server, envelope);
	}

	public boolean sendStableHttpEnvelopeForTest(String server, String deliveryId, JsonEnvelope envelope) {
		return sendStableHttpEnvelope(server, deliveryId, envelope);
	}

	public boolean sendHttpVoteEnvelopeWithRecoveryForTest(String server, JsonEnvelope envelope,
			OfflineBungeeVote cachedVote) {
		return sendHttpEnvelopeWithRecovery(server, envelope, cachedVote);
	}

	public boolean sendHttpBroadcastEnvelopeWithRecoveryForTest(String server, JsonEnvelope envelope,
			OfflineBungeeVote cachedVote) {
		return sendHttpBroadcastEnvelopeWithRecovery(server, envelope, cachedVote);
	}

	public void acknowledgeVotePartyDeliveryForTest(String server, String deliveryId) throws java.io.IOException {
		acknowledgeVotePartyDelivery(server, deliveryId);
	}

	public void acknowledgeHttpDeliveryForTest(String server, String deliveryId) throws java.io.IOException {
		acknowledgeHttpDelivery(server, deliveryId);
	}

	@Override
	protected boolean sendCommunicationTestEnvelopeNow(String server, JsonEnvelope envelope) {
		lastCommunicationTestEnvelope = envelope;
		return communicationTestDeliveryResult;
	}

	public void setCommunicationTestDeliveryResult(boolean communicationTestDeliveryResult) {
		this.communicationTestDeliveryResult = communicationTestDeliveryResult;
	}

	public JsonEnvelope getLastCommunicationTestEnvelope() {
		return lastCommunicationTestEnvelope;
	}

	public void handleLoginMessageForTest(JsonEnvelope envelope) {
		handleLoginMessage(envelope);
	}

	public void handleHttpTransportEnvelopeForTest(HttpProxyTransportServer.ReceivedEnvelope received) {
		handleHttpTransportEnvelope(received);
	}

	public void handleStatusOkayForTest(JsonEnvelope envelope) {
		handleStatusOkay(envelope);
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

	public boolean persistUncachedStandaloneBroadcastForTest(String uuid, OfflineBungeeVote state,
			boolean alreadyPersisted) {
		return persistUncachedStandaloneBroadcast(uuid, state, alreadyPersisted);
	}

	public boolean persistAndSendStandaloneBroadcastForTest(String uuid, OfflineBungeeVote state,
			java.util.Set<String> remainingTargets, java.util.Set<String> forwardedServers) {
		return persistAndSendStandaloneBroadcast(uuid, state, remainingTargets, forwardedServers);
	}

	public OfflineBungeeVote createCachedRewardVoteForTest(UUID voteId, String player, String uuid, String service,
			long time, boolean realVote, String text, boolean standaloneProxyBroadcast) {
		return createCachedRewardVote(voteId, player, uuid, service, time, realVote, text, standaloneProxyBroadcast);
	}

	public void retryPendingTimeBroadcastsForTest(String server) {
		retryPendingTimeBroadcasts(server);
	}

	public void retryPendingTimeBroadcastsForTest() {
		retryPendingTimeBroadcasts();
	}

	public void retryPendingVotePartyRewardsForTest() {
		retryPendingVotePartyRewards();
	}

	public boolean retryPendingVotePartyProxyEffectsForTest() {
		return retryPendingVotePartyProxyEffects();
	}

	public boolean quarantineInFlightVotePartyProxyCommandForReplacementForTest() {
		return quarantineInFlightVotePartyProxyCommandForReplacement();
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
	public void setVoteCachePendingVotePartyReward(String server, String deliveryId, boolean pending) {
		server = server.toLowerCase(java.util.Locale.ROOT);
		if (pending) pendingVotePartyRewards.computeIfAbsent(server, ignored -> new HashSet<>()).add(deliveryId);
		else {
			java.util.Set<String> rewards = pendingVotePartyRewards.get(server);
			if (rewards != null && rewards.remove(deliveryId) && rewards.isEmpty()) pendingVotePartyRewards.remove(server);
		}
	}

	@Override
	public void setVoteCachePendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		pendingVotePartyProxyEffects = effects;
	}

	@Override
	public void setVoteCacheQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		quarantinedVotePartyProxyEffects = effects;
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
