package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.HashMap;
import java.util.UUID;

import org.bukkit.configuration.file.FileConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

class VoteShopPurchaseServiceTest {
	@Test
	void unconfirmedRewardClaimIsCompensatedBeforeTheRewardCanStart() {
		assertTrue(VoteShopPurchaseService.requiresCompensation(
				SharedMysqlPurchaseJournal.ClaimOutcome.INDETERMINATE));
		assertTrue(VoteShopPurchaseService.requiresCompensation(
				SharedMysqlPurchaseJournal.ClaimOutcome.NOT_CLAIMED));
		assertFalse(VoteShopPurchaseService.requiresCompensation(
				SharedMysqlPurchaseJournal.ClaimOutcome.CLAIMED));
	}

	@Test
	void retainsSynchronousPurchaseDescriptorsForBinaryCompatibility() throws Exception {
		assertEquals(VoteShopPurchaseResult.class, VoteShopPurchaseService.class
				.getMethod("purchase", org.bukkit.entity.Player.class, VotingPluginUser.class, VoteShopItem.class)
				.getReturnType());
		assertEquals(VoteShopPurchaseResult.class, com.bencodez.votingplugin.voteshop.VoteShopManager.class
				.getMethod("purchase", org.bukkit.entity.Player.class, VotingPluginUser.class, VoteShopItem.class)
				.getReturnType());
	}

	@Test
	void limitGenerationUsesTheEarliestConfiguredResetBoundary() {
		LocalDateTime current = LocalDateTime.of(2026, 9, 8, 12, 0);
		long now = current.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		VoteShopPurchaseService.LimitGeneration generation = VoteShopPurchaseService.limitGeneration(
				current, now, true, true, true, 0);

		assertTrue(generation.value().contains("D:2026-09-08"));
		assertTrue(generation.value().contains("W:"));
		assertTrue(generation.value().contains("M:2026-9"));
		assertEquals(now + 43_200_000L, generation.expiresAt());
	}

	@Test
	void weeklyGenerationUsesANetworkWideCalendarConvention() {
		LocalDateTime saturday = LocalDateTime.of(2026, 9, 5, 12, 0);
		LocalDateTime sunday = saturday.plusDays(1);
		LocalDateTime monday = sunday.plusDays(1);
		assertEquals("W:2026-37", VoteShopPurchaseService.weeklyGenerationId(saturday, 0));
		assertEquals("W:2026-37", VoteShopPurchaseService.weeklyGenerationId(sunday, 0));
		assertEquals("W:2026-37", VoteShopPurchaseService.weeklyGenerationId(monday, 0));
		assertEquals("W:2026-38", VoteShopPurchaseService.weeklyGenerationId(saturday.plusWeeks(1), 0));
	}

	@Test
	void sharedMysqlResetUsesTheJournalEpochTransaction() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection schemaConnection = mock(Connection.class);
		Connection resetConnection = mock(Connection.class);
		PreparedStatement schema = mock(PreparedStatement.class);
		PreparedStatement generation = mock(PreparedStatement.class);
		PreparedStatement generationExpiry = mock(PreparedStatement.class);
		PreparedStatement epochColumn = mock(PreparedStatement.class);
		PreparedStatement epochTable = mock(PreparedStatement.class);
		PreparedStatement epochGeneration = mock(PreparedStatement.class);
		PreparedStatement index = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement wipe = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		UserDataCache initialCache = mock(UserDataCache.class);
		UserDataCache recreatedCache = mock(UserDataCache.class);
		UUID cachedUuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		HashMap<String, DataValue> initialValues = new HashMap<>();
		initialValues.put("VoteShopLimitdaily", mock(DataValue.class));
		HashMap<String, DataValue> recreatedValues = new HashMap<>();
		recreatedValues.put("VoteShopLimitdaily", mock(DataValue.class));
		recreatedValues.put("DailyTotal", mock(DataValue.class));
		var liveCaches = new java.util.concurrent.ConcurrentHashMap<UUID, UserDataCache>();
		liveCaches.put(cachedUuid, initialCache);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(schemaConnection, resetConnection);
		when(schemaConnection.prepareStatement(anyString())).thenReturn(schema, generation, generationExpiry,
				epochColumn, epochTable, epochGeneration, index);
		when(resetConnection.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, wipe, advance);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(11L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(advance.executeUpdate()).thenReturn(1);
		when(initialCache.getCache()).thenReturn(initialValues);
		when(recreatedCache.getCache()).thenReturn(recreatedValues);
		doAnswer(invocation -> {
			liveCaches.put(cachedUuid, recreatedCache);
			return 1;
		}).when(wipe).executeUpdate();
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(liveCaches);

		VoteShopPurchaseService.resetSharedMysqlLimit(plugin, "VoteShopLimitdaily");

		verify(table).checkColumn("VoteShopLimitdaily", com.bencodez.simpleapi.sql.DataType.INTEGER);
		verify(resetConnection).commit();
		ArgumentCaptor<String> sqlText = ArgumentCaptor.forClass(String.class);
		verify(resetConnection, times(4)).prepareStatement(sqlText.capture());
		assertTrue(sqlText.getAllValues().get(2).contains("`VoteShopLimitdaily` = 0"));
		verify(initialCache, never()).dump();
		assertFalse(initialValues.containsKey("VoteShopLimitdaily"));
		assertFalse(recreatedValues.containsKey("VoteShopLimitdaily"));
		assertTrue(recreatedValues.containsKey("DailyTotal"));
	}

	@Test
	void localPurchaseRefreshesCacheBeforeCheckingPointsWhenConfigured() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.FLAT);
		when(plugin.getConfigFile().isExtraVoteShopCheck()).thenReturn(true);
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getPermission()).thenReturn("");
		when(item.getCost()).thenReturn(10);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getPoints()).thenReturn(0);
		AtomicReference<VoteShopPurchaseResult> result = new AtomicReference<>();

		new VoteShopPurchaseService(plugin, definition).purchase(mock(org.bukkit.entity.Player.class), user, item,
				result::set);

		assertEquals(VoteShopPurchaseResult.NOT_ENOUGH_POINTS, result.get());
		InOrder refreshBeforeValidation = inOrder(user);
		refreshBeforeValidation.verify(user).cache();
		refreshBeforeValidation.verify(user).getPoints();
	}

	@Test
	void sharedMysqlGuiValidationDoesNotReadOrRefreshDynamicUserState() {
		VotingPluginMain plugin = sharedMysqlPlugin(mock(MySQL.class));
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getPermission()).thenReturn("");
		when(item.getLimit()).thenReturn(1);
		when(item.getIdentifier()).thenReturn("daily");
		when(item.getCost()).thenReturn(10);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopPurchaseService service = new VoteShopPurchaseService(plugin, definition);

		service.refreshUserForPurchaseValidation(user, true);
		VoteShopPurchaseResult result = service.validatePurchase(mock(org.bukkit.entity.Player.class), user, item);

		assertEquals(VoteShopPurchaseResult.SUCCESS, result);
		verify(user, never()).cache();
		verify(user, never()).getVoteShopIdentifierLimit(anyString());
		verify(user, never()).getPoints();
	}

	@Test
	void rejectedInitialSharedMysqlSubmissionCompletesAsFailed() {
		MySQL table = mock(MySQL.class);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException("saturated"))
				.when(persistenceExecutor).execute(any(Runnable.class));
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getPermission()).thenReturn("");
		AtomicReference<VoteShopPurchaseResult> result = new AtomicReference<>();

		new VoteShopPurchaseService(plugin, definition).purchase(player, purchaseUser(), item, result::set);

		assertEquals(VoteShopPurchaseResult.FAILED, result.get());
		verify(persistenceExecutor).execute(any(Runnable.class));
		verify(table, never()).getMysql();
	}

	@Test
	void sharedMysqlDebitIsRefundedWhenEntitySchedulerRetiresWithoutFallback() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection schemaConnection = mock(Connection.class);
		Connection pendingConnection = mock(Connection.class);
		Connection compensatingConnection = mock(Connection.class);
		Connection cleanupConnection = mock(Connection.class);
		Connection debitConnection = mock(Connection.class);
		Connection refundConnection = mock(Connection.class);
		PreparedStatement schema = mock(PreparedStatement.class);
		PreparedStatement schemaGeneration = mock(PreparedStatement.class);
		PreparedStatement schemaGenerationExpiry = mock(PreparedStatement.class);
		PreparedStatement schemaIndex = mock(PreparedStatement.class);
		PreparedStatement pending = mock(PreparedStatement.class);
		PreparedStatement compensating = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		PreparedStatement reserve = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement refundMark = mock(PreparedStatement.class);
		PreparedStatement refundSelect = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement refundUpdate = mock(PreparedStatement.class);
		ResultSet noPendingRows = emptyRows();
		ResultSet noCompensatingRows = emptyRows();
		ResultSet noTerminalRows = emptyRows();
		ResultSet pendingPurchase = purchaseRow("COMPENSATING", "Points", null, 10);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(schemaConnection, pendingConnection,
				compensatingConnection, cleanupConnection, debitConnection, refundConnection);
		when(schemaConnection.prepareStatement(anyString())).thenReturn(schema, schemaGeneration,
				schemaGenerationExpiry, schemaIndex);
		when(pendingConnection.prepareStatement(anyString())).thenReturn(pending);
		when(pending.executeQuery()).thenReturn(noPendingRows);
		when(compensatingConnection.prepareStatement(anyString())).thenReturn(compensating);
		when(compensating.executeQuery()).thenReturn(noCompensatingRows);
		when(cleanupConnection.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(noTerminalRows);
		when(debitConnection.prepareStatement(anyString())).thenReturn(reserve, debit);
		when(refundConnection.prepareStatement(anyString())).thenReturn(refundMark, refundSelect, refund, refundUpdate);
		when(refundSelect.executeQuery()).thenReturn(pendingPurchase);
		when(debit.executeUpdate()).thenReturn(1);
		when(refundMark.executeUpdate()).thenReturn(1);
		when(refund.executeUpdate()).thenReturn(1);
		when(refundUpdate.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation entityScheduler =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(plugin.isEnabled()).thenReturn(true);
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		when(definition.getTitle()).thenReturn("Vote Shop");
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		VotingPluginUser user = purchaseUser();
		AtomicInteger completions = new AtomicInteger();
		AtomicReference<VoteShopPurchaseResult> completionResult = new AtomicReference<>();

		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		when(entityScheduler.runAtEntityWithFallback(org.mockito.ArgumentMatchers.eq(player), any(),
				any(Runnable.class))).thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		new VoteShopPurchaseService(plugin, definition).purchase(player, user, item, result -> {
			completionResult.set(result);
			completions.incrementAndGet();
		});
		ArgumentCaptor<Runnable> work = ArgumentCaptor.forClass(Runnable.class);
		verify(persistenceExecutor).execute(work.capture());
		ExecutorService worker = Executors.newSingleThreadExecutor();
		Future<?> purchase = worker.submit(work.getValue());
		@SuppressWarnings("rawtypes")
		ArgumentCaptor<java.util.function.Consumer> scheduled = ArgumentCaptor.forClass(java.util.function.Consumer.class);
		ArgumentCaptor<Runnable> retirement = ArgumentCaptor.forClass(Runnable.class);
		verify(entityScheduler, org.mockito.Mockito.timeout(1000)).runAtEntityWithFallback(
					org.mockito.ArgumentMatchers.eq(player), scheduled.capture(), retirement.capture());
		purchase.get(5, TimeUnit.SECONDS);
		ArgumentCaptor<Runnable> compensation = ArgumentCaptor.forClass(Runnable.class);
		verify(persistenceExecutor, times(2)).execute(compensation.capture());
		compensation.getAllValues().get(1).run();

		ArgumentCaptor<String> refundSql = ArgumentCaptor.forClass(String.class);
		verify(refundConnection, times(4)).prepareStatement(refundSql.capture());
		assertTrue(refundSql.getAllValues().get(2).contains("`Points` = `Points` + ?"));
		verify(refund).setInt(1, 10);
		verify(refund, times(1)).executeUpdate();
		// Schema, stale cleanup, compensating cleanup, terminal cleanup, reservation,
		// and refund are the only database connections in the scheduler-retirement path. An eighth
		// checkout would be the reward claim and would make the debit unrecoverable.
		verify(sql.getConnectionManager(), times(7)).getConnection();
		verify(entityScheduler).runAtEntityWithFallback(
				org.mockito.ArgumentMatchers.eq(player), any(), any(Runnable.class));
		scheduled.getValue().accept(null);
		assertEquals(1, completions.get(), "a compensated purchase must complete exactly once");
		assertEquals(VoteShopPurchaseResult.FAILED, completionResult.get());
		verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), any(), any());
		worker.shutdownNow();
	}

	@Test
	void disabledShopResultStillSendsFeedback() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);

		new VoteShopPurchaseService(plugin, definition).sendFailureMessage(player, mock(VotingPluginUser.class), null,
				VoteShopPurchaseResult.SHOP_DISABLED);

		verify(player).sendMessage(anyString());
	}

	@Test
	void rejectedClaimedRewardQueuesDurableRefundAndFencesLateCallback() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation entityScheduler =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		RewardHandler rewardHandler = mock(RewardHandler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(plugin.getRewardHandler()).thenReturn(rewardHandler);
		@SuppressWarnings("rawtypes")
		ArgumentCaptor<java.util.function.Consumer> callback = ArgumentCaptor.forClass(java.util.function.Consumer.class);
		when(entityScheduler.runAtEntityWithFallback(any(), callback.capture(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		SharedMysqlPurchaseJournal journal = mock(SharedMysqlPurchaseJournal.class);
		when(journal.markCompensating("purchase-1")).thenReturn(true);
		when(journal.refundCompensatingReward("purchase-1")).thenReturn(false);
		VoteShopPurchaseService.SharedPurchaseDebit debit = new VoteShopPurchaseService.SharedPurchaseDebit(
				VoteShopPurchaseResult.SUCCESS, journal, "purchase-1", "Points", null);
		VoteShopPurchaseService service = new VoteShopPurchaseService(plugin, mock(VoteShopDefinition.class));
		VotingPluginUser user = mock(VotingPluginUser.class);

		service.scheduleClaimedReward(mock(org.bukkit.entity.Player.class), user, mock(VoteShopItem.class),
				new java.util.HashMap<>(), mock(FileConfiguration.class), ignored -> {}, debit);

		ArgumentCaptor<Runnable> refund = ArgumentCaptor.forClass(Runnable.class);
		verify(persistenceExecutor).execute(refund.capture());
		InOrder markerBeforeFallback = inOrder(journal, persistenceExecutor);
		markerBeforeFallback.verify(journal).markCompensating("purchase-1");
		markerBeforeFallback.verify(persistenceExecutor).execute(any(Runnable.class));
		refund.getValue().run();
		verify(journal).refundCompensatingReward("purchase-1");
		callback.getValue().accept(null);
		verify(rewardHandler, never()).giveReward(any(), any(), any(), any());
	}

	@Test
	void rejectedCompensationExecutorStillRunsTheDurableRefund() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation entityScheduler =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException("stopping"))
				.when(persistenceExecutor).execute(any(Runnable.class));
		SharedMysqlPurchaseJournal journal = mock(SharedMysqlPurchaseJournal.class);
		when(journal.markCompensating("purchase-1")).thenReturn(true);
		when(journal.refundCompensatingReward("purchase-1")).thenReturn(false);
		VoteShopPurchaseService.SharedPurchaseDebit debit = new VoteShopPurchaseService.SharedPurchaseDebit(
				VoteShopPurchaseResult.SUCCESS, journal, "purchase-1", "Points", null);

		new VoteShopPurchaseService(plugin, mock(VoteShopDefinition.class)).scheduleClaimedReward(
				mock(org.bukkit.entity.Player.class), mock(VotingPluginUser.class), mock(VoteShopItem.class),
				new java.util.HashMap<>(), mock(FileConfiguration.class), ignored -> {}, debit);

		ArgumentCaptor<Runnable> asyncRefund = ArgumentCaptor.forClass(Runnable.class);
		verify(scheduler).runTaskAsynchronously(eq(plugin), asyncRefund.capture());
		verify(journal, never()).refundUnstartedReward(anyString());
		asyncRefund.getValue().run();
		verify(journal).refundCompensatingReward("purchase-1");
	}

	@Test
	void rejectedCompensationSchedulersLeaveADurableRecoveryMarker() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation entityScheduler =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException("stopping"))
				.when(persistenceExecutor).execute(any(Runnable.class));
		org.mockito.Mockito.doThrow(new java.util.concurrent.RejectedExecutionException("disabling"))
				.when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		SharedMysqlPurchaseJournal journal = mock(SharedMysqlPurchaseJournal.class);
		when(journal.markCompensating("purchase-1")).thenReturn(true);
		VoteShopPurchaseService.SharedPurchaseDebit debit = new VoteShopPurchaseService.SharedPurchaseDebit(
				VoteShopPurchaseResult.SUCCESS, journal, "purchase-1", "Points", null);

		new VoteShopPurchaseService(plugin, mock(VoteShopDefinition.class)).scheduleClaimedReward(
				mock(org.bukkit.entity.Player.class), mock(VotingPluginUser.class), mock(VoteShopItem.class),
				new java.util.HashMap<>(), mock(FileConfiguration.class), ignored -> {}, debit);

		verify(journal).markCompensating("purchase-1");
		verify(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		verify(journal, never()).refundCompensatingReward(anyString());
	}

	@Test
	void sharedMysqlDebitWaitsForAndRemovesExistingCache() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		VotingPluginUser user = purchaseUser();
		UserDataCache cache = mock(UserDataCache.class);
		when(user.isCached()).thenReturn(true, false);
		when(user.getCache()).thenReturn(cache);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);

		assertEquals(VoteShopPurchaseResult.SUCCESS,
				new VoteShopPurchaseService(plugin, null).debitSharedMysql(user, item));

		verify(cache).dump();
		verify(plugin.getUserManager().getDataManager()).removeCache(
				java.util.UUID.fromString("00000000-0000-0000-0000-000000000001"), null);
	}

	@Test
	void sharedMysqlDebitClosesItsConnectionBeforeRefreshingTheCache() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		UserDataCache cache = mock(UserDataCache.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		VotingPluginUser user = purchaseUser();
		UUID userUuid = UUID.fromString(user.getUUID());
		HashMap<String, DataValue> cachedValues = new HashMap<>();
		cachedValues.put("Points", mock(DataValue.class));
		when(plugin.getUserManager().getDataManager().getUserDataCache()).thenReturn(
				new java.util.concurrent.ConcurrentHashMap<>(java.util.Map.of(userUuid, cache)));
		when(cache.getCache()).thenReturn(cachedValues);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);

		assertEquals(VoteShopPurchaseResult.SUCCESS,
				new VoteShopPurchaseService(plugin, null).debitSharedMysql(user, item));

		InOrder closeBeforeRefresh = inOrder(connection, cache);
		closeBeforeRefresh.verify(connection).close();
		closeBeforeRefresh.verify(cache).getCache();
		assertFalse(cachedValues.containsKey("Points"));
		verify(cache, never()).addChange(any(), org.mockito.ArgumentMatchers.anyBoolean());
	}

	@Test
	void sharedMysqlFailureReleasesDebitConnectionBeforeClassifyingTheLimit() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(connection);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeUpdate()).thenReturn(0);
		VotingPluginUser user = purchaseUser();
		UserData data = mock(UserData.class);
		when(user.getUserData()).thenReturn(data);
		when(data.getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE)).thenReturn(1);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(1);
		when(item.getIdentifier()).thenReturn("daily");

		assertEquals(VoteShopPurchaseResult.LIMIT_REACHED,
				new VoteShopPurchaseService(sharedMysqlPlugin(table), null).debitSharedMysql(user, item));

		InOrder closeThenClassify = inOrder(connection, data);
		closeThenClassify.verify(connection).close();
		closeThenClassify.verify(data).getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE);
	}

	@Test
	void sharedMysqlPurchaseQueuesDatabaseWorkOffCallingThread() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		when(table.getMysql()).thenReturn(sql);
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		when(definition.getTitle()).thenReturn("Vote Shop");
		VotingPluginUser user = purchaseUser();
		when(user.getPoints()).thenReturn(0);
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(1);
		when(user.getVoteShopIdentifierLimit(anyString())).thenReturn(1);

		new VoteShopPurchaseService(plugin, definition).purchase(mock(org.bukkit.entity.Player.class), user, item,
				result -> { });

		verify(persistenceExecutor).execute(any(Runnable.class));
		verify(sql.getConnectionManager(), never()).getConnection();
	}

	@Test
	void legacySharedMysqlPurchaseReportsPendingUntilDebitCompletes() {
		MySQL table = mock(MySQL.class);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		VoteShopItem item = mock(VoteShopItem.class);

		VoteShopPurchaseResult result = new VoteShopPurchaseService(plugin, definition)
				.purchase(mock(org.bukkit.entity.Player.class), purchaseUser(), item);

		assertEquals(VoteShopPurchaseResult.PENDING, result);
		verify(persistenceExecutor).execute(any(Runnable.class));
	}

	@Test
	void sharedPurchaseKeepsRewardConfigurationFromBeforeShopReload() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection schemaConnection = mock(Connection.class);
		Connection pendingConnection = mock(Connection.class);
		Connection compensatingConnection = mock(Connection.class);
		Connection cleanupConnection = mock(Connection.class);
		Connection reserveConnection = mock(Connection.class);
		Connection claimConnection = mock(Connection.class);
		Connection completeConnection = mock(Connection.class);
		PreparedStatement schema = mock(PreparedStatement.class);
		PreparedStatement schemaGeneration = mock(PreparedStatement.class);
		PreparedStatement schemaGenerationExpiry = mock(PreparedStatement.class);
		PreparedStatement schemaIndex = mock(PreparedStatement.class);
		PreparedStatement pending = mock(PreparedStatement.class);
		PreparedStatement compensating = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		PreparedStatement reserve = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement claim = mock(PreparedStatement.class);
		PreparedStatement completeSelect = mock(PreparedStatement.class);
		PreparedStatement completeUpdate = mock(PreparedStatement.class);
		ResultSet noPendingRows = emptyRows();
		ResultSet noCompensatingRows = emptyRows();
		ResultSet noTerminalRows = emptyRows();
		ResultSet hookStartedPurchase = purchaseRow("HOOK_STARTED", "Points", null, 10);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(schemaConnection, pendingConnection,
				compensatingConnection, cleanupConnection, reserveConnection, claimConnection, completeConnection);
		when(schemaConnection.prepareStatement(anyString())).thenReturn(schema, schemaGeneration,
				schemaGenerationExpiry, schemaIndex);
		when(pendingConnection.prepareStatement(anyString())).thenReturn(pending);
		when(pending.executeQuery()).thenReturn(noPendingRows);
		when(compensatingConnection.prepareStatement(anyString())).thenReturn(compensating);
		when(compensating.executeQuery()).thenReturn(noCompensatingRows);
		when(cleanupConnection.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(noTerminalRows);
		when(reserveConnection.prepareStatement(anyString())).thenReturn(reserve, debit);
		when(debit.executeUpdate()).thenReturn(1);
		AtomicReference<String> claimThread = new AtomicReference<>();
		when(claimConnection.prepareStatement(anyString())).thenAnswer(invocation -> {
			claimThread.set(Thread.currentThread().getName());
			return claim;
		});
		when(claim.executeUpdate()).thenReturn(1);
		when(completeConnection.prepareStatement(anyString())).thenReturn(completeSelect, completeUpdate);
		when(completeSelect.executeQuery()).thenReturn(hookStartedPurchase);
		when(completeUpdate.executeUpdate()).thenReturn(1);
		VotingPluginMain plugin = sharedMysqlPlugin(table);
		when(plugin.isEnabled()).thenReturn(true);
		RewardHandler rewardHandler = mock(RewardHandler.class);
		when(plugin.getRewardHandler()).thenReturn(rewardHandler);
		ScheduledExecutorService persistenceExecutor = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(persistenceExecutor);
		com.bencodez.simpleapi.scheduler.BukkitScheduler scheduler =
				mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
		com.bencodez.simpleapi.folialib.FoliaLib folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
		com.bencodez.simpleapi.folialib.impl.ServerImplementation entityScheduler =
				mock(com.bencodez.simpleapi.folialib.impl.ServerImplementation.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> claimWork = new AtomicReference<>();
		doAnswer(invocation -> {
			claimWork.set(invocation.getArgument(1, Runnable.class));
			return null;
		}).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SUCCESS));
		VoteShopDefinition definition = mock(VoteShopDefinition.class);
		when(definition.isEnabled()).thenReturn(true);
		when(definition.getTitle()).thenReturn("Vote Shop");
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		when(item.getIdentifier()).thenReturn("old-item");
		when(item.getIdentifierName()).thenReturn("Old item");
		when(item.getRewardsPath()).thenReturn("Shop.old-item.Rewards");
		when(item.getPurchaseMessage()).thenReturn("");
		VotingPluginUser user = purchaseUser();
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		FileConfiguration oldShopData = mock(FileConfiguration.class);
		FileConfiguration reloadedShopData = mock(FileConfiguration.class);
		when(plugin.getShopFile().getData()).thenReturn(oldShopData, reloadedShopData);
		org.bukkit.plugin.PluginManager pluginManager = mock(org.bukkit.plugin.PluginManager.class);
		AtomicReference<VoteShopPurchaseResult> result = new AtomicReference<>();

		try (org.mockito.MockedStatic<org.bukkit.Bukkit> bukkit = org.mockito.Mockito.mockStatic(org.bukkit.Bukkit.class)) {
			bukkit.when(org.bukkit.Bukkit::getPluginManager).thenReturn(pluginManager);
			new VoteShopPurchaseService(plugin, definition).purchase(player, user, item, result::set);
			ArgumentCaptor<Runnable> work = ArgumentCaptor.forClass(Runnable.class);
			verify(persistenceExecutor).execute(work.capture());
			// Simulate a reload replacing ShopFile's active configuration before the
			// delayed database/entity work gets to the reward executor.
			ExecutorService worker = Executors.newSingleThreadExecutor();
			Future<?> purchase = worker.submit(work.getValue());
			@SuppressWarnings("rawtypes")
			ArgumentCaptor<java.util.function.Consumer> rewardCallback = ArgumentCaptor.forClass(java.util.function.Consumer.class);
			verify(entityScheduler, org.mockito.Mockito.timeout(1000)).runAtEntityWithFallback(any(),
				rewardCallback.capture(), any(Runnable.class));
			// A stopped JVM at this point must leave the durable row PENDING: the
			// scheduler has accepted the reward callback but has not yet run it.
			verify(claimConnection, never()).prepareStatement(anyString());
			rewardCallback.getValue().accept(null);
			assertNotNull(claimWork.get(), "the entity gate must hand JDBC work to the async scheduler");
			verify(claimConnection, never()).prepareStatement(anyString());
			Thread claimWorker = new Thread(claimWork.get(), "vote-shop-claim-worker");
			claimWorker.start();
			claimWorker.join(1000);
			assertFalse(claimWorker.isAlive());
			InOrder callbackBeforeClaim = inOrder(entityScheduler, claimConnection);
			callbackBeforeClaim.verify(entityScheduler).runAtEntityWithFallback(any(), any(), any(Runnable.class));
			callbackBeforeClaim.verify(claimConnection).prepareStatement(anyString());
			verify(completeConnection, never()).prepareStatement(anyString());
			purchase.get(5, TimeUnit.SECONDS);
			@SuppressWarnings("rawtypes")
			ArgumentCaptor<java.util.function.Consumer> entityCallbacks =
					ArgumentCaptor.forClass(java.util.function.Consumer.class);
			verify(entityScheduler, times(2)).runAtEntityWithFallback(any(), entityCallbacks.capture(), any(Runnable.class));
			entityCallbacks.getAllValues().get(1).accept(null);
			ArgumentCaptor<Runnable> scheduledWork = ArgumentCaptor.forClass(Runnable.class);
			verify(persistenceExecutor, times(2)).execute(scheduledWork.capture());
			scheduledWork.getAllValues().get(1).run();
			worker.shutdownNow();
		}

		assertEquals(VoteShopPurchaseResult.SUCCESS, result.get());
		assertEquals("vote-shop-claim-worker", claimThread.get(), "the entity callback must not perform JDBC");
		verify(rewardHandler).giveReward(eq(user), eq(oldShopData), eq("Shop.old-item.Rewards"), any());
		verify(rewardHandler, never()).giveReward(eq(user), eq(reloadedShopData), anyString(), any());
	}

	@Test
	void sharedMysqlConditionAllowsOnlyOneBackendDebit() throws Exception {
		AtomicInteger sharedBalance = new AtomicInteger(10);
		CountDownLatch bothBackendsInsideUpdate = new CountDownLatch(2);
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);

		Connection firstConnection = mock(Connection.class);
		Connection secondConnection = mock(Connection.class);
		PreparedStatement firstStatement = conditionalDebitStatement(sharedBalance, bothBackendsInsideUpdate);
		PreparedStatement secondStatement = conditionalDebitStatement(sharedBalance, bothBackendsInsideUpdate);
		when(firstConnection.prepareStatement(anyString())).thenReturn(firstStatement);
		when(secondConnection.prepareStatement(anyString())).thenReturn(secondStatement);
		when(sql.getConnectionManager().getConnection()).thenReturn(firstConnection, secondConnection);

		VoteShopPurchaseService first = new VoteShopPurchaseService(sharedMysqlPlugin(table), null);
		VoteShopPurchaseService second = new VoteShopPurchaseService(sharedMysqlPlugin(table), null);
		VotingPluginUser firstUser = purchaseUser();
		VotingPluginUser secondUser = purchaseUser();
		VoteShopItem item = mock(VoteShopItem.class);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);

		ExecutorService executor = Executors.newFixedThreadPool(2);
		try {
			Future<VoteShopPurchaseResult> firstResult = executor.submit(() -> first.debitSharedMysql(firstUser, item));
			Future<VoteShopPurchaseResult> secondResult = executor.submit(() -> second.debitSharedMysql(secondUser, item));
			VoteShopPurchaseResult one = firstResult.get(5, TimeUnit.SECONDS);
			VoteShopPurchaseResult two = secondResult.get(5, TimeUnit.SECONDS);
			assertTrue((one == VoteShopPurchaseResult.SUCCESS && two == VoteShopPurchaseResult.NOT_ENOUGH_POINTS)
					|| (two == VoteShopPurchaseResult.SUCCESS && one == VoteShopPurchaseResult.NOT_ENOUGH_POINTS));
			assertEquals(0, sharedBalance.get());
		} finally {
			executor.shutdownNow();
		}
	}

	private static PreparedStatement conditionalDebitStatement(AtomicInteger balance, CountDownLatch entered)
			throws Exception {
		PreparedStatement statement = mock(PreparedStatement.class);
		when(statement.executeUpdate()).thenAnswer(invocation -> {
			entered.countDown();
			entered.await(5, TimeUnit.SECONDS);
			return balance.compareAndSet(10, 0) ? 1 : 0;
		});
		return statement;
	}

	private static ResultSet emptyRows() throws Exception {
		ResultSet rows = mock(ResultSet.class);
		when(rows.next()).thenReturn(false);
		return rows;
	}

	private static ResultSet purchaseRow(String state, String pointsColumn, String limitColumn, int cost)
			throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(state);
		when(row.getString(2)).thenReturn("00000000-0000-0000-0000-000000000001");
		when(row.getString(3)).thenReturn(pointsColumn);
		when(row.getString(4)).thenReturn(limitColumn);
		when(row.getInt(5)).thenReturn(cost);
		return row;
	}

	private static VotingPluginMain sharedMysqlPlugin(MySQL table) {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getMysql()).thenReturn(table);
		return plugin;
	}

	private static VotingPluginUser purchaseUser() {
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(user.getPointsPath()).thenReturn("Points");
		return user;
	}

	@Test
	void wrappersForSamePlayerShareLockAcrossServiceReloads() {
		VoteShopPurchaseService firstService = new VoteShopPurchaseService(null, null);
		VoteShopPurchaseService reloadedService = new VoteShopPurchaseService(null, null);
		String uuid = "00000000-0000-0000-0000-000000000001";
		assertSame(firstService.purchaseLock(uuid), reloadedService.purchaseLock(uuid));
	}

	@Test
	void debitUsesAsynchronousPersistence() {
		VoteShopPurchaseService service = new VoteShopPurchaseService(null, null);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		when(user.removePoints(10, true)).thenReturn(true);

		assertEquals(VoteShopPurchaseResult.SUCCESS, service.debitForPurchase(user, item));
		verify(user).removePoints(10, true);
	}

	@Test
	void concurrentWrappersCannotDebitSamePlayerTogether() throws Exception {
		VoteShopPurchaseService first = new VoteShopPurchaseService(null, null);
		VoteShopPurchaseService second = new VoteShopPurchaseService(null, null);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		CountDownLatch firstInsideDebit = new CountDownLatch(1);
		CountDownLatch releaseFirst = new CountDownLatch(1);
		AtomicInteger calls = new AtomicInteger();
		when(user.removePoints(10, true)).thenAnswer(invocation -> {
			if (calls.incrementAndGet() == 1) {
				firstInsideDebit.countDown();
				releaseFirst.await(5, TimeUnit.SECONDS);
				return true;
			}
			return false;
		});
		ExecutorService executor = Executors.newFixedThreadPool(2);
		try {
			Future<VoteShopPurchaseResult> one = executor.submit(() -> first.debitForPurchase(user, item));
			assertEquals(true, firstInsideDebit.await(5, TimeUnit.SECONDS));
			AtomicReference<Thread> secondThread = new AtomicReference<>();
			Future<VoteShopPurchaseResult> two = executor.submit(() -> {
				secondThread.set(Thread.currentThread());
				return second.debitForPurchase(user, item);
			});
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
			while (System.nanoTime() < deadline
					&& (secondThread.get() == null || secondThread.get().getState() != Thread.State.BLOCKED)
					&& !two.isDone()) {
				Thread.onSpinWait();
			}
			assertTrue(secondThread.get() != null && secondThread.get().getState() == Thread.State.BLOCKED,
					"the second debit did not block on the shared purchase lock");
			assertEquals(1, calls.get());
			releaseFirst.countDown();
			assertEquals(VoteShopPurchaseResult.SUCCESS, one.get(5, TimeUnit.SECONDS));
			assertEquals(VoteShopPurchaseResult.NOT_ENOUGH_POINTS, two.get(5, TimeUnit.SECONDS));
			verify(user, times(2)).removePoints(10, true);
		} finally {
			releaseFirst.countDown();
			executor.shutdownNow();
		}
	}
}
