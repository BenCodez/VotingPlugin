package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.inOrder;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

class VoteShopPurchaseServiceTest {
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
	void sharedMysqlDebitIsRefundedWhenEntitySchedulerRetiresWithoutFallback() throws Exception {
		MySQL table = mock(MySQL.class);
		com.bencodez.simpleapi.sql.mysql.MySQL sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		Connection debitConnection = mock(Connection.class);
		Connection refundConnection = mock(Connection.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		when(table.getTableName()).thenReturn("VotingPlugin_Users");
		when(table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(table.getMysql()).thenReturn(sql);
		when(sql.getConnectionManager().getConnection()).thenReturn(debitConnection, refundConnection);
		when(debitConnection.prepareStatement(anyString())).thenReturn(debit);
		when(refundConnection.prepareStatement(anyString())).thenReturn(refund);
		when(debit.executeUpdate()).thenReturn(1);
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

		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		when(entityScheduler.runAtEntityWithFallback(org.mockito.ArgumentMatchers.eq(player), any(),
				any(Runnable.class))).thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		new VoteShopPurchaseService(plugin, definition).purchase(player, user, item,
				result -> completions.incrementAndGet());
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

		ArgumentCaptor<String> refundSql = ArgumentCaptor.forClass(String.class);
		verify(refundConnection).prepareStatement(refundSql.capture());
		assertTrue(refundSql.getValue().contains("`Points` = `Points` + ?"));
		verify(refund).setInt(1, 10);
		verify(refund, times(1)).executeUpdate();
		verify(entityScheduler).runAtEntityWithFallback(
				org.mockito.ArgumentMatchers.eq(player), any(), any(Runnable.class));
		scheduled.getValue().accept(null);
		assertEquals(0, completions.get(), "a compensated purchase must not complete its reward later");
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
