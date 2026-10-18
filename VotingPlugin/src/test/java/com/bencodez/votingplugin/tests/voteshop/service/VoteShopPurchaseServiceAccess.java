package com.bencodez.votingplugin.tests.voteshop.service;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.voteshop.service.*;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService.*;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.function.Consumer;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

/** Typed test-only access to package-private VoteShopPurchaseService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteShopPurchaseServiceAccess {
    private VoteShopPurchaseServiceAccess() { }
    public static boolean requiresCompensation(SharedMysqlPurchaseJournalAccess.ClaimOutcome claim) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "requiresCompensation", new String[]{"ClaimOutcome"}, new Object[]{claim}); }
    public static void scheduleClaimedReward(VoteShopPurchaseService target, Player player, VotingPluginUser user, VoteShopItem item, HashMap<String, String> placeholders, FileConfiguration shopData, Consumer<VoteShopPurchaseResult> completion, VoteShopPurchaseServiceAccess.SharedPurchaseDebit debit) { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", target, "scheduleClaimedReward", new String[]{"Player", "VotingPluginUser", "VoteShopItem", "HashMap", "FileConfiguration", "Consumer", "SharedPurchaseDebit"}, new Object[]{player, user, item, placeholders, shopData, completion, debit}); }
    public static VoteShopPurchaseResult debitForPurchase(VoteShopPurchaseService target, VotingPluginUser user, VoteShopItem item) { return (VoteShopPurchaseResult) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", target, "debitForPurchase", new String[]{"VotingPluginUser", "VoteShopItem"}, new Object[]{user, item}); }
    public static boolean canRecoverSharedMysqlPurchases(VotingPluginMain plugin) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "canRecoverSharedMysqlPurchases", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static void withSharedMysqlCacheResetFence(Runnable action) { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "withSharedMysqlCacheResetFence", new String[]{"Runnable"}, new Object[]{action}); }
    public static void withSharedMysqlCacheDumpFence(Runnable action) { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "withSharedMysqlCacheDumpFence", new String[]{"Runnable"}, new Object[]{action}); }
    public static void recoverSharedMysqlPurchases(VotingPluginMain plugin) { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "recoverSharedMysqlPurchases", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static void recoverSharedMysqlPurchases(VotingPluginMain plugin, SharedMysqlPurchaseJournalAccess journal) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "recoverSharedMysqlPurchases", new String[]{"VotingPluginMain", "SharedMysqlPurchaseJournal"}, new Object[]{plugin, journal}); }
    public static VoteShopPurchaseResult debitSharedMysql(VoteShopPurchaseService target, VotingPluginUser user, VoteShopItem item) { return (VoteShopPurchaseResult) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", target, "debitSharedMysql", new String[]{"VotingPluginUser", "VoteShopItem"}, new Object[]{user, item}); }
    public static ZoneId networkTimeZone(String configured) { return (ZoneId) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "networkTimeZone", new String[]{"String"}, new Object[]{configured}); }
    public static LocalDateTime networkCurrentTime(long nowMillis, ZoneId timeZone, int hourOffset) { return (LocalDateTime) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "networkCurrentTime", new String[]{"long", "ZoneId", "int"}, new Object[]{nowMillis, timeZone, hourOffset}); }
    public static VoteShopPurchaseServiceAccess.LimitGeneration limitGeneration(LocalDateTime current, long nowMillis, boolean daily, boolean weekly, boolean monthly, int weekOffset) { return VoteShopPurchaseServiceAccess.LimitGeneration.view(ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "limitGeneration", new String[]{"LocalDateTime", "long", "boolean", "boolean", "boolean", "int"}, new Object[]{current, nowMillis, daily, weekly, monthly, weekOffset})); }
    public static String weeklyGenerationId(LocalDateTime current, int weekOffset) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", null, "weeklyGenerationId", new String[]{"LocalDateTime", "int"}, new Object[]{current, weekOffset}); }
    public static Object purchaseLock(VoteShopPurchaseService target, String uuid) { return (Object) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService", target, "purchaseLock", new String[]{"String"}, new Object[]{uuid}); }
    public static final class SharedPurchaseDebit implements ReflectiveAccess.View {
        public SharedPurchaseDebit() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private SharedPurchaseDebit(Object delegate) { this.delegate = delegate; }
        public static SharedPurchaseDebit view(Object delegate) { return delegate == null ? null : new SharedPurchaseDebit(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof SharedPurchaseDebit value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public SharedPurchaseDebit(VoteShopPurchaseResult result, SharedMysqlPurchaseJournalAccess journal, String purchaseId, String pointsColumn, String limitColumn) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", new String[]{"VoteShopPurchaseResult", "SharedMysqlPurchaseJournal", "String", "String", "String"}, new Object[]{result, journal, purchaseId, pointsColumn, limitColumn})); }
        public VoteShopPurchaseResult result() { return (VoteShopPurchaseResult) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", delegate, "result", new String[]{}, new Object[]{}); }
        public SharedMysqlPurchaseJournalAccess journal() { return SharedMysqlPurchaseJournalAccess.view(ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", delegate, "journal", new String[]{}, new Object[]{})); }
        public String purchaseId() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", delegate, "purchaseId", new String[]{}, new Object[]{}); }
        public String pointsColumn() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", delegate, "pointsColumn", new String[]{}, new Object[]{}); }
        public String limitColumn() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$SharedPurchaseDebit", delegate, "limitColumn", new String[]{}, new Object[]{}); }
    }

    public static final class LimitGeneration implements ReflectiveAccess.View {

        private final Object delegate;
        private LimitGeneration(Object delegate) { this.delegate = delegate; }
        public static LimitGeneration view(Object delegate) { return delegate == null ? null : new LimitGeneration(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof LimitGeneration value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String value() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$LimitGeneration", delegate, "value", new String[]{}, new Object[]{}); }
        public long expiresAt() { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService$LimitGeneration", delegate, "expiresAt", new String[]{}, new Object[]{}); }
    }

}
