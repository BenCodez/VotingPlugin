package com.bencodez.votingplugin.core.vote;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.UUID;

/** The accepted vote sequence. Platform operations are supplied by the receiving adapter. */
public final class SharedVoteProcessor {
    private SharedVoteProcessor() { }

    /** A queued proxy delivery may already have written its timestamp on the backend. */
    public static boolean isQueuedVoteAlreadyRecorded(boolean proxyVote, long messageVoteTime, long storedVoteTime) {
        return proxyVote && messageVoteTime > 0L && messageVoteTime == storedVoteTime;
    }

    public record Validation(boolean valid, String normalizedName, String source, String reason, boolean bedrock) { }
    public record Name(String value, String rationale) { }

    public interface Operations<S, U> {
        boolean enabled();
        String incomingName();
        String serviceSite();
        void debug(String message);
        void extraDebug(String message);
        void info(String message);
        void warning(String message);
        String properName(String incomingName);
        boolean allowUnJoinedCheckServer();
        Validation validate(String properName, boolean allowUnJoinedCheckServer);
        boolean allowUnjoined();
        boolean removeInvalidProxyUsers();
        void removeInvalidUser(String properName);
        Name resolveUnknownName(String properName);
        void triggerProxyEvent();
        S resolveSite();
        void reportMissingSite();
        boolean siteEnabled(S site);
        String siteKey(S site);
        String siteDisplayName(S site);
        U resolveUser(String creditedName);
        String userName(U user);
        String userId(U user);
        UUID userUuid(U user);
        boolean userOnline(U user);
        boolean userVanished(U user);
        long lastVoteTime(U user, S site);
        boolean waitUntilVoteDelay(S site);
        boolean canVoteSite(U user, S site);
        boolean bypassWaitPermission(U user);
        boolean processRewards();
        void giveWaitRewards(S site, U user, boolean online, boolean proxyVote);
        boolean proxyVote();
        boolean forceProxyRouting();
        boolean wasOnline();
        boolean realVote();
        boolean addTotals();
        boolean broadcastEnabled();
        boolean hasBroadcastHandler();
        void broadcast(UUID uuid, String name, String siteDisplayName, boolean online);
        boolean hasProxyTextTotals();
        UUID proxyVoteId();
        void cache(U user);
        void updateName(U user);
        void voteParty(U user, boolean realVote, boolean forceProxyRouting);
        long incomingTime();
        void setTime(U user, S site, long time);
        void setTimeNow(U user, S site);
        boolean giveOfflineRewards(S site);
        void playerVote(U user, S site, boolean online, boolean forceProxyRouting);
        int voteNumber();
        void sendVoteEffects(U user, boolean online);
        boolean closeInventoryOnVote();
        void closeInventory(U user);
        boolean offlineVotesLimitEnabled();
        int offlineVotes(U user, S site);
        int offlineVotesLimitAmount();
        void addOfflineVote(U user, String siteKey);
        SharedVotePolicy countingPolicy();
        void addTotal(U user);
        void addTotalDaily(U user);
        void addTotalWeekly(U user);
        void addPoints(U user);
        void checkDayVoteStreak(U user, boolean forceProxyRouting);
        boolean limitMonthlyVotes();
        int proxyMonthTotal();
        int userMonthTotal(U user);
        int currentDayOfMonth();
        int enabledSiteCount();
        void setMonthTotal(U user, int total);
        void milestones(U user, UUID voteId, boolean forceProxyRouting);
        void cooldown(U user, S site);
        void voteStreak(U user, long voteTime, UUID voteId);
        void postVote(S site, U user, String playerName, long voteTime, UUID voteId, boolean cached);
        boolean placeholderCacheAlways();
        void updatePlaceholders(U user);
        void clearCache(U user);
        void setUpdate();
    }

    public static <S, U> void process(Operations<S, U> ops) {
        if (!ops.enabled()) {
            ops.warning("Plugin disabled, ignoring vote");
            return;
        }
        String playerName = ops.incomingName();
        ops.debug("Processing PlayerVoteEvent: " + playerName + "/" + ops.serviceSite());
        String properName = ops.properName(playerName);
        Validation validation = ops.validate(properName, ops.allowUnJoinedCheckServer());
        ops.extraDebug("Vote validation result for " + properName + ": valid=" + validation.valid()
                + ", source=" + validation.source() + ", reason=" + validation.reason()
                + ", normalizedName=" + validation.normalizedName() + ", bedrock=" + validation.bedrock());
        if (!validation.valid() && !ops.allowUnjoined()) {
            ops.warning("Player " + properName + " has not joined before, disregarding vote. Reason: "
                    + validation.reason() + ". Set AllowUnjoined to true to accept.");
            if (ops.proxyVote() && ops.removeInvalidProxyUsers()) ops.removeInvalidUser(properName);
            return;
        }
        Name resolved;
        if (validation.valid()) {
            String name = validation.normalizedName();
            if (name == null || name.isEmpty()) name = properName;
            resolved = new Name(name, "validation-" + validation.source().toLowerCase(java.util.Locale.ROOT));
        } else {
            resolved = ops.resolveUnknownName(properName);
        }
        ops.debug("Vote name resolved: " + properName + " -> " + resolved.value() + " (" + resolved.rationale() + ")");
        playerName = resolved.value();
        if (playerName.isEmpty()) {
            ops.warning("Empty player name from vote, ignoring");
            return;
        }
        if (ops.proxyVote()) ops.triggerProxyEvent();
        S site = ops.resolveSite();
        if (site == null) {
            ops.reportMissingSite();
            return;
        }
        if (!ops.siteEnabled(site)) {
            ops.debug("Votesite: " + ops.siteKey(site) + " is not enabled");
            return;
        }
        U user = ops.resolveUser(playerName);
        boolean proxyForDelay = ops.proxyVote();
        long timeForDelay = ops.incomingTime();
        long lastTimeForDelay = ops.lastVoteTime(user, site);
        boolean recordedProxyVote = isQueuedVoteAlreadyRecorded(proxyForDelay, timeForDelay, lastTimeForDelay);
        if (ops.waitUntilVoteDelay(site) && !recordedProxyVote && !ops.canVoteSite(user, site)) {
            if (!ops.realVote()) {
                ops.info(ops.userName(user) + " did a not real vote, bypassing WaitUntilVoteDelay");
            } else if (!ops.bypassWaitPermission(user)) {
                ops.info(ops.userName(user) + " must wait until votedelay is over, ignoring vote");
                boolean online = ops.userOnline(user);
                if (ops.proxyVote()) online = ops.wasOnline();
                if (ops.processRewards()) ops.giveWaitRewards(site, user, online, ops.proxyVote());
                return;
            } else {
                ops.info(ops.userName(user) + " has bypass permission for WaitUntilVoteDelay, bypassing");
            }
        }
        if (recordedProxyVote) {
            ops.debug("Allowing queued proxy vote for " + ops.userName(user) + " on " + ops.siteKey(site)
                    + "; proxy vote time already matches LastVotes: " + ops.incomingTime());
        }
        UUID voteId = UUID.randomUUID();
        if (ops.proxyVote() && ops.hasProxyTextTotals()) {
            voteId = ops.proxyVoteId();
            if (voteId == null) voteId = UUID.randomUUID();
        }
        String userId = ops.userId(user);
        ops.cache(user);
        ops.updateName(user);
        ops.voteParty(user, ops.realVote(), ops.forceProxyRouting());
        if (ops.broadcastEnabled() && ops.hasBroadcastHandler()) {
            boolean currentOnline = ops.userOnline(user);
            boolean online = currentOnline;
            if (ops.proxyVote()) online = ops.wasOnline();
            if (!ops.userVanished(user)) {
                SharedVoteIdentity identity = new SharedVoteIdentity(ops.userUuid(user), playerName, currentOnline);
                ops.broadcast(identity.uuid(), identity.playerName(), ops.siteDisplayName(site), online);
            } else {
                ops.debug("Not broadcasting vote for vanished user: " + ops.userName(user));
            }
        }
        long voteTime;
        if (ops.incomingTime() != 0) {
            ops.setTime(user, site, ops.incomingTime());
            voteTime = ops.incomingTime();
        } else {
            ops.setTimeNow(user, site);
            voteTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        }
        boolean cached = false;
        if (SharedVoteDelivery.shouldDeliverNow(ops::proxyVote, () -> ops.userOnline(user),
                () -> ops.giveOfflineRewards(site), ops::processRewards)) {
            boolean online = true;
            if (ops.proxyVote()) online = ops.wasOnline();
            ops.playerVote(user, site, online, ops.forceProxyRouting());
            if (ops.voteNumber() == 1) ops.sendVoteEffects(user, online);
            if (ops.closeInventoryOnVote()) ops.closeInventory(user);
        } else if (!ops.offlineVotesLimitEnabled() || ops.offlineVotes(user, site) <= ops.offlineVotesLimitAmount()) {
            ops.addOfflineVote(user, ops.siteKey(site));
            cached = true;
            ops.debug("Offline vote set for " + playerName + " (" + ops.userId(user) + ") on " + ops.siteKey(site));
        } else {
            ops.debug("Not setting offline vote, offline vote limit reached");
        }
        SharedVotePolicy policy = ops.countingPolicy();
        SharedVoteInput input = new SharedVoteInput(voteId, playerName, ops.serviceSite(), voteTime,
                ops.realVote(), ops.addTotals(), ops.proxyVote(), ops.forceProxyRouting(), ops.wasOnline());
        SharedVoteAccounting.apply(input, policy, () -> ops.userOnline(user), () -> ops.addTotal(user),
                () -> ops.addTotalDaily(user), () -> ops.addTotalWeekly(user), () -> ops.addPoints(user));
        ops.checkDayVoteStreak(user, ops.forceProxyRouting());
        if (ops.limitMonthlyVotes()) {
            int value = ops.proxyVote() ? ops.proxyMonthTotal() : ops.userMonthTotal(user);
            int days = ops.currentDayOfMonth();
            ops.extraDebug("Current day of month: " + days + " Current total: " + value);
            if (value >= days * ops.enabledSiteCount()) {
                ops.debug("Detected higher month total, changing. Current Total: " + value + " Days: " + days
                        + " New Total: " + days * ops.enabledSiteCount());
                ops.setMonthTotal(user, days * ops.enabledSiteCount());
            }
        }
        ops.milestones(user, voteId, ops.forceProxyRouting());
        ops.cooldown(user, site);
        ops.voteStreak(user, voteTime, voteId);
        ops.postVote(site, user, playerName, voteTime, voteId, cached);
        if (ops.userOnline(user) || ops.placeholderCacheAlways()) ops.updatePlaceholders(user);
        if (!ops.userOnline(user)) ops.clearCache(user);
        ops.setUpdate();
        ops.extraDebug("Finished vote processing: " + playerName + "/" + userId);
    }
}
