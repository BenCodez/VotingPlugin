package com.bencodez.votingplugin.listeners;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.bedrock.BedrockNameResolver;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.validation.UserValidationResult;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.vote.SharedVoteAdmissionException;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.VoteTaskAdmission;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

/** Bukkit event entry point and platform operations for the shared vote sequence. */
public class PlayerVoteListener implements Listener {
    private final VotingPluginMain plugin;

    public PlayerVoteListener(VotingPluginMain plugin) {
        this.plugin = plugin;
    }

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onplayerVote(PlayerVoteEvent event) {
		if (!VoteTaskAdmission.isVoteTask() && Bukkit.isPrimaryThread()) {
			PlatformVoteState platformState = PlatformVoteState.capture(plugin, event.getPlayer());
			if (!VoteTaskAdmission.trySubmit(plugin.getVoteTimer(), () -> processVote(event, platformState))) {
				failAdmission(event, new SharedVoteAdmissionException("Vote executor rejected accounting admission"));
			}
			return;
		}
		processVote(event, PlatformVoteState.uncaptured());
	}

	private void processVote(PlayerVoteEvent event, PlatformVoteState platformState) {
        try {
            SharedVoteProcessor.process(new BukkitOperations(plugin, event, platformState));
        } catch (SharedVoteAdmissionException admissionFailure) {
			failAdmission(event, admissionFailure);
		} catch (RuntimeException processingFailure) {
			event.setProcessingFailed(true);
			plugin.getLogger().severe("Vote processing did not complete for " + event.getPlayer() + '/'
					+ event.getServiceSite() + "; a durable producer may retry it");
			plugin.debug(processingFailure);
        }
    }

	private void failAdmission(PlayerVoteEvent event, SharedVoteAdmissionException failure) {
		event.setAccountingAdmissionFailed(true);
		plugin.getLogger().severe("Vote processing aborted because shared accounting admission failed for "
				+ event.getPlayer() + '/' + event.getServiceSite());
		plugin.debug(failure);
	}

    static final class BukkitOperations implements SharedVoteProcessor.Operations<VoteSite, VotingPluginUser> {
        private final VotingPluginMain plugin;
        private final PlayerVoteEvent event;
		private final PlatformVoteState platformState;

		BukkitOperations(VotingPluginMain plugin, PlayerVoteEvent event, PlatformVoteState platformState) {
            this.plugin = plugin;
            this.event = event;
			this.platformState = platformState;
        }

        @Override public boolean enabled() { return plugin.isEnabled(); }
        @Override public String incomingName() { return event.getPlayer(); }
        @Override public String serviceSite() { return event.getServiceSite(); }
        @Override public void debug(String message) { plugin.debug(message); }
        @Override public void extraDebug(String message) { plugin.extraDebug(message); }
        @Override public void info(String message) { plugin.getLogger().info(message); }
        @Override public void warning(String message) { plugin.getLogger().warning(message); }
        @Override public String properName(String name) { return plugin.getUserManager().getProperName(name); }
        @Override public boolean allowUnJoinedCheckServer() { return plugin.getConfigFile().isAllowUnJoinedCheckServer(); }
        @Override public SharedVoteProcessor.Validation validate(String name, boolean allowUnJoinedCheckServer) {
            UserValidationResult result = plugin.getUserManager().getValidationService().validate(name, allowUnJoinedCheckServer);
            return new SharedVoteProcessor.Validation(result.isValid(), result.getNormalizedName(),
                    result.getSource() == null ? null : result.getSource().toString(),
                    String.valueOf(result.getReason()), result.isBedrock());
        }
        @Override public boolean allowUnjoined() { return plugin.getConfigFile().isAllowUnjoined(); }
        @Override public boolean removeInvalidProxyUsers() { return plugin.getBungeeSettings().isRemoveInvalidUsers(); }
        @Override public void removeInvalidUser(String name) { plugin.getVotingPluginUserManager().getVotingPluginUser(name).remove(); }
        @Override public SharedVoteProcessor.Name resolveUnknownName(String name) {
            BedrockNameResolver.Result resolved = plugin.getBedrockHandle().resolve(name);
            return new SharedVoteProcessor.Name(resolved.finalName, resolved.rationale);
        }
        @Override public void triggerProxyEvent() {
            plugin.debug("BungeePlayerVote forcebungee: " + event.isForceBungee() + ", bungeetotals: "
                    + event.getBungeeTextTotals());
            if (plugin.getBungeeSettings().isTriggerVotifierEvent()) {
                try {
                    new ProxyVotifierEvent().send(plugin, event);
                } catch (NoClassDefFoundError ex) {
                    plugin.getLogger().severe("Failed to trigger Votifier event for proxy vote. Either install Votifier or disable triggering the Votifier event");
                    plugin.getLogger().severe("Error: " + ex.getMessage());
                    plugin.debug(ex);
                }
            }
        }
        @Override public VoteSite resolveSite() {
            VoteSite site = event.getVoteSite();
            if (site == null) site = plugin.getVoteSiteManager()
                    .getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, event.getServiceSite()), true);
            return site;
        }
        @Override public void reportMissingSite() {
            if (!plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
                plugin.getLogger().warning("No voting site with the service site: '" + event.getServiceSite() + "'");
                ArrayList<String> services = new ArrayList<>();
                for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) services.add(site.getServiceSite());
                plugin.getLogger().warning("Currently set service sites: " + ArrayUtils.makeStringList(services));
            }
        }
        @Override public boolean siteEnabled(VoteSite site) { return site.isEnabled(); }
        @Override public String siteKey(VoteSite site) { return site.getKey(); }
        @Override public String siteDisplayName(VoteSite site) { return site.getDisplayName(); }
        @Override public VotingPluginUser resolveUser(String name) {
			if (event.getVotingPluginUser() != null) return event.getVotingPluginUser();
			if (platformState.matchesName(name)) return plugin.getVotingPluginUserManager()
					.getVotingPluginUser(platformState.uuid(), platformState.playerName());
			if (platformState.captured()) return plugin.getVotingPluginUserManager().getVotingPluginUser(name);
            Player player = Bukkit.getPlayerExact(name);
            if (player != null) return plugin.getVotingPluginUserManager().getVotingPluginUser(player);
            return plugin.getVotingPluginUserManager().getVotingPluginUser(name);
        }
        @Override public String userName(VotingPluginUser user) { return user.getPlayerName(); }
        @Override public String userId(VotingPluginUser user) { return user.getUUID(); }
        @Override public UUID userUuid(VotingPluginUser user) { return user.getJavaUUID(); }
		@Override public boolean userOnline(VotingPluginUser user) {
			return platformState.captured() ? platformState.matches(user) && platformState.online() : user.isOnline();
		}
		@Override public boolean userVanished(VotingPluginUser user) {
			return platformState.captured() ? platformState.matches(user) && platformState.vanished() : user.isVanished();
		}
        @Override public long lastVoteTime(VotingPluginUser user, VoteSite site) { return user.getTime(site); }
        @Override public boolean waitUntilVoteDelay(VoteSite site) { return site.isWaitUntilVoteDelay(); }
        @Override public boolean canVoteSite(VotingPluginUser user, VoteSite site) { return user.canVoteSite(site); }
		@Override public boolean bypassWaitPermission(VotingPluginUser user) {
			return platformState.captured()
					? platformState.matches(user) && platformState.bypassWaitPermission()
					: user.hasPermission("VotingPlugin.BypassWaitUntilVoteDelay");
		}
        @Override public boolean processRewards() { return plugin.getOptions().isProcessRewards(); }
        @Override public void giveWaitRewards(VoteSite site, VotingPluginUser user, boolean online, boolean proxyVote) {
            site.giveWaitUntilVoteDelayRewards(user, online, proxyVote);
        }
        @Override public boolean proxyVote() { return event.isBungee(); }
        @Override public boolean forceProxyRouting() { return event.isForceBungee(); }
        @Override public boolean wasOnline() { return event.isWasOnline(); }
        @Override public boolean realVote() { return event.isRealVote(); }
        @Override public boolean addTotals() { return event.isAddTotals(); }
        @Override public boolean broadcastEnabled() { return event.isBroadcast(); }
        @Override public boolean hasBroadcastHandler() { return plugin.getBroadcastHandler() != null; }
        @Override public void broadcast(UUID uuid, String name, String siteDisplayName, boolean online) {
			Runnable broadcast = () -> plugin.getBroadcastHandler().broadcastVote(uuid, name, siteDisplayName, online);
			if (!platformState.captured()) {
				broadcast.run();
			} else if (platformState.owner() != null) {
				plugin.getBukkitScheduler().runTask(plugin, broadcast, platformState.owner());
			} else {
				plugin.getBukkitScheduler().runTask(plugin, broadcast);
			}
        }
        @Override public boolean hasProxyTextTotals() { return event.getBungeeTextTotals() != null; }
        @Override public UUID incomingVoteId() {
            if (event.getVoteId() != null) return event.getVoteId();
            return event.getBungeeTextTotals() == null ? null : event.getBungeeTextTotals().getVoteUUID();
        }
		@Override public SharedVoteProcessor.AccountingAdmission prepareAccounting(
				VotingPluginUser user, UUID voteId, boolean countTotals, boolean awardPoints,
				int pointAmount, int pointCap) {
            boolean countVoteParty = plugin.getSpecialRewardsConfig().isVotePartyEnabled()
                    && (plugin.getSpecialRewardsConfig().isVotePartyCountFakeVotes() || event.isRealVote())
                    && (plugin.getSpecialRewardsConfig().isVotePartyCountOfflineVotes() || userOnline(user));
			VoteShopPurchaseService.VoteAccountingAdmission admission = VoteShopPurchaseService
					.prepareMysqlVoteAccounting(plugin, voteId, user.getUUID(), countTotals, awardPoints,
							countVoteParty, event.isForceBungee(), pointAmount, pointCap, user.getPointsPath());
			if (!admission.success()) {
                throw new SharedVoteAdmissionException("Unable to admit shared MySQL vote accounting before processing");
            }
			return new SharedVoteProcessor.AccountingAdmission(admission.countTotals(), admission.awardPoints(),
					admission.countVoteParty(), admission.pointAmount(), admission.pointCap(),
					admission.pointColumn(), admission.replayUnsafe());
        }
		@Override public int configuredPointAmount() { return plugin.getConfigFile().getPointsOnVote(); }
		@Override public int configuredPointCap() { return plugin.getConfigFile().getLimitVotePoints(); }
		@Override public void finishAccounting(UUID voteId) {
            VoteShopPurchaseService.finishMysqlVoteAccounting(voteId);
        }
        @Override public void cache(VotingPluginUser user) { user.cache(); }
        @Override public void updateName(VotingPluginUser user) { user.updateName(true); }
        @Override public void voteParty(VotingPluginUser user, boolean forceProxyRouting, UUID voteId, boolean eligible) {
            plugin.getVoteParty().voteAdmitted(user, forceProxyRouting, voteId, eligible);
        }
		@Override public void markReplayUnsafe(UUID voteId) {
			if (!VoteShopPurchaseService.markVoteReplayUnsafe(plugin, voteId)) {
				throw new SharedVoteAdmissionException("Unable to persist the vote effect boundary");
			}
			event.setReplayUnsafe(true);
		}
		@Override public boolean deferDeliveryCompletion() { return event.isDeferredDeliveryCompletion(); }
		@Override public void completeDelivery(UUID voteId) {
			VoteShopPurchaseService.completeVoteDelivery(plugin, voteId);
		}
		@Override public void restoreReplayUnsafe() { event.setReplayUnsafe(true); }
        @Override public long incomingTime() { return event.getTime(); }
        @Override public void setTime(VotingPluginUser user, VoteSite site, long time) { user.setTime(site, time); }
        @Override public void setTimeNow(VotingPluginUser user, VoteSite site) { user.setTime(site); }
        @Override public boolean giveOfflineRewards(VoteSite site) { return site.isGiveOffline(); }
        @Override public void playerVote(VotingPluginUser user, VoteSite site, boolean online, boolean forceProxyRouting) {
            user.playerVote(site, online, forceProxyRouting);
        }
        @Override public int voteNumber() { return event.getVoteNumber(); }
        @Override public void sendVoteEffects(VotingPluginUser user, boolean online) { user.sendVoteEffects(online); }
        @Override public boolean closeInventoryOnVote() { return plugin.getConfigFile().isCloseInventoryOnVote(); }
        @Override public void closeInventory(VotingPluginUser user) { user.closeInv(); }
        @Override public boolean offlineVotesLimitEnabled() { return plugin.getConfigFile().isOfflineVotesLimitEnabled(); }
        @Override public int offlineVotes(VotingPluginUser user, VoteSite site) { return user.getNumberOfOfflineVotes(site); }
        @Override public int offlineVotesLimitAmount() { return plugin.getConfigFile().getOfflineVotesLimitAmount(); }
        @Override public void addOfflineVote(VotingPluginUser user, String siteKey) { user.addOfflineVote(siteKey); }
        @Override public SharedVotePolicy countingPolicy() {
            return new SharedVotePolicy(plugin.getConfigFile().isCountFakeVotes(), plugin.getConfigFile().isAddTotals(),
                    plugin.getConfigFile().isAddTotalsOffline(), false, false);
        }
        @Override public void addTotal(VotingPluginUser user, UUID voteId) { user.addTotal(voteId); }
        @Override public void addTotalDaily(VotingPluginUser user, UUID voteId) { user.addTotalDaily(voteId); }
        @Override public void addTotalWeekly(VotingPluginUser user, UUID voteId) { user.addTotalWeekly(voteId); }
        @Override public void addPoints(VotingPluginUser user, UUID voteId, int amount, int cap, String pointColumn) {
			user.addVotePoints(voteId, amount, cap, pointColumn);
		}
        @Override public void checkDayVoteStreak(VotingPluginUser user, boolean forceProxyRouting, UUID voteId) {
            user.checkDayVoteStreak(forceProxyRouting, voteId);
        }
        @Override public boolean limitMonthlyVotes() { return plugin.getConfigFile().isLimitMonthlyVotes(); }
        @Override public int proxyMonthTotal() { return event.getBungeeTextTotals().getMonthTotal(); }
        @Override public int userMonthTotal(VotingPluginUser user) { return user.getTotal(TopVoter.Monthly); }
        @Override public int currentDayOfMonth() { return plugin.getTimeChecker().getTime().getDayOfMonth(); }
        @Override public int enabledSiteCount() { return plugin.getVoteSiteManager().getVoteSitesEnabled().size(); }
		@Override public void setMonthTotal(VotingPluginUser user, int total) {
			// Shared MySQL increments apply the same cap inside the boundary-serialized
			// transaction, so an absolute cache write here could only reintroduce a stale value.
			if (!UserStorage.MYSQL.equals(plugin.getStorageType())) user.capMonthTotal(total);
		}
        @Override public void milestones(VotingPluginUser user, UUID voteId, boolean forceProxyRouting) {
            plugin.getVoteMilestonesManager().handleVote(user, event.getBungeeTextTotals(), forceProxyRouting,
                    voteId, new HashMap<String, String>());
        }
        @Override public void cooldown(VotingPluginUser user, VoteSite site) { plugin.getCoolDownCheck().vote(user, site); }
        @Override public void voteStreak(VotingPluginUser user, long voteTime, UUID voteId) {
            plugin.getVoteStreakHandler().processVote(user, voteTime, voteId);
        }
        @Override public void postVote(VoteSite site, VotingPluginUser user, String playerName,
                long voteTime, UUID voteId, boolean cached) {
            PlayerPostVoteEvent post = new PlayerPostVoteEvent(site, user, event.isRealVote(), event.isForceBungee(),
                    voteTime, cached, site.getServiceSite(), user.getJavaUUID(), playerName, voteId);
            plugin.getServer().getPluginManager().callEvent(post);
        }
        @Override public boolean placeholderCacheAlways() { return plugin.getConfigFile().getPlaceholderCacheLevel().isCacheAlways(); }
        @Override public void updatePlaceholders(VotingPluginUser user) { plugin.getPlaceholders().onUpdate(user, true); }
        @Override public void clearCache(VotingPluginUser user) { user.clearCache(); }
        @Override public void setUpdate() { plugin.setUpdate(true); }
    }

	/** Bukkit-owned state captured before a primary-thread event enters the storage lane. */
	static record PlatformVoteState(boolean captured, Player owner, UUID uuid, String playerName, boolean online,
			boolean vanished, boolean bypassWaitPermission) {
		static PlatformVoteState uncaptured() {
			return new PlatformVoteState(false, null, null, null, false, false, false);
		}

		static PlatformVoteState capture(VotingPluginMain plugin, String requestedName) {
			if (requestedName == null || requestedName.isEmpty()) {
				return new PlatformVoteState(true, null, null, null, false, false, false);
			}
			Player player = Bukkit.getPlayerExact(requestedName);
			if (player == null) return new PlatformVoteState(true, null, null, null, false, false, false);
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
			boolean vanished = user.isVanished();
			boolean online = player.isOnline()
					&& (!plugin.getOptions().isTreatVanishAsOffline() || !vanished);
			return new PlatformVoteState(true, player, player.getUniqueId(), player.getName(), online, vanished,
					user.hasPermission("VotingPlugin.BypassWaitUntilVoteDelay"));
		}

		boolean matchesName(String name) {
			return uuid != null && playerName != null && name != null && playerName.equalsIgnoreCase(name);
		}

		boolean matches(VotingPluginUser user) {
			return uuid != null && user != null && uuid.equals(user.getJavaUUID());
		}
	}
}
