package com.bencodez.votingplugin.placeholders;

import java.text.NumberFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.TemporalField;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicLong;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.placeholder.CalculatingPlaceholder;
import com.bencodez.advancedcore.api.placeholder.NonPlayerPlaceHolder;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserDataChanged;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;

public class PlaceHolders {

	@Getter
	private ArrayList<NonPlayerPlaceHolder<VotingPluginUser>> nonPlayerPlaceholders = new ArrayList<>();

	@Getter
	private ArrayList<PlaceHolder<VotingPluginUser>> placeholders = new ArrayList<>();

	private VotingPluginMain plugin;

	private ArrayList<String> cachedPlaceholders = new ArrayList<>();

	private ConcurrentLinkedQueue<String> placeholdersToSetCacheOn = new ConcurrentLinkedQueue<>();
	private final Set<PlaceHolder<VotingPluginUser>> platformOwnedPlaceholders =
			Collections.newSetFromMap(new IdentityHashMap<>());
	private final Set<PlaceHolder<VotingPluginUser>> offlineWorkerPlaceholders =
			Collections.newSetFromMap(new IdentityHashMap<>());
	private record PlaceholderClassification(List<PlaceHolder<VotingPluginUser>> all,
			Set<PlaceHolder<VotingPluginUser>> platform, Set<PlaceHolder<VotingPluginUser>> offlineWorker) { }
	private volatile PlaceholderClassification userDataChangeClassification =
			new PlaceholderClassification(List.of(), Set.of(), Set.of());
	private final AtomicLong platformUpdateSequence = new AtomicLong();
	private record PlatformUpdateKey(UUID uuid, PlaceHolder<VotingPluginUser> placeholder) { }
	private final ConcurrentHashMap<PlatformUpdateKey, Long> platformUpdateGenerations = new ConcurrentHashMap<>();
	private boolean userDataChangeListenerRegistered;

	@Getter
	private PlaceholderCacheLevel cacheLevel;

	/**
	 * Constructor for PlaceHolders.
	 * @param plugin the voting plugin instance
	 */
	public PlaceHolders(VotingPluginMain plugin) {
		this.plugin = plugin;
		cacheLevel = plugin.getConfigFile().getPlaceholderCacheLevel();
	}

	/**
	 * Check for placeholders that need caching.
	 */
	public void checkNonCachedPlaceholders() {
		while (!placeholdersToSetCacheOn.isEmpty()) {
			String toCache = placeholdersToSetCacheOn.poll().toLowerCase();
			if (toCache.startsWith("votingplugin")) {
				toCache = toCache.substring("votingplugin_".length());
			}
			/*
			 * for (NonPlayerPlaceHolder<VotingPluginUser> placeholder :
			 * nonPlayerPlaceholders) { if (placeholder.matches(toCache)) {
			 * placeholder.setUseCache(true, toCache); cachedPlaceholders.add(toCache);
			 * plugin.getServerData().addAutoCachedPlaceholder(toCache);
			 * plugin.extraDebug("Auto Caching placeholder " + toCache); }
			 *
			 * }
			 */
			for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
				if (placeholder.matches(toCache)) {
					placeholder.setUseCache(true, toCache);
					cachedPlaceholders.add(toCache);
					plugin.getServerData().addAutoCachedPlaceholder(toCache);
					plugin.extraDebug("Auto Caching placeholder " + toCache);
				}
			}

		}
	}

	/**
	 * Get placeholder value for offline player.
	 * @param p the offline player
	 * @param identifier the placeholder identifier
	 * @return the placeholder value
	 */
	public String getPlaceHolder(OfflinePlayer p, String identifier) {
		return getPlaceHolder(p, identifier, true);
	}

	/**
	 * Get placeholder value for offline player with javascript support.
	 * @param p the offline player
	 * @param identifier1 the placeholder identifier
	 * @param javascript enable javascript processing
	 * @return the placeholder value
	 */
	public String getPlaceHolder(OfflinePlayer p, String identifier1, boolean javascript) {
		boolean forceProcess = false;
		boolean useCache = true;
		String identifier = identifier1.toLowerCase();
		boolean custom = false;
		if (identifier.endsWith("_process")
				|| (!Bukkit.isPrimaryThread() && plugin.getConfigFile().isAlwaysProcessAsyncPlaceholders())) {
			forceProcess = true;
			identifier = identifier.replaceAll("_process", "");
		}
		if (identifier.endsWith("_nocache")) {
			useCache = false;
			identifier = identifier.replaceAll("_nocache", "");
		}
		if (getCacheLevel().equals(PlaceholderCacheLevel.NONE)) {
			useCache = false;
		}

		if (identifier.startsWith("custom_")) {
			custom = true;
			identifier = identifier.replaceAll("custom_", "");
		}

		if (!custom || !plugin.getConfigFile().getCustomPlaceholderReturns().contains(identifier)) {
			return getPlaceholderValue(p, identifier, javascript, forceProcess, useCache);
		}
		String str = getPlaceholderValue(p, identifier, javascript, true, useCache);
		String returnStr = plugin.getConfigFile().getCustomPlaceholderReturns(identifier, str);
		if (!returnStr.isEmpty()) {
			return returnStr;
		}
		return str;

	}

	/**
	 * Get placeholder value for online player.
	 * @param p the player
	 * @param identifier the placeholder identifier
	 * @return the placeholder value
	 */
	public String getPlaceHolder(Player p, String identifier) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders()) {
			identifier = PlaceholderUtils.replaceJavascript(p, identifier);
		}
		return getPlaceHolder(p, identifier, false);
	}

	/**
	 * Get placeholder value with options.
	 * @param p the offline player
	 * @param identifier the placeholder identifier
	 * @param javascript enable javascript processing
	 * @param forceProcess force processing of placeholder
	 * @param useCache use cached value if available
	 * @return the placeholder value
	 */
	public String getPlaceholderValue(OfflinePlayer p, String identifier, boolean javascript, boolean forceProcess,
			boolean useCache) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders() && javascript && p != null) {
			identifier = PlaceholderUtils.replaceJavascript(p, identifier);
		}

		if (!identifier.toLowerCase().endsWith("position")) {
			for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
				if (placeholder.matches(identifier)) {
					String str = placeholder.placeholderRequest(identifier);
					if (str != null) {
						return str;
					}
				}
			}
		}

		if (p == null) {
			return "no player";
		}

		VotingPluginUser user = resolvePlaceholderUser(p);
		if (useCache) {
			if (!cachedPlaceholders.contains(identifier)) {
				// not cached placeholder
				if (!forceProcess) {
					for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
						if (placeholder.matches(identifier)) {
							if (getCacheLevel().shouldCache()) {
								if (!placeholdersToSetCacheOn.contains(identifier)) {
									placeholdersToSetCacheOn.add(identifier);
									schedulePlaceholderCheck(user);
								}
							}
							return "..";
						}
					}
					return "Not a valid placeholder";

				}
			}
		}

		UUID uuid = user.getJavaUUID();

		for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
			try {
				if (placeholder.matches(identifier)) {
					if (placeholder instanceof CalculatingPlaceholder<?>) {
						if (!useCache) {
							return placeholder.placeholderRequest(user, identifier);
						}
						CalculatingPlaceholder<VotingPluginUser> cPlaceholder = (CalculatingPlaceholder<VotingPluginUser>) placeholder;
						if (placeholder.isUsesCache() && placeholder.isCached(identifier)) {
							ConcurrentHashMap<UUID, String> cache = placeholder.getCache().get(identifier);
							if (cache.containsKey(uuid)) {
								if (cPlaceholder.getCacheData().containsKey(uuid)) {
									return cPlaceholder.placeholderRequest(user, identifier);
								}
								return cache.get(uuid);
							} else if (!forceProcess) {
								schedulePlaceholderCheck(user);
								return "...";
							}
						}

						if (forceProcess) {
							if (getCacheLevel().shouldCache()) {
								if (!placeholdersToSetCacheOn.contains(identifier)
										&& !cachedPlaceholders.contains(identifier)) {
									placeholdersToSetCacheOn.add(identifier);
									schedulePlaceholderCheck(user);
								}
							}
							return placeholder.placeholderRequest(user, identifier);
						} else {
							if (getCacheLevel().shouldCache()) {
								if (!placeholdersToSetCacheOn.contains(identifier)
										&& !cachedPlaceholders.contains(identifier)) {
									placeholdersToSetCacheOn.add(identifier);
									schedulePlaceholderCheck(user);
								}
							}
							return ".";
						}
					}
					if (!useCache) {
						return placeholder.placeholderRequest(user, identifier);
					}
					if (placeholder.isUsesCache() && placeholder.isCached(identifier)) {
						ConcurrentHashMap<UUID, String> cache = placeholder.getCache().get(identifier);

						if (cache.containsKey(uuid)) {
							return cache.get(uuid);
						} else if (!forceProcess) {
							schedulePlaceholderCheck(user);
							return "...";
						}
					}

					if (forceProcess) {
						if (getCacheLevel().shouldCache()) {
							if (!placeholdersToSetCacheOn.contains(identifier)
									&& !cachedPlaceholders.contains(identifier)) {
								placeholdersToSetCacheOn.add(identifier);
								schedulePlaceholderCheck(user);
							}
						}
						return placeholder.placeholderRequest(user, identifier);
					} else {
						if (getCacheLevel().shouldCache()) {
							if (!placeholdersToSetCacheOn.contains(identifier)
									&& !cachedPlaceholders.contains(identifier)) {
								placeholdersToSetCacheOn.add(identifier);
								schedulePlaceholderCheck(user);
							}
						}
						return ".";
					}
				}
			} catch (Exception e) {
				plugin.debug(e);
				return "Error";
			}
		}

		return "Not a valid placeholder";
	}

	/**
	 * Load all placeholders.
	 */
	public void load() {
		placeholders.clear();
		nonPlayerPlaceholders.clear();
		platformOwnedPlaceholders.clear();
		offlineWorkerPlaceholders.clear();

		// older placeholders, might be removed in the future
		placeholders.add(new PlaceHolder<VotingPluginUser>("total") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.Monthly));
			}
		}.withDescription("Month total").updateDataKey("MonthTotal"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("alltimetotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.AllTime));
			}
		}.withDescription("Alltime total").updateDataKey("AllTimeTotal"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("lastmonthtotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getLastMonthTotal());
			}
		}.withDescription("Last month total").updateDataKey("LastMonthTotal"));

		// end of older placeholders
		
		placeholders.add(new PlaceHolder<VotingPluginUser>("CanLike_NameMC") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return user.hasClaimedNameMCLikeReward() ? "Complete" : "Incomplete";
			}
		}.withDescription("Return Complete/Incomplete depending on whether the player has liked the server on NameMC")
				.updateDataKey("ClaimedNameMCLikeReward"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("DisableBroadcast") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getDisableBroadcast();
			}
		}.withDescription("Returns true/false if user has broadcast disabled").updateDataKey("DisableBroadcast"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("DisableReminders") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return plugin.getVoteRemindersManager().isRemindersEnabled(user.getJavaUUID()) ? "False" : "True";
			}
		}.withDescription("Returns true/false if user has reminders disabled"));

		for (final String identifier : plugin.getShopFile().getShopIdentifiers()) {
			if (plugin.getShopFile().getShopIdentifierLimit(identifier) > 0) {
				placeholders.add(new PlaceHolder<VotingPluginUser>("VoteShopLimit_" + identifier) {

					@Override
					public String placeholderRequest(VotingPluginUser user, String ident) {
						return "" + user.getVoteShopIdentifierLimit(identifier);
					}
				}.withDescription("User voteshop limit for " + identifier).updateDataKey("VoteShopLimit" + identifier));
			}
		}

		for (final TopVoter top : TopVoter.values()) {
			placeholders.add(new PlaceHolder<VotingPluginUser>("Total_" + top.toString()) {

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
					return Integer.toString(user.getTotal(top));
				}
			}.withDescription("User total for " + top.getName()).updateDataKey(top.getColumnName()));
		}

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestDailyTotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestDailyTotal());
			}
		}.withDescription("Best daily total").updateDataKey("HighestDailyTotal"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestWeeklyTotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestWeeklyTotal());
			}
		}.withDescription("Best weekly total").updateDataKey("HighestWeeklyTotal"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestMonthlyTotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestMonthlyTotal());
			}
		}.withDescription("Best monthly total").updateDataKey("HighestMonthlyTotal"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("DailyVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getDayVoteStreak());
			}
		}.withDescription("Current daily votestreak").updateDataKey("DayVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("WeeklyVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getWeekVoteStreak());
			}
		}.withDescription("Current weekly votestreak").updateDataKey("WeekVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("MonthVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getMonthVoteStreak());
			}
		}.withDescription("Current month votestreak").updateDataKey("MonthVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("VoteStreakAmount_") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				String target = identifier.substring("VoteStreakAmount_".length());
				int amount = plugin.getVoteStreakHandler().getVoteStreakAmount(user, target);
				return amount >= 0 ? Integer.toString(amount) : "invalid";
			}
		}.withDescription("Current amount for a VoteStreak ID or progress group").useStartsWith());

		placeholders.add(new PlaceHolder<VotingPluginUser>("VoteStreakBestAmount_") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				String target = identifier.substring("VoteStreakBestAmount_".length());
				int amount = plugin.getVoteStreakHandler().getVoteStreakBestAmount(user, target);
				return amount >= 0 ? Integer.toString(amount) : "invalid";
			}
		}.withDescription("Best amount for a VoteStreak ID or progress group").useStartsWith());

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestDailyVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestDayVoteStreak());
			}
		}.withDescription("Best daily votestreak").updateDataKey("BestDayVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestWeeklyVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestWeekVoteStreak());
			}
		}.withDescription("Best weekly votestreak").updateDataKey("BestWeekVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestMonthVoteStreak") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestMonthVoteStreak());
			}
		}.withDescription("Best month votestreak").updateDataKey("BestMonthVoteStreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Points") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Integer.toString(user.getPoints());
			}
		}.withDescription("User points").updateDataKey("Points"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Points_Format") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				NumberFormat numberFormat = NumberFormat.getNumberInstance(Locale.US);
				return numberFormat.format(user.getPoints());
			}
		}.withDescription("User points").updateDataKey("Points"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("CanVote") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return Boolean.toString(user.canVoteAll());
			}
		}.withDescription("Return true/false if player can vote on all sites").updateDataKey("LastVotes"));

		placeholders.add(platformOwnedWithOfflineWorkerFallback(new PlaceHolder<VotingPluginUser>("CanVoteSites") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getSitesNotVotedOn();
			}
		}.withDescription("Return number of votesites available").updateDataKey("LastVotes")));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Next_AnySite") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				if (user.canVoteAny()) {
					return plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				}
				long smallest = -1;
				HashMap<Long, VoteSite> times = new HashMap<>();
				for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
					long t = user.voteNextDurationTime(site);
					if (smallest == -1) {
						smallest = t;
					}
					if (t < smallest) {
						smallest = t;
					}
					times.put(t, site);
				}
				for (Entry<Long, VoteSite> entry : times.entrySet()) {
					if (entry.getKey().longValue() == smallest) {
						return user.voteCommandNextInfo(entry.getValue());
					}
				}

				return "Error";
			}
		}.withDescription("How long until user can vote on anysite").updateDataKey("LastVotes"));

		placeholders.add(platformOwnedWithOfflineWorkerFallback(new PlaceHolder<VotingPluginUser>("SitesAvailable") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getSitesNotVotedOn();
			}
		}.withDescription("Get number of sites available to be voted on").updateDataKey("LastVotes")));

		placeholders.add(platformOwnedWithOfflineWorkerFallback(new PlaceHolder<VotingPluginUser>("SitesAvailableTotal") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getTotalNumberOfSites();
			}
		}.withDescription("Get total number of sites available to be voted on")));

		for (final VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			placeholders.add(new CalculatingPlaceholder<VotingPluginUser>("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderDataRequest(VotingPluginUser user, String identifier) {
					long time = user.getTime(voteSite);
					return "" + time;
				}

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
					if (getCacheData().containsKey(user.getJavaUUID())) {
						String data = getCacheData().get(user.getJavaUUID());
						if (!data.isEmpty()) {
							long time = Long.valueOf(data);
							return user.voteCommandNextInfo(voteSite, time);
						}
					}
					return user.voteCommandNextInfo(voteSite);
				}
			}.withDescription("How long until user can vote on " + voteSite.getKey()).updateDataKey("LastVotes"));
			placeholders.add(new PlaceHolder<VotingPluginUser>("Last_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
					return user.voteCommandLastDuration(voteSite);
				}
			}.withDescription("How long ago user voted on " + voteSite.getKey()).updateDataKey("LastVotes"));
			placeholders.add(new PlaceHolder<VotingPluginUser>("CanVote_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
					return "" + user.canVoteSite(voteSite);
				}
			}.withDescription("Whether or not player can vote on " + voteSite.getKey()).updateDataKey("LastVotes"));
		}

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_All_Position") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				int num = 1;
				UUID toMatch = UUID.fromString(user.getUUID());
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (entry.getKey().getUuid().equals(toMatch)) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("EnoughPoints_") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				if (!MessageAPI.isInt(identifier.split("_")[1])) {
					return "invalid";
				}
				int number = Integer.parseInt(identifier.split("_")[1]);
				if (user.getPoints() >= number) {
					return "true";
				}
				return "false";
			}
		}.withDescription("Return true/false if player has said points").updateDataKey("Points").useStartsWith());

		// VoteMilestones placeholders (explicit per group, like
		// VoteShopLimit_<identifier>)
		// Base/default (no group suffix)
		placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneNext") {
			@Override
			public String placeholderRequest(VotingPluginUser user, String ident) {
				Long next = plugin.getVoteMilestonesManager().getNextMilestoneValue("default", user);
				return next == null ? "none" : Long.toString(next);
			}
		}.withDescription("Next VoteMilestone value for default group").updateDataKey("LastVotes"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneLast") {
			@Override
			public String placeholderRequest(VotingPluginUser user, String ident) {
				Long last = plugin.getVoteMilestonesManager().getLastMilestoneValue("default", user);
				return last == null ? "none" : Long.toString(last);
			}
		}.withDescription("Last achieved VoteMilestone value for default group").updateDataKey("LastVotes"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneVotesUntilNext") {
			@Override
			public String placeholderRequest(VotingPluginUser user, String ident) {
				return Long.toString(plugin.getVoteMilestonesManager().getVotesUntilNextMilestone("default", user));
			}
		}.withDescription("Votes until next VoteMilestone for default group").updateDataKey("LastVotes"));

		Set<String> groups = new HashSet<>();
		groups.add("default");

		for (VoteMilestone m : plugin.getVoteMilestonesManager().getConfig().getMilestones().values()) {
			String g = m.getGroupKey();
			if (g == null)
				continue;
			g = g.trim().toLowerCase(Locale.ROOT);
			if (!g.isEmpty())
				groups.add(g);
		}

		for (final String groupIdRaw : groups) {
			final String groupId = (groupIdRaw == null || groupIdRaw.trim().isEmpty()) ? "default" : groupIdRaw.trim();

			placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneNext_" + groupId) {
				@Override
				public String placeholderRequest(VotingPluginUser user, String ident) {
					Long next = plugin.getVoteMilestonesManager().getNextMilestoneValue(groupId, user);
					return next == null ? "none" : Long.toString(next);
				}
			}.withDescription("Next VoteMilestone value for group " + groupId).updateDataKey("LastVotes"));

			placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneLast_" + groupId) {
				@Override
				public String placeholderRequest(VotingPluginUser user, String ident) {
					Long last = plugin.getVoteMilestonesManager().getLastMilestoneValue(groupId, user);
					return last == null ? "none" : Long.toString(last);
				}
			}.withDescription("Last achieved VoteMilestone value for group " + groupId).updateDataKey("LastVotes"));

			placeholders.add(new PlaceHolder<VotingPluginUser>("VoteMilestoneVotesUntilNext_" + groupId) {
				@Override
				public String placeholderRequest(VotingPluginUser user, String ident) {
					return Long.toString(plugin.getVoteMilestonesManager().getVotesUntilNextMilestone(groupId, user));
				}
			}.withDescription("Votes until next VoteMilestone for group " + groupId).updateDataKey("LastVotes"));
		}

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_AllVotes_") {

			@Override
			public String placeholderRequest(String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_All_") {

			@Override
			public String placeholderRequest(String identifier) {
				if (!MessageAPI.isInt(identifier.split("_")[2])) {
					return "invalid";
				}
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get username at postion in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Month_Position") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				int num = 1;
				UUID toMatch = UUID.fromString(user.getUUID());
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (entry.getKey().getUuid().equals(toMatch)) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user's current top voter position"));

		if (plugin.getGui().isLastMonthGUI()) {
			placeholders.add(new PlaceHolder<VotingPluginUser>("Top_LastMonth_Position") {

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
					int num = 1;
					UUID toMatch = UUID.fromString(user.getUUID());
					for (Entry<TopVoterPlayer, Integer> entry : plugin.getLastMonthTopVoter().entrySet()) {
						if (entry.getKey().getUuid().equals(toMatch)) {
							return "" + num;
						}
						num++;
					}
					return "";
				}
			}.withDescription("Get user top voter position for lastmonth"));
			nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_LastMonth_") {

				@Override
				public String placeholderRequest(String identifier) {
					if (!MessageAPI.isInt(identifier.split("_")[2])) {
						return "invalid";
					}
					int num = 1;
					int number = Integer.parseInt(identifier.split("_")[2]);
					for (Entry<TopVoterPlayer, Integer> entry : plugin.getLastMonthTopVoter().entrySet()) {
						if (num == number) {
							return entry.getKey().getPlayerName();
						}
						num++;
					}
					return "";
				}
			}.useStartsWith().withDescription("Get user at position in last month top voter"));

			nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_LastMonthVotes_") {

				@Override
				public String placeholderRequest(String identifier) {
					int num = 1;
					int number = Integer.parseInt(identifier.split("_")[2]);
					for (Entry<TopVoterPlayer, Integer> entry : plugin.getLastMonthTopVoter().entrySet()) {
						if (num == number) {
							return "" + entry.getValue().intValue();
						}
						num++;
					}
					return "";
				}
			}.useStartsWith().withDescription("Get user votes at position in last month top voter"));

		}

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_Month_") {

			@Override
			public String placeholderRequest(String identifier) {
				if (!MessageAPI.isInt(identifier.split("_")[2])) {
					return "invalid";
				}
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get username at position in top voter"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_MonthVotes_") {

			@Override
			public String placeholderRequest(String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Week_Position") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				int num = 1;
				UUID toMatch = UUID.fromString(user.getUUID());
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (entry.getKey().getUuid().equals(toMatch)) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_Week_") {

			@Override
			public String placeholderRequest(String identifier) {
				if (!MessageAPI.isInt(identifier.split("_")[2])) {
					return "invalid";
				}
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get username at postion in top voter"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_WeekVotes_") {

			@Override
			public String placeholderRequest(String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Daily_Position") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				int num = 1;
				UUID toMatch = UUID.fromString(user.getUUID());
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (entry.getKey().getUuid().equals(toMatch)) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_Daily_") {

			@Override
			public String placeholderRequest(String identifier) {
				if (!MessageAPI.isInt(identifier.split("_")[2])) {
					return "invalid";
				}
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get username at postion in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("VotePartyContributedVotes") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getVotePartyVotes();
			}
		}.useStartsWith().withDescription("See vote party placeholders contributed").updateDataKey("VotePartyVotes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_DailyVotes_") {

			@Override
			public String placeholderRequest(String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		// non players

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("VotePartyVotesCurrent") {

			@Override
			public String placeholderRequest(String identifier) {
				return Integer.toString(plugin.getVoteParty().getTotalVotes());
			}
		}.withDescription("Current amount of voteparty votes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("VotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(String identifier) {
				return Integer.toString(plugin.getVoteParty().getNeededVotes());
			}
		}.withDescription("Voteparty votes needed"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("VotePartyVotesRequired") {

			@Override
			public String placeholderRequest(String identifier) {
				return Integer.toString(plugin.getVoteParty().getVotesRequired());
			}
		}.withDescription("Amount of votes needed for voteparty"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("BungeeVotePartyVotesCurrent") {

			@Override
			public String placeholderRequest(String identifier) {
				if (plugin.getBackendProxyHandler() != null) {
					return Integer.toString(plugin.getBackendProxyHandler().getBungeeVotePartyCurrent());
				}
				return "-1";
			}
		}.withDescription("Current amount of bungee voteparty votes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("BungeeVotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(String identifier) {
				if (plugin.getBackendProxyHandler() != null) {
					return Integer.toString(plugin.getBackendProxyHandler().getBungeeVotePartyRequired()
							- plugin.getBackendProxyHandler().getBungeeVotePartyCurrent());
				}
				return "-1";
			}
		}.withDescription("Voteparty bungee votes needed"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("BungeeVotePartyVotesRequired") {

			@Override
			public String placeholderRequest(String identifier) {
				if (plugin.getBackendProxyHandler() != null) {
					return Integer.toString(plugin.getBackendProxyHandler().getBungeeVotePartyRequired());
				}
				return "-1";
			}
		}.withDescription("Amount of votes needed for bungee  voteparty"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("GlobalMonthTotal") {

			@Override
			public String placeholderRequest(String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Monthly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global month total"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("GlobalAllTimeTotal") {

			@Override
			public String placeholderRequest(String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.AllTime).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global alltime total"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("GlobalWeeklyTotal") {

			@Override
			public String placeholderRequest(String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Weekly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global weekly total"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("GlobalDailyTotal") {

			@Override
			public String placeholderRequest(String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Daily).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global daily total"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("TimeUntilDayReset") {

			@Override
			public String placeholderRequest(String identifier) {
				LocalDateTime now = plugin.getTimeChecker().getTime();
				LocalDateTime offsetoclocktoday = plugin.getTimeChecker().getTime().withHour(0).withMinute(0);
				LocalDateTime offsetoclocktomorrow = plugin.getTimeChecker().getTime().plusDays(1).withHour(0)
						.withMinute(0);

				String timeMsg = "%hours% Hours %minutes% Minutes";
				Duration dur = null;
				if (!now.isBefore(offsetoclocktoday)) {
					dur = Duration.between(now, offsetoclocktomorrow);

				} else {
					dur = Duration.between(now, offsetoclocktoday);
				}
				int diffHours = (int) (dur.getSeconds() / (60 * 60));
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

				if (diffHours < 0) {
					diffHours = diffHours * -1;
				}
				if (diffHours >= 24) {
					diffHours = diffHours - 24;
				}
				if (diffMinutes < 0) {
					diffMinutes = diffMinutes * -1;
				}

				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
				return timeMsg;

			}
		}.withDescription("Time until plugin time day changes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("TimeUntilWeekReset") {

			@Override
			public String placeholderRequest(String identifier) {
				LocalDateTime now = plugin.getTimeChecker().getTime().plusDays(plugin.getOptions().getTimeWeekOffSet());
				LocalDateTime newWeek = plugin.getTimeChecker().getTime().withHour(0).withMinute(0);

				TemporalField woy = WeekFields.of(Locale.getDefault()).weekOfWeekBasedYear();
				int weekNumber = newWeek.get(woy);
				int newWeekNumber = weekNumber;

				while (weekNumber == newWeekNumber) {
					newWeek = newWeek.plusDays(1);
					newWeekNumber = newWeek.get(woy);
				}

				String timeMsg = "%days% Days %hours% Hours %minutes% Minutes";
				Duration dur = Duration.between(now, newWeek);

				int diffDays = (int) (dur.getSeconds() / (60 * 60 * 24));
				int diffHours = (int) (dur.getSeconds() / (60 * 60) - diffDays * 24);
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60 - diffDays * 24 * 60;

				if (diffHours < 0) {
					diffHours = diffHours * -1;
				}
				if (diffHours >= 24) {
					diffHours = diffHours - 24;
				}
				if (diffMinutes < 0) {
					diffMinutes = diffMinutes * -1;
				}

				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%days%", Integer.toString(diffDays));
				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
				return timeMsg;

			}
		}.withDescription("Time until plugin time week changes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("TimeUntilMonthReset") {

			@Override
			public String placeholderRequest(String identifier) {
				LocalDateTime now = plugin.getTimeChecker().getTime();
				LocalDateTime newMonth = plugin.getTimeChecker().getTime().plusMonths(1).withDayOfMonth(1).withHour(0)
						.withMinute(0);

				String timeMsg = "%days% Days %hours% Hours %minutes% Minutes";
				Duration dur = Duration.between(now, newMonth);

				int diffDays = (int) (dur.getSeconds() / (60 * 60 * 24));
				int diffHours = (int) (dur.getSeconds() / (60 * 60) - diffDays * 24);
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60 - diffDays * 24 * 60;

				if (diffHours < 0) {
					diffHours = diffHours * -1;
				}
				if (diffHours >= 24) {
					diffHours = diffHours - 24;
				}
				if (diffMinutes < 0) {
					diffMinutes = diffMinutes * -1;
				}

				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%days%", Integer.toString(diffDays));
				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
				return timeMsg;

			}
		}.withDescription("Time until plugin time month changes"));

		Set<String> placeholdersSet = new HashSet<>();
		placeholdersSet.addAll(plugin.getConfigFile().getCachedPlaceholders());
		if (getCacheLevel().equals(PlaceholderCacheLevel.AUTO)) {
			placeholdersSet.addAll(plugin.getServerData().getAutoCachedPlaceholder());
		}

		for (String toCache : placeholdersSet) {
			if (toCache.startsWith("votingplugin")) {
				toCache = toCache.substring("votingplugin_".length());
			}
			for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
				if (placeholder.matches(toCache)) {
					placeholder.setUseCache(true, toCache);
					cachedPlaceholders.add(toCache);
					plugin.extraDebug("Caching placeholder " + toCache);
				}
			}

			for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
				if (placeholder.matches(toCache)) {
					placeholder.setUseCache(true, toCache);
					cachedPlaceholders.add(toCache);
					plugin.extraDebug("Caching placeholder " + toCache);
				}
			}
		}

		publishUserDataChangePlaceholders();
		if (!userDataChangeListenerRegistered) {
			plugin.getUserManager().getUserDataChange().add(new UserDataChanged() {

				@Override
				public void onChange(AdvancedCoreUser user, String... keys) {
					onUserDataChange(user, keys);
				}
			});
			userDataChangeListenerRegistered = true;
		}
	}

	PlaceHolder<VotingPluginUser> platformOwned(PlaceHolder<VotingPluginUser> placeholder) {
		platformOwnedPlaceholders.add(placeholder);
		return placeholder;
	}

	PlaceHolder<VotingPluginUser> platformOwnedWithOfflineWorkerFallback(
			PlaceHolder<VotingPluginUser> placeholder) {
		offlineWorkerPlaceholders.add(placeholder);
		return platformOwned(placeholder);
	}

	void publishUserDataChangePlaceholders() {
		userDataChangeClassification = new PlaceholderClassification(List.copyOf(placeholders),
				Set.copyOf(platformOwnedPlaceholders), Set.copyOf(offlineWorkerPlaceholders));
	}

	void onUserDataChange(AdvancedCoreUser user, String... keys) {
		if (user == null || keys == null) return;
		VotingPluginUser vpUser = plugin.getVotingPluginUserManager().getVotingPluginUser(user);
		if (vpUser == null) return;
		if (!vpUser.isCached()) vpUser.userDataFetechMode(UserDataFetchMode.NO_CACHE);

		UUID uuid = user.getJavaUUID();
		List<PlaceHolder<VotingPluginUser>> platformUpdates = new ArrayList<>();
		List<PlaceHolder<VotingPluginUser>> offlineUpdates = new ArrayList<>();
		PlaceholderClassification classification = userDataChangeClassification;
		Player owner = plugin.getPlaceholderPlayerPresence().schedulerOwner(uuid);
		for (PlaceHolder<VotingPluginUser> placeholder : classification.all()) {
			if (!shouldRefresh(placeholder, uuid, keys)) continue;
			if (classification.platform().contains(placeholder)) {
				if (owner != null) platformUpdates.add(placeholder);
				else if (classification.offlineWorker().contains(placeholder)) offlineUpdates.add(placeholder);
			}
			else updateCachedPlaceholder(placeholder, vpUser, uuid, keys);
		}

		if (!offlineUpdates.isEmpty()) updateOfflinePlatformPlaceholders(uuid, vpUser, offlineUpdates, keys);
		if (platformUpdates.isEmpty() || !plugin.isEnabled()) return;
		schedulePlatformUpdates(uuid, vpUser, List.copyOf(platformUpdates), keys.clone());
	}

	private void schedulePlatformUpdates(UUID uuid, VotingPluginUser user,
			List<PlaceHolder<VotingPluginUser>> platformUpdates, String[] keys) {
		IdentityHashMap<PlaceHolder<VotingPluginUser>, Long> generations = new IdentityHashMap<>();
		for (PlaceHolder<VotingPluginUser> placeholder : platformUpdates) {
			long generation = platformUpdateGenerations.compute(new PlatformUpdateKey(uuid, placeholder),
					(key, current) -> platformUpdateSequence.incrementAndGet());
			generations.put(placeholder, generation);
		}
		schedulePlatformUpdates(uuid, user, platformUpdates, keys,
				capturePlatformPlaceholderSnapshot(user, platformUpdates), generations);
	}

	private void schedulePlatformUpdates(UUID uuid, VotingPluginUser user,
			List<PlaceHolder<VotingPluginUser>> platformUpdates, String[] keys,
			PlatformPlaceholderSnapshot snapshot,
			IdentityHashMap<PlaceHolder<VotingPluginUser>, Long> generations) {
		if (!hasCurrentPlatformUpdate(uuid, platformUpdates, generations)) return;
		Player owner = plugin.getPlaceholderPlayerPresence().schedulerOwner(uuid);
		if (owner == null) {
			dispatchOfflinePlatformUpdates(uuid, platformUpdates, keys, generations);
			return;
		}
		Runnable update = () -> {
			if (!plugin.isEnabled() || !hasCurrentPlatformUpdate(uuid, platformUpdates, generations)) return;
			if (plugin.getPlaceholderPlayerPresence().schedulerOwner(uuid) != owner) {
				schedulePlatformUpdates(uuid, user, platformUpdates, keys, snapshot, generations);
				return;
			}
			if (getCacheLevel().onlineOnly() && !plugin.getPlaceholderPlayerPresence().isOnline(uuid)) return;
			for (PlaceHolder<VotingPluginUser> placeholder : platformUpdates) {
				if (!isCurrentPlatformUpdate(uuid, placeholder, generations)) continue;
				if (!shouldRefresh(placeholder, uuid, keys)) continue;
				String identifier = placeholder.getIdentifier();
				if (identifier.equalsIgnoreCase("CanVoteSites") || identifier.equalsIgnoreCase("SitesAvailable")) {
					publishCachedValue(placeholder, user, uuid,
							Integer.toString(snapshot.sitesAvailable(user)), keys);
				} else updateCachedPlaceholder(placeholder, user, uuid, keys);
			}
		};
		BukkitCompletionScheduler.run(plugin, owner, update, () -> {
			if (!plugin.isEnabled() || !hasCurrentPlatformUpdate(uuid, platformUpdates, generations)) return;
			plugin.getPlaceholderPlayerPresence().playerOffline(uuid, owner);
			dispatchOfflinePlatformUpdates(uuid, platformUpdates, keys, generations);
		}, () -> { });
	}

	private boolean hasCurrentPlatformUpdate(UUID uuid, List<PlaceHolder<VotingPluginUser>> placeholdersToUpdate,
			IdentityHashMap<PlaceHolder<VotingPluginUser>, Long> generations) {
		return placeholdersToUpdate.stream().anyMatch(placeholder ->
				isCurrentPlatformUpdate(uuid, placeholder, generations));
	}

	private boolean isCurrentPlatformUpdate(UUID uuid, PlaceHolder<VotingPluginUser> placeholder,
			IdentityHashMap<PlaceHolder<VotingPluginUser>, Long> generations) {
		return generations.get(placeholder) != null && generations.get(placeholder).equals(
				platformUpdateGenerations.get(new PlatformUpdateKey(uuid, placeholder)));
	}

	private PlatformPlaceholderSnapshot capturePlatformPlaceholderSnapshot(VotingPluginUser user,
			List<PlaceHolder<VotingPluginUser>> platformUpdates) {
		boolean needsVoteEligibility = platformUpdates.stream().map(PlaceHolder::getIdentifier)
				.anyMatch(identifier -> identifier.equalsIgnoreCase("CanVoteSites")
						|| identifier.equalsIgnoreCase("SitesAvailable"));
		if (!needsVoteEligibility) return PlatformPlaceholderSnapshot.EMPTY;
		List<String> votableSitePermissions = new ArrayList<>();
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!site.isHidden() && user.canVoteSite(site)) {
				votableSitePermissions.add(site.getPermissionToView());
			}
		}
		return new PlatformPlaceholderSnapshot(List.copyOf(votableSitePermissions));
	}

	private record PlatformPlaceholderSnapshot(List<String> votableSitePermissions) {
		private static final PlatformPlaceholderSnapshot EMPTY = new PlatformPlaceholderSnapshot(List.of());

		private int sitesAvailable(VotingPluginUser user) {
			int amount = 0;
			for (String permission : votableSitePermissions) {
				if (permission.isEmpty() || user.hasPermission(permission, false)) amount++;
			}
			return amount;
		}
	}

	private void dispatchOfflinePlatformUpdates(UUID uuid,
			List<PlaceHolder<VotingPluginUser>> platformUpdates, String[] keys) {
		dispatchOfflinePlatformUpdates(uuid, platformUpdates, keys, null);
	}

	private void dispatchOfflinePlatformUpdates(UUID uuid,
			List<PlaceHolder<VotingPluginUser>> platformUpdates, String[] keys,
			IdentityHashMap<PlaceHolder<VotingPluginUser>, Long> generations) {
		try {
			plugin.getUserManager().getDataManager().getTimer().execute(() -> {
				if (!plugin.isEnabled()) return;
				List<PlaceHolder<VotingPluginUser>> currentUpdates = generations == null ? platformUpdates
						: platformUpdates.stream()
								.filter(placeholder -> isCurrentPlatformUpdate(uuid, placeholder, generations))
								.toList();
				if (currentUpdates.isEmpty()) return;
				VotingPluginUser currentUser = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
				if (currentUser == null) return;
				if (plugin.getPlaceholderPlayerPresence().schedulerOwner(uuid) != null) {
					schedulePlatformUpdates(uuid, currentUser, currentUpdates, keys);
					return;
				}
				if (getCacheLevel().onlineOnly()) return;
				PlaceholderClassification classification = userDataChangeClassification;
				List<PlaceHolder<VotingPluginUser>> offlineUpdates = currentUpdates.stream()
						.filter(classification.offlineWorker()::contains)
						.filter(placeholder -> shouldRefresh(placeholder, uuid, keys))
						.toList();
				if (!offlineUpdates.isEmpty()) updateOfflinePlatformPlaceholders(uuid, currentUser, offlineUpdates, keys);
			});
		} catch (RejectedExecutionException rejected) {
			plugin.debug(rejected);
		}
	}

	private void updateOfflinePlatformPlaceholders(UUID uuid, VotingPluginUser user,
			List<PlaceHolder<VotingPluginUser>> placeholdersToUpdate, String... keys) {
		IdentityHashMap<PlaceHolder<VotingPluginUser>, String> values = new IdentityHashMap<>();
		for (PlaceHolder<VotingPluginUser> placeholder : placeholdersToUpdate) {
			String value = placeholder.getIdentifier().equalsIgnoreCase("SitesAvailableTotal")
					? Integer.toString(user.getTotalNumberOfSitesWithoutOnlinePermissions())
					: Integer.toString(user.getSitesNotVotedOnWithoutOnlinePermissions());
			values.put(placeholder, value);
		}
		boolean published = plugin.getPlaceholderPlayerPresence().runIfOffline(uuid, () -> {
			for (PlaceHolder<VotingPluginUser> placeholder : placeholdersToUpdate) {
				for (String ident : Set.copyOf(placeholder.getCache().keySet())) {
					ConcurrentHashMap<UUID, String> cachedValues = placeholder.getCache().get(ident);
					if (cachedValues != null) cachedValues.put(uuid, values.get(placeholder));
				}
			}
		});
		if (published) {
			for (PlaceHolder<VotingPluginUser> placeholder : placeholdersToUpdate) {
				plugin.devDebug("Updated offline placeholder cache for " + user.getUUID() + " on "
						+ matchingKey(placeholder, keys) + " with " + values.get(placeholder));
			}
		} else if (plugin.isEnabled()) {
			schedulePlatformUpdates(uuid, user, List.copyOf(placeholdersToUpdate), keys.clone());
		}
	}

	private boolean shouldRefresh(PlaceHolder<VotingPluginUser> placeholder, UUID uuid, String... keys) {
		if (!placeholder.isUsesCache() || placeholder.getCache() == null) return false;
		if (getCacheLevel().onlineOnly() && !plugin.getPlaceholderPlayerPresence().isOnline(uuid)) return false;
		for (String key : keys) {
			if (key != null && placeholder.getUpdateDataKey().equalsIgnoreCase(key)) return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	private void updateCachedPlaceholder(PlaceHolder<VotingPluginUser> placeholder, VotingPluginUser user,
			UUID uuid, String... keys) {
		for (String ident : Set.copyOf(placeholder.getCache().keySet())) {
			ConcurrentHashMap<UUID, String> values = placeholder.getCache().get(ident);
			if (values == null) continue;
			if (placeholder instanceof CalculatingPlaceholder<?>) {
				CalculatingPlaceholder<VotingPluginUser> calculating =
						(CalculatingPlaceholder<VotingPluginUser>) placeholder;
				synchronized (calculating) {
					calculating.getCacheData().put(uuid, calculating.placeholderDataRequest(user, ident));
					String value = calculating.placeholderRequest(user, ident);
					values.put(uuid, value);
					if (getCacheLevel().onlineOnly() && !plugin.getPlaceholderPlayerPresence().isOnline(uuid)) {
						values.remove(uuid);
						calculating.getCacheData().remove(uuid);
						continue;
					}
					plugin.devDebug("Updated calculating placeholder cache for " + user.getUUID() + " on "
							+ matchingKey(placeholder, keys) + " with " + value);
				}
			} else {
				String value = placeholder.placeholderRequest(user, ident);
				values.put(uuid, value);
				if (getCacheLevel().onlineOnly() && !plugin.getPlaceholderPlayerPresence().isOnline(uuid)) {
					values.remove(uuid);
					continue;
				}
				plugin.devDebug("Updated placeholder cache for " + user.getUUID() + " on "
						+ matchingKey(placeholder, keys) + " with " + value);
			}
		}
	}

	private void publishCachedValue(PlaceHolder<VotingPluginUser> placeholder, VotingPluginUser user, UUID uuid,
			String value, String... keys) {
		for (String ident : Set.copyOf(placeholder.getCache().keySet())) {
			ConcurrentHashMap<UUID, String> values = placeholder.getCache().get(ident);
			if (values == null) continue;
			values.put(uuid, value);
			if (getCacheLevel().onlineOnly() && !plugin.getPlaceholderPlayerPresence().isOnline(uuid)) {
				values.remove(uuid);
				continue;
			}
			plugin.devDebug("Updated placeholder cache for " + user.getUUID() + " on "
					+ matchingKey(placeholder, keys) + " with " + value);
		}
	}

	private String matchingKey(PlaceHolder<VotingPluginUser> placeholder, String... keys) {
		for (String key : keys) {
			if (key != null && placeholder.getUpdateDataKey().equalsIgnoreCase(key)) return key;
		}
		return placeholder.getUpdateDataKey();
	}
	
	/**
	 * Handle player logout.
	 * @param user the voting plugin user
	 */
	public void onLogout(VotingPluginUser user) {
		if (user != null) onLogout(user.getJavaUUID());
	}

	/** Clears online-only cache entries without resolving user storage. */
	public void onLogout(UUID uuid) {
		platformUpdateGenerations.keySet().removeIf(key -> key.uuid().equals(uuid));
		if (getCacheLevel().onlineOnly()) {
			PlaceholderClassification classification = userDataChangeClassification;
			for (PlaceHolder<VotingPluginUser> placeholder : classification.all()) {
				if (placeholder.isUsesCache()) {
					if (placeholder instanceof CalculatingPlaceholder<?>) {
						synchronized (placeholder) { placeholder.clearCachePlayer(uuid); }
					} else placeholder.clearCachePlayer(uuid);
				}
			}
		}
	}

	/**
	 * Update all placeholders.
	 */
	public void onUpdate() {
		checkNonCachedPlaceholders();
		for (Player p : Bukkit.getOnlinePlayers()) {
			onUpdate(getPresenceUser(p), true);
		}
		/*
		 * for (NonPlayerPlaceHolder<VotingPluginUser> placeholder :
		 * nonPlayerPlaceholders) { if (placeholder.isUsesCache()) { for (String ident :
		 * placeholder.getCache().keySet()) { if (ident != null) { String str =
		 * placeholder.placeholderRequest(ident); if (str != null) {
		 * placeholder.getCache().put(ident, str); } } else {
		 * plugin.debug("ident null: " + placeholder.getIdentifier()); } } } }
		 */

	}

	/**
	 * Update placeholders for specific user.
	 * @param user the voting plugin user
	 * @param login whether this is a login update
	 */
	public void onUpdate(VotingPluginUser user, boolean login) {
		if (user == null) return;
		UUID uuid = user.getJavaUUID();
		Player owner = plugin.getPlaceholderPlayerPresence().schedulerOwner(uuid);
		PlaceholderClassification classification = userDataChangeClassification;
		List<PlaceHolder<VotingPluginUser>> platformUpdates = new ArrayList<>();
		List<PlaceHolder<VotingPluginUser>> offlineUpdates = new ArrayList<>();
		for (PlaceHolder<VotingPluginUser> placeholder : classification.all()) {
			if (!placeholder.isUsesCache() || placeholder.getCache() == null
					|| (!placeholder.isCached(placeholder.getIdentifier(), uuid) && !login)) continue;
			if (classification.platform().contains(placeholder)) {
				if (owner != null) platformUpdates.add(placeholder);
				else if (!getCacheLevel().onlineOnly() && classification.offlineWorker().contains(placeholder)) {
					offlineUpdates.add(placeholder);
				}
			} else updateCachedPlaceholder(placeholder, user, uuid, placeholder.getUpdateDataKey());
		}
		if (!offlineUpdates.isEmpty()) {
			updateOfflinePlatformPlaceholders(uuid, user, offlineUpdates,
					offlineUpdates.stream().map(PlaceHolder::getUpdateDataKey).toArray(String[]::new));
		}
		if (!platformUpdates.isEmpty() && plugin.isEnabled()) {
			schedulePlatformUpdates(uuid, user, List.copyOf(platformUpdates),
					platformUpdates.stream().map(PlaceHolder::getUpdateDataKey).toArray(String[]::new));
		}
	}

	/**
	 * Handle vote party update.
	 */
	public void onVotePartyUpdate() {
		/*
		 * for (NonPlayerPlaceHolder<VotingPluginUser> placeholder :
		 * nonPlayerPlaceholders) { if (placeholder.isUsesCache()) { if
		 * (placeholder.getIdentifier().startsWith("VoteParty")) { for (String ident :
		 * placeholder.getCache().keySet()) { placeholder.getCache().put(ident,
		 * placeholder.placeholderRequest(ident)); } } } }
		 */
	}

	/**
	 * Reload placeholders.
	 */
	public void reload() {
		platformUpdateGenerations.clear();
		plugin.refreshPlaceholderPlayerPresence();
		cacheLevel = plugin.getConfigFile().getPlaceholderCacheLevel();
		onUpdate();
		if (!cacheLevel.equals(PlaceholderCacheLevel.NONE)) {
			for (Player player : Bukkit.getOnlinePlayers()) {
				onUpdate(getPresenceUser(player), player.isOnline());
			}
		}
	}

	private VotingPluginUser getPresenceUser(Player player) {
		UUID storageUuid = plugin.getPlaceholderPlayerPresence().storageUuid(player);
		if (storageUuid != null) {
			return plugin.getVotingPluginUserManager().getVotingPluginUser(storageUuid);
		}
		return plugin.getVotingPluginUserManager().getVotingPluginUser(player);
	}

	VotingPluginUser resolvePlaceholderUser(OfflinePlayer player) {
		if (player instanceof Player onlinePlayer) {
			UUID storageUuid = plugin.getPlaceholderPlayerPresence().storageUuid(onlinePlayer);
			if (storageUuid != null) {
				return plugin.getVotingPluginUserManager().getVotingPluginUser(storageUuid);
			}
		}
		return plugin.getVotingPluginUserManager().getVotingPluginUser(player);
	}

	/**
	 * Schedule placeholder check for user.
	 * @param user the voting plugin user
	 */
	public void schedulePlaceholderCheck(VotingPluginUser user) {
		plugin.getTimer().execute(new Runnable() {

			@Override
			public void run() {
				checkNonCachedPlaceholders();
				onUpdate(user, plugin.getPlaceholderPlayerPresence().isOnline(user.getJavaUUID()));
			}
		});
	}
}
