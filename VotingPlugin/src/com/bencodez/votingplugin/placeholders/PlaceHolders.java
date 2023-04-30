package com.bencodez.votingplugin.placeholders;

import java.text.NumberFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.placeholder.NonPlayerPlaceHolder;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserDataChanged;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlaceHolders.
 */
public class PlaceHolders {

	@Getter
	private ArrayList<NonPlayerPlaceHolder<VotingPluginUser>> nonPlayerPlaceholders = new ArrayList<NonPlayerPlaceHolder<VotingPluginUser>>();

	@Getter
	private ArrayList<PlaceHolder<VotingPluginUser>> placeholders = new ArrayList<PlaceHolder<VotingPluginUser>>();

	private VotingPluginMain plugin;

	public PlaceHolders(VotingPluginMain plugin) {
		this.plugin = plugin;
		cacheLevel = plugin.getConfigFile().getPlaceholderCacheLevel();
	}

	public void reload() {
		cacheLevel = plugin.getConfigFile().getPlaceholderCacheLevel();
		onUpdate();
		if (!cacheLevel.equals(PlaceholderCacheLevel.NONE)) {
			for (Player player : Bukkit.getOnlinePlayers()) {
				onUpdate(UserManager.getInstance().getVotingPluginUser(player));
			}
		}
	}

	private ArrayList<String> cachedPlaceholders = new ArrayList<String>();

	private ConcurrentLinkedQueue<String> placeholdersToSetCacheOn = new ConcurrentLinkedQueue<String>();

	@Getter
	private PlaceholderCacheLevel cacheLevel;

	public String getPlaceHolder(OfflinePlayer p, String identifier) {
		return getPlaceHolder(p, identifier, true);
	}

	public String getPlaceHolder(OfflinePlayer p, String identifier1, boolean javascript) {
		boolean forceProcess = false;
		boolean useCache = true;
		String identifier = identifier1.toLowerCase();
		boolean custom = false;
		if (identifier.endsWith("_process")) {
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

		if (custom && plugin.getConfigFile().getCustomPlaceholderReturns().contains(identifier)) {
			String returnStr = plugin.getConfigFile().getCustomPlaceholderReturns(identifier,
					getPlaceholderValue(p, identifier, javascript, true, useCache));
			if (!returnStr.isEmpty()) {
				return returnStr;
			}
			return "No value";
		} else {
			return getPlaceholderValue(p, identifier, javascript, forceProcess, useCache);
		}

	}

	public String getPlaceholderValue(OfflinePlayer p, String identifier, boolean javascript, boolean forceProcess,
			boolean useCache) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders() && javascript && p != null) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}

		if (!identifier.endsWith("Position")) {
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

		VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(p);
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
		if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders()) {
			if (user.hasPrimaryAccount()) {
				user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
			}
		}

		for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
			try {
				if (placeholder.matches(identifier)) {
					if (useCache) {
						if (placeholder.isUsesCache() && placeholder.isCached(identifier)) {
							ConcurrentHashMap<UUID, String> cache = placeholder.getCache().get(identifier);
							if (cache.containsKey(p.getUniqueId())) {
								return cache.get(p.getUniqueId());
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
					} else {
						return placeholder.placeholderRequest(user, identifier);
					}
				}
			} catch (Exception e) {
				plugin.debug(e);
				return "Error";
			}
		}

		return "Not a valid placeholder";
	}

	public String getPlaceHolder(Player p, String identifier) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders()) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}
		return getPlaceHolder(p, identifier, false);
	}

	public void load() {
		placeholders.clear();
		nonPlayerPlaceholders.clear();

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

		placeholders.add(new PlaceHolder<VotingPluginUser>("DisableBroadcast") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getDisableBroadcast();
			}
		}.withDescription("Returns true/false if user has broadcast disabled").updateDataKey("DisableBroadcast"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("MilestoneCount") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getMilestoneCount();
			}
		}.withDescription("User milestonecount").updateDataKey("MilestoneCount"));

		Set<String> mVotes = plugin.getSpecialRewardsConfig().getMilestoneVotes();
		for (String vote : mVotes) {
			if (StringParser.getInstance().isInt(vote)) {

				final int num = Integer.parseInt(vote);
				if (plugin.getSpecialRewardsConfig().getMilestoneRewardEnabled(num)) {
					placeholders.add(new PlaceHolder<VotingPluginUser>("milestone_numberofvotesuntil_" + num) {

						@Override
						public String placeholderRequest(VotingPluginUser user, String identifier) {
							int toGo = num - user.getMilestoneCount();
							if (toGo >= 0) {
								return "" + toGo;
							}
							return "0";
						}
					}.withDescription("Get number of votes until milestone").updateDataKey("MilestoneCount"));
				}
			}
		}

		for (final String identifier : plugin.getGui().getChestShopIdentifiers()) {
			if (plugin.getGui().getChestShopIdentifierLimit(identifier) > 0) {
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

		placeholders.add(new PlaceHolder<VotingPluginUser>("CanVoteSites") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				return "" + user.getSitesNotVotedOn();
			}
		}.withDescription("Return number of votesites available").updateDataKey("LastVotes"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Next_AnySite") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				if (user.canVoteAny()) {
					return plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				}
				long smallest = -1;
				HashMap<Long, VoteSite> times = new HashMap<Long, VoteSite>();
				for (VoteSite site : plugin.getVoteSites()) {
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

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder<VotingPluginUser>("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(VotingPluginUser user, String identifier) {
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

		placeholders.add(new PlaceHolder<VotingPluginUser>("Points_") {

			@Override
			public String placeholderRequest(VotingPluginUser user, String identifier) {
				int number = Integer.parseInt(identifier.split("_")[1]);
				if (user.getPoints() >= number) {
					return "true";
				}
				return "false";
			}
		}.withDescription("Return true/false if player has said points").updateDataKey("Points").useStartsWith());

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
				if (!StringParser.getInstance().isInt(identifier.split("_")[2])) {
					return null;
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
		}.useStartsWith().withDescription("Get user at postion in top voter"));

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
		}.withDescription("Get user top voter position"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("Top_Month_") {

			@Override
			public String placeholderRequest(String identifier) {
				if (!StringParser.getInstance().isInt(identifier.split("_")[2])) {
					return null;
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
		}.useStartsWith().withDescription("Get user at position in top voter"));

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
				if (!StringParser.getInstance().isInt(identifier.split("_")[2])) {
					return null;
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
		}.useStartsWith().withDescription("Get user at postion in top voter"));

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
				if (!StringParser.getInstance().isInt(identifier.split("_")[2])) {
					return null;
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
		}.useStartsWith().withDescription("Get user at postion in top voter"));

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
				if (plugin.getBungeeHandler() != null) {
					return Integer.toString(plugin.getBungeeHandler().getBungeeVotePartyCurrent());
				}
				return "" + 0;
			}
		}.withDescription("Current amount of bungee voteparty votes"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("BungeeVotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(String identifier) {
				if (plugin.getBungeeHandler() != null) {
					return Integer.toString(plugin.getBungeeHandler().getBungeeVotePartyRequired()
							- plugin.getBungeeHandler().getBungeeVotePartyCurrent());
				}
				return "" + 0;
			}
		}.withDescription("Voteparty bungee votes needed"));

		nonPlayerPlaceholders.add(new NonPlayerPlaceHolder<VotingPluginUser>("BungeeVotePartyVotesRequired") {

			@Override
			public String placeholderRequest(String identifier) {
				if (plugin.getBungeeHandler() != null) {
					return Integer.toString(plugin.getBungeeHandler().getBungeeVotePartyRequired());
				}
				return "" + 0;
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
				LocalDateTime now = LocalDateTime.now();
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

				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
						Long.toString(diffMinutes));
				return timeMsg;

			}
		}.withDescription("Time until plugin time day changes"));

		Set<String> placeholdersSet = new HashSet<String>();
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

		plugin.getUserManager().getUserDataChange().add(new UserDataChanged() {

			@Override
			public void onChange(AdvancedCoreUser user, String... keys) {
				VotingPluginUser vpUser = UserManager.getInstance().getVotingPluginUser(user);
				if (!vpUser.isCached()) {
					vpUser.dontCache();
				}
				for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
					if (placeholder.isUsesCache()) {
						for (String key : keys) {
							if (placeholder.getUpdateDataKey().equalsIgnoreCase(key)) {
								for (String ident : placeholder.getCache().keySet()) {
									String value = placeholder.placeholderRequest(vpUser, ident);
									placeholder.getCache().get(ident).put(vpUser.getJavaUUID(), value);
									plugin.devDebug("Updated placeholder cache for " + vpUser.getUUID() + " on " + key
											+ " with " + value);

								}

							}
						}
					}
				}
			}
		});
	}

	public void onVotePartyUpdate() {
		for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
			if (placeholder.isUsesCache()) {
				if (placeholder.getIdentifier().startsWith("VoteParty")) {
					for (String ident : placeholder.getCache().keySet()) {
						placeholder.getCache().put(ident, placeholder.placeholderRequest(ident));
					}
				}
			}
		}
	}

	public void onBungeeVotePartyUpdate() {
		for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
			if (placeholder.isUsesCache()) {
				if (placeholder.getIdentifier().startsWith("BungeeVoteParty")) {
					for (String ident : placeholder.getCache().keySet()) {
						placeholder.getCache().put(ident, placeholder.placeholderRequest(ident));
					}
				}
			}
		}
	}

	public void onUpdate(VotingPluginUser user) {
		for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
			if (placeholder.isUsesCache()) {
				for (String ident : placeholder.getCache().keySet()) {
					placeholder.getCache().get(ident).put(user.getJavaUUID(),
							placeholder.placeholderRequest(user, ident));
				}
			}
		}
	}

	public void checkNonCachedPlaceholders() {
		while (!placeholdersToSetCacheOn.isEmpty()) {
			String toCache = placeholdersToSetCacheOn.poll().toLowerCase();
			if (toCache.startsWith("votingplugin")) {
				toCache = toCache.substring("votingplugin_".length());
			}
			for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
				if (placeholder.matches(toCache)) {
					placeholder.setUseCache(true, toCache);
					cachedPlaceholders.add(toCache);
					plugin.getServerData().addAutoCachedPlaceholder(toCache);
					plugin.extraDebug("Auto Caching placeholder " + toCache);
				}

			}
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

	public void schedulePlaceholderCheck(VotingPluginUser user) {
		plugin.getTimer().execute(new Runnable() {

			@Override
			public void run() {
				checkNonCachedPlaceholders();
				onUpdate(user);
			}
		});
	}

	public void onUpdate() {
		checkNonCachedPlaceholders();
		for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
			if (placeholder.isUsesCache()) {
				for (String ident : placeholder.getCache().keySet()) {
					if (ident != null) {
						String str = placeholder.placeholderRequest(ident);
						if (str != null) {
							placeholder.getCache().put(ident, str);
						}
					} else {
						plugin.debug("ident null: " + placeholder.getIdentifier());
					}
				}
			}
		}

	}
}
