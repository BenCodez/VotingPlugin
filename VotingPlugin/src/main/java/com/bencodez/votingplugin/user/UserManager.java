package com.bencodez.votingplugin.user;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.UUID;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyBoolean;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyInt;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesManager;
import com.bencodez.votingplugin.topvoter.TopVoter;

/**
 * Manages VotingPlugin user operations and data.
 */
public class UserManager {
	private VotingPluginMain plugin;

	/**
	 * Constructs a new user manager.
	 * @param plugin the plugin instance
	 */
	public UserManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Adds caching keys to the user data manager.
	 */
	public void addCachingKeys() {
		UserDataManager manager = plugin.getUserManager().getDataManager();
		manager.addKey(new UserDataKeyBoolean("TopVoterIgnore"));
		manager.addKey(new UserDataKeyInt("VotePartyVotes"));
		manager.addKey(new UserDataKeyString("LastVotes"));
		manager.addKey(new UserDataKeyBoolean(getCoolDownCheckPath()));
		manager.addKey(new UserDataKeyString("OfflineVotes").setColumnType("MEDIUMTEXT"));
		//manager.addKey(new UserDataKeyInt("MilestoneCount"));
		manager.addKey(new UserDataKeyInt("MonthTotal"));

		if (plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()) {
			manager.addKey(new UserDataKeyInt(getMonthTotalsWithDatePath(LocalDateTime.now().minusMonths(1))));
			manager.addKey(new UserDataKeyInt(getMonthTotalsWithDatePath(LocalDateTime.now())));
			manager.addKey(new UserDataKeyInt(getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(1))));
			manager.addKey(new UserDataKeyInt(getMonthTotalsWithDatePath(LocalDateTime.now().plusMonths(2))));
		}

		manager.addKey(new UserDataKeyInt("AllTimeTotal"));
		manager.addKey(new UserDataKeyInt("DailyTotal"));
		manager.addKey(new UserDataKeyInt("WeeklyTotal"));
		manager.addKey(new UserDataKeyInt("Points"));
		manager.addKey(new UserDataKeyInt("DayVoteStreak"));
		manager.addKey(new UserDataKeyInt("BestDayVoteStreak"));
		manager.addKey(new UserDataKeyString("DayVoteStreakLastUpdate").setColumnType("MEDIUMTEXT"));
		manager.addKey(new UserDataKeyString(VoteMilestonesManager.getLIMITS_STORAGE_KEY()).setColumnType("LONGTEXT"));
		manager.addKey(new UserDataKeyBoolean("Reminded"));
		manager.addKey(new UserDataKeyBoolean("DisableBroadcast"));
		manager.addKey(new UserDataKeyInt("WeekVoteStreak"));
		manager.addKey(new UserDataKeyInt("BestWeekVoteStreak"));
		manager.addKey(new UserDataKeyInt("MonthVoteStreak"));
		manager.addKey(new UserDataKeyInt("HighestDailyTotal"));
		manager.addKey(new UserDataKeyInt("HighestMonthlyTotal"));
		manager.addKey(new UserDataKeyInt("HighestWeeklyTotal"));
		manager.addKey(new UserDataKeyInt("LastMonthTotal"));
		manager.addKey(new UserDataKeyInt("LastWeeklyTotal"));
		manager.addKey(new UserDataKeyInt("LastDailyTotal"));
		manager.addKey(new UserDataKeyInt("BestMonthVoteStreak"));
		manager.addKey(new UserDataKeyString(getCoolDownCheckSitePath()).setColumnType("LONGTEXT"));
		if (plugin.getOptions().isPerServerRewards()) {
			manager.addKey(new UserDataKeyString("OfflineRewards" + plugin.getOptions().getServer())
					.setColumnType("MEDIUMTEXT"));
		}
		if (plugin.getConfigFile().isExtraAllSitesCheck()) {
			manager.addKey(new UserDataKeyInt(getGottenAllSitesDayPath()));
		}
		manager.addKey(new UserDataKeyInt(getGottenAlmostAllSitesDayPath()));
		
		manager.addKey(new UserDataKeyString("VoteRemindersLast").setColumnType("BIGINT"));
		
		manager.addKey(new UserDataKeyString("VoteRemindersMap").setColumnType("LONGTEXT"));

	}

	/**
	 * Gets all user UUIDs.
	 * @return list of all UUIDs
	 */
	public ArrayList<String> getAllUUIDs() {
		return plugin.getUserManager().getAllUUIDs();
	}

	/**
	 * Gets the cooldown check path based on server configuration.
	 * @return the cooldown check path
	 */
	public String getCoolDownCheckPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "CoolDownCheck";
	}

	/**
	 * Gets the cooldown check site path based on server configuration.
	 * @return the cooldown check site path
	 */
	public String getCoolDownCheckSitePath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage() + "_Sites";
		}
		return "CoolDownCheck" + "_Sites";
	}

	/**
	 * Gets the path for tracking when all sites were voted on.
	 * @return the all sites day path
	 */
	public String getGottenAllSitesDayPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "AllSitesLast_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "AllSitesLast";
	}

	/**
	 * Gets the path for tracking when almost all sites were voted on.
	 * @return the almost all sites day path
	 */
	public String getGottenAlmostAllSitesDayPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "AlmostAllSitesLast_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "AlmostAllSitesLast";
	}

	/**
	 * Gets the monthly totals path for the current time.
	 * @return the month totals path
	 */
	public String getMonthTotalsWithDatePath() {
		LocalDateTime cTime = plugin.getTimeChecker().getTime();
		return getMonthTotalsWithDatePath(cTime);
	}

	/**
	 * Gets the monthly totals path for a specific time.
	 * @param cTime the time to get the path for
	 * @return the month totals path
	 */
	public String getMonthTotalsWithDatePath(LocalDateTime cTime) {
		return "MonthTotal-" + cTime.getMonth().toString() + "-" + cTime.getYear();
	}

	/**
	 * Gets a VotingPluginUser from an AdvancedCoreUser.
	 * @param user the AdvancedCoreUser
	 * @return the VotingPluginUser
	 */
	public VotingPluginUser getVotingPluginUser(com.bencodez.advancedcore.api.user.AdvancedCoreUser user) {
		return new VotingPluginUser(plugin, user);
	}

	/**
	 * Gets a VotingPluginUser from an offline player.
	 * @param player the offline player
	 * @return the VotingPluginUser
	 */
	public VotingPluginUser getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getUniqueId(), player.getName());
	}

	/**
	 * Gets a VotingPluginUser from an online player.
	 * @param player the online player
	 * @return the VotingPluginUser
	 */
	public VotingPluginUser getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getUniqueId(), player.getName());
	}

	/**
	 * Gets a VotingPluginUser by player name.
	 * @param playerName the player name
	 * @return the VotingPluginUser
	 */
	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(String playerName) {
		return new VotingPluginUser(plugin, plugin.getUserManager().getProperName(playerName));
	}

	/**
	 * Gets a VotingPluginUser by UUID.
	 * @param uuid the player UUID
	 * @return the VotingPluginUser
	 */
	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid) {
		return new VotingPluginUser(plugin, uuid);
	}

	/**
	 * Gets a VotingPluginUser by UUID with optional name loading.
	 * @param uuid the player UUID
	 * @param loadName whether to load the player name
	 * @return the VotingPluginUser
	 */
	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid, boolean loadName) {
		return new VotingPluginUser(plugin, uuid, loadName);
	}

	/**
	 * Gets a VotingPluginUser by UUID and player name.
	 * @param uuid the player UUID
	 * @param playerName the player name
	 * @return the VotingPluginUser
	 */
	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid, String playerName) {
		return new VotingPluginUser(plugin, uuid, playerName);
	}

	/**
	 * Purges old players with no voting data.
	 */
	public void purgeOldPlayersNoData() {
		if (plugin.getOptions().isPurgeOldData() && plugin.getConfigFile().isPurgeNoDataOnStartup()) {
			plugin.addUserStartup(new UserStartup() {

				@Override
				public void onFinish() {
					plugin.debug("Finished no data purging");
				}

				@Override
				public void onStart() {

				}

				@Override
				public void onStartUp(AdvancedCoreUser acUser) {
					VotingPluginUser user = getVotingPluginUser(acUser);
					user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
					int daysOld = plugin.getOptions().getPurgeMinimumDays();
					int days = user.getNumberOfDaysSinceLogin();
					if (days == -1) {
						// fix ones with no last online
						user.setLastOnline(System.currentTimeMillis());
					} else if (days > daysOld) {
						if (user.getTotal(TopVoter.AllTime) == 0 && user.getTotal(TopVoter.Monthly) == 0
								&& user.getTotal(TopVoter.Weekly) == 0 && user.getTotal(TopVoter.Daily) == 0) {
							plugin.debug("Removing " + user.getUUID() + " because of no data purge");
							user.remove();
						}
					}

				}
			});

		}

		plugin.getUserManager().getDataManager().clearCache();
	}

	/**
	 * Immediately purges old players with no voting data.
	 */
	public void purgeOldPlayersNowNoData() {
		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (plugin.isEnabled()) {
				VotingPluginUser user = getVotingPluginUser(uuid, false);
				if (user != null) {
					user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
					user.updateTempCacheWithColumns(columns);
					int daysOld = plugin.getOptions().getPurgeMinimumDays();
					int days = user.getNumberOfDaysSinceLogin();
					if (days == -1) {
						// fix ones with no last online
						user.setLastOnline(System.currentTimeMillis());
					} else if (days > daysOld) {
						if (user.getTotal(TopVoter.AllTime) == 0 && user.getTotal(TopVoter.Monthly) == 0
								&& user.getTotal(TopVoter.Weekly) == 0 && user.getTotal(TopVoter.Daily) == 0) {
							plugin.debug("Removing " + user.getUUID() + " because of no data purge");
							user.remove();
						}
					}

					user.clearTempCache();
					user = null;
				}
			}

		}, (count) -> {
			plugin.getUserManager().getDataManager().clearCache();
			plugin.setUpdate(true);
		});
	}
}
