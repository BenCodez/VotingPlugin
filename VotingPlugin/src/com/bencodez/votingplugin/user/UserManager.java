package com.bencodez.votingplugin.user;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.UUID;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyBoolean;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyInt;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.advancedcore.api.user.userstorage.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;

public class UserManager {
	private VotingPluginMain plugin;

	public UserManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void addCachingKeys() {
		UserDataManager manager = plugin.getUserManager().getDataManager();
		manager.addKey(new UserDataKeyBoolean("TopVoterIgnore"));
		manager.addKey(new UserDataKeyInt("VotePartyVotes"));
		manager.addKey(new UserDataKeyString("LastVotes"));
		manager.addKey(new UserDataKeyBoolean(getCoolDownCheckPath()));
		manager.addKey(new UserDataKeyString("OfflineVotes").setColumnType("MEDIUMTEXT"));
		manager.addKey(new UserDataKeyInt("MilestoneCount"));
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
		manager.addKey(new UserDataKeyString("GottenMileStones").setColumnType("LONGTEXT"));
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
	}

	public String getGottenAllSitesDayPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "AllSitesLast_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "AllSitesLast";
	}

	public String getGottenAlmostAllSitesDayPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "AlmostAllSitesLast_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "AlmostAllSitesLast";
	}

	public String getCoolDownCheckSitePath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage() + "_Sites";
		} else {
			return "CoolDownCheck" + "_Sites";
		}
	}

	public String getCoolDownCheckPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage();
		} else {
			return "CoolDownCheck";
		}
	}

	public String getMonthTotalsWithDatePath() {
		LocalDateTime cTime = plugin.getTimeChecker().getTime();
		return getMonthTotalsWithDatePath(cTime);
	}

	public String getMonthTotalsWithDatePath(LocalDateTime cTime) {
		return "MonthTotal-" + cTime.getMonth().toString() + "-" + cTime.getYear();
	}

	public ArrayList<String> getAllUUIDs() {
		return plugin.getUserManager().getAllUUIDs();
	}

	public VotingPluginUser getVotingPluginUser(com.bencodez.advancedcore.api.user.AdvancedCoreUser user) {
		return new VotingPluginUser(plugin, user);
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid, String playerName) {
		return new VotingPluginUser(plugin, uuid, playerName);
	}

	public VotingPluginUser getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getUniqueId(), player.getName());
	}

	public VotingPluginUser getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getUniqueId(), player.getName());
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(String playerName) {
		return new VotingPluginUser(plugin, plugin.getUserManager().getProperName(playerName));
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid) {
		return new VotingPluginUser(plugin, uuid);
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid, boolean loadName) {
		return new VotingPluginUser(plugin, uuid, loadName);
	}

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
					user.dontCache();
					int daysOld = plugin.getOptions().getPurgeMinimumDays();
					int days = user.getNumberOfDaysSinceLogin();
					if (days == -1) {
						// fix ones with no last online
						user.setLastOnline(System.currentTimeMillis());
					} else if (days > daysOld) {
						if (user.getTotal(TopVoter.AllTime) == 0 && user.getMilestoneCount() == 0
								&& user.getTotal(TopVoter.Monthly) == 0 && user.getTotal(TopVoter.Weekly) == 0
								&& user.getTotal(TopVoter.Daily) == 0) {
							plugin.debug("Removing " + user.getUUID() + " because of no data purge");
							user.remove();
						}
					}

				}
			});

		}

		plugin.getUserManager().getDataManager().clearCache();
	}

	public void purgeOldPlayersNowNoData() {
		HashMap<UUID, ArrayList<Column>> cols = plugin.getUserManager().getAllKeys();
		for (Entry<UUID, ArrayList<Column>> playerData : cols.entrySet()) {
			String uuid = playerData.getKey().toString();
			if (plugin.isEnabled()) {
				if (uuid != null) {
					VotingPluginUser user = getVotingPluginUser(UUID.fromString(uuid), false);
					if (user != null) {
						user.dontCache();
						user.updateTempCacheWithColumns(playerData.getValue());
						int daysOld = plugin.getOptions().getPurgeMinimumDays();
						int days = user.getNumberOfDaysSinceLogin();
						if (days == -1) {
							// fix ones with no last online
							user.setLastOnline(System.currentTimeMillis());
						} else if (days > daysOld) {
							if (user.getTotal(TopVoter.AllTime) == 0 && user.getMilestoneCount() == 0
									&& user.getTotal(TopVoter.Monthly) == 0 && user.getTotal(TopVoter.Weekly) == 0
									&& user.getTotal(TopVoter.Daily) == 0)
								plugin.debug("Removing " + user.getUUID() + " because of no data purge");
							user.remove();
						}

						user.clearTempCache();
						cols.put(playerData.getKey(), null);
						user = null;
					}
				}
			}
		}
		cols.clear();
		cols = null;

		plugin.getUserManager().getDataManager().clearCache();
		plugin.setUpdate(true);
	}
}
