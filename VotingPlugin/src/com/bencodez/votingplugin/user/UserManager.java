package com.bencodez.votingplugin.user;

import java.util.ArrayList;
import java.util.UUID;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyBoolean;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyInt;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.votingplugin.VotingPluginMain;

public class UserManager {
	/** The instance. */
	static UserManager instance = new UserManager();
	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Gets the single instance of UserManager.
	 *
	 * @return single instance of UserManager
	 */
	public static UserManager getInstance() {
		return instance;
	}

	public UserManager() {
	}

	public void addCachingKeys() {
		UserDataManager manager = plugin.getUserManager().getDataManager();
		manager.addKey(new UserDataKeyBoolean("TopVoterIgnore"));
		manager.addKey(new UserDataKeyInt("VotePartyVotes"));
		manager.addKey(new UserDataKeyString("LastVotes"));
		manager.addKey(new UserDataKeyBoolean("CoolDownCheck"));
		manager.addKey(new UserDataKeyString("OfflineVotes").setColumnType("MEDIUMTEXT"));
		manager.addKey(new UserDataKeyInt("MilestoneCount"));
		manager.addKey(new UserDataKeyInt("MonthTotal"));
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
}
