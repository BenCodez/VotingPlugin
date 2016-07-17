package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

public class ConfigFormat {

	static ConfigFormat instance = new ConfigFormat();

	static Main plugin = Main.plugin;

	public static ConfigFormat getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigFormat() {
	}

	public ConfigFormat(Main plugin) {
		ConfigFormat.plugin = plugin;
	}

	public String getBroadCastMsg() {
		String str = getData().getString("Format.BroadcastMsg");
		if (str != null) {
			return str;
		} else {
			return "&6[&4Broadcast&6] &2Thanks &c%player% &2for voting on %SiteName%";
		}
	}

	public boolean getBroadcastWhenOnline() {
		return getData().getBoolean("Format.BroadcastWhenOnline");
	}

	public String getCommandsVoteHelpLine() {

		String str = getData().getString("Format.Commands.Vote.Help.Line");
		if (str != null) {
			return str;
		} else {
			return "&3&l%Command% - &3%HelpMessage%";
		}

	}

	public boolean getCommandsVoteHelpRequirePermission() {
		return getData().getBoolean(
				"Format.Commands.Vote.Help.RequirePermission");
	}

	public String getCommandsVoteHelpTitle() {
		String str = getData().getString("Format.Commands.Vote.Help.Title");
		if (str != null) {
			return str;
		} else {
			return "Voting Player Help";
		}

	}

	public String getCommandsVoteLastLine() {
		String str = getData().getString("Format.Commands.Vote.Last.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%time%";
		}
	}

	public String getCommandsVoteLastTitle() {
		String str = getData().getString("Format.Commands.Vote.Last.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Last Vote Times:";
		}
	}

	public String getCommandsVoteNextInfoCanVote() {
		String str = getData().getString(
				"Format.Commands.Vote.Next.Info.CanVote");
		if (str != null) {
			return str;
		} else {
			return "Go Vote!";
		}
	}

	public String getCommandsVoteNextInfoError() {
		String str = getData()
				.getString("Format.Commands.Vote.Next.Info.Error");
		if (str != null) {
			return str;
		} else {
			return "";
		}
	}

	public String getCommandsVoteNextInfoTime() {
		String str = getData().getString(
				"Format.Commands.Vote.Next.Info.TimeUntilVote");
		if (str != null) {
			return str;
		} else {
			return "&cCould not caculate time until next vote!";
		}
	}

	public String getCommandsVoteNextLayout() {
		String str = getData().getString("Format.Commands.Vote.Next.Layout");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%info%";
		}
	}

	public String getCommandsVoteNextTitle() {
		String str = getData().getString("Format.Commands.Vote.Next.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Next Votes:";
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsVoteParty() {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"Format.Commands.Vote.Party");
			if (list != null) {
				return list;
			}
			ArrayList<String> msg = new ArrayList<String>();
			msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
			return msg;
		} catch (Exception ex) {
			ArrayList<String> msg = new ArrayList<String>();
			msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
			return msg;
		}
	}

	@SuppressWarnings("unchecked")
	public List<String> getCommandsVoteTitle() {
		List<String> str;
		try {
			str = (List<String>) getData()
					.getList("Format.Commands.Vote.Title");
			if (str != null) {
				return str;
			} else {
				str = new ArrayList<String>();
				str.add("&4&lVote for our server!");
				return str;
			}
		} catch (Exception ex) {
			str = new ArrayList<String>();
			str.add("&4&lVote for our server!");
			return str;
		}
	}

	public String getCommandsVoteTotalAllLine() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName% &6%Total%";
		}
	}

	public String getCommandsVoteTotalAllTitle() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&lAll Votes Total:";
		}
	}

	public String getCommandsVoteTotalAllTotal() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Total");
		if (str != null) {
			return str;
		} else {
			return "&3&lTotal: &6&l%Totals%";
		}
	}

	public String getCommandsVoteTotalLine() {
		String str = getData().getString("Format.Commands.Vote.Total.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%Total%";
		}
	}

	public String getCommandsVoteTotalTitle() {
		String str = getData().getString("Format.Commands.Vote.Total.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Total Votes:";
		}
	}

	public String getCommandsVoteTotalTotal() {
		String str = getData().getString("Format.Commands.Vote.Total.Total");
		if (str != null) {
			return str;
		} else {
			return "&3&lTotal: &6&l%Totals%";
		}
	}

	public String getCommandsVoteURLS() {
		String str = getData().getString("Format.Commands.Vote.Sites");
		if (str != null) {
			return str;
		} else {
			return "&4%num%: &c&l%SiteName% - &c%url%";
		}
	}

	public String getCommandVoteTopLine() {
		String str = getData().getString("Format.Commands.Vote.Top.Line");
		if (str != null) {
			return str;
		} else {
			return "&c%num%: &6%player%, %votes%";
		}
	}

	public String getCommandVoteTopTitle() {
		String str = getData().getString("Format.Commands.Vote.Top.Title");
		if (str != null) {
			return str;
		} else {
			return "&3Top Voters %page%/%maxpages%";
		}
	}

	public FileConfiguration getData() {
		return data;
	}

	public String getLoginMsg() {
		String str = getData().getString("Format.LoginMsg");
		if (str != null) {
			return str;
		} else {
			return "&cRemember to vote!";
		}
	}

	public String getNoPerms() {
		String str = getData().getString("Format.NoPerms");
		if (str != null) {
			return str;
		} else {
			return "&cYou do not have enough permission!";
		}
	}

	public String getNotNumber() {
		String str = getData().getString("Format.NotNumber");
		if (str != null) {
			return str;
		} else {
			return "&cError on &6%arg%&c, number expected!";
		}
	}

	public int getPageSize() {
		return 10;
	}

	public String getRewardMsg() {
		String str = getData().getString("Format.DefaultRewardMsg");
		if (str != null) {
			return str;
		} else {
			return "&aYou were given rewards!";
		}
	}

	public String getSignTopVoterSignLine1() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line1");
		if (str != null) {
			return str;
		} else {
			return "TopVoter: %SiteName%";
		}
	}

	public String getSignTopVoterSignLine2() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line2");
		if (str != null) {
			return str;
		} else {
			return "#%position%";
		}
	}

	public String getSignTopVoterSignLine3() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line3");
		if (str != null) {
			return str;
		} else {
			return "%player%";
		}
	}

	public String getSignTopVoterSignLine4() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line4");
		if (str != null) {
			return str;
		} else {
			return "%votes% Votes";
		}
	}

	public String getSignTopVoterSignRightClickMessage() {
		String str = getData().getString("Format.Signs.RightClickMessage");
		if (str != null) {
			return str;
		} else {
			return "&c&l%player% &cis &c&l%position% &cwith &c&l%votes% &cin &c&l%SiteName%";
		}
	}

	public String getTimeFormat() {
		String string = getData().getString("Format.TimeFormat");
		if (string == null) {
			return "EEE, d MMM yyyy HH:mm";
		}
		return string;
	}

	public String getTimeZone() {
		String str = getData().getString("Format.TimeZone");
		if (str != null) {
			return str;
		}
		return "UTC";
	}

	public String getTopVoterRewardMsg() {
		String msg = getData().getString("Format.TopVoterAwardMsg");
		if (msg != null) {
			return msg;
		} else {
			return "&aYou came in %place% in top voters of the month! Here is an award!";
		}
	}

	@SuppressWarnings("unchecked")
	public List<String> getVoteHelp() {
		return (List<String>) getData().getList(
				"Format.Commands.Vote.Help.Lines");
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		Files.getInstance().editFile(dFile, data);

	}

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "Format.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("Format.yml", true);
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create Format.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}
}
