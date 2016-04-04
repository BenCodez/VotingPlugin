package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.List;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

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
		return getData().getString("Format.broadcastmsg");
	}

	public String getCommandsVoteLastLine() {
		return getData().getString("Format.Commands.Vote.Last.Line");
	}

	public String getCommandsVoteLastTitle() {
		return getData().getString("Format.Commands.Vote.Last.Title");
	}

	public String getCommandsVoteNextInfoCanVote() {
		return getData().getString("Format.Commands.Vote.Next.Info.CanVote");
	}

	public String getCommandsVoteNextInfoError() {
		return getData().getString("Format.Commands.Vote.Next.Info.Error");
	}

	public String getCommandsVoteNextInfoTime() {
		return getData().getString(
				"Format.Commands.Vote.Next.Info.TimeUntilVote");
	}

	public String getCommandsVoteNextLayout() {
		return getData().getString("Format.Commands.Vote.Next.Layout");
	}

	public String getCommandsVoteNextTitle() {
		return getData().getString("Format.Commands.Vote.Next.Title");
	}

	@SuppressWarnings("unchecked")
	public List<String> getCommandsVoteTitle() {
		return (List<String>) getData().getList("Format.Commands.Vote.Title");
	}

	public String getCommandsVoteTotalAllLine() {
		return getData().getString("Format.Commands.Vote.TotalAll.Line");
	}

	public String getCommandsVoteTotalAllTitle() {
		return getData().getString("Format.Commands.Vote.TotalAll.Title");
	}

	public String getCommandsVoteTotalAllTotal() {
		return getData().getString("Format.Commands.Vote.TotalAll.Total");
	}

	public String getCommandsVoteTotalLine() {
		return getData().getString("Format.Commands.Vote.Total.Line");
	}

	public String getCommandsVoteTotalTitle() {
		return getData().getString("Format.Commands.Vote.Total.Title");
	}

	public String getCommandsVoteTotalTotal() {
		return getData().getString("Format.Commands.Vote.Total.Total");
	}

	public String getCommandsVoteURLS() {
		return getData().getString("Format.Commands.Vote.Sites");
	}

	public String getCommandVoteTopLine() {
		return getData().getString("Format.Commands.Vote.Top.Line");
	}

	public String getCommandVoteTopTitle() {
		return getData().getString("Format.Commands.Vote.Top.Title");
	}

	public FileConfiguration getData() {
		return data;
	}

	public String getExtraRewardMsg() {
		String msg = getData().getString("Format.extrarewardmsg");
		if (msg != null) {
			return msg;
		} else {
			return "&aLooks like you got lucky!";
		}
	}

	public String getLoginMsg() {
		return getData().getString("Format.loginmsg");
	}

	public int getPageSize() {
		int size = getData().getInt("Format.pagesize");
		if (size == 0) {
			size = 10;
		}
		return size;
	}

	public String getRewardMsg() {
		return getData().getString("Format.rewardmsg");
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

	public String getTimeFormat() {
		String string = getData().getString("Format.timeformat");
		if (string == null) {
			return "EEE, d MMM yyyy HH:mm";
		}
		return string;
	}

	public String getTopVoterRewardMsg() {
		String msg = getData().getString("Format.topvoterawardmsg");
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
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
			.severe(ChatColor.RED + "Could not save Format.yml!");
		}
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
