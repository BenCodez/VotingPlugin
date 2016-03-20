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

	private ConfigFormat() {
	}

	static ConfigFormat instance = new ConfigFormat();

	public static ConfigFormat getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public ConfigFormat(Main plugin) {
		ConfigFormat.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

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

	public FileConfiguration getData() {
		return data;
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save Format.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public String getBroadCastMsg() {
		return getData().getString("Format.broadcastmsg");
	}

	public String getCommandsVoteURLS() {
		return getData().getString("Format.Commands.Vote.Sites");
	}

	public String getCommandsVoteNextTitle() {
		return getData().getString("Format.Commands.Vote.Next.Title");
	}

	public String getCommandsVoteNextLayout() {
		return getData().getString("Format.Commands.Vote.Next.Layout");
	}

	public String getCommandsVoteNextInfoCanVote() {
		return getData().getString("Format.Commands.Vote.Next.Info.CanVote");
	}

	public String getCommandsVoteNextInfoTime() {
		return getData().getString(
				"Format.Commands.Vote.Next.Info.TimeUntilVote");
	}

	public String getCommandsVoteNextInfoError() {
		return getData().getString("Format.Commands.Vote.Next.Info.Error");
	}

	public String getCommandsVoteLastTitle() {
		return getData().getString("Format.Commands.Vote.Last.Title");
	}

	public String getCommandsVoteLastLine() {
		return getData().getString("Format.Commands.Vote.Last.Line");
	}

	public String getCommandsVoteTotalTitle() {
		return getData().getString("Format.Commands.Vote.Total.Title");
	}

	public String getCommandsVoteTotalLine() {
		return getData().getString("Format.Commands.Vote.Total.Line");
	}

	public String getCommandsVoteTotalTotal() {
		return getData().getString("Format.Commands.Vote.Total.Total");
	}

	public String getCommandsVoteTotalAllTitle() {
		return getData().getString("Format.Commands.Vote.TotalAll.Title");
	}

	public String getCommandsVoteTotalAllLine() {
		return getData().getString("Format.Commands.Vote.TotalAll.Line");
	}

	public String getCommandsVoteTotalAllTotal() {
		return getData().getString("Format.Commands.Vote.TotalAll.Total");
	}

	public String getLoginMsg() {
		return getData().getString("Format.loginmsg");
	}

	public String getRewardMsg() {
		return getData().getString("Format.rewardmsg");
	}

	@SuppressWarnings("unchecked")
	public List<String> getVoteHelp() {
		return (List<String>) getData().getList(
				"Format.Commands.Vote.Help.Lines");
	}

	public String getCommandVoteTopTitle() {
		return getData().getString("Format.Commands.Vote.Top.Title");
	}

	public String getCommandVoteTopLine() {
		return getData().getString("Format.Commands.Vote.Top.Line");
	}

	public String getChanceRewardMsg() {
		return getData().getString("Format.chancerewardmsg");
	}

	public String getTimeFormat() {
		String string = getData().getString("Format.timeformat");
		if (string == null) {
			return "EEE, d MMM yyyy HH:mm";
		}
		return string;
	}

}
