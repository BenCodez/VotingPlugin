package com.Ben12345rocks.VotingPlugin.UserData;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

public class OldData {

	private OldData() {
	}

	static OldData instance = new OldData();

	public static OldData getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public OldData(Main plugin) {
		OldData.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "data.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create data.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public FileConfiguration getData() {
		dFile = new File(plugin.getDataFolder(), "data.yml");
		data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save data.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	/*public void convertData() {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {
			
			public void run() {
				plugin.getLogger().info("Starting to convert old data file to new data files!");
				Set<String> names = getData().getConfigurationSection("")
						.getKeys(false);
				Set<String> voteSites = ConfigVoteSites.getInstance().getVoteSites();
				for (String playerName : names) {
					String uuid = Utils.getInstance().getUUID(playerName);
					Data.getInstance().setup(playerName);
					for (String voteSite : voteSites) {
						Data.getInstance().set(
								playerName,
								uuid + ".LastVote." + voteSite + ".Month",
								getData()
										.getInt(playerName + ".LastVote." + voteSite
												+ ".Month"));
						Data.getInstance().set(
								playerName,
								uuid + ".LastVote." + voteSite + ".Day",
								getData().getInt(
										playerName + ".LastVote." + voteSite + ".Day"));
						Data.getInstance()
								.set(playerName,
										uuid + ".LastVote." + voteSite + ".Hour",
										getData().getInt(
												playerName + ".LastVote." + voteSite
														+ ".Hour"));
						Data.getInstance().set(
								playerName,
								uuid + ".LastVote." + voteSite + ".Min",
								getData().getInt(
										playerName + ".LastVote." + voteSite + ".Min"));
						Data.getInstance().set(playerName, uuid + ".Total." + voteSite,
								getData().getInt(playerName + ".Total." + voteSite));
						Data.getInstance().set(playerName,
								uuid + ".OfflineVotes." + voteSite,
								getData().getInt(playerName + ".OffVotes." + voteSite));
					}
				}
				plugin.getLogger().info("Finished converting data file!");
			}
		});
		
	}*/

}
