package com.Ben12345rocks.VotingPlugin.Data;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

public class UUIDs {

	static UUIDs instance = new UUIDs();

	static Main plugin = Main.plugin;

	public static UUIDs getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private UUIDs() {
	}

	public UUIDs(Main plugin) {
		UUIDs.plugin = plugin;
	}

	public FileConfiguration getData() {
		return data;
	}

	public String getPlayerName(String uuid) {
		return getData().getString(uuid);
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void saveData() {
		Files.getInstance().editFile(dFile, data);
		/*try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
					.severe(ChatColor.RED + "Could not save uuids.yml!");
		}*/
	}

	public void setName(String playerName, String uuid) {
		getData().set(uuid, playerName);
		saveData();
	}

	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "uuids.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create uuids.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

}
