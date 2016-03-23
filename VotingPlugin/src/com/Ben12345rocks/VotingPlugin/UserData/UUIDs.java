package com.Ben12345rocks.VotingPlugin.UserData;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;

public class UUIDs {

	private UUIDs() {
	}

	static UUIDs instance = new UUIDs();

	public static UUIDs getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public UUIDs(Main plugin) {
		UUIDs.plugin = plugin;
	}

	FileConfiguration data;
	File dFile;

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

	public FileConfiguration getData() {
		return data;
	}

	public void saveData() {
		try {
			data.save(dFile);
		} catch (IOException e) {
			Bukkit.getServer().getLogger()
			.severe(ChatColor.RED + "Could not save uuids.yml!");
		}
	}

	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	public void setName(String playerName, String uuid) {
		getData().set(uuid, playerName);
		saveData();
	}

	public String getPlayerName(String uuid) {
		return getData().getString(uuid);
	}

}
