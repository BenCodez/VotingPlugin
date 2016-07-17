package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

public class ConfigBungeeVoting {

	static ConfigBungeeVoting instance = new ConfigBungeeVoting();

	static Main plugin = Main.plugin;

	public static ConfigBungeeVoting getInstance() {
		return instance;
	}

	FileConfiguration data;

	File dFile;

	private ConfigBungeeVoting() {
	}

	public ConfigBungeeVoting(Main plugin) {
		ConfigBungeeVoting.plugin = plugin;
	}

	public FileConfiguration getData() {
		return data;
	}

	public boolean getEnabled() {
		return getData().getBoolean("Enabled");
	}

	public String getServerIP(String server) {
		return getData().getString("Servers." + server + ".IP");
	}

	public String getServerKey(String server) {
		return getData().getString("Servers." + server + ".Key");
	}

	public int getServerPort(String server) {
		return getData().getInt("Servers." + server + ".Port");
	}

	public Set<String> getServers() {
		return getData().getConfigurationSection("Servers").getKeys(false);
	}

	public String getServerServiceSite(String server) {
		return getData().getString("Servers." + server + ".ServiceSite");
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

		dFile = new File(p.getDataFolder(), "BungeeVoting.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("BungeeVoting.yml", true);
			} catch (IOException e) {
				Bukkit.getServer()
				.getLogger()
				.severe(ChatColor.RED
						+ "Could not create BungeeVoting.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}

}
