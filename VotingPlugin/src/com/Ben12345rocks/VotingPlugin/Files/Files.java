package com.Ben12345rocks.VotingPlugin.Files;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;

public class Files {

	public class ReadThread extends Thread {
		public void editFile(File file, FileConfiguration data) {
			try {
				data.save(file);
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED + "Could not save "
								+ file.getName());
			}
		}

		@Override
		public void run() {
			plugin.getLogger().info("File Editing Thread Loaded!");
		}
	}

	static Files instance = new Files();

	static Main plugin = Main.plugin;

	public static Files getInstance() {
		return instance;
	}

	public ReadThread thread;

	private Files() {
	}

	public Files(Main plugin) {
		Files.plugin = plugin;
	}

	public void editFile(File file, FileConfiguration data) {
		thread.editFile(file, data);
	}

	public void loadFileEditngThread() {
		thread = new ReadThread();
		thread.start();
	}
}
