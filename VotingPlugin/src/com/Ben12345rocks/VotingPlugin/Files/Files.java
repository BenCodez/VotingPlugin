package com.Ben12345rocks.VotingPlugin.Files;

import java.io.File;
import java.io.IOException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;

public class Files {

	public ReadThread thread;

	static Files instance = new Files();

	static Main plugin = Main.plugin;

	public static Files getInstance() {
		return instance;
	}

	private Files() {
	}

	public Files(Main plugin) {
		Files.plugin = plugin;
	}

	public class ReadThread extends Thread {
		@Override
		public void run() {
			plugin.getLogger().info("File editing thread started!");
		}

		public void editFile(File file, FileConfiguration data) {
			try {
				data.save(file);
				plugin.getLogger().info("Saved file " + file.getName());
			} catch (IOException e) {
				Bukkit.getServer()
						.getLogger()
						.severe(ChatColor.RED + "Could not save "
								+ file.getName());
			}
		}
	}

	public void loadFileEditngThread() {
		this.thread = new ReadThread();
		this.thread.start();
	}

	public void editFile(File file, FileConfiguration data) {
		thread.editFile(file, data);
	}
}
