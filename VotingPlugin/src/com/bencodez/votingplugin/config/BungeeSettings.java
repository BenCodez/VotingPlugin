package com.bencodez.votingplugin.config;

import java.io.File;

import com.bencodez.advancedcore.yml.YMLFile;
import com.bencodez.advancedcore.yml.annotation.AnnotationHandler;
import com.bencodez.advancedcore.yml.annotation.ConfigDataBoolean;
import com.bencodez.advancedcore.yml.annotation.ConfigDataInt;
import com.bencodez.advancedcore.yml.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class BungeeSettings extends YMLFile {
	/** The instance. */
	static BungeeSettings instance = new BungeeSettings();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static BungeeSettings getInstance() {
		return instance;
	}

	@ConfigDataBoolean(path = "UseBungeecord", secondPath = "UseBungeecoord")
	@Getter
	private boolean useBungeecoord = false;

	@ConfigDataBoolean(path = "BungeeBroadcast")
	@Getter
	private boolean bungeeBroadcast = false;

	@ConfigDataBoolean(path = "BungeeBroadcastAlways")
	@Getter
	private boolean bungeeBroadcastAlways = false;

	@ConfigDataBoolean(path = "BungeeDebug")
	@Getter
	private boolean bungeeDebug = false;

	@ConfigDataString(path = "BungeeMethod")
	@Getter
	private String bungeeMethod = "SOCKETS";

	@ConfigDataString(path = "BungeeServer.Host")
	@Getter
	private String bungeeServerHost = "";

	@ConfigDataString(path = "Server")
	@Getter
	private String server = "PleaseSet";

	@ConfigDataString(path = "SpigotServer.Host")
	@Getter
	private String spigotServerHost = "";

	@ConfigDataInt(path = "BungeeServer.Port")
	@Getter
	private int bungeeServerPort = 1297;

	@ConfigDataInt(path = "SpigotServer.Port")
	@Getter
	private int spigotServerPort = 1298;

	public BungeeSettings() {
		super(new File(VotingPluginMain.plugin.getDataFolder(), "BungeeSettings.yml"));
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("BungeeSettings.yml", true);
	}
}
