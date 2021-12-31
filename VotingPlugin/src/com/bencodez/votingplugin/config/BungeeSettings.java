package com.bencodez.votingplugin.config;

import java.io.File;

import com.bencodez.advancedcore.api.yml.YMLFile;
import com.bencodez.advancedcore.api.yml.annotation.AnnotationHandler;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataBoolean;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataInt;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class BungeeSettings extends YMLFile {

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
	private String bungeeMethod = "PLUGINMESSAGING";

	@ConfigDataString(path = "BungeeServer.Host")
	@Getter
	private String bungeeServerHost = "";

	@ConfigDataInt(path = "BungeeServer.Port")
	@Getter
	private int bungeeServerPort = 1297;

	@ConfigDataBoolean(path = "PerServerPoints")
	@Getter
	private boolean perServerPoints = false;

	@ConfigDataBoolean(path = "PerServerMilestones")
	@Getter
	private boolean perServerMilestones = false;
	
	@ConfigDataBoolean(path = "PerServerRewards")
	@Getter
	private boolean perServerRewards = false;

	@ConfigDataBoolean(path = "RemoveInvalidUsers")
	@Getter
	private boolean removeInvalidUsers = false;

	@ConfigDataString(path = "Server")
	@Getter
	private String server = "PleaseSet";

	@ConfigDataString(path = "SpigotServer.Host")
	@Getter
	private String spigotServerHost = "";

	@ConfigDataInt(path = "SpigotServer.Port")
	@Getter
	private int spigotServerPort = 1298;

	@ConfigDataBoolean(path = "TriggerVotifierEvent")
	@Getter
	private boolean triggerVotifierEvent = false;

	@ConfigDataBoolean(path = "UseBungeecord", secondPath = "UseBungeecoord")
	@Getter
	private boolean useBungeecoord = false;

	@ConfigDataBoolean(path = "VotifierBypass")
	@Getter
	private boolean votifierBypass = false;

	@ConfigDataBoolean(path = "DisableBroadcast")
	@Getter
	private boolean disableBroadcast = false;

	public BungeeSettings(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "BungeeSettings.yml"));
	}

	public String getServerNameStorage() {
		return getServer().replace("-", "_");
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		getPlugin().saveResource("BungeeSettings.yml", true);
	}
}
