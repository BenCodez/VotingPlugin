package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;

import com.bencodez.simpleapi.file.YMLFile;
import com.bencodez.simpleapi.file.annotation.AnnotationHandler;
import com.bencodez.simpleapi.file.annotation.ConfigDataBoolean;
import com.bencodez.simpleapi.file.annotation.ConfigDataInt;
import com.bencodez.simpleapi.file.annotation.ConfigDataListString;
import com.bencodez.simpleapi.file.annotation.ConfigDataString;
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

	@ConfigDataString(path = "Redis.Host")
	@Getter
	private String redisHost = "";

	@ConfigDataString(path = "Redis.Username")
	@Getter
	private String redisUsername = "";

	@ConfigDataInt(path = "Redis.Db-Index")
	@Getter
	private int redisdbindex = 0;

	@ConfigDataString(path = "PluginMessageChannel")
	@Getter
	private String pluginMessagingChannel = "vp:vp";

	@ConfigDataBoolean(path = "PluginMessageEncryption")
	@Getter
	private boolean pluginMessageEncryption = false;

	@ConfigDataString(path = "Redis.Prefix")
	@Getter
	private String redisPrefix = "";

	@ConfigDataString(path = "Redis.Password")
	@Getter
	private String redisPassword = "";

	@ConfigDataInt(path = "Redis.Port")
	@Getter
	private int redisPort = 6379;

	@ConfigDataString(path = "MQTT.ClientID")
	@Getter
	private String mqttClientID = "";

	@ConfigDataString(path = "MQTT.BrokerURL")
	@Getter
	private String mqttBrokerURL = "tcp://localhost:1883";

	@ConfigDataString(path = "MQTT.Username")
	@Getter
	private String mqttUsername = "";

	@ConfigDataString(path = "MQTT.Password")
	@Getter
	private String mqttPassword = "";

	@ConfigDataString(path = "MQTT.Prefix")
	@Getter
	private String mqttPrefix = "";

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

	@ConfigDataBoolean(path = "GiveExtraAllSitesRewards")
	@Getter
	private boolean giveExtraAllSitesRewards = false;

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

	@ConfigDataBoolean(path = "GlobalData.UseMainMySQL")
	@Getter
	private boolean globlalDataUseMainMySQL = true;

	@ConfigDataBoolean(path = "GlobalData.Enabled")
	@Getter
	private boolean globlalDataEnabled = false;

	@ConfigDataListString(path = "BungeeVotePartyGlobalCommands")
	@Getter
	private ArrayList<String> bungeeVotePartyGlobalCommands = new ArrayList<>();

	public BungeeSettings(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "BungeeSettings.yml"));
		setIgnoreCase(true);
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
