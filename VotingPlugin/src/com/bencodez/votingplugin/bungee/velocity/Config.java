package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import ninja.leaping.configurate.ConfigurationNode;

public class Config extends VelocityYMLFile {

	public Config(File file) {
		super(file);
	}

	public boolean getAllowUnJoined() {
		return getBoolean(getNode("AllowUnJoined"), false);
	}

	public List<String> getBlockedServers() {
		return getStringList(getNode("BlockedServers"), new ArrayList<String>());
	}

	public boolean getBroadcast() {
		return getBoolean(getNode("Broadcast"), false);
	}

	public String getBungeeHost() {
		return getString(getNode("BungeeServer", "Host"), "");
	}

	public boolean getBungeeManageTotals() {
		return getBoolean(getNode("BungeeManageTotals"), true);
	}

	public String getBungeeMethod() {
		return getString(getNode("BungeeMethod"), "PLUGINMESSAGING");
	}

	public int getBungeePort() {
		return getInt(getNode("BungeeServer", "Port"), 1297);
	}

	public boolean getDebug() {
		return getBoolean(getNode("Debug"), false);
	}

	public String getFallBack() {
		return getString(getNode("FallBackServer"), "");
	}

	public long getLong(ConfigurationNode node, long def) {
		return node.getLong(def);
	}

	public ConfigurationNode getMysqlNode() {
		return getNode("MySQL");
	}

	public int getPointsOnVote() {
		return getInt(getNode("PointsOnVote"), 1);
	}

	public boolean getSendVotesToAllServers() {
		return getBoolean(getNode("SendVotesToAllServers"), true);
	}

	public ConfigurationNode getSpigotServerConfiguration(String s) {
		return getNode("SpigotServers", s);
	}

	public Collection<String> getSpigotServers() {
		ArrayList<String> list = new ArrayList<String>();
		for (ConfigurationNode conf : getNode("SpigotServers").getChildrenList()) {
			list.add(conf.getKey().toString());
		}
		return list;
	}

	public boolean getWaitForUserOnline() {
		return getBoolean(getNode("WaitForUserOnline"), false);
	}
}
