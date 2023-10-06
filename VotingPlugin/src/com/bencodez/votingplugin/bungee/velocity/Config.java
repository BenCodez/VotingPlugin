package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.checkerframework.checker.nullness.qual.NonNull;

import com.bencodez.advancedcore.bungeeapi.velocity.VelocityYMLFile;

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

	public List<String> getWhiteListedServers() {
		return getStringList(getNode("WhiteListedServers"), new ArrayList<String>());
	}

	public boolean getBroadcast() {
		return getBoolean(getNode("Broadcast"), false);
	}

	public boolean getOnlineMode() {
		return getBoolean(getNode("OnlineMode"), true);
	}

	public boolean getGlobalDataEnabled() {
		return getBoolean(getNode("GlobalData", "Enabled"), false);
	}

	public boolean getGlobalDataUseMainMySQL() {
		return getBoolean(getNode("GlobalData", "UseMainMySQL"), true);
	}

	public boolean getBungeeManageTotals() {
		return getBoolean(getNode("BungeeManageTotals"), true);
	}

	public String getBungeeMethod() {
		return getString(getNode("BungeeMethod"), "PLUGINMESSAGING");
	}

	public String getBungeeHost() {
		return getString(getNode("BungeeServer", "Host"), "");
	}

	public int getBungeePort() {
		return getInt(getNode("BungeeServer", "Port"), 1297);
	}

	public int getMaxAmountOfVotesPerDay() {
		return getInt(getNode("MaxAmountOfVotesPerDay"), -1);
	}

	public int getVoteCacheTime() {
		return getInt(getNode("VoteCacheTime"), -1);
	}

	public boolean getDebug() {
		return getBoolean(getNode("Debug"), false);
	}

	public String getFallBack() {
		return getString(getNode("FallBackServer"), "");
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

	public boolean getVotePartyEnabled() {
		return getBoolean(getVotePartyNode().getNode("Enabled"), false);
	}

	public ConfigurationNode getVotePartyNode() {
		return getNode("VoteParty");
	}

	public boolean getVotePartySendToAllServers() {
		return getBoolean(getVotePartyNode().getNode("SendToAllServers"), false);
	}

	public int getVotePartyVotesRequired() {
		return getInt(getVotePartyNode().getNode("VotesRequired"), 100);
	}

	public int getVotePartyIncreaseVotesRequired() {
		return getInt(getVotePartyNode().getNode("IncreaseVotesRequired"), 0);
	}

	public String getVotePartyBroadcast() {
		return getString(getVotePartyNode().getNode("Broadcast"), "");
	}

	public List<String> getVotePartyServersToSend() {
		return getStringList(getVotePartyNode().getNode("ServersToSend"), new ArrayList<String>());
	}

	public List<String> getVotePartyBungeeCommands() {
		return getStringList(getVotePartyNode().getNode("BungeeCommands"), new ArrayList<String>());
	}

	public ConfigurationNode getSpigotServerConfiguration(String s) {
		return getNode("SpigotServers", s);
	}

	public @NonNull Collection<? extends ConfigurationNode> getSpigotServers() {
		return getNode("SpigotServers").getChildrenMap().values();
	}

	public boolean getWaitForUserOnline() {
		return getBoolean(getNode("WaitForUserOnline"), false);
	}

	public boolean getMultiProxySupport() {
		return getBoolean(getNode("MultiProxySupport"), false);
	}

	public boolean getMultiProxyOneGlobalReward() {
		return getBoolean(getNode("MultiProxyOneGlobalReward"), false);
	}

	public boolean getGeyserSupport() {
		return getBoolean(getNode("GeyserSupport"), false);
	}

	public boolean getPrimaryServer() {
		return getBoolean(getNode("PrimaryServer"), false);
	}

	public String getGeyserPrefix() {
		return getString(getNode("GeyserPrefix"), "*");
	}

	public ConfigurationNode getMultiProxyServers(String s) {
		return getNode("MultiProxyServers", s);
	}

	public @NonNull Collection<? extends ConfigurationNode> getMultiProxyServers() {
		return getNode("MultiProxyServers").getChildrenMap().values();
	}

	public String getMultiProxySocketHostHost() {
		return getString(getNode("MultiProxySocketHost", "Host"), "");
	}

	public int getMultiProxySocketHostPort() {
		return getInt(getNode("MultiProxySocketHost", "Port"), 1297);
	}

}
