package com.Ben12345rocks.VotingPlugin.bungee;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HashMap;

import com.Ben12345rocks.AdvancedCore.UserStorage.sql.DataType;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.ClientHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketHandler;
import com.Ben12345rocks.AdvancedCore.Util.Sockets.SocketReceiver;
import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import lombok.Getter;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.config.Configuration;

public class Bungee extends Plugin implements net.md_5.bungee.api.plugin.Listener {

	@Getter
	private Config config;

	@Getter
	private BungeeMySQL mysql;

	private SocketHandler socketHandler;

	private HashMap<String, ClientHandler> clientHandles;

	@Override
	public void onEnable() {
		getProxy().getPluginManager().registerListener(this, this);
		config = new Config(this);
		config.load();

		mysql = new BungeeMySQL("VotingPlugin_Users", config.getData());

		// column types
		getMysql().alterColumnType("TopVoterIgnore", "VARCHAR(5)");
		getMysql().alterColumnType("CheckWorld", "VARCHAR(5)");
		getMysql().alterColumnType("Reminded", "VARCHAR(5)");
		getMysql().alterColumnType("DisableBroadcast", "VARCHAR(5)");
		getMysql().alterColumnType("LastOnline", "VARCHAR(20)");
		getMysql().alterColumnType("PlayerName", "VARCHAR(30)");
		getMysql().alterColumnType("DailyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("WeeklyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("DayVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("BestDayVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("WeekVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("BestWeekVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("VotePartyVotes", "INT DEFAULT '0'");
		getMysql().alterColumnType("MonthVoteStreak", "INT DEFAULT '0'");
		getMysql().alterColumnType("Points", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestDailyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("MileStoneTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("AllTimeTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestMonthlyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("MilestoneCount", "INT DEFAULT '0'");
		getMysql().alterColumnType("MonthTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("HighestWeeklyTotal", "INT DEFAULT '0'");
		getMysql().alterColumnType("LastMonthTotal", "INT DEFAULT '0'");

		getProxy().getPluginManager().registerCommand(this, new VotingPluginBungeeCommand(this));

		socketHandler = new SocketHandler(getDescription().getVersion(), config.getBungeeHost(),
				config.getBungeePort());

		socketHandler.add(new SocketReceiver() {

			@Override
			public void onReceive(String[] data) {
				if (data.length > 2) {
					if (data[0].equalsIgnoreCase("Broadcast")) {
						sendServerMessage(data);
					}
				}
			}
		});

		clientHandles = new HashMap<String, ClientHandler>();
		for (String s : config.getSpigotServers()) {
			Configuration d = config.getSpigotServerConfiguration(s);
			clientHandles.put(s, new ClientHandler(d.getString("Host", ""), d.getInt("Port", 1298)));
		}
	}

	public void reload() {
		config.load();
	}

	public void onVote(VotifierEvent event) {
		Vote vote = event.getVote();
		saveVote(vote.getUsername(), vote.getServiceName());
	}

	public String getUUID(String playerName) {
		ProxiedPlayer p = getProxy().getPlayer(playerName);
		if (p != null && p.isConnected()) {
			return p.getUniqueId().toString();
		}
		String str = mysql.getUUID(playerName);
		if (str != null) {
			return str;
		}
		return "";
	}

	public void saveVote(String name, String service) {
		String uuid = getUUID(name);

		if (!uuid.isEmpty()) {
			String data = mysql.getProxyVotes(uuid, "online");
			String finalData = service + "%time%"
					+ LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
			if (data != null && !data.isEmpty()) {
				finalData = data + "%line%" + service + "%time%"
						+ LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
			}
			if (config.getSendVotesToAllServers()) {
				for (String send : getProxy().getServers().keySet()) {
					mysql.update(uuid, "Proxy_" + send, finalData, DataType.STRING);
				}
				sendServerMessage("bungeevote", uuid, name);
				if (config.getBroadcast()) {
					sendServerMessage("BungeeBroadcast", service, uuid, name);
				}
			} else if (config.getSendToOnlineServer()) {
				mysql.update(uuid, "Proxy_Online", finalData, DataType.STRING);
				sendServerMessage("bungeevote", uuid, name);
				if (config.getBroadcast()) {
					sendServerMessage("BungeeBroadcast", service, uuid, name);
				}
			}
		}
	}

	public void sendServerMessage(String... messageData) {
		for (ClientHandler h : clientHandles.values()) {
			h.sendMessage(messageData);
		}
	}

}
