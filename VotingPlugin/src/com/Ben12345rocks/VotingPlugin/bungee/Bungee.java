package com.Ben12345rocks.VotingPlugin.bungee;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.time.LocalDateTime;
import java.time.ZoneId;

import com.Ben12345rocks.AdvancedCore.UserStorage.sql.DataType;
import com.vexsoftware.votifier.bungee.events.VotifierEvent;
import com.vexsoftware.votifier.model.Vote;

import lombok.Getter;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.event.PluginMessageEvent;
import net.md_5.bungee.api.plugin.Plugin;
import net.md_5.bungee.event.EventHandler;

public class Bungee extends Plugin implements net.md_5.bungee.api.plugin.Listener {

	@Getter
	private Config config;

	@Getter
	private BungeeMySQL mysql;

	@Override
	public void onEnable() {
		getProxy().getPluginManager().registerListener(this, this);
		this.getProxy().registerChannel("VotingPlugin:VotingPlugin".toLowerCase());
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
	}

	public void reload() {
		config.load();
	}

	@EventHandler
	public void onPluginMessage(PluginMessageEvent ev) {
		if (!ev.getTag().equals("VotingPlugin:VotingPlugin".toLowerCase())) {
			return;
		}
		ByteArrayInputStream instream = new ByteArrayInputStream(ev.getData());
		DataInputStream in = new DataInputStream(instream);
		try {
			ByteArrayOutputStream outstream = new ByteArrayOutputStream();
			DataOutputStream out = new DataOutputStream(outstream);
			String subchannel = in.readUTF();
			int size = in.readInt();
			out.writeUTF(subchannel);
			out.writeInt(size);
			for (int i = 0; i < size; i++) {
				out.writeUTF(in.readUTF());
			}
			for (String send : getProxy().getServers().keySet()) {
				if (getProxy().getServers().get(send).getPlayers().size() > 0) {
					getProxy().getServers().get(send).sendData("VotingPlugin:VotingPlugin".toLowerCase(),
							outstream.toByteArray());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
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
					sendPluginMessage("bungeevote", uuid, name);
				}
			} else if (config.getSendToOnlineServer()) {
				mysql.update(uuid, "Proxy_Online", finalData, DataType.STRING);
				sendPluginMessage("bungeevote", uuid, name);
			}
		}
	}

	public void sendPluginMessage(String channel, String... messageData) {
		ByteArrayOutputStream byteOutStream = new ByteArrayOutputStream();
		DataOutputStream out = new DataOutputStream(byteOutStream);
		try {
			out.writeUTF(channel);
			out.writeInt(messageData.length);
			for (String message : messageData) {
				out.writeUTF(message);
			}
			for (String send : getProxy().getServers().keySet()) {
				if (getProxy().getServers().get(send).getPlayers().size() > 0) {
					getProxy().getServers().get(send).sendData("VotingPlugin:VotingPlugin".toLowerCase(),
							byteOutStream.toByteArray());
				}
			}
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
