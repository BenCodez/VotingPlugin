package com.Ben12345rocks.VotingPlugin.VoteParty;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteParty {
	static VoteParty instance = new VoteParty();

	static Main plugin = Main.plugin;

	public static VoteParty getInstance() {
		return instance;
	}

	private VoteParty() {
	}

	public VoteParty(Main plugin) {
		VoteParty.plugin = plugin;
	}

	public void addTotal() {
		setTotalVotes(getTotalVotes() + 1);
	}

	public void addVotePlayer(User user) {
		String uuid = user.getUUID();
		ArrayList<String> voted = getVotedUsers();
		if (voted == null) {
			voted = new ArrayList<String>();
		}
		if (!voted.contains(uuid)) {
			voted.add(uuid);
			setVotedUsers(voted);
		}
	}

	public void check() {
		if (ConfigOtherRewards.getInstance().getVotePartyEnabled()) {
			if (getTotalVotes() >= ConfigOtherRewards.getInstance()
					.getVotesRequired()) {
				setTotalVotes(getTotalVotes()
						- ConfigOtherRewards.getInstance().getVotesRequired());
				giveRewards();
			}
		}
	}

	public void commandVoteParty(CommandSender sender) {
		ArrayList<String> msg = ConfigFormat.getInstance()
				.getCommandsVoteParty();
		ArrayList<String> lines = new ArrayList<String>();
		int votesRequired = ConfigOtherRewards.getInstance().getVotesRequired();
		int votes = getTotalVotes();
		int neededVotes = votesRequired - votes;
		for (String line : msg) {
			line = line.replace("%VotesRequired%", "" + votesRequired)
					.replace("%NeededVotes%", "" + neededVotes)
					.replace("%Votes%", "" + votes);
			lines.add(Utils.getInstance().colorize(line));
		}
		sender.sendMessage(Utils.getInstance().convertArray(lines));
	}

	public int getOfflineVotePartyVotes(User user) {
		return Data.getInstance().getData(user)
				.getInt("VoteParty.OfflineVotes");
	}

	public int getTotalVotes() {
		return ServerData.getInstance().getData().getInt("VoteParty.Total");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotedUsers() {
		return (ArrayList<String>) ServerData.getInstance().getData()
				.getList("VoteParty.Voted");
	}

	public void giveReward(User user) {
		if (Utils.getInstance().isPlayerOnline(user.getPlayerName())) {
			for (String reward : ConfigOtherRewards.getInstance()
					.getVotePartyRewards()) {
				user.giveReward(ConfigRewards.getInstance().getReward(reward));
			}
		} else {
			setOfflineVotePartyVotes(user, getOfflineVotePartyVotes(user) + 1);
		}
	}

	public void giveRewards() {
		if (ConfigOtherRewards.getInstance().getVotePartyGiveAllPlayers()) {
			for (User user : Data.getInstance().getUsers()) {
				giveReward(user);
			}
		} else {
			for (String uuid : getVotedUsers()) {
				User user = new User(new UUID(uuid));
				giveReward(user);
			}
		}
		setVotedUsers(new ArrayList<String>());
	}

	public void setOfflineVotePartyVotes(User user, int value) {
		Data.getInstance().getData(user).set("VoteParty.OfflineVotes", value);
	}

	public void setTotalVotes(int value) {
		ServerData.getInstance().getData().set("VoteParty.Total", value);
		ServerData.getInstance().saveData();
	}

	public void setVotedUsers(ArrayList<String> value) {
		ServerData.getInstance().getData().set("VoteParty.Voted", value);
		ServerData.getInstance().saveData();
	}

}
