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

// TODO: Auto-generated Javadoc
/**
 * The Class VoteParty.
 */
public class VoteParty {
	
	/** The instance. */
	static VoteParty instance = new VoteParty();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of VoteParty.
	 *
	 * @return single instance of VoteParty
	 */
	public static VoteParty getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new vote party.
	 */
	private VoteParty() {
	}

	/**
	 * Instantiates a new vote party.
	 *
	 * @param plugin the plugin
	 */
	public VoteParty(Main plugin) {
		VoteParty.plugin = plugin;
	}

	/**
	 * Adds the total.
	 */
	public void addTotal() {
		setTotalVotes(getTotalVotes() + 1);
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
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

	/**
	 * Check.
	 */
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

	/**
	 * Command vote party.
	 *
	 * @param sender the sender
	 */
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

	/**
	 * Gets the offline vote party votes.
	 *
	 * @param user the user
	 * @return the offline vote party votes
	 */
	public int getOfflineVotePartyVotes(User user) {
		return Data.getInstance().getData(user)
				.getInt("VoteParty.OfflineVotes");
	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		return ServerData.getInstance().getData().getInt("VoteParty.Total");
	}

	/**
	 * Gets the voted users.
	 *
	 * @return the voted users
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotedUsers() {
		ArrayList<String> list = (ArrayList<String>) ServerData.getInstance()
				.getData().getList("VoteParty.Voted");
		if (list != null) {
			return list;
		}
		return new ArrayList<String>();
	}

	/**
	 * Give reward.
	 *
	 * @param user the user
	 */
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

	/**
	 * Give rewards.
	 */
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

	/**
	 * Sets the offline vote party votes.
	 *
	 * @param user the user
	 * @param value the value
	 */
	public void setOfflineVotePartyVotes(User user, int value) {
		Data.getInstance().getData(user).set("VoteParty.OfflineVotes", value);
	}

	/**
	 * Sets the total votes.
	 *
	 * @param value the new total votes
	 */
	public void setTotalVotes(int value) {
		ServerData.getInstance().getData().set("VoteParty.Total", value);
		ServerData.getInstance().saveData();
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value the new voted users
	 */
	public void setVotedUsers(ArrayList<String> value) {
		ServerData.getInstance().getData().set("VoteParty.Voted", value);
		ServerData.getInstance().saveData();
	}

}
