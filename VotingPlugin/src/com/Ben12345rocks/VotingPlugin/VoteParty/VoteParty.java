package com.Ben12345rocks.VotingPlugin.VoteParty;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.DayChangeEvent;
import com.Ben12345rocks.AdvancedCore.Objects.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteParty.
 */
public class VoteParty implements Listener {

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
	 * @param plugin
	 *            the plugin
	 */
	public VoteParty(Main plugin) {
		VoteParty.plugin = plugin;
	}

	public void addTotal(User user) {
		setTotalVotes(getTotalVotes() + 1);
		user.setVotePartyVotes(user.getVotePartyVotes() + 1);
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user
	 *            the user
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
		if (getTotalVotes() >= Config.getInstance().getVotePartyVotesRequired()) {
			setTotalVotes(getTotalVotes() - Config.getInstance().getVotePartyVotesRequired());
			giveRewards();
			MiscUtils.getInstance().broadcast(Config.getInstance().getVotePartyBroadcast());
		}

	}

	/**
	 * Command vote party.
	 *
	 * @param sender
	 *            the sender
	 */
	public void commandVoteParty(CommandSender sender) {
		if (Config.getInstance().getVotePartyEnabled()) {
			ArrayList<String> msg = Config.getInstance().getFormatCommandsVoteParty();
			int votesRequired = Config.getInstance().getVotePartyVotesRequired();
			int votes = getTotalVotes();
			int neededVotes = votesRequired - votes;
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("votesrequired", "" + votesRequired);
			placeholders.put("neededvotes", "" + neededVotes);
			placeholders.put("votes", "" + votes);
			msg = ArrayUtils.getInstance().colorize(ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders));
			sender.sendMessage(ArrayUtils.getInstance().convert(msg));
		} else {
			sender.sendMessage(StringUtils.getInstance().colorize("&cVoteParty not enabled"));
		}
	}

	/**
	 * Gets the needed votes.
	 *
	 * @return the needed votes
	 */
	public int getNeededVotes() {
		int votesRequired = Config.getInstance().getVotePartyVotesRequired();
		int votes = getTotalVotes();
		int neededVotes = votesRequired - votes;
		return neededVotes;
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
		ArrayList<String> list = (ArrayList<String>) ServerData.getInstance().getData().getList("VoteParty.Voted");
		if (list != null) {
			return list;
		}
		return new ArrayList<String>();
	}

	/**
	 * Give reward.
	 *
	 * @param user
	 *            the user
	 */
	public void giveReward(User user) {
		/*
		 * if (PlayerUtils.getInstance().isPlayerOnline(user.getPlayerName())) { if
		 * (user.getVotePartyVotes() >= Config.getInstance().getUserVotesRequired()) {
		 * RewardHandler.getInstance().giveReward(user, Config.getInstance().getData(),
		 * Config.getInstance().getVotePartyRewardsPath()); } } else {
		 * user.addOfflineOtherReward("VoteParty"); }
		 */
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getVotePartyRewardsPath())
				.setOnline(user.isOnline())
				.withPlaceHolder("VotesRequired", "" + Config.getInstance().getVotePartyVotesRequired()).send(user);
	}

	/**
	 * Give rewards.
	 */
	public void giveRewards() {
		if (Config.getInstance().getVotePartyGiveAllPlayers()) {
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				giveReward(user);
			}
		} else {
			for (String uuid : getVotedUsers()) {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				giveReward(user);
			}
		}
		reset();
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (Config.getInstance().getVotePartyResetEachDay()) {
			setTotalVotes(0);
			setVotedUsers(new ArrayList<String>());
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void reset() {
		setVotedUsers(new ArrayList<String>());
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getVotePartyVotes() != 0) {
				user.setVotePartyVotes(0);
			}
		}
	}

	/**
	 * Sets the total votes.
	 *
	 * @param value
	 *            the new total votes
	 */
	public void setTotalVotes(int value) {
		ServerData.getInstance().getData().set("VoteParty.Total", value);
		ServerData.getInstance().saveData();
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value
	 *            the new voted users
	 */
	public void setVotedUsers(ArrayList<String> value) {
		ServerData.getInstance().getData().set("VoteParty.Voted", value);
		ServerData.getInstance().saveData();
	}

}
