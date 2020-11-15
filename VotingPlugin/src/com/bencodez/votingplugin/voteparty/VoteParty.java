
package com.bencodez.votingplugin.voteparty;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.VotePartyEvent;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.usermanager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteParty.
 */
public class VoteParty implements Listener {

	/** The instance. */
	static VoteParty instance = new VoteParty();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

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
	public VoteParty(VotingPluginMain plugin) {
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
		if (getTotalVotes() >= getVotesRequired()) {
			setTotalVotes(getTotalVotes() - getVotesRequired());

			VotePartyEvent event = new VotePartyEvent();
			Bukkit.getPluginManager().callEvent(event);
			if (event.isCancelled()) {
				return;
			}
			MiscUtils.getInstance().broadcast(SpecialRewardsConfig.getInstance().getVotePartyBroadcast());
			giveRewards();

			if (SpecialRewardsConfig.getInstance().getVotePartyIncreaseVotesRquired() > 0) {
				ServerData.getInstance().setVotePartyExtraRequired(ServerData.getInstance().getVotePartyExtraRequired()
						+ SpecialRewardsConfig.getInstance().getVotePartyIncreaseVotesRquired());
			}
		}

	}

	/**
	 * Command vote party.
	 *
	 * @param sender
	 *            the sender
	 */
	public void commandVoteParty(CommandSender sender) {
		if (SpecialRewardsConfig.getInstance().getVotePartyEnabled()) {
			ArrayList<String> msg = Config.getInstance().getFormatCommandsVoteParty();
			int votesRequired = getVotesRequired();
			int votes = getTotalVotes();
			int neededVotes = votesRequired - votes;
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("votesrequired", "" + votesRequired);
			placeholders.put("neededvotes", "" + neededVotes);
			placeholders.put("votes", "" + votes);
			msg = ArrayUtils.getInstance().colorize(ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders));
			sender.sendMessage(ArrayUtils.getInstance().convert(msg));
		} else {
			sender.sendMessage(StringParser.getInstance().colorize("&cVoteParty not enabled"));
		}
	}

	/**
	 * Gets the needed votes.
	 *
	 * @return the needed votes
	 */
	public int getNeededVotes() {
		int votesRequired = getVotesRequired();
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

	public int getVotesRequired() {
		int required = SpecialRewardsConfig.getInstance().getVotePartyVotesRequired();
		int extra = ServerData.getInstance().getVotePartyExtraRequired();
		if (extra > 0) {
			return required + extra;
		}
		return required;
	}

	public void giveReward(User user) {
		if (SpecialRewardsConfig.getInstance().getVotePartyUserVotesRequired() > 0) {
			if (user.getVotePartyVotes() < SpecialRewardsConfig.getInstance().getVotePartyUserVotesRequired()) {
				return;
			}
		}
		giveReward(user, user.isOnline());
	}

	public void giveReward(User user, boolean online) {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getVotePartyRewardsPath()).setOnline(online)
						.withPlaceHolder("VotesRequired",
								"" + SpecialRewardsConfig.getInstance().getVotePartyVotesRequired())
						.send(user);
	}

	/**
	 * Give rewards.
	 */
	public void giveRewards() {
		for (final String cmd : SpecialRewardsConfig.getInstance().getVotePartyCommands()) {
			Bukkit.getScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), cmd);
				}

			});
		}

		if (SpecialRewardsConfig.getInstance().getVotePartyGiveAllPlayers()) {
			plugin.debug("Trying to give all players vote party");
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				giveReward(user);
			}
		} else {
			plugin.debug("Trying to give all online players vote party");
			plugin.debug(ArrayUtils.getInstance().makeStringList(getVotedUsers()));
			for (String uuid : getVotedUsers()) {
				User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				giveReward(user);
			}
		}
		reset();
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (SpecialRewardsConfig.getInstance().getVotePartyResetEachDay()) {
			reset();
		}
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (SpecialRewardsConfig.getInstance().getVotePartyResetMontly()) {
			reset();
		}

		if (SpecialRewardsConfig.getInstance().isVotePartyResetExtraVotesMonthly()) {
			ServerData.getInstance().setVotePartyExtraRequired(0);
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void reset() {
		setVotedUsers(new ArrayList<String>());
		setTotalVotes(0);
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

	public synchronized void vote(User user, boolean realVote) {
		if (SpecialRewardsConfig.getInstance().getVotePartyEnabled()) {
			if (SpecialRewardsConfig.getInstance().getVotePartyCountFakeVotes() || realVote) {
				if (SpecialRewardsConfig.getInstance().getVotePartyCountOfflineVotes() || user.isOnline()) {
					addTotal(user);
					addVotePlayer(user);
					check();
				}
			}
		}
	}

}
