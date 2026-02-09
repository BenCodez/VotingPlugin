
package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VotePartyEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteParty.
 */
public class VoteParty implements Listener {

	private VotingPluginMain plugin;

	public VoteParty(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void addTotal(VotingPluginUser user) {
		setTotalVotes(getTotalVotes() + 1);
		user.setVotePartyVotes(user.getVotePartyVotes() + 1);
		plugin.getPlaceholders().onVotePartyUpdate();
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
	public void addVotePlayer(VotingPluginUser user) {
		String uuid = user.getUUID();
		List<String> voted = getVotedUsers();
		if (voted == null) {
			voted = new ArrayList<>();
		}
		if (!voted.contains(uuid)) {
			voted.add(uuid);
			setVotedUsers(voted);
		}
		// Update timestamp for rolling window mode
		long timestamp = System.currentTimeMillis();
		plugin.getServerData().setVotePartyVotedTimestamp(uuid, timestamp);
		plugin.debug("[VoteParty] Updated timestamp for " + uuid + ": " + timestamp + " ("
				+ new java.util.Date(timestamp) + ")");
	}

	public void check(VotingPluginUser user, boolean forceBungee) {
		if (getTotalVotes() < getVotesRequired()) {
			plugin.extraDebug("Not enough votes for vote party: " + getTotalVotes() + " / " + getVotesRequired());
			return;
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
				&& plugin.getServerData().isLastVotePartySameDay()) {
			plugin.extraDebug("Already had vote party today");
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
				&& plugin.getServerData().isLastVotePartySameWeek()) {
			plugin.extraDebug("Already had vote party this week");
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyResetCount()) {
			setTotalVotes(getTotalVotes() - getVotesRequired());
		}

		VotePartyEvent event = new VotePartyEvent();
		Bukkit.getPluginManager().callEvent(event);
		if (event.isCancelled()) {
			return;
		}

		plugin.debug("[VoteParty] ========== VOTEPARTY TRIGGERED ==========");
		plugin.debug("[VoteParty] Total votes: " + getTotalVotes() + ", Required: " + getVotesRequired());
		plugin.debug("[VoteParty] RollingWindow24h: " + plugin.getSpecialRewardsConfig().isVotePartyRollingWindow24h());
		plugin.debug("[VoteParty] GiveAllPlayers: " + plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers());
		plugin.debug("[VoteParty] Timestamps map size BEFORE giveRewards: " + plugin.getServerData().getVotePartyVotedTimestamps().size());
		plugin.debug("[VoteParty] VotedUsers list size BEFORE giveRewards: " + getVotedUsers().size());

		giveRewards(user, forceBungee);

		plugin.debug("[VoteParty] Timestamps map size AFTER giveRewards: " + plugin.getServerData().getVotePartyVotedTimestamps().size());
		plugin.debug("[VoteParty] VotedUsers list size AFTER giveRewards: " + getVotedUsers().size());
		plugin.debug("[VoteParty] =========================================");

		if (plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired() > 0) {
			plugin.getServerData().setVotePartyExtraRequired(plugin.getServerData().getVotePartyExtraRequired()
					+ plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired());
		}

		plugin.getServerData().updateLastVoteParty();

		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()) {
			plugin.getServerData().updateLastVotePartyWeek();
		}

	}

	public void checkVoteReminder(VotingPluginUser user) {
		if (!user.isVanished()) {
			int neededVotes = getNeededVotes();

			for (Integer num1 : plugin.getSpecialRewardsConfig().getVotePartyVoteReminderAtVotes()) {
				int num = num1.intValue();
				if (neededVotes == num) {
					ArrayList<String> broadcastMessages = plugin.getSpecialRewardsConfig().getVotePartyVoteReminderBroadcast();
					HashMap<String, String> placeholders = new HashMap<>();
					placeholders.put("player", user.getPlayerName());
					placeholders.put("votesrequired", "" + neededVotes);
					for (String broadcastMessage : broadcastMessages) {
						MiscUtils.getInstance()
								.broadcast(PlaceholderUtils.replacePlaceHolder(broadcastMessage, placeholders));
					}
				}
			}
		}

	}

	/**
	 * Command vote party.
	 *
	 * @param sender the sender
	 */
	public void commandVoteParty(CommandSender sender) {
		if (plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
			ArrayList<String> msg = plugin.getConfigFile().getFormatCommandsVoteParty();
			int votesRequired = getVotesRequired();
			int votes = getTotalVotes();
			int neededVotes = votesRequired - votes;
			HashMap<String, String> placeholders = new HashMap<>();
			placeholders.put("votesrequired", "" + votesRequired);
			placeholders.put("neededvotes", "" + neededVotes);
			placeholders.put("votes", "" + votes);
			msg = ArrayUtils.colorize(PlaceholderUtils.replacePlaceHolder(msg, placeholders));
			if (sender instanceof Player) {
				Player p = (Player) sender;
				sender.sendMessage(ArrayUtils.convert(PlaceholderUtils.replacePlaceHolders(msg, p)));
			} else {
				sender.sendMessage(ArrayUtils.convert(msg));
			}
		} else {
			sender.sendMessage(MessageAPI.colorize("&cVoteParty not enabled"));
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

	public String getRandomPlayerName() {
		ArrayList<String> allPlayers = new ArrayList<>();
		for (Player players : Bukkit.getOnlinePlayers()) {
			allPlayers.add(players.getName());
		}
		if (allPlayers.size() == 0) {
			return "No Player";
		}
		int random = new Random().nextInt(allPlayers.size());
		return allPlayers.get(random);
	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		return plugin.getServerData().getData().getInt("VoteParty.Total");
	}

	/**
	 * Gets the voted users.
	 *
	 * @return the voted users
	 */
	public List<String> getVotedUsers() {
		List<String> list =  plugin.getServerData().getData().getStringList("VoteParty.Voted");
		if (list != null) {
			return list;
		}
		return new ArrayList<>();
	}

	public int getVotesRequired() {
		int required = plugin.getSpecialRewardsConfig().getVotePartyVotesRequired();
		int extra = plugin.getServerData().getVotePartyExtraRequired();
		if (extra > 0) {
			return required + extra;
		}
		return required;
	}

	public void giveReward(VotingPluginUser user, boolean useBungee) {
		if (plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired() > 0) {
			if (user.getVotePartyVotes() < plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired()) {
				return;
			}
		}
		giveReward(user, user.isOnline(), useBungee);
	}

	public void giveReward(VotingPluginUser user, boolean online, boolean useBungee) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVotePartyRewardsPath()).setOnline(online)
				.withPlaceHolder("VotesRequired", "" + plugin.getSpecialRewardsConfig().getVotePartyVotesRequired())
				.withPlaceHolder("FirstVotePartyToday", "" + !plugin.getServerData().isLastVotePartySameDay())
				.setServer(useBungee).send(user);
	}

	public void giveRewards(VotingPluginUser orgUser, boolean forceBungee) {
		ArrayList<String> broadcastMessages = plugin.getSpecialRewardsConfig().getVotePartyBroadcast();
		for (String broadcastMessage : broadcastMessages) {
			MiscUtils.getInstance().broadcast(broadcastMessage);
		}

		String player = getRandomPlayerName();
		for (final String cmd : plugin.getSpecialRewardsConfig().getVotePartyGlobalCommands()) {
			plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
							PlaceholderUtils.replacePlaceHolder(cmd, "randomonlineplayer", player));
				}

			});
		}

		final ArrayList<String> list = plugin.getSpecialRewardsConfig().getVotePartyGlobalRandomCommand();
		if (list.size() > 0) {
			plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), PlaceholderUtils.replacePlaceHolder(
							list.get(ThreadLocalRandom.current().nextInt(list.size())), "randomonlineplayer", player));
				}

			});
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers()) {
			plugin.debug("Trying to give all players vote party");

			ArrayList<String> alreadyGotten = new ArrayList<>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				VotingPluginUser user;
				if (orgUser != null && orgUser.getJavaUUID().equals(p.getUniqueId())) {
					user = orgUser;
				} else {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
					user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
				}

				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveReward(user, forceBungee);
				}
				alreadyGotten.add(p.getUniqueId().toString());
			}
			for (String uuid : plugin.getVotingPluginUserManager().getAllUUIDs()) {
				if (!alreadyGotten.contains(uuid)) {
					VotingPluginUser user;
					if (orgUser != null && orgUser.getJavaUUID().toString().equals(uuid)) {
						user = orgUser;
					} else {
						user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid));
						user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
					}

					if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
						giveReward(user, forceBungee);
					}
				}
			}
		} else {
			plugin.debug("Trying to give all voted players vote party");
			plugin.debug(ArrayUtils.makeStringList(getVotedUsers()));

			// Check if rolling window 24h mode is enabled
			if (plugin.getSpecialRewardsConfig().isVotePartyRollingWindow24h()) {
				plugin.debug("[VoteParty] Using 24h rolling window mode for VoteParty rewards");
				long currentTime = System.currentTimeMillis();
				long cutoffTime = currentTime - (24 * 60 * 60 * 1000L); // 24 hours ago
				Map<String, Long> timestamps = plugin.getServerData().getVotePartyVotedTimestamps();

				plugin.debug("[VoteParty] Current time: " + currentTime + " (" + new java.util.Date(currentTime) + ")");
				plugin.debug("[VoteParty] Cutoff time (24h ago): " + cutoffTime + " (" + new java.util.Date(cutoffTime) + ")");
				plugin.debug("[VoteParty] Total timestamps in map: " + timestamps.size());

				// Filter players who voted within last 24 hours
				List<String> validUuids = new ArrayList<>();
				for (Map.Entry<String, Long> entry : timestamps.entrySet()) {
					long voteTime = entry.getValue();
					boolean isValid = voteTime >= cutoffTime;
					plugin.debug("[VoteParty] Player " + entry.getKey() + ": voteTime=" + voteTime + " ("
							+ new java.util.Date(voteTime) + "), valid=" + isValid + ", hoursAgo="
							+ ((currentTime - voteTime) / (60 * 60 * 1000.0)));
					if (isValid) {
						validUuids.add(entry.getKey());
					}
				}

				plugin.debug("[VoteParty] Found " + validUuids.size() + " players who voted in last 24h out of "
						+ timestamps.size() + " total");

				// Cleanup old timestamps (older than 24h) to prevent data accumulation
				// Only cleanup if there are many timestamps to avoid lag on large servers
				// Cleanup async to not block the main thread
				if (timestamps.size() > 50) {
					final long finalCutoffTime = cutoffTime;
					plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {
						@Override
						public void run() {
							int removedCount = plugin.getServerData().removeOldVotePartyVotedTimestamps(finalCutoffTime);
							if (removedCount > 0) {
								plugin.debug("[VoteParty] Cleaned up " + removedCount + " old timestamps (older than 24h) [ASYNC]");
							}
						}
					});
				} else {
					// For small servers, cleanup sync is fine
					int removedCount = plugin.getServerData().removeOldVotePartyVotedTimestamps(cutoffTime);
					if (removedCount > 0) {
						plugin.debug("[VoteParty] Cleaned up " + removedCount + " old timestamps (older than 24h)");
					}
				}

				if (validUuids.isEmpty()) {
					plugin.debug("[VoteParty] WARNING: No valid players found in 24h window! This might indicate a problem.");
					plugin.debug("[VoteParty] All timestamps in map:");
					for (Map.Entry<String, Long> entry : timestamps.entrySet()) {
						double hoursAgo = (currentTime - entry.getValue()) / (60.0 * 60.0 * 1000.0);
						plugin.debug("[VoteParty]   - " + entry.getKey() + ": " + hoursAgo + " hours ago ("
								+ new java.util.Date(entry.getValue()) + ")");
					}
				}

				for (String uuid : validUuids) {
					VotingPluginUser user;
					if (orgUser != null && orgUser.getJavaUUID().toString().equals(uuid)) {
						user = orgUser;
					} else {
						user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid));
						user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
					}
					plugin.debug("[VoteParty] Processing reward for " + uuid + " (online=" + user.isOnline()
							+ ", name=" + user.getPlayerName() + ")");
					if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
						plugin.debug("[VoteParty] ✓ Giving reward to " + uuid + " (" + user.getPlayerName() + ")");
						giveReward(user, forceBungee);
					} else {
						plugin.debug("[VoteParty] ✗ Skipping " + uuid + " (GiveOnlinePlayersOnly=true and player offline)");
					}
				}

				plugin.debug("[VoteParty] Finished giving rewards to " + validUuids.size() + " players");
			} else {
				// Old behavior: give rewards to all voted players
				for (String uuid : getVotedUsers()) {
					VotingPluginUser user;
					if (orgUser != null && orgUser.getJavaUUID().toString().equals(uuid)) {
						user = orgUser;
					} else {
						user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid));
						user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
					}
					if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
						giveReward(user, forceBungee);
					}
				}
			}
		}

		reset(false);
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetEachDay()) {
			reset(true);
		}
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetMonthly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetWeekly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesWeekly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void reset(boolean override) {
		plugin.debug("[VoteParty] reset() called with override=" + override);
		Map<String, Long> timestampsBeforeReset = plugin.getServerData().getVotePartyVotedTimestamps();
		long currentTime = System.currentTimeMillis();
		plugin.debug("[VoteParty] Timestamps BEFORE reset: " + timestampsBeforeReset.size() + " entries");
		for (Map.Entry<String, Long> entry : timestampsBeforeReset.entrySet()) {
			long hoursAgo = (currentTime - entry.getValue()) / (60 * 60 * 1000L);
			long minutesAgo = (currentTime - entry.getValue()) / (60 * 1000L);
			plugin.debug("[VoteParty]   - " + entry.getKey() + ": " + hoursAgo + " hours ago (" + minutesAgo
					+ " minutes ago, " + new java.util.Date(entry.getValue()) + ")");
		}

		if (override) {
			setTotalVotes(0);
		}
		setVotedUsers(new ArrayList<>());
		// Only clear timestamps if override is true (full reset)
		// If override is false, we keep timestamps for rolling window mode
		if (override) {
			plugin.debug("[VoteParty] Clearing timestamps (override=true)");
			plugin.getServerData().clearVotePartyVotedTimestamps();
		} else {
			plugin.debug("[VoteParty] Keeping timestamps for rolling window mode (override=false)");
			Map<String, Long> timestampsAfterReset = plugin.getServerData().getVotePartyVotedTimestamps();
			plugin.debug("[VoteParty] Timestamps AFTER reset (kept): " + timestampsAfterReset.size() + " entries");
			// Verify timestamps are still there
			if (timestampsAfterReset.size() != timestampsBeforeReset.size()) {
				plugin.getLogger().warning("[VoteParty] WARNING: Timestamp count changed after reset! Before: "
						+ timestampsBeforeReset.size() + ", After: " + timestampsAfterReset.size());
			}
		}
		resetVotePartyCount();

	}

	public void resetVotePartyCount() {
		plugin.getUserManager().removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
	}

	/**
	 * Sets the total votes.
	 *
	 * @param value the new total votes
	 */
	public void setTotalVotes(int value) {
		plugin.getServerData().getData().set("VoteParty.Total", value);
		plugin.getServerData().saveData();
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value the new voted users
	 */
	public void setVotedUsers(List<String> value) {
		plugin.getServerData().getData().set("VoteParty.Voted", value);
		plugin.getServerData().saveData();
	}

	public synchronized void vote(VotingPluginUser user, boolean realVote, boolean forceBungee) {
		if (plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
			if (plugin.getSpecialRewardsConfig().isVotePartyCountFakeVotes() || realVote) {
				if (plugin.getSpecialRewardsConfig().isVotePartyCountOfflineVotes() || user.isOnline()) {
					addTotal(user);
					addVotePlayer(user);
					check(user, forceBungee);
					checkVoteReminder(user);
				}
			}
		}
	}

}
