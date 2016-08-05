package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.md_5.bungee.api.chat.TextComponent;

import org.apache.commons.lang3.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.Effect;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.potion.PotionEffect;
import org.bukkit.potion.PotionEffectType;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteReminding;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.Util.Effects.ActionBar;
import com.Ben12345rocks.VotingPlugin.Util.Effects.BossBar;
import com.Ben12345rocks.VotingPlugin.Util.Effects.Title;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.Ben12345rocks.VotingPlugin.VoteReminding.VoteReminding;

// TODO: Auto-generated Javadoc
/**
 * The Class User.
 */
public class User {

	/** The plugin. */
	static Main plugin = Main.plugin;

	/** The player name. */
	private String playerName;

	/** The uuid. */
	private String uuid;

	/**
	 * Instantiates a new user.
	 *
	 * @param player
	 *            the player
	 */
	public User(Player player) {
		playerName = player.getName();
		uuid = player.getUniqueId().toString();
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param playerName
	 *            the player name
	 */
	public User(String playerName) {
		this.playerName = playerName;
		uuid = Utils.getInstance().getUUID(playerName);

	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid
	 *            the uuid
	 */
	public User(UUID uuid) {
		this.uuid = uuid.getUUID();
		playerName = Utils.getInstance().getPlayerName(this.uuid);

	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid
	 *            the uuid
	 * @param loadName
	 *            the load name
	 */
	public User(UUID uuid, boolean loadName) {
		this.uuid = uuid.getUUID();
		if (loadName) {
			playerName = Utils.getInstance().getPlayerName(this.uuid);
		}
	}

	/**
	 * Adds the cumulative reward.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addCumulativeReward(VoteSite voteSite) {
		Data.getInstance().addCumulativeSite(this, voteSite.getSiteName());
	}

	/**
	 * Adds the offline vote.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addOfflineVote(VoteSite voteSite) {
		setOfflineVotes(voteSite, getOfflineVotes(voteSite) + 1);
	}

	/**
	 * Adds the points.
	 */
	public void addPoints() {
		setPoints(getPoints() + 1);
	}

	/**
	 * Adds the total.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotal(VoteSite voteSite) {
		User user = this;
		Data.getInstance().addTotal(user, voteSite.getSiteName());
	}

	/**
	 * Adds the total daily.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotalDaily(VoteSite voteSite) {
		Data.getInstance()
				.setTotalDaily(
						this,
						voteSite.getSiteName(),
						Data.getInstance().getTotalDaily(this,
								voteSite.getSiteName()) + 1);
	}

	/**
	 * Adds the total weekly.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotalWeekly(VoteSite voteSite) {
		Data.getInstance()
				.setTotalWeek(
						this,
						voteSite.getSiteName(),
						Data.getInstance().getTotalWeek(this,
								voteSite.getSiteName()) + 1);
	}

	/**
	 * Can vote all.
	 *
	 * @return true, if successful
	 */
	public boolean canVoteAll() {
		ArrayList<VoteSite> voteSites = ConfigVoteSites.getInstance()
				.getVoteSites();

		for (VoteSite voteSite : voteSites) {
			boolean canVote = canVoteSite(voteSite);
			if (!canVote) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Can vote site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return true, if successful
	 */
	@SuppressWarnings("deprecation")
	/**
	 * @param voteSite	VoteSite
	 * @return			True if player can vote on specified site
	 */
	public boolean canVoteSite(VoteSite voteSite) {
		String siteName = voteSite.getSiteName();
		long time = getTime(voteSite);
		if (time == 0) {
			return true;
		}
		Date date = new Date(time);
		int month = date.getMonth();
		int day = date.getDate();
		int hour = date.getHours();
		int min = date.getMinutes();

		int votedelay = ConfigVoteSites.getInstance().getVoteDelay(siteName);

		if (votedelay == 0) {
			return false;
		}

		Date voteTime = new Date(new Date().getYear(), month, day, hour, min);
		Date nextvote = DateUtils.addHours(voteTime, votedelay);

		int cday = new Date().getDate();
		int cmonth = new Date().getMonth();
		int chour = new Date().getHours();
		int cmin = new Date().getMinutes();
		Date currentDate = new Date(new Date().getYear(), cmonth, cday, chour,
				cmin);

		if ((nextvote != null) && (day != 0) && (hour != 0)) {
			if (currentDate.after(nextvote)) {
				return true;

			}
		}

		return false;
	}

	/**
	 * Check all votes.
	 *
	 * @return true, if successful
	 */
	public boolean checkAllVotes() {
		User user = this;

		ArrayList<VoteSite> voteSites = plugin.voteSites;
		ArrayList<Integer> months = new ArrayList<Integer>();
		ArrayList<Integer> days = new ArrayList<Integer>();

		for (int i = 0; i < voteSites.size(); i++) {
			months.add(Utils.getInstance().getMonthFromMili(
					user.getTime(voteSites.get(i))));
			days.add(Utils.getInstance().getDayFromMili(
					user.getTime(voteSites.get(i))));
		}

		// check months
		for (int i = 0; i < months.size(); i++) {
			if (!months.get(0).equals(months.get(i))) {
				return false;
			}
		}

		// check days
		for (int i = 0; i < days.size(); i++) {
			if (!days.get(0).equals(days.get(i))) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Daily top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void dailyTopVoterAward(int place) {
		if (playerName == null) {
			playerName = Utils.getInstance().getPlayerName(uuid);
		}

		if (Utils.getInstance().isPlayerOnline(playerName)) {
			// online
			giveDailyTopVoterAward(place);
		} else {
			Data.getInstance().setTopVoterAwardOfflineDaily(this, place);
		}
	}

	/**
	 * Gets the cumulative reward.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the cumulative reward
	 */
	public int getCumulativeReward(VoteSite voteSite) {
		return Data.getInstance().getCumulativeSite(this,
				voteSite.getSiteName());
	}

	/**
	 * Gets the last vote times sorted.
	 *
	 * @return the last vote times sorted
	 */
	public HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		HashMap<VoteSite, Long> times = new HashMap<VoteSite, Long>();

		for (VoteSite voteSite : plugin.voteSites) {
			times.put(voteSite, getTime(voteSite));
		}
		HashMap<VoteSite, Long> sorted = (HashMap<VoteSite, Long>) times
				.entrySet()
				.stream()
				.sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				.collect(
						Collectors
								.toMap(Map.Entry::getKey, Map.Entry::getValue));
		return sorted;
	}

	/**
	 * Gets the offline top voter.
	 *
	 * @return the offline top voter
	 */
	public int getOfflineTopVoter() {
		return Data.getInstance().getTopVoterAwardOffline(this);
	}

	/**
	 * Gets the offline votes.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the offline votes
	 */
	public int getOfflineVotes(VoteSite voteSite) {
		User user = this;
		return Data.getInstance().getOfflineVotesSite(user,
				voteSite.getSiteName());
	}

	/**
	 * Gets the player.
	 *
	 * @return the player
	 */
	public Player getPlayer() {
		return Bukkit.getPlayer(java.util.UUID.fromString(uuid));
	}

	/**
	 * Gets the player name.
	 *
	 * @return the player name
	 */
	public String getPlayerName() {
		return playerName;

	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public int getPoints() {
		return Data.getInstance().getVotingPoints(this);
	}

	/**
	 * Gets the reminded.
	 *
	 * @return the reminded
	 */
	public boolean getReminded() {
		return Data.getInstance().getReminded(this);
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the time
	 */
	public long getTime(VoteSite voteSite) {
		return Data.getInstance().getTimeSite(this, voteSite.getSiteName());
	}

	/**
	 * Gets the timed reward.
	 *
	 * @param reward
	 *            the reward
	 * @return the timed reward
	 */
	public long getTimedReward(Reward reward) {
		return Data.getInstance().getTimedReward(this, reward.getRewardName());
	}

	/**
	 * Gets the total.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total
	 */
	public int getTotal(VoteSite voteSite) {
		User user = this;
		return Data.getInstance().getTotal(user, voteSite.getSiteName());
	}

	/**
	 * Gets the total daily.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total daily
	 */
	public int getTotalDaily(VoteSite voteSite) {
		return Data.getInstance().getTotalDaily(this, voteSite.getSiteName());
	}

	/**
	 * Gets the total daily all.
	 *
	 * @return the total daily all
	 */
	public int getTotalDailyAll() {
		int total = 0;
		for (VoteSite voteSite : plugin.voteSites) {
			total += getTotalDaily(voteSite);
		}
		return total;

	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		int total = 0;
		for (VoteSite voteSite : ConfigVoteSites.getInstance().getVoteSites()) {
			total += getTotalVotesSite(voteSite);
		}
		return total;
	}

	/**
	 * Gets the total votes site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total votes site
	 */
	public int getTotalVotesSite(VoteSite voteSite) {
		return Data.getInstance().getTotal(this, voteSite.getSiteName());
	}

	/**
	 * Gets the total votes today.
	 *
	 * @return the total votes today
	 */
	@SuppressWarnings("deprecation")
	public int getTotalVotesToday() {
		int total = 0;
		for (VoteSite voteSite : plugin.voteSites) {
			Date date = new Date(getTime(voteSite));
			if (date.getDate() == new Date().getDate()) {
				total++;
			}
		}
		return total;

	}

	/**
	 * Gets the total weekly.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total weekly
	 */
	public int getTotalWeekly(VoteSite voteSite) {
		return Data.getInstance().getTotalWeek(this, voteSite.getSiteName());
	}

	/**
	 * Gets the total weekly all.
	 *
	 * @return the total weekly all
	 */
	public int getTotalWeeklyAll() {
		int total = 0;
		for (VoteSite voteSite : plugin.voteSites) {
			total += getTotalWeekly(voteSite);
		}
		return total;

	}

	/**
	 * Gets the uuid.
	 *
	 * @return the uuid
	 */
	public String getUUID() {
		return uuid;
	}

	/**
	 * Gets the vote time last.
	 *
	 * @return the vote time last
	 */
	public long getVoteTimeLast() {
		ArrayList<Long> times = new ArrayList<Long>();
		for (VoteSite voteSite : plugin.voteSites) {
			times.add(getTime(voteSite));
		}
		Long last = Collections.max(times);
		return last;
	}

	/**
	 * Give daily top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void giveDailyTopVoterAward(int place) {
		for (String reward : ConfigTopVoterAwards.getInstance()
				.getDailyAwardRewards(place)) {
			giveReward(ConfigRewards.getInstance().getReward(reward), Utils
					.getInstance().isPlayerOnline(getPlayerName()));
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(
					ConfigFormat.getInstance().getTopVoterRewardMsg()
							.replace("%place%", "" + place)));
		}
	}

	/**
	 * Give exp.
	 *
	 * @param exp
	 *            the exp
	 */
	public void giveExp(int exp) {
		Player player = getPlayer();
		if (player != null) {
			player.giveExp(exp);
		}
	}

	/**
	 * Give item.
	 *
	 * @param id
	 *            the id
	 * @param amount
	 *            the amount
	 * @param data
	 *            the data
	 * @param itemName
	 *            the item name
	 * @param lore
	 *            the lore
	 * @param enchants
	 *            the enchants
	 */
	@SuppressWarnings("deprecation")
	/**
	 * Give the user an item
	 * @param id	Item id
	 * @param amount	Item amount
	 * @param data		Item data
	 * @param itemName	Item name
	 * @param lore		Item lore
	 * @param enchants	Item enchants
	 */
	public void giveItem(int id, int amount, int data, String itemName,
			List<String> lore, HashMap<String, Integer> enchants) {

		if (amount == 0) {
			return;
		}

		String playerName = getPlayerName();

		ItemStack item = new ItemStack(id, amount, (short) data);
		item = Utils.getInstance().nameItem(item, itemName);
		item = Utils.getInstance().addLore(item, lore);
		Player player = Bukkit.getPlayer(playerName);
		// player.getInventory().addItem(item);

		item = Utils.getInstance().addEnchants(item, enchants);

		HashMap<Integer, ItemStack> excess = player.getInventory()
				.addItem(item);
		for (Map.Entry<Integer, ItemStack> me : excess.entrySet()) {
			player.getWorld().dropItem(player.getLocation(), me.getValue());
		}

		player.updateInventory();

	}

	/**
	 * Give item.
	 *
	 * @param item
	 *            the item
	 */
	public void giveItem(ItemStack item) {
		if (item.getAmount() == 0) {
			return;
		}

		String playerName = getPlayerName();

		Player player = Bukkit.getPlayer(playerName);

		HashMap<Integer, ItemStack> excess = player.getInventory()
				.addItem(item);
		for (Map.Entry<Integer, ItemStack> me : excess.entrySet()) {
			Bukkit.getScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					player.getWorld().dropItem(player.getLocation(),
							me.getValue());
				}
			});

		}

		player.updateInventory();

	}

	/**
	 * Give money.
	 *
	 * @param money
	 *            the money
	 */
	@SuppressWarnings("deprecation")
	/**
	 * Give user money, needs vault installed
	 * @param money		Amount of money to give
	 */
	public void giveMoney(int money) {
		String playerName = getPlayerName();
		if (Bukkit.getServer().getPluginManager().getPlugin("Vault") != null) {
			if (money > 0) {
				plugin.econ.depositPlayer(playerName, money);
			} else if (money < 0) {
				plugin.econ.withdrawPlayer(playerName, money);
			}
		}
	}

	/**
	 * Give monthly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void giveMonthlyTopVoterAward(int place) {
		for (String reward : ConfigTopVoterAwards.getInstance()
				.getMonthlyAwardRewards(place)) {
			giveReward(ConfigRewards.getInstance().getReward(reward), Utils
					.getInstance().isPlayerOnline(getPlayerName()));
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(
					ConfigFormat.getInstance().getTopVoterRewardMsg()
							.replace("%place%", "" + place)));
		}
	}

	/**
	 * Give potion effect.
	 *
	 * @param potionName
	 *            the potion name
	 * @param duration
	 *            the duration
	 * @param amplifier
	 *            the amplifier
	 */
	public void givePotionEffect(String potionName, int duration, int amplifier) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			Bukkit.getScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					player.addPotionEffect(
							new PotionEffect(PotionEffectType
									.getByName(potionName), 20 * duration,
									amplifier), true);
				}
			});

		}
	}

	/**
	 * Give reward.
	 *
	 * @param reward
	 *            the reward
	 * @param online
	 *            the online
	 */
	public void giveReward(Reward reward, boolean online) {
		reward.giveReward(this, online);
	}

	/**
	 * Give weekly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void giveWeeklyTopVoterAward(int place) {
		for (String reward : ConfigTopVoterAwards.getInstance()
				.getWeeklyAwardRewards(place)) {
			giveReward(ConfigRewards.getInstance().getReward(reward), Utils
					.getInstance().isPlayerOnline(getPlayerName()));
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(
					ConfigFormat.getInstance().getTopVoterRewardMsg()
							.replace("%place%", "" + place)));
		}
	}

	/**
	 * Checks for gotten first vote.
	 *
	 * @return true, if successful
	 */
	public boolean hasGottenFirstVote() {
		return Data.getInstance().getHasGottenFirstReward(this);
	}

	/**
	 * Checks for joined before.
	 *
	 * @return true, if successful
	 */
	public boolean hasJoinedBefore() {
		return Data.getInstance().hasJoinedBefore(this);
	}

	/**
	 * Load name.
	 */
	public void loadName() {
		playerName = Utils.getInstance().getPlayerName(uuid);
	}

	/**
	 * Login message.
	 */
	public void loginMessage() {
		if (ConfigVoteReminding.getInstance().getRemindOnLogin()) {
			VoteReminding.getInstance().runRemind(this);
		}
	}

	/**
	 * Monthly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void monthlyTopVoterAward(int place) {
		if (playerName == null) {
			playerName = Utils.getInstance().getPlayerName(uuid);
		}

		if (Utils.getInstance().isPlayerOnline(playerName)) {
			// online
			giveMonthlyTopVoterAward(place);
		} else {
			Data.getInstance().setTopVoterAwardOffline(this, place);
		}
	}

	/**
	 * Off vote.
	 */
	public void offVote() {
		ArrayList<VoteSite> voteSites = ConfigVoteSites.getInstance()
				.getVoteSites();

		ArrayList<String> offlineVotes = new ArrayList<String>();

		String playerName = getPlayerName();

		boolean sendEffects = false;

		for (VoteSite voteSite : voteSites) {
			int offvotes = getOfflineVotes(voteSite);
			if (offvotes > 0) {
				sendEffects = true;

				plugin.debug("Offline Vote Reward on Site '"
						+ voteSite.getSiteName() + "' given for player '"
						+ playerName + "'");

				for (int i = 0; i < offvotes; i++) {
					offlineVotes.add(voteSite.getSiteName());
				}
			}
		}

		if (sendEffects) {
			sendVoteEffects(true);
		}

		for (int i = 0; i < offlineVotes.size(); i++) {
			playerVote(plugin.getVoteSite(offlineVotes.get(i)), false);
		}
		for (int i = 0; i < offlineVotes.size(); i++) {
			setOfflineVotes(plugin.getVoteSite(offlineVotes.get(i)), 0);
		}

		for (int i = 0; i < Data.getInstance().getFirstVoteOffline(this); i++) {
			OtherVoteReward.getInstance().giveFirstVoteRewards(this, false);
		}

		for (int i = 0; i < Data.getInstance().getAllSitesOffline(this); i++) {
			OtherVoteReward.getInstance().giveAllSitesRewards(this, false);
		}

		for (Reward reward : plugin.rewards) {
			int offVotes = Data.getInstance().getOfflineReward(this, reward);
			for (int i = 0; i < offVotes; i++) {
				OtherVoteReward.getInstance().giveAllSitesRewards(this, false);
			}
			Data.getInstance().setOfflineReward(this, reward, 0);
		}

		Set<String> list = ConfigOtherRewards.getInstance()
				.getCumulativeVotes();
		for (String str : list) {
			if (Utils.getInstance().isInt(str)) {
				int votesRequired = Integer.parseInt(str);
				if (votesRequired != 0) {
					if (ConfigOtherRewards.getInstance()
							.getCumulativeRewardEnabled(votesRequired)) {
						int offlineVote = Data.getInstance()
								.getCumulativeVotesOffline(this, votesRequired);
						for (int i = 0; i < offlineVote; i++) {
							OtherVoteReward.getInstance()
									.giveCumulativeVoteReward(this, false,
											votesRequired);

						}
						if (offlineVote != 0) {
							Data.getInstance().setCumuatliveVotesOffline(this,
									votesRequired, 0);
						}
					}
				}
			}
		}

		Data.getInstance().setFirstVoteOffline(this, 0);
		Data.getInstance().setAllSitesOffline(this, 0);

		int place = getOfflineTopVoter();
		if (place > 0) {
			giveMonthlyTopVoterAward(place);
			Data.getInstance().setTopVoterAwardOffline(this, 0);
		}

		for (int i = 0; i <= 6; i++) {
			int place1 = Data.getInstance().getTopVoterAwardOfflineWeekly(this,
					i);
			if (place1 > 0) {
				giveMonthlyTopVoterAward(place1);
				Data.getInstance().setTopVoterAwardOffline(this, 0);
			}
		}

		for (int i = 0; i <= 31; i++) {
			int place2 = Data.getInstance().getTopVoterAwardOfflineDaily(this,
					i);
			if (place2 > 0) {
				giveMonthlyTopVoterAward(place2);
				Data.getInstance().setTopVoterAwardOffline(this, 0);
			}
		}

		if (VoteParty.getInstance().getOfflineVotePartyVotes(this) > 0) {
			for (int i = VoteParty.getInstance().getOfflineVotePartyVotes(this); i > 0; i++) {
				VoteParty.getInstance().giveReward(this);
			}
			VoteParty.getInstance().setOfflineVotePartyVotes(this, 0);
		}

	}

	/**
	 * Off vote world.
	 *
	 * @param world
	 *            the world
	 */
	public void offVoteWorld(String world) {
		for (VoteSite voteSite : plugin.voteSites) {
			for (Reward reward : plugin.rewards) {
				ArrayList<String> worlds = reward.getWorlds();
				if ((world != null) && (worlds != null)) {
					if (reward.isGiveInEachWorld()) {
						for (String worldName : worlds) {

							plugin.debug("Checking world: " + worldName
									+ ", reard: " + reward + ", votesite: "
									+ voteSite.getSiteName());

							if (worldName != "") {
								if (worldName.equals(world)) {

									plugin.debug("Giving reward...");

									int worldRewards = Data.getInstance()
											.getOfflineVotesSiteWorld(this,
													reward.name, worldName);

									while (worldRewards > 0) {
										reward.giveRewardUser(this);
										worldRewards--;
									}

									Data.getInstance()
											.setOfflineVotesSiteWorld(this,
													reward.name, worldName, 0);
								}
							}

						}
					} else {
						if (worlds.contains(world)) {
							int worldRewards = Data.getInstance()
									.getOfflineVotesSiteWorld(this,
											reward.name, world);

							while (worldRewards > 0) {
								reward.giveRewardUser(this);
								worldRewards--;
							}

							Data.getInstance().setOfflineVotesSiteWorld(this,
									reward.name, world, 0);
						}
					}
				}
			}
		}
	}

	/**
	 * Player vote.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param online
	 *            the online
	 */
	public synchronized void playerVote(VoteSite voteSite, boolean online) {
		if (Config.getInstance().getBroadCastVotesEnabled()
				&& ConfigFormat.getInstance().getBroadcastWhenOnline()) {
			voteSite.broadcastVote(this);
		}
		voteSite.giveSiteReward(this, online);
	}

	/**
	 * Play particle effect.
	 *
	 * @param effectName
	 *            the effect name
	 * @param data
	 *            the data
	 * @param particles
	 *            the particles
	 * @param radius
	 *            the radius
	 */
	@SuppressWarnings("deprecation")
	/**
	 * Send a particle effect to the user
	 * @param effectName
	 * @param data
	 * @param particles
	 * @param radius
	 */
	public synchronized void playParticleEffect(String effectName, int data,
			int particles, int radius) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if ((player != null) && (effectName != null)) {
			Effect effect = Effect.valueOf(effectName);
			player.spigot().playEffect(player.getLocation(), effect,
					effect.getId(), data, 0f, 0f, 0f, 1f, particles, radius);
			// player.getWorld().spigot().playEffect(player.getLocation(),
			// effect);
		}
	}

	/**
	 * Play sound.
	 *
	 * @param soundName
	 *            the sound name
	 * @param volume
	 *            the volume
	 * @param pitch
	 *            the pitch
	 */
	public synchronized void playSound(String soundName, float volume,
			float pitch) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			Sound sound = Sound.valueOf(soundName);
			if (sound != null) {
				player.playSound(player.getLocation(), sound, volume, pitch);
			} else {
				plugin.debug("Invalid sound: " + soundName);
			}
		}
	}

	/**
	 * Reminded.
	 *
	 * @return true, if successful
	 */
	public boolean reminded() {
		User user = this;
		return Data.getInstance().getReminded(user);
	}

	/**
	 * Removes the points.
	 *
	 * @param points
	 *            the points
	 * @return true, if successful
	 */
	public boolean removePoints(int points) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points);
			return true;
		}
		return false;
	}

	/**
	 * Send action bar.
	 *
	 * @param msg
	 *            the msg
	 * @param delay
	 *            the delay
	 */
	public void sendActionBar(String msg, int delay) {
		// plugin.debug("attempting to send action bar");
		if (msg != null && msg != "") {
			Player player = getPlayer();
			if (player != null) {

				try {
					ActionBar actionBar = new ActionBar(msg, delay);
					actionBar.send(player);
				} catch (Exception ex) {
					plugin.getLogger()
							.info("Failed to send ActionBar, turn debug on to see stack trace");
					if (Config.getInstance().getDebugEnabled()) {
						ex.printStackTrace();
					}
				}
			}
		}
	}

	/**
	 * Send boss bar.
	 *
	 * @param msg
	 *            the msg
	 * @param color
	 *            the color
	 * @param style
	 *            the style
	 * @param progress
	 *            the progress
	 * @param delay
	 *            the delay
	 */
	public void sendBossBar(String msg, String color, String style,
			double progress, int delay) {
		// plugin.debug("attempting to send action bar");
		if (msg != null && msg != "") {
			Player player = getPlayer();
			if (player != null) {
				try {
					BossBar bossBar = new BossBar(msg, color, style, progress);
					bossBar.send(player, delay);
				} catch (Exception ex) {
					plugin.getLogger()
							.info("Failed to send BossBar, turn debug on to see stack trace");
					if (Config.getInstance().getDebugEnabled()) {
						ex.printStackTrace();
					}
				}
			}
		}
	}

	/**
	 * Send json.
	 *
	 * @param messages
	 *            the messages
	 */
	public void sendJson(ArrayList<TextComponent> messages) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if ((player != null) && (messages != null)) {
			/*
			 * TextComponent msg = new TextComponent(); TextComponent newLine =
			 * new TextComponent( ComponentSerializer.parse("{text: \"\n\"}"));
			 * for (int i = 0; i < messages.size(); i++) {
			 * msg.addExtra(messages.get(i)); if (i != (messages.size() - 1)) {
			 * msg.addExtra(newLine); } } player.spigot().sendMessage(msg);
			 */
			for (TextComponent txt : messages) {
				player.spigot().sendMessage(txt);
			}
		}
	}

	/**
	 * Send json.
	 *
	 * @param message
	 *            the message
	 */
	public void sendJson(TextComponent message) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if ((player != null) && (message != null)) {
			player.spigot().sendMessage(message);
		}
	}

	/**
	 * Send message.
	 *
	 * @param msg
	 *            the msg
	 */
	public void sendMessage(String msg) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if ((player != null) && (msg != null)) {
			if (msg != "") {
				player.sendMessage(Utils.getInstance().colorize(
						Utils.getInstance().replacePlaceHolders(player, msg)));
			}
		}
	}

	/**
	 * Send message.
	 *
	 * @param msg
	 *            the msg
	 */
	public void sendMessage(String[] msg) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if ((player != null) && (msg != null)) {

			for (int i = 0; i < msg.length; i++) {
				msg[i] = Utils.getInstance()
						.replacePlaceHolders(player, msg[i]);
			}
			player.sendMessage(Utils.getInstance().colorize(msg));

		}
	}

	/**
	 * Send title.
	 *
	 * @param title
	 *            the title
	 * @param subTitle
	 *            the sub title
	 * @param fadeIn
	 *            the fade in
	 * @param showTime
	 *            the show time
	 * @param fadeOut
	 *            the fade out
	 */
	public void sendTitle(String title, String subTitle, int fadeIn,
			int showTime, int fadeOut) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			// Title.getInstance().sendTitle(player, title, subTitle, fadeIn,
			// showTime, fadeOut);
			try {
				Title titleObject = new Title(title, subTitle, fadeIn,
						showTime, fadeOut);
				titleObject.send(player);
			} catch (Exception ex) {
				plugin.getLogger()
						.info("Failed to send Title, turn debug on to see stack trace");
				if (Config.getInstance().getDebugEnabled()) {
					ex.printStackTrace();
				}
			}
		}
	}

	/**
	 * Send vote effects.
	 *
	 * @param online
	 *            the online
	 */
	public void sendVoteEffects(boolean online) {
		for (String reward : Config.getInstance().getRewards()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward)
						.giveReward(this, online);
			}
		}
	}

	/**
	 * Sets the cumulative reward.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setCumulativeReward(VoteSite voteSite, int value) {
		Data.getInstance().setCumulativeSite(this, voteSite.getSiteName(),
				value);
	}

	/**
	 * Sets the checks for gotten first vote.
	 *
	 * @param value
	 *            the new checks for gotten first vote
	 */
	public void setHasGottenFirstVote(boolean value) {
		Data.getInstance().setHasGottenFirstReward(this, value);
	}

	/**
	 * Sets the offline top voter.
	 *
	 * @param place
	 *            the new offline top voter
	 */
	public void setOfflineTopVoter(int place) {
		Data.getInstance().setTopVoterAwardOffline(this, place);
	}

	/**
	 * Sets the offline votes.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setOfflineVotes(VoteSite voteSite, int amount) {
		User user = this;
		Data.getInstance().setOfflineVotesSite(user, voteSite.getSiteName(),
				amount);
	}

	/**
	 * Sets the player name.
	 */
	public void setPlayerName() {
		User user = this;
		Data.getInstance().setPlayerName(user);
	}

	/**
	 * Sets the player name.
	 *
	 * @param playerName
	 *            the new player name
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	/**
	 * Sets the points.
	 *
	 * @param value
	 *            the new points
	 */
	public void setPoints(int value) {
		Data.getInstance().setVotingPoints(this, value);
	}

	/**
	 * Sets the reminded.
	 *
	 * @param reminded
	 *            the new reminded
	 */
	public void setReminded(boolean reminded) {
		User user = this;
		Data.getInstance().setReminded(user, reminded);
	}

	/**
	 * Sets the time.
	 *
	 * @param voteSite
	 *            the new time
	 */
	public void setTime(VoteSite voteSite) {
		User user = this;
		Data.getInstance().setTime(voteSite.getSiteName(), user);
	}

	/**
	 * Sets the timed reward.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setTimedReward(Reward reward, long value) {
		Data.getInstance().setTimedReward(this, reward.getRewardName(), value);
	}

	/**
	 * Sets the total.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotal(VoteSite voteSite, int amount) {
		User user = this;
		Data.getInstance().setTotal(user, voteSite.getSiteName(), amount);
	}

	/**
	 * Sets the total daily.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotalDaily(VoteSite voteSite, int amount) {
		Data.getInstance().setTotalDaily(this, voteSite.getSiteName(), amount);
	}

	/**
	 * Sets the total weekly.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotalWeekly(VoteSite voteSite, int amount) {
		Data.getInstance().setTotalWeek(this, voteSite.getSiteName(), amount);
	}

	/**
	 * Sets the uuid.
	 *
	 * @param uuid
	 *            the new uuid
	 */
	public void setUUID(String uuid) {
		this.uuid = uuid;
	}

	/**
	 * Weekly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void weeklyTopVoterAward(int place) {
		if (playerName == null) {
			playerName = Utils.getInstance().getPlayerName(uuid);
		}

		if (Utils.getInstance().isPlayerOnline(playerName)) {
			// online
			giveWeeklyTopVoterAward(place);
		} else {
			Data.getInstance().setTopVoterAwardOfflineWeekly(this, place);
		}
	}

}
