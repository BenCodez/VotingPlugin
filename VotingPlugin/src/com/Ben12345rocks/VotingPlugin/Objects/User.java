package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.BonusReward.BonusVoteReward;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterAwards;

public class User {
	static Main plugin = Main.plugin;
	private String playerName;

	private String uuid;

	public User(Main plugin) {
		User.plugin = plugin;
	}

	/**
	 * New user
	 *
	 * @param player
	 *            Player
	 */
	public User(Player player) {
		this.playerName = player.getName();
		this.uuid = player.getUniqueId().toString();
	}

	/**
	 * New user
	 *
	 * @param playerName
	 *            The user's name
	 */
	public User(String playerName) {
		this.playerName = playerName;
		this.uuid = Utils.getInstance().getUUID(playerName);

	}

	/**
	 * New user
	 *
	 * @param uuid
	 *            UUID
	 */
	public User(UUID uuid) {
		this.uuid = uuid.getUUID();
		this.playerName = Utils.getInstance().getPlayerName(this.uuid);

	}

	/**
	 * New user
	 *
	 * @param uuid
	 *            UUID
	 * @param loadName
	 *            Whether or not to preload name
	 */
	public User(UUID uuid, boolean loadName) {
		this.uuid = uuid.getUUID();
		if (loadName) {
			this.playerName = Utils.getInstance().getPlayerName(this.uuid);
		}
	}

	/**
	 * Add offline vote to bonus
	 */
	public void addBonusOfflineVote() {
		this.setBonusOfflineVotes(this.getBonusOfflineVotes() + 1);
	}

	/**
	 *
	 * @param voteSite
	 *            VoteSite to add offline votes to
	 */
	public void addOfflineVote(VoteSite voteSite) {
		this.setOfflineVotes(voteSite, this.getOfflineVotes(voteSite) + 1);
	}

	/**
	 * Add total for VoteSite to user
	 *
	 * @param voteSite
	 *            VoteSite to add vote to
	 */
	public void addTotal(VoteSite voteSite) {
		User user = this;
		Data.getInstance().set(user,
				user.getUUID() + ".Total." + voteSite.getSiteName(),
				Data.getInstance().getTotal(user, voteSite.getSiteName()) + 1);
	}

	/**
	 *
	 * @return True if player can vote on all sites
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

	@SuppressWarnings("deprecation")
	/**
	 *
	 * @param voteSite	VoteSite
	 * @return			True if player can vote on specified site
	 */
	public boolean canVoteSite(VoteSite voteSite) {
		String siteName = voteSite.getSiteName();
		Date date = new Date(this.getTime(voteSite));
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
	 * Check if user has voted on all sites in one day
	 *
	 * @return True if player has voted on all sites in one day, False if bonus
	 *         reward disabled or player has not voted all sites in one day
	 */
	public boolean checkAllVotes() {
		User user = this;
		if (!ConfigBonusReward.getInstance().getGiveBonusReward()) {
			return false;
		}

		ArrayList<VoteSite> voteSites = ConfigVoteSites.getInstance()
				.getVoteSites();
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
			if (!months.get(0).equals(months.get(i))
					|| days.get(i).equals(
							Utils.getInstance().getMonthFromMili(
									user.getTimeAll()))) {
				return false;
			}
		}

		// check days
		for (int i = 0; i < days.size(); i++) {
			if (!days.get(0).equals(days.get(i))
					|| days.get(i).equals(
							Utils.getInstance().getDayFromMili(
									user.getTimeAll()))) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Get Amount of offline votes
	 *
	 * @return Amount of bonus offline votes
	 */
	public int getBonusOfflineVotes() {
		User user = this;
		return Data.getInstance().getData(user)
				.getInt(user.getUUID() + ".BonusOfflineVotes");
	}

	/**
	 * Get amount of offline votes for VoteSite
	 *
	 * @param voteSite
	 *            VoteSite to get offline votes of
	 * @return Amount of offline votes
	 */
	public int getOfflineVotes(VoteSite voteSite) {
		User user = this;
		return Data
				.getInstance()
				.getData(user)
				.getInt(user.getUUID() + ".OfflineVotes."
						+ voteSite.getSiteName());
	}

	/**
	 *
	 * @return User's game name
	 */
	public String getPlayerName() {
		return playerName;

	}

	/**
	 * Get time of last vote from VoteSite for user
	 *
	 * @param voteSite
	 *            VoteSite to check for last vote
	 * @return Time in milliseconds when last vote occurred
	 */
	public long getTime(VoteSite voteSite) {
		User user = this;
		long mills = Data
				.getInstance()
				.getData(user)
				.getLong(
						uuid + ".LastVote." + voteSite.getSiteName()
								+ ".Miliseconds");
		return mills;
	}

	/**
	 * Get time of last bonus vote for user
	 *
	 * @return Time in milliseconds when last bonus reward occurred
	 */
	public long getTimeAll() {
		User user = this;
		long mills = Data.getInstance().getData(user)
				.getLong(uuid + ".LastBonus.Miliseconds");

		return mills;
	}

	/**
	 * Get total from VoteSite for user
	 *
	 * @param voteSite
	 * @return
	 */
	public int getTotal(VoteSite voteSite) {
		User user = this;
		return Data.getInstance().getData(user)
				.getInt(user.getUUID() + ".Total." + voteSite.getSiteName());
	}

	public void topVoterAward(int place) {
		if (playerName == null) {
			playerName = Utils.getInstance().getPlayerName(uuid);
		}
		if (Utils.getInstance().isPlayerOnline(playerName)) {
			// online
			giveTopVoterAward(place);
		} else {
			Data.getInstance().setTopVoterAwardOffline(this, place);
		}

	}

	public void giveTopVoterAward(int place) {
		this.giveMoney(TopVoterAwards.getInstance()
				.getTopVoterAwardMoney(place));
		try {
			for (String item : TopVoterAwards.getInstance().getItems(place)) {
				this.giveItem(TopVoterAwards.getInstance()
						.getTopVoterAwardItemStack(place, item));
			}
		} catch (Exception ex) {
			if (Config.getInstance().getDebugEnabled()) {
				ex.printStackTrace();
			}
		}
		TopVoterAwards.getInstance().doTopVoterAwardCommands(this, place);
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(
					ConfigFormat.getInstance().getTopVoterRewardMsg()));
		}
	}

	public void setOfflineTopVoter(int place) {
		Data.getInstance().setTopVoterAwardOffline(this, place);
	}

	public int getOfflineTopVoter() {
		return Data.getInstance().getTopVoterAwardOffline(this);
	}

	/**
	 *
	 * @return Returns totals of all votes sites
	 */
	public int getTotalVotes() {
		int total = 0;
		for (VoteSite voteSite : ConfigVoteSites.getInstance().getVoteSites()) {
			total += getTotalVotesSite(voteSite);
		}
		return total;
	}

	/**
	 * Get total votes for VoteSite
	 *
	 * @param voteSite
	 *            VoteSite
	 * @return Total votes from VoteSite
	 */
	public int getTotalVotesSite(VoteSite voteSite) {
		return Data.getInstance().getTotal(this, voteSite.getSiteName());
	}

	/**
	 * Get user's uuid
	 *
	 * @return uuid - as string
	 */
	public String getUUID() {
		return uuid;
	}

	/**
	 * Trigger Bonus Reward
	 */
	public void giveBonus() {
		BonusVoteReward.getInstance().giveBonusReward(this);
	}

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

		String playerName = this.getPlayerName();

		ItemStack item = new ItemStack(id, amount, (short) data);
		item = Utils.getInstance().nameItem(item, itemName);
		item = Utils.getInstance().addlore(item, lore);
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
	 * Give the user an item, will drop on ground if inv full
	 *
	 * @param item
	 *            ItemStack to give player
	 */
	public void giveItem(ItemStack item) {
		String playerName = this.getPlayerName();

		Player player = Bukkit.getPlayer(playerName);

		HashMap<Integer, ItemStack> excess = player.getInventory()
				.addItem(item);
		for (Map.Entry<Integer, ItemStack> me : excess.entrySet()) {
			player.getWorld().dropItem(player.getLocation(), me.getValue());
		}

		player.updateInventory();

	}

	@SuppressWarnings("deprecation")
	/**
	 * Give user money, needs vault installed
	 * @param money		Amount of money to give
	 */
	public void giveMoney(int money) {
		String playerName = this.getPlayerName();
		if ((Bukkit.getServer().getPluginManager().getPlugin("Vault") != null)
				&& (money > 0)) {
			Main.econ.depositPlayer(playerName, money);
		}
	}

	/**
	 *
	 * @return True if the plaeyer has joined before
	 */
	public boolean hasJoinedBefore() {
		return Data.getInstance().hasJoinedBefore(this);
	}

	/**
	 * Load the user's name from uuid
	 */
	public void loadName() {
		this.playerName = Utils.getInstance().getPlayerName(this.uuid);
	}

	/**
	 * Login message if player can vote
	 */
	public void loginMessage() {
		String playerName = this.getPlayerName();
		if (Config.getInstance().getRemindVotesEnabled()) {
			if (Utils.getInstance().hasPermission(playerName,
					"VotingPlugin.Login.RemindVotes")
					|| Utils.getInstance().hasPermission(playerName,
							"VotingPlugin.Player")) {
				if (this.canVoteAll()) {
					Player player = Bukkit.getPlayer(playerName);
					if (player != null) {
						String msg = Utils.getInstance().colorize(
								ConfigFormat.getInstance().getLoginMsg());
						msg = msg.replace("%player%", playerName);
						player.sendMessage(msg);
						this.setReminded(true);
					}

				}
			}
		}
	}

	/**
	 * Check for offline votes
	 */
	public void offVote() {
		ArrayList<VoteSite> voteSites = ConfigVoteSites.getInstance()
				.getVoteSites();

		ArrayList<String> offlineVotes = new ArrayList<String>();

		String playerName = this.getPlayerName();

		for (VoteSite voteSite : voteSites) {
			int offvotes = this.getOfflineVotes(voteSite);
			if (offvotes > 0) {
				if (Config.getInstance().getDebugEnabled()) {
					plugin.getLogger()
							.info("Offline Vote Reward on Site '"
									+ voteSite.getSiteName()
									+ "' given for player '" + playerName + "'");
				}
				for (int i = 0; i < offvotes; i++) {
					offlineVotes.add(voteSite.getSiteName());
				}
			}

		}

		for (int i = 0; i < offlineVotes.size(); i++) {
			this.playerVote(plugin.getVoteSite(offlineVotes.get(i)));
		}
		for (int i = 0; i < offlineVotes.size(); i++) {
			this.setOfflineVotes(plugin.getVoteSite(offlineVotes.get(i)), 0);
		}

		for (int i = 0; i < this.getBonusOfflineVotes(); i++) {
			BonusVoteReward.getInstance().giveBonusReward(this);
		}

		this.setBonusOfflineVotes(0);

		int place = this.getOfflineTopVoter();
		if (place > 0) {
			giveTopVoterAward(place);
			Data.getInstance().setTopVoterAwardOffline(this, 0);
		}

	}

	/**
	 * Trigger a vote for the user
	 *
	 * @param voteSite
	 *            Site player voted on
	 */
	public void playerVote(VoteSite voteSite) {
		voteSite.giveSiteReward(this);
	}

	/**
	 * Get whether or not user has been reminded to vote
	 *
	 * @return T
	 */
	public boolean reminded() {
		User user = this;
		return Data.getInstance().getData(user)
				.getBoolean(user.getUUID() + ".Reminded");
	}

	/**
	 * Send the user a message
	 *
	 * @param msg
	 *            Message to send
	 */
	public void sendMessage(String msg) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(msg));
		}
	}

	/**
	 * Set Bonus offline votes
	 *
	 * @param amount
	 *            Amount of set
	 */
	public void setBonusOfflineVotes(int amount) {
		User user = this;
		Data.getInstance().set(user, user.getUUID() + ".BonusOfflineVotes",
				amount);
	}

	/**
	 * Set name in player's file
	 */
	public void setName() {
		User user = this;
		Data.getInstance().set(user, user.getUUID() + ".Name",
				user.getPlayerName());
	}

	/**
	 * Set offline votes for VoteSite for user
	 *
	 * @param voteSite
	 *            VoteSite to set
	 * @param amount
	 *            Offline Votes to set
	 */
	public void setOfflineVotes(VoteSite voteSite, int amount) {
		User user = this;
		Data.getInstance().set(user,
				user.getUUID() + ".OfflineVotes." + voteSite.getSiteName(),
				amount);
	}

	/**
	 * Sets the user's ingame name
	 *
	 * @param playerName
	 *            Player name
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	/**
	 * Set whether or not the user has been reminded to vote
	 *
	 * @param reminded
	 *            boolean
	 */
	public void setReminded(boolean reminded) {
		User user = this;
		Data.getInstance().set(user, user.getUUID() + ".Reminded", reminded);
	}

	/**
	 * Set time of last vote for VoteSite
	 *
	 * @param siteName
	 */
	public void setTime(VoteSite voteSite) {
		User user = this;
		Data.getInstance().setTime(voteSite.getSiteName(), user);
	}

	/**
	 * Set time of bonus reward
	 */
	public void setTimeBonus() {
		User user = this;
		Data.getInstance().setTimeAll(user);
	}

	/**
	 * Set total for VoteSite for user
	 *
	 * @param voteSite
	 *            VoteSite to set total
	 * @param amount
	 *            Total to set
	 */
	public void setTotal(VoteSite voteSite, int amount) {
		User user = this;
		Data.getInstance().set(user,
				user.getUUID() + ".Total." + voteSite.getSiteName(), amount);
	}

	/**
	 * Set user's uuid
	 *
	 * @param uuid
	 *            uuid to set to
	 */
	public void setUUID(String uuid) {
		this.uuid = uuid;
	}

}
