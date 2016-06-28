package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;

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
		playerName = player.getName();
		uuid = player.getUniqueId().toString();
	}

	/**
	 * New user
	 *
	 * @param playerName
	 *            The user's name
	 */
	public User(String playerName) {
		this.playerName = playerName;
		uuid = Utils.getInstance().getUUID(playerName);

	}

	/**
	 * New user
	 *
	 * @param uuid
	 *            UUID
	 */
	public User(UUID uuid) {
		this.uuid = uuid.getUUID();
		playerName = Utils.getInstance().getPlayerName(this.uuid);

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
			playerName = Utils.getInstance().getPlayerName(this.uuid);
		}
	}

	public void addCumulativeReward(VoteSite voteSite) {
		Data.getInstance().addCumulativeSite(this, voteSite.getSiteName());
	}

	/**
	 *
	 * @param voteSite
	 *            VoteSite to add offline votes to
	 */
	public void addOfflineVote(VoteSite voteSite) {
		setOfflineVotes(voteSite, getOfflineVotes(voteSite) + 1);
	}

	/**
	 * Add total for VoteSite to user
	 *
	 * @param voteSite
	 *            VoteSite to add vote to
	 */
	public void addTotal(VoteSite voteSite) {
		User user = this;
		Data.getInstance().addTotal(user, voteSite.getSiteName());
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
	 * Check if user has voted on all sites in one day
	 *
	 * @return True if player has voted on all sites in one day
	 */
	public boolean checkAllVotes() {
		User user = this;

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

	public int getCumulativeReward(VoteSite voteSite) {
		return Data.getInstance().getCumulativeSite(this,
				voteSite.getSiteName());
	}

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

	public int getOfflineTopVoter() {
		return Data.getInstance().getTopVoterAwardOffline(this);
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
		return Data.getInstance().getOfflineVotesSite(user,
				voteSite.getSiteName());
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
		return Data.getInstance().getTimeSite(this, voteSite.getSiteName());
	}

	/**
	 * Get total from VoteSite for user
	 *
	 * @param voteSite
	 * @return
	 */
	public int getTotal(VoteSite voteSite) {
		User user = this;
		return Data.getInstance().getTotal(user, voteSite.getSiteName());
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

	public long getVoteTimeLast() {
		ArrayList<Long> times = new ArrayList<Long>();
		for (VoteSite voteSite : plugin.voteSites) {
			times.add(getTime(voteSite));
		}
		Long last = Collections.max(times);
		return last;
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
	 * Give the user an item, will drop on ground if inv full
	 *
	 * @param item
	 *            ItemStack to give player
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
		String playerName = getPlayerName();
		if ((Bukkit.getServer().getPluginManager().getPlugin("Vault") != null)
				&& (money > 0)) {
			Main.econ.depositPlayer(playerName, money);
		}
	}

	public void giveReward(Reward reward) {
		reward.giveReward(this);
	}

	public void giveTopVoterAward(int place) {
		for (String reward : ConfigTopVoterAwards.getInstance()
				.getAwardRewards(place)) {
			giveReward(ConfigRewards.getInstance().getReward(reward));
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.sendMessage(Utils.getInstance().colorize(
					ConfigFormat.getInstance().getTopVoterRewardMsg()
							.replace("%place%", "" + place)));
		}
	}

	/**
	 *
	 * @return True if the player has joined before
	 */
	public boolean hasJoinedBefore() {
		return Data.getInstance().hasJoinedBefore(this);
	}

	/**
	 * Load the user's name from uuid
	 */
	public void loadName() {
		playerName = Utils.getInstance().getPlayerName(uuid);
	}

	/**
	 * Login message if player can vote
	 */
	public void loginMessage() {
		String playerName = getPlayerName();
		if (Config.getInstance().getRemindVotesEnabled()) {
			if (Utils.getInstance().hasPermission(playerName,
					"VotingPlugin.Login.RemindVotes")
					|| Utils.getInstance().hasPermission(playerName,
							"VotingPlugin.Player")) {
				if (canVoteAll()) {
					Player player = Bukkit.getPlayer(playerName);
					if (player != null) {
						String msg = Utils.getInstance().colorize(
								ConfigFormat.getInstance().getLoginMsg());
						msg = msg.replace("%player%", playerName);
						player.sendMessage(msg);
						setReminded(true);
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

		String playerName = getPlayerName();

		boolean playSound = false;

		for (VoteSite voteSite : voteSites) {
			int offvotes = getOfflineVotes(voteSite);
			if (offvotes > 0) {
				playSound = true;
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
			playerVote(plugin.getVoteSite(offlineVotes.get(i)));
		}
		for (int i = 0; i < offlineVotes.size(); i++) {
			setOfflineVotes(plugin.getVoteSite(offlineVotes.get(i)), 0);
		}

		for (int i = 0; i < Data.getInstance().getFirstVoteOffline(this); i++) {
			OtherVoteReward.getInstance().giveFirstVoteRewards(this);
		}

		for (int i = 0; i < Data.getInstance().getAllSitesOffline(this); i++) {
			OtherVoteReward.getInstance().giveAllSitesRewards(this);
		}

		for (int i = 0; i < Data.getInstance().getNumberOfVotesOffline(this); i++) {
			OtherVoteReward.getInstance().giveNumberOfVotesRewards(this);
		}

		Data.getInstance().setFirstVoteOffline(this, 0);
		Data.getInstance().setAllSitesOffline(this, 0);
		Data.getInstance().setNumberOfVotesOffline(this, 0);

		if (playSound) {
			playVoteSound();
			playVoteEffect();
			playVoteTitle();
		}

		int place = getOfflineTopVoter();
		if (place > 0) {
			giveTopVoterAward(place);
			Data.getInstance().setTopVoterAwardOffline(this, 0);
		}
	}

	public void offVoteWorld(String world) {
		for (VoteSite voteSite : plugin.voteSites) {
			for (Reward reward : plugin.rewards) {
				ArrayList<String> worlds = reward.getWorlds();
				if (world != null && worlds != null) {
					if (reward.isGiveInEachWorld()) {
						for (String worldName : worlds) {
							if (Config.getInstance().getDebugEnabled()) {
								plugin.getLogger().info(
										"Checking world: " + worldName
												+ ", reard: " + reward
												+ ", votesite: "
												+ voteSite.getSiteName());
							}
							if (worldName != "") {
								if (worldName.equals(world)) {
									if (Config.getInstance().getDebugEnabled()) {
										plugin.getLogger().info(
												"Giving reward...");
									}
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
	 * Trigger a vote for the user
	 *
	 * @param voteSite
	 *            Site player voted on
	 */
	public void playerVote(VoteSite voteSite) {
		if (Config.getInstance().getBroadCastVotesEnabled()
				&& ConfigFormat.getInstance().getBroadcastWhenOnline()) {
			voteSite.broadcastVote(this);
		}
		voteSite.giveSiteReward(this);
	}

	public void playSound(String soundName, float volume, float pitch) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			Sound sound = Sound.valueOf(soundName);
			if (sound != null) {
				player.playSound(player.getLocation(), sound, volume, pitch);
			} else if (Config.getInstance().getDebugEnabled()) {
				plugin.getLogger().info("Invalid sound: " + soundName);
			}
		}
	}

	public void playVoteSound() {
		if (Config.getInstance().getVoteSoundEnabled()) {
			try {
				playSound(Config.getInstance().getVoteSoundSound(), Config
						.getInstance().getVoteSoundVolume(), Config
						.getInstance().getVoteSoundPitch());
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * Get whether or not user has been reminded to vote
	 *
	 * @return T
	 */
	public boolean reminded() {
		User user = this;
		return Data.getInstance().getReminded(user);
	}

	/**
	 * Send the user a message
	 *
	 * @param msg
	 *            Message to send
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
	 * Send the user a message
	 *
	 * @param msg
	 *            Message to send
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

	public void playVoteTitle() {
		if (Config.getInstance().getVoteTitleEnabled()) {
			sendTitle(Config.getInstance().getVoteTitleTitle(), Config
					.getInstance().getVoteTitleTitleColor(), Config
					.getInstance().getVoteTitleSubTitle(), Config.getInstance()
					.getVoteTitleSubTitleColor(), Config.getInstance()
					.getVoteTitleFadeIn(), Config.getInstance()
					.getVoteTitleShowTime(), Config.getInstance()
					.getVoteTitleFadeOut());
		}
	}

	public void sendTitle(String title, String titleColor, String subTitle,
			String subTitleColor, int fadeIn, int showTime, int fadeOut) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			Utils.getInstance().sendTitle(player, title, subTitle, titleColor,
					subTitleColor, fadeIn, showTime, fadeOut);
		}
	}

	public void setCumulativeReward(VoteSite voteSite, int value) {
		Data.getInstance().setCumulativeSite(this, voteSite.getSiteName(),
				value);
	}

	/**
	 * Set name in player's file
	 */
	public void setName() {
		User user = this;
		Data.getInstance().setName(user);
	}

	public void setOfflineTopVoter(int place) {
		Data.getInstance().setTopVoterAwardOffline(this, place);
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
		Data.getInstance().setOfflineVotesSite(user, voteSite.getSiteName(),
				amount);
	}

	public void playVoteEffect() {
		if (Config.getInstance().getVoteEffectEnabled()) {
			playParticleEffect(Config.getInstance().getVoteEffectEffect(),
					Config.getInstance().getVoteEffectData(), Config
							.getInstance().getVoteEffectParticles(), Config
							.getInstance().getVoteEffectRadius());
		}
	}

	@SuppressWarnings("deprecation")
	public void playParticleEffect(String effectName, int data, int particles,
			int radius) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null && effectName != null) {
			Effect effect = Effect.valueOf(effectName);
			player.spigot().playEffect(player.getLocation(), effect,
					effect.getId(), data, 0f, 0f, 0f, 1f, particles, radius);
			// player.getWorld().spigot().playEffect(player.getLocation(),
			// effect);
		}
	}

	public void givePotionEffect(String potionName, int duration, int amplifier) {
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(uuid));
		if (player != null) {
			player.addPotionEffect(
					new PotionEffect(PotionEffectType.getByName(potionName),
							20 * duration, amplifier), true);
		}
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
		Data.getInstance().setReminded(user, reminded);
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
	 * Set total for VoteSite for user
	 *
	 * @param voteSite
	 *            VoteSite to set total
	 * @param amount
	 *            Total to set
	 */
	public void setTotal(VoteSite voteSite, int amount) {
		User user = this;
		Data.getInstance().setTotal(user, voteSite.getSiteName(), amount);
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

}
