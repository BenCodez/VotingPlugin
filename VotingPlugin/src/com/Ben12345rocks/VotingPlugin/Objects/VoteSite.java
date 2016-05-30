package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;

public class VoteSite {
	static Config config = Config.getInstance();
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();
	static ConfigFormat format = ConfigFormat.getInstance();

	static Main plugin = Main.plugin;

	private String serviceSite;
	private String siteName;
	private int voteDelay;
	private boolean disabled;

	private ArrayList<String> consoleCommands;
	private ArrayList<ItemStack> items;
	private int money;
	private ArrayList<String> playerCommands;

	private HashMap<String, ArrayList<String>> extraRewardsConsoleCommands;
	private HashMap<String, ArrayList<String>> extraRewardsPlayerCommands;
	private HashMap<String, Integer> extraRewardsMoney;
	private HashMap<String, ArrayList<ItemStack>> extraRewardsItems;
	private HashMap<String, String> extraRewardsPermission;
	private HashMap<String, ArrayList<String>> extraRewardsWorld;
	private HashMap<String, Integer> extraRewardsChance;

	private ArrayList<String> cumulativeConsoleCommands;
	private ArrayList<ItemStack> cumulativeItems;
	private int cumulativeMoney;
	private int cumulativeVotes;
	private ArrayList<String> cumulativePlayerCommands;

	private String voteURL;

	public VoteSite(Main plugin) {
		VoteSite.plugin = plugin;
	}

	/**
	 * New VoteSite
	 *
	 * @param siteName
	 *            Sitename
	 */
	public VoteSite(String siteName) {
		setSiteName(siteName);
		if (!configVoteSites.getVoteSiteFile(siteName).exists()) {
			if (Config.getInstance().getDisableAutoCreateVoteSites()) {
				configVoteSites.generateVoteSite(siteName);
			}
			init();
			plugin.loadVoteSites();
		} else {
			init();
		}
	}

	/**
	 * Broad cast a vote
	 *
	 * @param user
	 *            User to broadcast with
	 */
	public void broadcastVote(User user) {
		String playerName = user.getPlayerName();
		String bc = Utils.getInstance().colorize(format.getBroadCastMsg());
		bc = bc.replace("%player%", playerName).replace("%SiteName%", siteName);
		Bukkit.broadcastMessage(bc);
	}

	/**
	 *
	 * @param user
	 *            User to execute commands with
	 */
	public void doCumulativeRewardSiteCommands(User user) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = getCumulativeConsoleCommands();

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = getCumulativePlayerCommands();

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	/**
	 *
	 * @param user
	 *            User to execute commands with
	 */
	public void doExtraRewardSiteCommands(User user, String reward) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = getExtraRewardsConsoleCommands().get(
				reward);

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = getExtraRewardsPlayerCommands().get(
				reward);
		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	/**
	 *
	 * @param user
	 *            User to execute commands with
	 */
	public void doSiteCommands(User user) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = configVoteSites
				.getConsoleCommands(siteName);

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = configVoteSites
				.getPlayerCommands(siteName);

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	/**
	 * @return the consoleCommands
	 */
	public ArrayList<String> getConsoleCommands() {
		return consoleCommands;
	}

	/**
	 * @return the cumulativeConsoleCommands
	 */
	public ArrayList<String> getCumulativeConsoleCommands() {
		return cumulativeConsoleCommands;
	}

	/**
	 * @return the cumulativeItems
	 */
	public ArrayList<ItemStack> getCumulativeItems() {
		return cumulativeItems;
	}

	/**
	 * @return the cumulativeMoney
	 */
	public int getCumulativeMoney() {
		return cumulativeMoney;
	}

	/**
	 * @return the cumulativePlayerCommands
	 */
	public ArrayList<String> getCumulativePlayerCommands() {
		return cumulativePlayerCommands;
	}

	@SuppressWarnings("deprecation")
	public ItemStack getCumulativeRewardItemStackItem(String item) {
		int id = configVoteSites.getCumulativeRewardItemID(siteName, item);
		int amount = configVoteSites.getCumulativeRewardItemAmount(siteName,
				item);
		int data = configVoteSites.getCumulativeRewardItemData(siteName, item);

		String itemName = configVoteSites.getCumulativeRewardItemName(siteName,
				item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = configVoteSites.getCumulativeRewardItemLore(
				siteName, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addLore(itemStack, lore);
		itemStack = Utils.getInstance()
				.addEnchants(
						itemStack,
						configVoteSites.getCumulativeRewardEnchantments(
								siteName, item));
		return itemStack;
	}

	/**
	 * @return the cumulativeVotes
	 */
	public int getCumulativeVotes() {
		return cumulativeVotes;
	}

	/**
	 * @return Chance
	 */
	public int getExtraRewardChance(String reward) {
		int chance = configVoteSites.getExtraRewardChance(siteName, reward);
		if (chance <= 0) {
			chance = 100;
		} else if (chance > 100) {
			chance = 100;
		}
		return chance;
	}

	public int getExtraRewardItemsStackAmount(String reward, String item) {
		int amount = configVoteSites.getExtraRewardItemAmount(siteName, reward,
				item);
		int maxAmount = configVoteSites.getExtraRewardMaxItemAmount(siteName,
				reward, item);
		int minAmount = configVoteSites.getExtraRewardMinItemAmount(siteName,
				reward, item);
		if (amount != 0) {
			return amount;
		}
		if ((maxAmount == 0) && (minAmount == 0)) {
			return amount;
		} else {
			int num = minAmount
					+ (int) (Math.random() * ((maxAmount - minAmount) + 1));
			return num;
		}
	}

	@SuppressWarnings("deprecation")
	public ItemStack getExtraRewardItemStackItem(String reward, String item) {
		int id = configVoteSites.getExtraRewardItemID(siteName, reward, item);
		int amount = getExtraRewardItemsStackAmount(reward, item);
		int data = configVoteSites.getExtraRewardItemData(siteName, reward,
				item);

		String itemName = configVoteSites.getExtraRewardItemName(siteName,
				reward, item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = configVoteSites.getExtraRewardItemLore(
				siteName, reward, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addLore(itemStack, lore);
		itemStack = Utils.getInstance().addEnchants(
				itemStack,
				configVoteSites.getExtraRewardEnchantments(siteName, reward,
						item));
		return itemStack;
	}

	public int getExtraRewardMoneyAmount(String reward) {
		int amount = configVoteSites
				.getExtraRewardMoneyAmount(siteName, reward);
		int maxAmount = configVoteSites
				.getExtraRewardMaxMoney(siteName, reward);
		int minAmount = configVoteSites
				.getExtraRewardMinMoney(siteName, reward);
		if (amount != 0) {
			return amount;
		}
		if ((maxAmount == 0) && (minAmount == 0)) {
			return amount;
		} else {
			int num = minAmount
					+ (int) (Math.random() * ((maxAmount - minAmount) + 1));
			return num;
		}
	}

	/**
	 * @return the extraRewardsChance
	 */
	public HashMap<String, Integer> getExtraRewardsChance() {
		return extraRewardsChance;
	}

	/**
	 * @return the extraRewardsConsoleCommands
	 */
	public HashMap<String, ArrayList<String>> getExtraRewardsConsoleCommands() {
		return extraRewardsConsoleCommands;
	}

	/**
	 * @return the extraRewardsItems
	 */
	public HashMap<String, ArrayList<ItemStack>> getExtraRewardsItems() {
		return extraRewardsItems;
	}

	/**
	 * @return the extraRewardsMoney
	 */
	public HashMap<String, Integer> getExtraRewardsMoney() {
		return extraRewardsMoney;
	}

	/**
	 * @return the extraRewardsPermission
	 */
	public HashMap<String, String> getExtraRewardsPermission() {
		return extraRewardsPermission;
	}

	/**
	 * @return the extraRewardsPlayerCommands
	 */
	public HashMap<String, ArrayList<String>> getExtraRewardsPlayerCommands() {
		return extraRewardsPlayerCommands;
	}

	/**
	 * @return the extraRewardsWorld
	 */
	public HashMap<String, ArrayList<String>> getExtraRewardsWorld() {
		return extraRewardsWorld;
	}

	/**
	 * @return the items
	 */
	public ArrayList<ItemStack> getItems() {
		return items;
	}

	@SuppressWarnings("deprecation")
	public ItemStack getItemStackItem(String item) {
		int id = configVoteSites.getItemID(siteName, item);
		int amount = configVoteSites.getItemAmount(siteName, item);
		int data = configVoteSites.getItemData(siteName, item);

		String itemName = configVoteSites.getItemName(siteName, item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = configVoteSites.getItemLore(siteName, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addLore(itemStack, lore);
		itemStack = Utils.getInstance().addEnchants(itemStack,
				configVoteSites.getEnchantments(siteName, item));
		return itemStack;
	}

	/**
	 * @return the money
	 */
	public int getMoney() {
		return money;
	}

	/**
	 * @return the playerCommands
	 */
	public ArrayList<String> getPlayerCommands() {
		return playerCommands;
	}

	/**
	 * @return the serviceSite
	 */
	public String getServiceSite() {
		return serviceSite;
	}

	/**
	 * @return the siteName
	 */
	public String getSiteName() {
		return siteName;
	}

	/**
	 * @return the voteDelay
	 */
	public int getVoteDelay() {
		return voteDelay;
	}

	/**
	 * @return the voteURL
	 */
	public String getVoteURL() {
		return voteURL;
	}

	public void giveCumulativeReward(User user) {
		try {

			user.addCumulativeReward(this);

			if ((user.getCumulativeReward(this) >= configVoteSites
					.getCumulativeRewardVotesAmount(siteName))
					&& (configVoteSites
							.getCumulativeRewardVotesAmount(siteName) != 0)) {

				doCumulativeRewardSiteCommands(user);

				giveCumulativeRewardItemSiteReward(user);

				giveCumulativeRewardMoneySite(user);

				user.setCumulativeReward(this, 0);

				user.sendMessage(Utils
						.getInstance()
						.replaceIgnoreCase(
								ConfigFormat.getInstance()
										.getCumulativeRewardMsg(),
								"%votes%",
								""
										+ configVoteSites
												.getCumulativeRewardVotesAmount(siteName)));
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

	/**
	 *
	 * @param user
	 *            User to give items to
	 */
	public void giveCumulativeRewardItemSiteReward(User user) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning(
						"Error giving player items! Player = null");
			}
			return;
		}

		ArrayList<ItemStack> items = getCumulativeItems();
		for (ItemStack item : items) {
			user.giveItem(item);
		}
	}

	/**
	 * @param user
	 *            User to give money to
	 */
	public void giveCumulativeRewardMoneySite(User user) {
		int money = getCumulativeMoney();
		user.giveMoney(money);
	}

	public void giveExtraReward(User user, String reward) {
		try {
			String perm = getExtraRewardsPermission().get(reward);
			if (perm != null) {
				if (!Utils.getInstance().hasPermission(user.getPlayerName(),
						perm)) {
					return;
				}
			}
			int chance = getExtraRewardsChance().get(reward);

			int randomNum = (int) (Math.random() * 100) + 1;
			if (config.getDebugEnabled()) {
				plugin.getLogger().info(
						"Random: " + randomNum + ", Chance: " + chance);
			}
			if (randomNum <= chance) {
				if (config.getDebugEnabled()) {
					plugin.getLogger().info("Giving reward");
				}
				ArrayList<String> worlds = extraRewardsWorld.get(reward);
				Player player = Bukkit.getPlayer(user.getPlayerName());
				if ((player != null) && (worlds != null)) {
					if (ConfigVoteSites.getInstance()
							.getExtraRewardGiveInEachWorld(perm, reward)) {
						for (String world : worlds) {
							if (player.getWorld().getName().equals(world)) {
								giveExtraRewardReward(user, reward, chance);
							} else {
								Data.getInstance().setOfflineVotesWorld(
										user,
										getSiteName(),
										reward,
										world,
										Data.getInstance()
												.getOfflineVotesWorld(user,
														getSiteName(), reward,
														world) + 1);

							}
						}
					} else {
						if (worlds.contains(player.getWorld().getName())) {
							giveExtraRewardReward(user, reward, chance);
						} else {
							Data.getInstance().setOfflineVotesExtraReward(
									user,
									getSiteName(),
									reward,
									Data.getInstance()
											.getOfflineVotesExtraReward(user,
													getSiteName(), reward) + 1);
						}
					}
				} else {
					giveExtraRewardReward(user, reward, chance);
				}

			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

	/**
	 *
	 * @param user
	 *            User to give items to
	 */
	public void giveExtraRewardItemSiteReward(User user, String reward) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning(
						"Error giving player items! Player = null");
			}
			return;
		}

		Set<String> items = configVoteSites.getExtraRewardItems(siteName,
				reward);
		for (String item : items) {
			user.giveItem(getExtraRewardItemStackItem(reward, item));
		}
	}

	/**
	 * @param user
	 *            User to give money to
	 */
	public void giveExtraRewardMoneySite(User user, String reward) {
		int money = getExtraRewardsMoney().get(reward);
		user.giveMoney(money);
	}

	public void giveExtraRewardReward(User user, String reward, int chance) {
		if (chance != 100) {
			user.sendMessage(ConfigFormat.getInstance().getExtraRewardMsg());
		}
		doExtraRewardSiteCommands(user, reward);
		try {
			giveExtraRewardItemSiteReward(user, reward);
		} catch (Exception ex) {
			if (config.getDebugEnabled()) {
				ex.printStackTrace();
			}
		}
		giveExtraRewardMoneySite(user, reward);
	}

	/**
	 *
	 * @param user
	 *            User to give items to
	 */
	public void giveItemSiteReward(User user) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning(
						"Error giving player items! Player = null");
			}
			return;
		}

		Set<String> items = configVoteSites.getItems(siteName);
		for (String item : items) {
			user.giveItem(getItemStackItem(item));
		}

	}

	/**
	 * @param user
	 *            User to give money to
	 */
	public void giveMoneySite(User user) {
		int money = configVoteSites.getMoneyAmount(siteName);
		user.giveMoney(money);
	}

	/**
	 *
	 * @param user
	 *            User to give rewards to
	 */
	public void giveSiteReward(User user) {
		VoteSite voteSite = this;
		if (!voteSite.isDisabled()) {

			giveItemSiteReward(user);

			giveMoneySite(user);
			doSiteCommands(user);

			String playerName = user.getPlayerName();

			String rewardmsg = format.getRewardMsg();
			rewardmsg = rewardmsg
					.replace("%player%", playerName)
					.replace("%SiteName%", voteSite.getSiteName())
					.replace("%money%",
							"" + configVoteSites.getMoneyAmount(siteName));

			rewardmsg = rewardmsg.replace(
					"%items%",
					Utils.getInstance().makeStringList(
							Utils.getInstance().convert(
									configVoteSites.getItems(siteName))));

			if ((rewardmsg != null) && (rewardmsg != "")) {
				user.sendMessage(rewardmsg);
			}
			for (String reward : configVoteSites
					.getExtraRewardRewards(siteName)) {
				giveExtraReward(user, reward);
			}

			giveCumulativeReward(user);

		} else {
			plugin.getLogger().info(
					"VoteSite '" + voteSite.getSiteName() + "' is Disabled!");
		}

	}

	public void init() {
		setDisabled(configVoteSites.getVoteSiteDisabled(siteName));
		serviceSite = configVoteSites.getServiceSite(siteName);
		voteURL = configVoteSites.getVoteURL(siteName);
		voteDelay = configVoteSites.getVoteDelay(siteName);
		money = configVoteSites.getMoneyAmount(siteName);
		try {
			items = new ArrayList<ItemStack>();
			for (String item : configVoteSites.getItems(siteName)) {
				items.add(getItemStackItem(item));
			}
		} catch (Exception ex) {
			if (config.getDebugEnabled()) {
				ex.printStackTrace();
			}
		}
		try {
			consoleCommands = configVoteSites.getConsoleCommands(siteName);
			playerCommands = configVoteSites.getPlayerCommands(siteName);
		} catch (Exception ex) {
			if (config.getDebugEnabled()) {
				ex.printStackTrace();
			}
		}

		Set<String> rewards = configVoteSites.getExtraRewardRewards(siteName);
		extraRewardsConsoleCommands = new HashMap<String, ArrayList<String>>();
		extraRewardsPlayerCommands = new HashMap<String, ArrayList<String>>();
		extraRewardsItems = new HashMap<String, ArrayList<ItemStack>>();
		extraRewardsPermission = new HashMap<String, String>();
		extraRewardsWorld = new HashMap<String, ArrayList<String>>();
		extraRewardsChance = new HashMap<String, Integer>();
		extraRewardsMoney = new HashMap<String, Integer>();
		for (String reward : rewards) {
			try {
				extraRewardsConsoleCommands.put(reward, configVoteSites
						.getExtraRewardConsoleCommands(siteName, reward));
				extraRewardsPlayerCommands.put(reward, configVoteSites
						.getExtraRewardPlayerCommands(siteName, reward));

			} catch (Exception ex) {
				if (config.getDebugEnabled()) {
					ex.printStackTrace();
				}
			}

			extraRewardsMoney
					.put(reward, configVoteSites.getExtraRewardMoneyAmount(
							siteName, reward));

			try {

				ArrayList<ItemStack> extraRewardsRewardItems = new ArrayList<ItemStack>();
				for (String item : configVoteSites.getExtraRewardItems(
						siteName, reward)) {
					extraRewardsRewardItems.add(getExtraRewardItemStackItem(
							reward, item));
				}

				extraRewardsItems.put(reward, extraRewardsRewardItems);

			} catch (Exception ex) {
				if (config.getDebugEnabled()) {
					ex.printStackTrace();
				}
			}
			extraRewardsPermission.put(reward,
					configVoteSites.getExtraRewardPermission(siteName, reward));

			extraRewardsWorld.put(reward,
					configVoteSites.getExtraRewardWorld(siteName, reward));

			extraRewardsChance.put(reward,
					configVoteSites.getExtraRewardChance(siteName, reward));

		}

		cumulativeMoney = configVoteSites
				.getCumulativeRewardMoneyAmount(siteName);
		cumulativeVotes = configVoteSites
				.getCumulativeRewardVotesAmount(siteName);
		try {
			cumulativeItems = new ArrayList<ItemStack>();
			for (String item : configVoteSites
					.getCumulativeRewardItems(siteName)) {
				cumulativeItems.add(getCumulativeRewardItemStackItem(item));
			}
		} catch (Exception ex) {
			if (config.getDebugEnabled()) {
				ex.printStackTrace();
			}
		}
		try {
			cumulativeConsoleCommands = configVoteSites
					.getCumulativeRewardConsoleCommands(siteName);
			cumulativePlayerCommands = configVoteSites
					.getCumulativeRewardPlayerCommands(siteName);
		} catch (Exception ex) {
			if (config.getDebugEnabled()) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * @return the disabled
	 */
	public boolean isDisabled() {
		return disabled;
	}

	/**
	 * @param consoleCommands
	 *            the consoleCommands to set
	 */
	public void setConsoleCommands(ArrayList<String> consoleCommands) {
		this.consoleCommands = consoleCommands;
	}

	/**
	 * @param cumulativeConsoleCommands
	 *            the cumulativeConsoleCommands to set
	 */
	public void setCumulativeConsoleCommands(
			ArrayList<String> cumulativeConsoleCommands) {
		this.cumulativeConsoleCommands = cumulativeConsoleCommands;
	}

	/**
	 * @param cumulativeItems
	 *            the cumulativeItems to set
	 */
	public void setCumulativeItems(ArrayList<ItemStack> cumulativeItems) {
		this.cumulativeItems = cumulativeItems;
	}

	/**
	 * @param cumulativeMoney
	 *            the cumulativeMoney to set
	 */
	public void setCumulativeMoney(int cumulativeMoney) {
		this.cumulativeMoney = cumulativeMoney;
	}

	/**
	 * @param cumulativePlayerCommands
	 *            the cumulativePlayerCommands to set
	 */
	public void setCumulativePlayerCommands(
			ArrayList<String> cumulativePlayerCommands) {
		this.cumulativePlayerCommands = cumulativePlayerCommands;
	}

	/**
	 * @param cumulativeVotes
	 *            the cumulativeVotes to set
	 */
	public void setCumulativeVotes(int cumulativeVotes) {
		this.cumulativeVotes = cumulativeVotes;
	}

	/**
	 * @param disabled
	 *            the enabled to set
	 */
	public void setDisabled(boolean disabled) {
		this.disabled = disabled;
	}

	/**
	 * @param extraRewardsChance
	 *            the extraRewardsChance to set
	 */
	public void setExtraRewardsChance(
			HashMap<String, Integer> extraRewardsChance) {
		this.extraRewardsChance = extraRewardsChance;
	}

	/**
	 * @param extraRewardsConsoleCommands
	 *            the extraRewardsConsoleCommands to set
	 */
	public void setExtraRewardsConsoleCommands(
			HashMap<String, ArrayList<String>> extraRewardsConsoleCommands) {
		this.extraRewardsConsoleCommands = extraRewardsConsoleCommands;
	}

	/**
	 * @param extraRewardsItems
	 *            the extraRewardsItems to set
	 */
	public void setExtraRewardsItems(
			HashMap<String, ArrayList<ItemStack>> extraRewardsItems) {
		this.extraRewardsItems = extraRewardsItems;
	}

	/**
	 * @param extraRewardsMoney
	 *            the extraRewardsMoney to set
	 */
	public void setExtraRewardsMoney(HashMap<String, Integer> extraRewardsMoney) {
		this.extraRewardsMoney = extraRewardsMoney;
	}

	/**
	 * @param extraRewardsPermission
	 *            the extraRewardsPermission to set
	 */
	public void setExtraRewardsPermission(
			HashMap<String, String> extraRewardsPermission) {
		this.extraRewardsPermission = extraRewardsPermission;
	}

	/**
	 * @param extraRewardsPlayerCommands
	 *            the extraRewardsPlayerCommands to set
	 */
	public void setExtraRewardsPlayerCommands(
			HashMap<String, ArrayList<String>> extraRewardsPlayerCommands) {
		this.extraRewardsPlayerCommands = extraRewardsPlayerCommands;
	}

	/**
	 * @param extraRewardsWorld
	 *            the extraRewardsWorld to set
	 */
	public void setExtraRewardsWorld(
			HashMap<String, ArrayList<String>> extraRewardsWorld) {
		this.extraRewardsWorld = extraRewardsWorld;
	}

	/**
	 * @param items
	 *            the items to set
	 */
	public void setItems(ArrayList<ItemStack> items) {
		this.items = items;
	}

	/**
	 * @param money
	 *            the money to set
	 */
	public void setMoney(int money) {
		this.money = money;
	}

	/**
	 * @param playerCommands
	 *            the playerCommands to set
	 */
	public void setPlayerCommands(ArrayList<String> playerCommands) {
		this.playerCommands = playerCommands;
	}

	/**
	 * @param serviceSite
	 *            the serviceSite to set
	 */
	public void setServiceSite(String serviceSite) {
		this.serviceSite = serviceSite;
	}

	/**
	 * @param siteName
	 *            the siteName to set
	 */
	public void setSiteName(String siteName) {
		this.siteName = siteName;
	}

	/**
	 * @param voteDelay
	 *            the voteDelay to set
	 */
	public void setVoteDelay(int voteDelay) {
		this.voteDelay = voteDelay;
	}

	/**
	 * @param voteURL
	 *            the voteURL to set
	 */
	public void setVoteURL(String voteURL) {
		this.voteURL = voteURL;
	}
}
