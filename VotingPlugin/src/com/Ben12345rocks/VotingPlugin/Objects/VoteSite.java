package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

public class VoteSite {
	static Config config = Config.getInstance();
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();
	static ConfigFormat format = ConfigFormat.getInstance();
	static Main plugin = Main.plugin;
	private ArrayList<String> consoleCommands;
	private boolean disabled;
	private ArrayList<ItemStack> items;
	private int money;
	private ArrayList<String> playerCommands;

	private String serviceSite;

	private String siteName;

	private int voteDelay;

	// static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

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
			configVoteSites.generateVoteSite(siteName);
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
	public void doChanceRewardSiteCommands(User user, String reward) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = configVoteSites
				.getChanceRewardConsoleCommands(siteName, reward);

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
				.getChanceRewardPlayerCommands(siteName, reward);

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
	 * @return Chance
	 */
	public int getChanceRewardChance(String reward) {
		int chance = configVoteSites.getChanceRewardChance(siteName, reward);
		if (chance <= 0) {
			chance = 100;
		} else if (chance > 100) {
			chance = 100;
		}
		return chance;
	}

	public int getChanceRewardItemsStackAmount(String reward, String item) {
		int amount = configVoteSites.getChanceRewardItemAmount(siteName,
				reward, item);
		int maxAmount = configVoteSites.getChanceRewardMaxItemAmount(siteName,
				reward, item);
		int minAmount = configVoteSites.getChanceRewardMinItemAmount(siteName,
				reward, item);
		if (maxAmount == 0 && minAmount == 0) {
			return amount;
		} else {
			int num = (int) (Math.random() * maxAmount);
			num++;
			if (num < minAmount) {
				num = minAmount;
			}
			return num;
		}
	}

	@SuppressWarnings("deprecation")
	public ItemStack getChanceRewardItemStackItem(String reward, String item) {
		int id = configVoteSites.getChanceRewardItemID(siteName, reward, item);
		int amount = getChanceRewardItemsStackAmount(reward, item);
		int data = configVoteSites.getChanceRewardItemData(siteName, reward,
				item);

		String itemName = configVoteSites.getChanceRewardItemName(siteName,
				reward, item);
		itemName = Utils.getInstance().colorize(itemName);

		ArrayList<String> lore = configVoteSites.getChanceRewardItemLore(
				siteName, reward, item);
		lore = Utils.getInstance().colorize(lore);
		ItemStack itemStack = new ItemStack(id, amount, (short) data);
		itemStack = Utils.getInstance().nameItem(itemStack, itemName);
		itemStack = Utils.getInstance().addlore(itemStack, lore);
		itemStack = Utils.getInstance().addEnchants(
				itemStack,
				configVoteSites.getChanceRewardEnchantments(siteName, reward,
						item));
		return itemStack;
	}

	public int getChanceRewardMoneyAmount(String reward) {
		int amount = configVoteSites.getChanceRewardMoneyAmount(siteName,
				reward);
		int maxAmount = configVoteSites.getChanceRewardMaxMoney(siteName,
				reward);
		int minAmount = configVoteSites.getChanceRewardMinMoney(siteName,
				reward);
		if (maxAmount == 0 && minAmount == 0) {
			return amount;
		} else {
			int num = (int) (Math.random() * maxAmount);
			num++;
			if (num < minAmount) {
				num = minAmount;
			}
			return num;
		}
	}

	/**
	 * @return the consoleCommands
	 */
	public ArrayList<String> getConsoleCommands() {
		return consoleCommands;
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
		itemStack = Utils.getInstance().addlore(itemStack, lore);
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

	public void giveChanceReward(User user, String reward) {
		try {
			int chance = getChanceRewardChance(reward);
			int randomNum = (int) (Math.random() * 100) + 1;
			if (randomNum <= chance) {
				if (chance != 100) {
					user.sendMessage(ConfigFormat.getInstance()
							.getChanceRewardMsg());
				}
				doChanceRewardSiteCommands(user, reward);
				try {
					giveChanceRewardItemSiteReward(user, reward);
				} catch (Exception ex) {
				}
				giveChanceRewardMoneySite(user, reward);
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
	public void giveChanceRewardItemSiteReward(User user, String reward) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning(
						"Error giving player items! Player = null");
			}
			return;
		}

		Set<String> items = configVoteSites.getChanceRewardItems(siteName,
				reward);
		for (String item : items) {
			user.giveItem(getChanceRewardItemStackItem(reward, item));
		}
	}

	/**
	 * @param user
	 *            User to give money to
	 */
	public void giveChanceRewardMoneySite(User user, String reward) {
		int money = getChanceRewardMoneyAmount(reward);
		user.giveMoney(money);
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

			Player player = Bukkit.getPlayer(playerName);
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
				player.sendMessage(Utils.getInstance().colorize(rewardmsg));
			}
			for (String reward : ConfigVoteSites.getInstance()
					.getChanceRewardRewards(siteName)) {
				giveChanceReward(user, reward);
			}
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
	 * @param disabled
	 *            the enabled to set
	 */
	public void setDisabled(boolean disabled) {
		this.disabled = disabled;
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
