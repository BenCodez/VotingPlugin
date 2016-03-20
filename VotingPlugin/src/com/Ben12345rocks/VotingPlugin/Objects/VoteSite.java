package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

public class VoteSite {
	private String siteName;
	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	// static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	static Main plugin = Main.plugin;

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
		this.setSiteName(siteName);
	}

	/**
	 * 
	 * @return Site name
	 */
	public String getSiteName() {
		return siteName;
	}

	/**
	 * Set name of site
	 * 
	 * @param siteName
	 *            Name to set to
	 */
	public void setSiteName(String siteName) {
		this.siteName = siteName;
	}

	/**
	 * 
	 * @return Service Site of VoteSite
	 */
	public String getVoteSiteServiceSite() {
		return ConfigVoteSites.getInstance().getData()
				.getString("VoteSites." + siteName + ".Site");
	}

	/**
	 * 
	 * @return /vote url
	 */
	public String getVoteURL() {
		return ConfigVoteSites.getInstance().getData()
				.getString("VoteSites." + siteName + ".VoteURL");
	}

	/**
	 * 
	 * @return Vote delay
	 */
	public int getVoteDelay() {
		return ConfigVoteSites.getInstance().getData()
				.getInt("VoteSites." + siteName + ".votedelay");
	}

	/**
	 * 
	 * @return Amount of money to give
	 */
	public int getMoneyAmount() {
		return ConfigVoteSites.getInstance().getData()
				.getInt("VoteSites." + siteName + ".Money");
	}

	@SuppressWarnings("unchecked")
	/**
	 * 
	 * @return		Console commands
	 */
	public ArrayList<String> getConsoleCommands() {
		return (ArrayList<String>) ConfigVoteSites.getInstance().getData()
				.getList("VoteSites." + siteName + ".Commands.Console");
	}

	@SuppressWarnings("unchecked")
	/**
	 * 
	 * @return		Player commands
	 */
	public ArrayList<String> getPlayerCommands() {
		return (ArrayList<String>) ConfigVoteSites.getInstance().getData()
				.getList("VoteSites." + siteName + ".Commands.Player");
	}

	/**
	 * 
	 * @return Items of VoteSite
	 */
	public Set<String> getItems() {
		return ConfigVoteSites.getInstance().getData()
				.getConfigurationSection("VoteSites." + siteName + ".Items")
				.getKeys(false);
	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @return Id of item
	 */
	public int getItemID(String item) {
		return ConfigVoteSites.getInstance().getData()
				.getInt("VoteSites." + siteName + ".Items." + item + ".id");
	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @return Item data value
	 */
	public int getItemData(String item) {
		return ConfigVoteSites.getInstance().getData()
				.getInt("VoteSites." + siteName + ".Items." + item + ".Data");
	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @return Amount of items
	 */
	public int getItemAmount(String item) {
		return ConfigVoteSites.getInstance().getData()
				.getInt("VoteSites." + siteName + ".Items." + item + ".Amount");
	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @return Name of item
	 */
	public String getItemName(String item) {
		return ConfigVoteSites
				.getInstance()
				.getData()
				.getString("VoteSites." + siteName + ".Items." + item + ".Name");
	}

	@SuppressWarnings("unchecked")
	/**
	 * 
	 * @param item 	Item
	 * @return		Lore of item
	 */
	public ArrayList<String> getItemLore(String item) {
		return (ArrayList<String>) ConfigVoteSites.getInstance().getData()
				.getList("VoteSites." + siteName + ".Items." + item + ".Lore");
	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @return Enchants of item
	 */
	public HashMap<String, Integer> getEnchantments(String item) {
		try {
			HashMap<String, Integer> enchantments = new HashMap<String, Integer>();
			Set<String> enchants = ConfigVoteSites
					.getInstance()
					.getData()
					.getConfigurationSection(
							"VoteSites." + siteName + ".Items." + item
									+ ".Enchants").getKeys(false);
			for (String enchant : enchants) {
				enchantments.put(enchant, getEnchantLevel(item, enchant));
			}

			return enchantments;
		} catch (Exception ex) {
			return null;
		}

	}

	/**
	 * 
	 * @param item
	 *            Item
	 * @param enchant
	 *            Enchant
	 * @return Level of enchantment
	 */
	public int getEnchantLevel(String item, String enchant) {
		return ConfigVoteSites
				.getInstance()
				.getData()
				.getInt("VoteSites." + siteName + ".Items." + item
						+ ".Enchants." + enchant);
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
		bc = bc.replace("%player%", playerName).replace("%SiteName%",
				this.getSiteName());
		Bukkit.broadcastMessage(bc);
	}

	/**
	 * 
	 * @param user
	 *            User to give rewards to
	 */
	public void giveSiteReward(User user) {
		VoteSite voteSite = this;
		try {
			giveItemSiteReward(user);
		} catch (Exception ex) {
		}
		giveMoneySite(user);
		doSiteCommands(user);

		String playerName = user.getPlayerName();

		@SuppressWarnings("deprecation")
		Player player = Bukkit.getPlayer(playerName);
		String rewardmsg = format.getRewardMsg();
		rewardmsg = rewardmsg
				.replace("%player%", playerName)
				.replace("%SiteName%", voteSite.getSiteName())
				.replace("%money%", "" + getMoneyAmount())
				.replace(
						"%items%",
						Utils.getInstance().makeStringList(
								Utils.getInstance().convert(getItems())));
		if (rewardmsg != null && rewardmsg != "") {
			player.sendMessage(Utils.getInstance().colorize(rewardmsg));
		}

	}

	/**
	 * 
	 * @param item
	 *            Item to get chance of
	 * @return Chance if giving item
	 */
	public int getChanceItem(String item) {
		int chance = ConfigVoteSites
				.getInstance()
				.getData()
				.getInt("VoteSites." + this.getSiteName() + ".Items." + item
						+ ".Chance");
		if (chance <= 0) {
			chance = 100;
		} else if (chance > 100) {
			chance = 100;
		}
		return chance;
	}

	@SuppressWarnings("deprecation")
	/**
	 * 
	 * @param user	User to give items to
	 */
	public void giveItemSiteReward(User user) {
		VoteSite voteSite = this;
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning(
						"Error giving player items! Player = null");
			}
			return;
		}

		Set<String> items = voteSite.getItems();
		for (String item : items) {
			int chance = getChanceItem(item);
			int randomNum = (int) (Math.random() * 100) + 1;
			if (randomNum <= chance) {
				if (chance != 100) {
					user.sendMessage(ConfigFormat.getInstance()
							.getChanceRewardMsg());
				}
				int id = voteSite.getItemID(item);
				int amount = voteSite.getItemAmount(item);
				int data = voteSite.getItemData(item);

				String itemName = voteSite.getItemName(item);
				itemName = Utils.getInstance().colorize(itemName);

				ArrayList<String> lore = voteSite.getItemLore(item);
				lore = Utils.getInstance().colorize(lore);

				user.giveItem(id, amount, data, itemName, lore,
						voteSite.getEnchantments(item));
			}

		}

	}

	/**
	 * @param user
	 *            User to give money to
	 */
	public void giveMoneySite(User user) {
		int money = this.getMoneyAmount();
		user.giveMoney(money);
	}

	@SuppressWarnings("deprecation")
	/**
	 * 
	 * @param user		User to execute commands with
	 */
	public void doSiteCommands(User user) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = this.getConsoleCommands();

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
		ArrayList<String> playercmds = this.getPlayerCommands();

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if (player != null && playercmd.length() > 0) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}
}
