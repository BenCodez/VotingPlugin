package com.Ben12345rocks.VotingPlugin.BonusReward;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class BonusVoteReward {

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static BonusVoteReward instance = new BonusVoteReward();

	static Main plugin = Main.plugin;

	public static BonusVoteReward getInstance() {
		return instance;
	}

	private BonusVoteReward() {
	}

	public BonusVoteReward(Main plugin) {
		BonusVoteReward.plugin = plugin;
	}

	public void doBonusCommands(User user) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = bonusReward.getConsoleCommands();

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
		ArrayList<String> playercmds = bonusReward.getPlayerCommands();

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

	public void doChanceRewardBonusCommands(User user, String reward) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = bonusReward
				.getChanceRewardConsoleCommands(reward);

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
		ArrayList<String> playercmds = bonusReward
				.getChanceRewardPlayerCommands(reward);

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

	public int getChanceRewardItemsStackAmount(String reward, String item) {
		int amount = bonusReward.getChanceRewardItemAmount(reward, item);
		int maxAmount = bonusReward.getChanceRewardMaxItemAmount(reward, item);
		int minAmount = bonusReward.getChanceRewardMinItemAmount(reward, item);
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

	public int getChanceRewardMoneyAmount(String reward) {
		int amount = bonusReward.getChanceRewardMoneyAmount(reward);
		int maxAmount = bonusReward.getChanceRewardMaxMoneyAmount(reward);
		int minAmount = bonusReward.getChanceRewardMinMoneyAmount(reward);
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

	public void giveBonusReward(User user) {
		String playerName = user.getPlayerName();
		if (ConfigBonusReward.getInstance().getGiveBonusReward()) {
			giveItemBonusReward(user);
			giveMoneyBonus(user);
			doBonusCommands(user);
			Player player = Bukkit.getPlayer(playerName);
			player.sendMessage(ChatColor.RED
					+ "You were given bonus Items for voting on all sites in one day!");
			for (String reward : ConfigBonusReward.getInstance()
					.getChanceRewardRewards()) {
				giveChanceReward(user, reward);
			}
		}

	}

	public void giveChanceReward(User user, String reward) {
		try {
			int chance = bonusReward.getChanceRewardChance(reward);
			int randomNum = (int) (Math.random() * 100) + 1;
			if (randomNum <= chance) {
				if (chance != 100) {
					user.sendMessage(ConfigFormat.getInstance()
							.getChanceRewardMsg());
				}
				doChanceRewardBonusCommands(user, reward);
				giveChanceRewardItemBonusReward(user, reward);
				giveChanceRewardMoneyBonus(user, reward);
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void giveChanceRewardItemBonusReward(User user, String reward) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning("Error giving player items!");
			}
			return;
		}

		Set<String> items = bonusReward.getChanceRewardItems(reward);
		for (String item : items) {
			int id = bonusReward.getChanceRewardItemID(reward, item);
			int amount = getChanceRewardItemsStackAmount(reward, item);

			int data = bonusReward.getChanceRewardItemData(reward, item);

			String itemName = bonusReward.getChanceRewardItemName(reward, item);

			itemName = Utils.getInstance().colorize(itemName);

			ArrayList<String> lore = bonusReward.getChanceRewardItemLore(
					reward, item);
			lore = Utils.getInstance().colorize(lore);

			user.giveItem(id, amount, data, itemName, lore, ConfigBonusReward
					.getInstance().getChanceRewardEnchantments(reward, item));

		}

	}

	public void giveChanceRewardMoneyBonus(User user, String reward) {
		int money = getChanceRewardMoneyAmount(reward);
		user.giveMoney(money);
	}

	public void giveItemBonusReward(User user) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning("Error giving player items!");
			}
			return;
		}

		Set<String> items = bonusReward.getItems();
		for (String item : items) {
			int id = bonusReward.getItemID(item);
			int amount = bonusReward.getItemAmount(item);

			int data = bonusReward.getItemData(item);

			String itemName = bonusReward.getItemName(item);

			itemName = Utils.getInstance().colorize(itemName);

			ArrayList<String> lore = bonusReward.getItemLore(item);
			lore = Utils.getInstance().colorize(lore);

			user.giveItem(id, amount, data, itemName, lore, ConfigBonusReward
					.getInstance().getEnchantments(item));

		}

	}

	public void giveMoneyBonus(User user) {
		int money = bonusReward.getMoneyAmount();
		user.giveMoney(money);
	}

}
