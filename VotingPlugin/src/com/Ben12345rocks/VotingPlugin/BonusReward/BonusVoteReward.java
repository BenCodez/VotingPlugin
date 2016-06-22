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
import com.Ben12345rocks.VotingPlugin.Data.Data;
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

	public void doExtraRewardBonusCommands(User user, String reward) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = bonusReward
				.getExtraRewardConsoleCommands(reward);

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
				.getExtraRewardPlayerCommands(reward);

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

	public int getExtraRewardItemsStackAmount(String reward, String item) {
		int amount = bonusReward.getExtraRewardItemAmount(reward, item);
		int maxAmount = bonusReward.getExtraRewardMaxItemAmount(reward, item);
		int minAmount = bonusReward.getExtraRewardMinItemAmount(reward, item);
		if ((maxAmount == 0) && (minAmount == 0)) {
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

	public int getExtraRewardMoneyAmount(String reward) {
		int amount = bonusReward.getExtraRewardMoneyAmount(reward);
		int maxAmount = bonusReward.getExtraRewardMaxMoneyAmount(reward);
		int minAmount = bonusReward.getExtraRewardMinMoneyAmount(reward);
		if ((maxAmount == 0) && (minAmount == 0)) {
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
		if (ConfigBonusReward.getInstance().getGiveBonusReward()) {
			giveItemBonusReward(user);
			giveMoneyBonus(user);
			doBonusCommands(user);
			user.sendMessage(ChatColor.RED
					+ "You were given bonus Items for voting on all sites in one day!");
			for (String reward : ConfigBonusReward.getInstance()
					.getExtraRewardRewards()) {
				giveExtraReward(user, reward);
			}
		}

	}

	public boolean giveBonusRewardUser(User user) {

		int userVotes = Data.getInstance().getVotesBonusReward(user);
		if (ConfigBonusReward.getInstance().getRequirementFirstVote()) {
			if (userVotes == 0) {
				return true;
			}

		}
		int votesNeeded = ConfigBonusReward.getInstance().getRequirementVotes();

		if (userVotes >= votesNeeded && votesNeeded != 0) {
			Data.getInstance().setVotesBonusReward(user,
					userVotes - votesNeeded);
			return true;
		} else if (ConfigBonusReward.getInstance().getRequirementVoteAllSites()) {
			return user.checkAllVotes();
		} else {
			return false;
		}

	}

	public void giveExtraReward(User user, String reward) {
		try {
			String perm = bonusReward.getExtraRewardPermission(reward);
			if (perm != null) {
				if (!Utils.getInstance().hasPermission(user.getPlayerName(),
						perm)) {
					return;
				}
			}
			int chance = bonusReward.getExtraRewardChance(reward);
			int randomNum = (int) (Math.random() * 100) + 1;
			if (randomNum <= chance) {
				if (chance != 100) {
					user.sendMessage(ConfigFormat.getInstance()
							.getExtraRewardMsg());
				}
				doExtraRewardBonusCommands(user, reward);
				giveExtraRewardItemBonusReward(user, reward);
				giveExtraRewardMoneyBonus(user, reward);
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void giveExtraRewardItemBonusReward(User user, String reward) {
		String playerName = user.getPlayerName();
		Player player = Bukkit.getPlayer(playerName);
		if (player == null) {
			if (config.getDebugEnabled()) {
				plugin.getLogger().warning("Error giving player items!");
			}
			return;
		}

		Set<String> items = bonusReward.getExtraRewardItems(reward);
		for (String item : items) {
			int id = bonusReward.getExtraRewardItemID(reward, item);
			int amount = getExtraRewardItemsStackAmount(reward, item);

			int data = bonusReward.getExtraRewardItemData(reward, item);

			String itemName = bonusReward.getExtraRewardItemName(reward, item);

			itemName = Utils.getInstance().colorize(itemName);

			ArrayList<String> lore = bonusReward.getExtraRewardItemLore(reward,
					item);
			lore = Utils.getInstance().colorize(lore);

			user.giveItem(id, amount, data, itemName, lore, ConfigBonusReward
					.getInstance().getExtraRewardEnchantments(reward, item));

		}

	}

	public void giveExtraRewardMoneyBonus(User user, String reward) {
		int money = getExtraRewardMoneyAmount(reward);
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
