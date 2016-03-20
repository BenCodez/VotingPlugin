package com.Ben12345rocks.VotingPlugin.API;

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

	private BonusVoteReward() {
	}

	static BonusVoteReward instance = new BonusVoteReward();

	public static BonusVoteReward getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public BonusVoteReward(Main plugin) {
		BonusVoteReward.plugin = plugin;
	}

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	@SuppressWarnings("deprecation")
	public void giveBonusReward(User user) {
		String playerName = user.getPlayerName();
		if (config.getBonusRewardEnabled()) {
			giveItemBonusReward(user);
			giveMoneyBonus(user);
			doBonusCommands(user);
			Player player = Bukkit.getPlayer(playerName);
			player.sendMessage(ChatColor.RED
					+ "You were given bonus Items for voting on all sites in one day!");
		}

	}

	@SuppressWarnings("deprecation")
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
			int chance = bonusReward.getChance(item);
			int randomNum = (int) (Math.random() * 100) + 1;
			if (randomNum <= chance) {
				if (chance != 100) {
					user.sendMessage(ConfigFormat.getInstance()
							.getChanceRewardMsg());
				}
				int id = bonusReward.getItemID(item);
				int amount = bonusReward.getItemAmount(item);

				int data = bonusReward.getItemData(item);

				String itemName = bonusReward.getItemName(item);

				itemName = Utils.getInstance().colorize(itemName);

				ArrayList<String> lore = bonusReward.getItemLore(item);
				lore = Utils.getInstance().colorize(lore);

				user.giveItem(id, amount, data, itemName, lore,
						ConfigBonusReward.getInstance().getEnchantments(item));
			}
		}

	}

	public void giveMoneyBonus(User user) {
		int money = bonusReward.getMoneyAmount();
		user.giveMoney(money);
	}

	@SuppressWarnings("deprecation")
	public static void doBonusCommands(User user) {

		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = bonusReward.getPlayerCommands();

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
				if (player != null && playercmd.length() > 0) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

}
