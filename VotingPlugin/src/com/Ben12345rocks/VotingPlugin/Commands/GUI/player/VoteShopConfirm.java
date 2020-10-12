package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteShopConfirm extends GUIHandler {

	private User user;
	private Main plugin;
	private String identifier;

	public VoteShopConfirm(Main plugin, CommandSender player, User user, String identifier) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.identifier = identifier;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		PlayerUtils.getInstance().setPlayerMeta(player, "ident", identifier);
		BInventory inv = new BInventory("Confirm Purchase?");
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aYes")) {

			@Override
			public void onClick(ClickEvent event) {
				String ident = (String) PlayerUtils.getInstance().getPlayerMeta(event.getPlayer(), "ident");
				int points = GUI.getInstance().getChestShopIdentifierCost(ident);
				int limit = GUI.getInstance().getChestShopIdentifierLimit(identifier);
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("identifier", identifier);
				placeholders.put("points", "" + points);
				placeholders.put("limit", "" + limit);
				if (user.removePoints(points)) {

					RewardHandler.getInstance().giveReward(user, GUI.getInstance().getData(),
							GUI.getInstance().getChestShopIdentifierRewardsPath(identifier),
							new RewardOptions().setPlaceholders(placeholders));

					user.sendMessage(StringParser.getInstance()
							.replacePlaceHolder(Config.getInstance().getFormatShopPurchaseMsg(), placeholders));
					if (limit > 0) {
						user.setVoteShopIdentifierLimit(identifier, user.getVoteShopIdentifierLimit(identifier) + 1);
					}
				} else {
					user.sendMessage(StringParser.getInstance()
							.replacePlaceHolder(Config.getInstance().getFormatShopFailedMsg(), placeholders));
				}
				Bukkit.getScheduler().runTask(plugin, new Runnable() {

					@Override
					public void run() {
						event.getPlayer().closeInventory();
					}
				});

			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.BARRIER).setName("&cNo")) {

			@Override
			public void onClick(ClickEvent event) {
				event.getPlayer().closeInventory();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		return null;
	}
	
	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
