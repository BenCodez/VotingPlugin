package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Messages.Messages;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class CommandVoteGUI implements CommandExecutor {

	private Main plugin;

	public CommandVoteGUI(Main plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		if (!Utils.getInstance().isPlayer(sender)) {
			sender.sendMessage("You must be a player to use this command");
			return true;
		}
		Player player = (Player) sender;
		
		if (!Utils.getInstance().hasPermission(player, "Commands.Vote.GUI")) {
			player.sendMessage("&cNo Permissions to use this command");
			return true;
		}

		if (args.length == 1) {
			if (args[0].equalsIgnoreCase("votesites")) {
				openVoteSitesListGUI(player, 1);
				return true;
			}
		}

		if (args.length == 2) {
			if (args[0].equalsIgnoreCase("votesites")) {
				if (Utils.getInstance().isInt(args[1])) {
					openVoteSitesListGUI(player, Integer.parseInt(args[1]));
				} else {
					player.sendMessage("&c" + args[1] + " is not an integer");
				}
				return true;
			}
		}

		// invalid command
		sender.sendMessage(Messages.getInstance().invalidCommand());

		return true;
	}

	public void openVoteSitesListGUI(Player player, int page) {
		String guiName = "VotingPlugin: VoteSites";

		ArrayList<ItemStack> items = new ArrayList<ItemStack>();

		for (VoteSite voteSite : plugin.voteSites) {
			ItemStack item = new ItemStack(Material.STONE);
			item = Utils.getInstance().nameItem(item, voteSite.getSiteName());
			items.add(item);
		}

		Inventory inv = Bukkit.createInventory(null, 54, guiName);

		int slot = 0;

		for (int i = (page - 1) * 45; i < items.size() && slot <= 54; i++) {
			try {
				inv.setItem(slot, items.get(i));
				slot++;
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}

		ItemStack placeHolder = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 7);

		ItemStack prevPage = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 5);

		int maxPages = (int) page / 45;
		maxPages++;

		List<String> lore = new ArrayList<String>();
		lore.add("&bCurrent Page: &6" + page);
		lore.add("&bMax Pages: &6" + maxPages);

		prevPage = Utils.getInstance()
				.addLore(
						Utils.getInstance().nameItem(prevPage,
								"&cPrevious Page"), lore);

		ItemStack nextPage = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 5);

		nextPage = Utils.getInstance().addLore(
				Utils.getInstance().nameItem(nextPage, "&cNext Page"), lore);

		ItemStack back = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 12);

		back = Utils.getInstance().nameItem(back, "&cBack");

		inv.setItem(45, placeHolder);

		inv.setItem(46, placeHolder);

		inv.setItem(47, prevPage);

		inv.setItem(48, placeHolder);
		inv.setItem(49, placeHolder);
		inv.setItem(50, placeHolder);

		inv.setItem(51, nextPage);

		inv.setItem(52, placeHolder);

		inv.setItem(53, back);

		player.openInventory(inv);
	}

}
