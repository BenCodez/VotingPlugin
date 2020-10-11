package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class VoteURLVoteSite extends GUIHandler {

	private User user;
	private Main plugin;
	private String voteSite;

	public VoteURLVoteSite(Main plugin, CommandSender player, User user, String voteSite) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.voteSite = voteSite;
	}

	@Override
	public void onBook(Player player) {

	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		if (!plugin.isVoteSite(voteSite)) {
			player.sendMessage("Not a valid votesite");
			return;
		}
		VoteSite site = plugin.getVoteSite(voteSite);
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteURLSiteName());
		inv.addPlaceholder("site", site.getDisplayName());
		inv.setMeta(player, "VoteSite", site);
		if (!Config.getInstance().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Material.BOW).setName("&4URL").addLoreLine("Click to see URL")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						VoteSite site = (VoteSite) clickEvent.getMeta("VoteSite");
						clickEvent.getWhoClicked().sendMessage(site.getVoteURL());
					}
				});

		inv.addButton(new BInventoryButton(
				new ItemBuilder(Material.COMPASS).setName("&4Next Vote").addLoreLine(user.voteCommandNextInfo(site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new VoteURL(plugin, clickEvent.getPlayer(), user, true).open();
			}
		});

		inv.addButton(new BInventoryButton(
				new ItemBuilder(Material.CLOCK).setName("&4Last Vote").addLoreLine(user.voteCommandLastLine(site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new VoteURL(plugin, clickEvent.getPlayer(), user, true).open();
			}
		});

		if (Config.getInstance().getGUIVoteURLBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}
		inv.openInventory(player);

	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
