package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteURLVoteSite extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;
	private String voteSite;

	public VoteURLVoteSite(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, String voteSite) {
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
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteURLSiteName());
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

		inv.addButton(new UpdatingBInventoryButton(
				new ItemBuilder(Material.COMPASS).setName("&4Next Vote").addLoreLine(user.voteCommandNextInfo(site)),
				1000, 1000) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new VoteURL(plugin, clickEvent.getPlayer(), user, true).open();
			}

			@Override
			public ItemBuilder onUpdate(Player arg0) {
				return new ItemBuilder(Material.COMPASS).setName("&4Next Vote").addLoreLine(user.voteCommandNextInfo(site));
			}
		});

		inv.addButton(new BInventoryButton(
				new ItemBuilder(Material.CLOCK).setName("&4Last Vote").addLoreLine(user.voteCommandLastLine(site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new VoteURL(plugin, clickEvent.getPlayer(), user, true).open();
			}
		});

		if (GUI.getInstance().getChestVoteURLBackButton()) {
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
