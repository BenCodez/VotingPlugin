package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteLast extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteLast(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();

		String playerName = user.getPlayerName();

		msg.add(StringParser.getInstance().replaceIgnoreCase(plugin.getConfigFile().getFormatCommandsVoteLastTitle(),
				"%player%", playerName));

		for (VoteSite voteSite : plugin.getVoteSites()) {
			if (!voteSite.isHidden()) {
				msg.add(user.voteCommandLastLine(voteSite));
			}
		}

		return ArrayUtils.getInstance().colorize(msg);
	}

	@Override
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(plugin.getGui().getChestVoteLastName());
		inv.addPlaceholder("player", user.getPlayerName());
		for (VoteSite site : plugin.getVoteSites()) {
			if (!site.isHidden()) {
				inv.addButton(inv.getNextSlot(),
						new UpdatingBInventoryButton(site.getItem().setName(site.getDisplayName())
								.setLore(user.voteCommandLastGUILine(site)).setAmountNone(1), 1000, 1000) {

							@Override
							public void onClick(ClickEvent event) {
								if (plugin.getGui().isChestVoteLastClickableLinks()) {
									Player player = event.getPlayer();
									if (player != null) {
										VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
										user.sendMessage(
												StringParser.getInstance()
														.replacePlaceHolder(
																StringParser.getInstance().replacePlaceHolder(
																		StringParser.getInstance()
																				.replacePlaceHolder(plugin.getGui()
																						.getChestVoteURLURLText(),
																						"voteurl", site.getVoteURL()),
																		"sitename", site.getDisplayName()),
																"player", player.getName()));

									}
								}
							}

							@Override
							public ItemBuilder onUpdate(Player p) {
								return site.getItem().setName(site.getDisplayName())
										.setLore(user.voteCommandLastGUILine(site)).setAmountNone(1);
							}
						});
			}
		}

		if (plugin.getGui().getChestVoteLastBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodLast().toUpperCase()));
	}

}
