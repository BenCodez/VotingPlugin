package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteTotal extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteTotal(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		ArrayList<String> msg = new ArrayList<String>();

		int daily = user.getTotal(TopVoter.Daily);
		int weekly = user.getTotal(TopVoter.Weekly);
		int month = user.getTotal(TopVoter.Monthly);
		int all = user.getTotal(TopVoter.AllTime);

		for (String s : plugin.getConfigFile().getFormatCommandsVoteTotal()) {
			String str = StringParser.getInstance().replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%player%", user.getPlayerName());
			msg.add(str);
		}

		return ArrayUtils.getInstance().colorize(msg);
	}

	@Override
	public void onBook(Player player) {
		// future release?
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(plugin.getGui().getChestVoteTotalName());
		inv.addPlaceholder("player", user.getPlayerName());

		for (final TopVoter top : TopVoter.values()) {
			inv.addButton(new UpdatingBInventoryButton(
					new ItemBuilder(plugin.getGui().getChestVoteTotalItem(top))
							.addPlaceholder("Total", "" + user.getTotal(top)).addPlaceholder("topvoter", top.getName()),
					1000, 1000) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}

				@Override
				public ItemBuilder onUpdate(Player arg0) {
					return new ItemBuilder(plugin.getGui().getChestVoteTotalItem(top))
							.addPlaceholder("Total", "" + user.getTotal(top)).addPlaceholder("topvoter", top.getName())
							.addPlaceholder("player", user.getPlayerName());
				}
			});
		}

		String guiPath = "VoteTotal";
		for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
			inv.addButton(
					new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							new RewardBuilder(plugin.getGui().getData(),
									"CHEST." + guiPath + ".ExtraItems." + str + "."
											+ clickEvent.getButton().getLastRewardsPath(player))
									.setGiveOffline(false).send(clickEvent.getPlayer());

						}
					});
		}

		if (plugin.getGui().getChestVoteTotalBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTotal().toUpperCase()));
	}

}
