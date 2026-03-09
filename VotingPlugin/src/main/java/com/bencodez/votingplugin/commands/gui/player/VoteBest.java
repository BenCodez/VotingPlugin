package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * GUI handler for vote best/streak display.
 */
public class VoteBest extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	/**
	 * Constructs a new vote best GUI.
	 * 
	 * @param plugin the plugin instance
	 * @param player the command sender
	 * @param user   the voting plugin user
	 */
	public VoteBest(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<>();
		msg.add(plugin.getConfigFile().getFormatCommandsVoteBestTitle());
		msg.addAll(plugin.getConfigFile().getFormatCommandsVoteBestLines());

		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("HighestDailyTotal", "" + user.getHighestDailyTotal());
		placeholders.put("HighestWeeklyTotal", "" + user.getHighestWeeklyTotal());
		placeholders.put("HighestMonthlyTotal", "" + user.getHighestMonthlyTotal());

		placeholders.put("player", user.getPlayerName());

		msg = PlaceholderUtils.replacePlaceHolder(msg, placeholders);

		return ArrayUtils.colorize(msg);
	}

	@Override
	public void onDialog(Player player) {
		plugin.getDialogService().multiAction(player)
				.placeholder("player", user.getPlayerName())
				.placeholder("HighestDailyTotal", "" + user.getHighestDailyTotal())
				.placeholder("HighestWeeklyTotal", "" + user.getHighestWeeklyTotal())
				.placeholder("HighestMonthlyTotal", "" + user.getHighestMonthlyTotal())
				.title(plugin.getGui().getChestVoteBestName())
				.body(plugin.getConfigFile().getFormatCommandsVoteBestTitle())
				.columns(2)
				.button(plugin.getGui().getChestVoteBestDayBestItem().getName(),
						"&7Best daily total: &e%HighestDailyTotal%",
						payload -> {
						})
				.button(plugin.getGui().getChestVoteBestWeekBestItem().getName(),
						"&7Best weekly total: &e%HighestWeeklyTotal%",
						payload -> {
						})
				.button(plugin.getGui().getChestVoteBestMonthBestItem().getName(),
						"&7Best monthly total: &e%HighestMonthlyTotal%",
						payload -> {
						})
				.button(plugin.getGui().isChestVoteBestBackButton() ? "&eBack" : "",
						plugin.getGui().isChestVoteBestBackButton() ? "&7Return to previous menu" : "",
						payload -> {
							if (plugin.getGui().isChestVoteBestBackButton()) {
								Player clicked = player.getServer().getPlayer(payload.owner());
								if (clicked != null) {
									new VoteGUI(plugin, clicked, user).open();
								}
							}
						})
				.open();
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
		BInventory inv = new BInventory(plugin.getGui().getChestVoteBestName());
		inv.addPlaceholder("player", user.getPlayerName());

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteBestDayBestItem())
				.addPlaceholder("Best", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteBestWeekBestItem())
				.addPlaceholder("Best", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteBestMonthBestItem())
				.addPlaceholder("Best", "" + user.getBestMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		if (plugin.getGui().isChestVoteBestBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		String guiPath = "VoteBest";
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

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodBest().toUpperCase()));
	}

}
