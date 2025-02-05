package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.player.VoteGUI;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class AdminVoteTopPoints extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public AdminVoteTopPoints(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, int page) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.page = page;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		try {
			Set<Entry<TopVoterPlayer, Integer>> users = null;

			LinkedHashMap<TopVoterPlayer, Integer> topPoints1 = new LinkedHashMap<TopVoterPlayer, Integer>();

			for (String uuid : plugin.getVotingPluginUserManager().getAllUUIDs()) {
				VotingPluginUser vpUser = plugin.getVotingPluginUserManager()
						.getVotingPluginUser(UUID.fromString(uuid));
				vpUser.dontCache();
				int points = vpUser.getPoints();
				if (points > 0) {
					topPoints1.put(vpUser.getTopVoterPlayer(), points);

				}
			}

			LinkedHashMap<TopVoterPlayer, Integer> topPoints = plugin.getTopVoterHandler().sortByValues(topPoints1,
					false);

			users = topPoints.entrySet();

			ConfigurationSection customization = plugin.getGui().getChestVoteTopCustomization();
			boolean customzationEnabled = false;
			Queue<Integer> playerSlots = new ConcurrentLinkedQueue<>();
			if (customization != null) {
				customzationEnabled = customization.getBoolean("Enabled");
				List<Integer> customizationPlayerSlots = customization.getIntegerList("PlayerSlots");
				playerSlots.addAll(customizationPlayerSlots);
			}

			BInventory inv = new BInventory("Top Points");
			if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
				inv.dontClose();
			}

			int pos = 1;
			for (Entry<TopVoterPlayer, Integer> entry : users) {

				ItemBuilder playerItem = new ItemBuilder(Material.PAPER);

				if (plugin.getGui().isChestVoteTopUseSkull()) {
					playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
				} else {
					playerItem = new ItemBuilder(Material.valueOf(plugin.getGui().getChestVoteTopPlayerItemMaterial()));
				}

				playerItem.setLore(new ArrayList<>());

				BInventoryButton button = new BInventoryButton(playerItem
						.setName(plugin.getGui().getChestVoteTopItemName())
						.addLoreLine(plugin.getGui().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
						.addPlaceholder("player", entry.getKey().getPlayerName())
						.addPlaceholder("votes", "" + entry.getValue())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						if (plugin.getGui().isChestVoteTopOpenMainGUIOnClick()) {
							TopVoterPlayer user = (TopVoterPlayer) getData("User");
							new VoteGUI(plugin, player, user.getUser())
									.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
						}
					}
				}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey());

				if (customzationEnabled && !playerSlots.isEmpty()) {
					button.setSlot(playerSlots.remove());
				}

				inv.setCloseInv(plugin.getGui().isChestVoteTopCloseGUIOnClick());

				inv.addButton(button);
				pos++;
			}

			if (plugin.getGui().isChestVoteTopBackButton()) {
				if (customzationEnabled) {
					inv.addButton(plugin.getCommandLoader().getBackButton(user)
							.setSlot(customization.getInt("BackButtonSlot", 0)));
				} else {
					inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(1));
				}
			}

			String guiPath = "VoteTop.Customization";
			for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
				inv.addButton(
						new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {

							@Override
							public void onClick(ClickEvent clickEvent) {
								plugin.getCommandLoader().processSlotClick(player, user, str);
								new RewardBuilder(plugin.getGui().getData(),
										"CHEST." + guiPath + ".ExtraItems." + str + ".Rewards").setGiveOffline(false)
										.send(clickEvent.getPlayer());

							}
						});
			}

			if (customization == null || !customzationEnabled || !customization.getBoolean("RemoveBottomBar")) {
				inv.setPages(true);
			}
			inv.setMaxInvSize(plugin.getGui().getChestVoteTopSize());
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
