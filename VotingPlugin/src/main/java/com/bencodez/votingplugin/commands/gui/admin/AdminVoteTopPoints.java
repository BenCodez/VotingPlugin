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
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.player.VoteGUI;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Admin top points GUI handler.
 */
public class AdminVoteTopPoints extends GUIHandler {

	@SuppressWarnings("unused")
	private int page;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	/**
	 * Constructor for AdminVoteTopPoints.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 * @param user   the voting plugin user
	 * @param page   the page number
	 */
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
	public void onDialog(Player player) {

	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		org.bukkit.inventory.Inventory expectedTop = player.getOpenInventory().getTopInventory();
		try {
			plugin.getUserManager().getDataManager().getTimer().execute(() -> {
				try {
					LinkedHashMap<TopVoterPlayer, Integer> raw = new LinkedHashMap<>();
					for (String uuidString : plugin.getVotingPluginUserManager().getAllUUIDs()) {
						UUID uuid = UUID.fromString(uuidString);
						VotingPluginUser vpUser = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
						vpUser.userDataFetechMode(UserDataFetchMode.NO_CACHE);
						int points = vpUser.getPoints();
						if (points <= 0) continue;
						String name = vpUser.getUserData().getString("PlayerName", UserDataFetchMode.NO_CACHE);
						String lastOnlineValue = vpUser.getUserData().getString("LastOnline", UserDataFetchMode.NO_CACHE);
						long lastOnline = 0L;
						try {
							if (lastOnlineValue != null && !lastOnlineValue.isBlank() && !"null".equalsIgnoreCase(lastOnlineValue)) {
								lastOnline = Long.parseLong(lastOnlineValue);
							}
						} catch (NumberFormatException ignored) { }
						raw.put(new TopVoterPlayer(uuid, name == null ? "" : name, lastOnline), points);
					}
					LinkedHashMap<TopVoterPlayer, Integer> sorted = plugin.getTopVoterHandler().sortByValues(raw, false);
					plugin.getBukkitScheduler().runTask(plugin, () -> {
						if (player.getOpenInventory().getTopInventory() != expectedTop) return;
						openComputedChest(player, sorted);
					}, player);
				} catch (Throwable failure) { reportLoadFailure(player, "Failed to load top points", failure); }
			});
		} catch (RuntimeException failure) { reportLoadFailure(player, "Failed to schedule top points load", failure); }
	}

	private void reportLoadFailure(Player player, String message, Throwable failure) {
		plugin.getLogger().log(java.util.logging.Level.WARNING, message, failure);
		try {
			plugin.getBukkitScheduler().runTask(plugin,
					() -> player.sendMessage(MessageAPI.colorize("&cFailed to load top points")), player);
		} catch (RuntimeException schedulingFailure) {
			plugin.getLogger().log(java.util.logging.Level.WARNING,
					"Failed to notify player about top points load failure", schedulingFailure);
		}
	}

	private void openComputedChest(Player player, LinkedHashMap<TopVoterPlayer, Integer> topPoints) {
		try {
			Set<Entry<TopVoterPlayer, Integer>> users = topPoints.entrySet();
			ConfigurationSection customization = plugin.getGui().getChestVoteTopCustomization();
			boolean customzationEnabled = false;
			Queue<Integer> playerSlots = new ConcurrentLinkedQueue<>();
			if (customization != null) {
				customzationEnabled = customization.getBoolean("Enabled");
				playerSlots.addAll(customization.getIntegerList("PlayerSlots"));
			}
			BInventory inv = new BInventory("Top Points");
			if (!plugin.getConfigFile().isAlwaysCloseInventory()) inv.dontClose();
			int pos = 1;
			for (Entry<TopVoterPlayer, Integer> entry : users) {
				ItemBuilder playerItem = plugin.getGui().isChestVoteTopUseSkull()
						? new ItemBuilder(entry.getKey().getPlayerHead())
						: new ItemBuilder(Material.valueOf(plugin.getGui().getChestVoteTopPlayerItemMaterial()));
				playerItem.setLore(new ArrayList<>());
				BInventoryButton button = new BInventoryButton(playerItem.setName(plugin.getGui().getChestVoteTopItemName())
						.addLoreLine(plugin.getGui().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
						.addPlaceholder("player", entry.getKey().getPlayerName()).addPlaceholder("votes", "" + entry.getValue())) {
					@Override public void onClick(ClickEvent clickEvent) {
						if (plugin.getGui().isChestVoteTopOpenMainGUIOnClick()) {
							TopVoterPlayer selected = (TopVoterPlayer) getData("User");
							new VoteGUI(plugin, player, selected.getUser()).open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
						}
					}
				}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey());
				if (customzationEnabled && !playerSlots.isEmpty()) button.setSlot(playerSlots.remove());
				inv.setCloseInv(plugin.getGui().isChestVoteTopCloseGUIOnClick());
				inv.addButton(button);
				pos++;
			}
			if (plugin.getGui().isChestVoteTopBackButton()) {
				if (customzationEnabled) inv.addButton(plugin.getCommandLoader().getBackButton(user).setSlot(customization.getInt("BackButtonSlot", 0)));
				else inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(1));
			}
			String guiPath = "VoteTop.Customization";
			for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
				inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {
					@Override public void onClick(ClickEvent clickEvent) {
						plugin.getCommandLoader().processSlotClick(player, user, str);
						new RewardBuilder(plugin.getGui().getData(), "CHEST." + guiPath + ".ExtraItems." + str + ".Rewards")
								.setGiveOffline(false).send(clickEvent.getPlayer());
					}
				});
			}
			if (customization == null || !customzationEnabled || !customization.getBoolean("RemoveBottomBar")) inv.setPages(true);
			inv.setMaxInvSize(plugin.getGui().getChestVoteTopSize());
			inv.openInventory(player);
		} catch (Exception failure) {
			plugin.getLogger().log(java.util.logging.Level.WARNING, "Failed to open top points", failure);
			player.sendMessage(MessageAPI.colorize("&cFailed to load top points"));
		}
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
