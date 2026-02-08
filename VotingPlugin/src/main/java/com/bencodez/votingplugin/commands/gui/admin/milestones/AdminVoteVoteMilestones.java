package com.bencodez.votingplugin.commands.gui.admin.milestones;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.valuerequest.InputMethod;
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.ValueRequestBuilder;
import com.bencodez.advancedcore.api.valuerequest.listeners.NumberListener;
import com.bencodez.advancedcore.api.valuerequest.listeners.StringListener;
import com.bencodez.advancedcore.command.gui.RewardEditGUI;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteVoteMilestones extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteVoteMilestones(VotingPluginMain plugin, CommandSender player) {
		super(plugin, player);
		this.plugin = plugin;
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
		BInventory inv = new BInventory("Edit VoteMilestones");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteMilestones");
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER)
				.setName("&cEdit existing vote milestone rewards (Legacy milestones not included)")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Set<String> keys = plugin.getVoteMilestonesManager().getConfig().getMilestones().keySet();
				for (String key : keys) {
					if (key.startsWith("Legacy_")) {
						keys.remove(key);
					}
				}
				new ValueRequestBuilder(new StringListener() {

					@Override
					public void onInput(Player player, String value) {
						RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
								plugin.getRewardHandler().getDirectlyDefined("VoteMilestones." + value + ".Rewards"));
					}
				}, ArrayUtils.convertSet(keys)).allowCustomOption(false).usingMethod(InputMethod.INVENTORY)
						.request(clickEvent.getPlayer());

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd milestone")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest().requestNumber(player, new NumberListener() {

					@Override
					public void onInput(Player player, Number value) {
						plugin.getSpecialRewardsConfig().setVoteMilestone(value.intValue());
						plugin.reload();
					}
				});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove milestone")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteMilestoneRemove(plugin, clickEvent.getPlayer()).open();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
