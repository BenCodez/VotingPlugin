package com.bencodez.votingplugin.commands.gui.admin.milestones;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.valuerequest.InputMethod;
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.ValueRequestBuilder;
import com.bencodez.advancedcore.api.valuerequest.listeners.NumberListener;
import com.bencodez.advancedcore.command.gui.RewardEditGUI;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteMilestones extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteMilestones(VotingPluginMain plugin, CommandSender player) {
		super(player);
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
		BInventory inv = new BInventory("Edit MileStones");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.MileStones");
		inv.addButton(
				new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&cEdit existing milestone rewards")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						ArrayList<Integer> nums = new ArrayList<Integer>();
						for (String num : plugin.getSpecialRewardsConfig().getMilestoneVotes()) {
							if (StringParser.getInstance().isInt(num)) {
								nums.add(Integer.parseInt(num));
							}
						}
						Number[] options = new Number[nums.size()];
						for (int i = 0; i < nums.size(); i++) {
							options[i] = nums.get(i);
						}
						new ValueRequestBuilder(new NumberListener() {

							@Override
							public void onInput(Player player, Number value) {
								RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
										RewardHandler.getInstance()
												.getDirectlyDefined("MileStones." + value.intValue() + ".Rewards"));
							}
						}, options).allowCustomOption(false).usingMethod(InputMethod.INVENTORY)
								.request(clickEvent.getPlayer());
						;
					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd milestone")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest().requestNumber(player, new NumberListener() {

					@Override
					public void onInput(Player player, Number value) {
						plugin.getSpecialRewardsConfig().setMilestone(value.intValue());
						plugin.reload();
					}
				});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove milestone")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteMilestoneRemove(plugin, clickEvent.getPlayer()).open();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
