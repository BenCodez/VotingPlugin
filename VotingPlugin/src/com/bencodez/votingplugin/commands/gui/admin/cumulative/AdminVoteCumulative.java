package com.bencodez.votingplugin.commands.gui.admin.cumulative;

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

public class AdminVoteCumulative extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteCumulative(VotingPluginMain plugin, CommandSender player) {
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
		BInventory inv = new BInventory("Edit Cumulative");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.Cumulative");
		inv.addButton(
				new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&cEdit existing cumulative rewards")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						ArrayList<Integer> nums = new ArrayList<Integer>();
						for (String num : plugin.getSpecialRewardsConfig().getCumulativeVotes()) {
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
												.getDirectlyDefined("Cumulative." + value.intValue() + ".Rewards"));
							}
						}, options).allowCustomOption(false).usingMethod(InputMethod.INVENTORY)
								.request(clickEvent.getPlayer());

					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd cumulative reward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest().requestNumber(player, new NumberListener() {

					@Override
					public void onInput(Player player, Number value) {
						plugin.getSpecialRewardsConfig().setCumulative(value.intValue());
						plugin.reload();
					}
				});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove cumulative")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteCumulativeRemove(plugin, clickEvent.getPlayer()).open();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
