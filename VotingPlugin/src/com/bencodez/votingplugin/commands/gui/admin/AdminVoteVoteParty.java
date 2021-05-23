package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUI;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueBoolean;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueList;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueString;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.valuerequest.ValueRequestBuilder;
import com.bencodez.advancedcore.api.valuerequest.listeners.NumberListener;
import com.bencodez.advancedcore.command.gui.RewardEditGUI;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteVoteParty extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteVoteParty(VotingPluginMain plugin, CommandSender player) {
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
		EditGUI inv = new EditGUI("Edit VoteParty");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteParty");

		// to add
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueBoolean("Enabled", plugin.getSpecialRewardsConfig().getVotePartyEnabled()) {

					@Override
					public void setValue(Player player, boolean name) {
						setPathData(getKey(), name);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.EMERALD, 1),
				new EditGUIValueNumber("VotesRequired", plugin.getSpecialRewardsConfig().getVotePartyVotesRequired()) {

					@Override
					public void setValue(Player player, Number num) {
						setPathData(getKey(), num.intValue());
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.LADDER, 1), new EditGUIValueNumber(
				"IncreaseVotesRquired", plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRquired()) {

			@Override
			public void setValue(Player player, Number num) {
				setPathData(getKey(), num.intValue());
			}
		}));

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.LADDER).setName("&cCurrent increase amount")
				.addLoreLine("&cCurrent value: " + plugin.getServerData().getVotePartyExtraRequired())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequestBuilder(new NumberListener() {

					@Override
					public void onInput(Player player, Number value) {
						plugin.getServerData().setVotePartyExtraRequired(value.intValue());
					}
				}, new Number[] { 0, 10, 50, 100 })
						.currentValue("" + plugin.getServerData().getVotePartyExtraRequired()).allowCustomOption(true)
						.request(clickEvent.getPlayer());
				;
			}
		});

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.EMERALD, 1), new EditGUIValueNumber(
				"UserVotesRequired", plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired()) {

			@Override
			public void setValue(Player player, Number num) {
				setPathData(getKey(), num.intValue());
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean("GiveAllPlayers",
				plugin.getSpecialRewardsConfig().getVotePartyGiveAllPlayers()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean(
				"GiveOnlinePlayersOnly", plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueBoolean("ResetEachDay", plugin.getSpecialRewardsConfig().getVotePartyResetEachDay()) {

					@Override
					public void setValue(Player player, boolean name) {
						setPathData(getKey(), name);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueBoolean("ResetWeekly", plugin.getSpecialRewardsConfig().getVotePartyResetWeekly()) {

					@Override
					public void setValue(Player player, boolean name) {
						setPathData(getKey(), name);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueBoolean("ResetMonthly", plugin.getSpecialRewardsConfig().getVotePartyResetMontly()) {

					@Override
					public void setValue(Player player, boolean name) {
						setPathData(getKey(), name);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean("OnlyOncePerDay",
				plugin.getSpecialRewardsConfig().getVotePartyOnlyOncePerDay()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean(
				"ResetExtraVotesMonthly", plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean("CountFakeVotes",
				plugin.getSpecialRewardsConfig().getVotePartyCountFakeVotes()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueBoolean("CountOfflineVotes",
				plugin.getSpecialRewardsConfig().getVotePartyCountOfflineVotes()) {

			@Override
			public void setValue(Player player, boolean name) {
				setPathData(getKey(), name);
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString("Broadcast", plugin.getSpecialRewardsConfig().getVotePartyBroadcast()) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueList("Commands", plugin.getSpecialRewardsConfig().getVotePartyCommands()) {

					@Override
					public void setValue(Player player, ArrayList<String> value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DISPENSER, 1).setName("&cRewards")
				.addLoreLine("&aUse this for per player rewards, also set online only rewards here as well")
				.addLoreLine("&cTo set rewards to be given to players online only, set RewardType to ONLINE")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						RewardHandler.getInstance().getDirectlyDefined("VoteParty.Rewards"));
			}
		});

		// implement item reward?

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	private void setPathData(String path, Object value) {
		plugin.getSpecialRewardsConfig().getData().set("VoteParty." + path, value);
		plugin.getSpecialRewardsConfig().saveData();
		plugin.reload();
	}

}
