package com.bencodez.votingplugin.commands.gui;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUI;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueBoolean;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueList;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueString;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.valuerequest.InputMethod;
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.ValueRequestBuilder;
import com.bencodez.advancedcore.api.valuerequest.listeners.StringListener;
import com.bencodez.advancedcore.command.gui.RewardEditGUI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteBungeeSettings;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteVoteParty;
import com.bencodez.votingplugin.commands.gui.admin.cumulative.AdminVoteCumulative;
import com.bencodez.votingplugin.commands.gui.admin.milestones.AdminVoteMilestones;
import com.bencodez.votingplugin.commands.gui.admin.voteshop.AdminVoteVoteShop;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;

public class AdminGUI {

	private VotingPluginMain plugin;

	public AdminGUI(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Open admin GUI.
	 *
	 * @return ArrayList of buttons
	 */
	public ArrayList<BInventoryButton> adminGUIButtons() {
		ArrayList<BInventoryButton> buttons = new ArrayList<BInventoryButton>();
		buttons.add(new BInventoryButton("&cVoteSites",
				new String[] { "&cOnly enabled sites are listed in this section", "&cMiddle Click to create" },
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				Player player = event.getPlayer();
				if (event.getClick().equals(ClickType.MIDDLE)) {
					new ValueRequest().requestString(player, new StringListener() {

						@Override
						public void onInput(Player player, String value) {
							plugin.getConfigVoteSites().generateVoteSite(value);
							player.sendMessage("Generated site");
							plugin.reload();
							openAdminGUIVoteSiteSite(player, plugin.getVoteSite(value, true));
						}
					});
				} else {
					openAdminGUIVoteSites(player);
				}
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.CHEST, 1).setName("&cEdit VoteShop")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShop(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.DIAMOND_BLOCK, 1).setName("&cEdit Milestones")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteMilestones(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.COBBLESTONE, 1).setName("&cEdit Cumulative")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteCumulative(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit VoteParty")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteParty(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});
		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit FirstVote reward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("FirstVote"));
			}
		});
		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit FirstVoteToday reward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("FirstVoteToday"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit AllSites reward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("AllSites"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit AlmostAllSites reward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("AlmostAllSites"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit AnySiteRewards")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("AnySiteRewards"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit EverySiteReward")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("EverySiteReward"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder(Material.STONE, 1).setName("&cEdit BungeeVotePartyRewards")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("BungeeVotePartyRewards"));
			}
		});

		buttons.add(new BInventoryButton(new ItemBuilder("GRASS_BLOCK").setName("&cEdit BungeeSettings.yml")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteBungeeSettings(plugin, clickEvent.getPlayer()).open();
			}
		});

		buttons.add(new BInventoryButton("&cReload Plugin", new String[] {}, new ItemStack(Material.BUCKET, 1)) {

			@Override
			public void onClick(ClickEvent event) {
				plugin.reload();
			}

		});

		buttons.add(new BInventoryButton(
				new ItemBuilder(Material.STONE, 1).setName("&cBackground task time").addLoreLine("&aClick to see")) {

			@Override
			public void onClick(ClickEvent event) {
				event.getPlayer().sendMessage("Last background task time: " + plugin.getLastBackgroundTaskTimeTaken());
			}

		});

		return buttons;
	}

	public void loadHook() {
		for (BInventoryButton b : adminGUIButtons()) {
			com.bencodez.advancedcore.command.gui.AdminGUI.getInstance().addButton(b);
		}
	}

	/**
	 * Open admin GUI vote sites.
	 *
	 * @param player the player
	 */
	public void openAdminGUIVoteSites(Player player) {
		BInventory inv = new BInventory("VoteSites");
		int count = 0;
		for (VoteSite voteSite : plugin.getVoteSites()) {
			ArrayList<String> lore = new ArrayList<String>();
			lore.add("Priority: " + voteSite.getPriority());
			lore.add("Name: " + voteSite.getDisplayName());
			lore.add("ServiceSite: " + voteSite.getServiceSite());
			lore.add("VoteURL: " + voteSite.getVoteURL());
			lore.add("VoteDelay: " + voteSite.getVoteDelay());
			lore.add("VoteDelayMin: " + voteSite.getVoteDelayMin());

			inv.addButton(count, new BInventoryButton(voteSite.getKey(), ArrayUtils.getInstance().convert(lore),
					new ItemStack(Material.STONE)) {

				@Override
				public void onClick(ClickEvent event) {

					Player player = event.getWhoClicked();
					openAdminGUIVoteSiteSite(player, voteSite);

				}
			});
			count++;
		}
		inv.openInventory(player);
	}

	/**
	 * Open admin GUI vote site site.
	 *
	 * @param player   the player
	 * @param voteSite the vote site
	 */
	public void openAdminGUIVoteSiteSite(Player player, VoteSite voteSite) {
		EditGUI inv = new EditGUI("VoteSite: " + voteSite.getDisplayName());
		inv.setMeta(player, "VoteSite", voteSite);

		inv.addButton(new BInventoryButton(voteSite.getItem().setName("&cForce vote")) {

			@Override
			public void onClick(ClickEvent event) {
				ArrayList<String> playerNames = new ArrayList<String>();
				for (Player p : Bukkit.getOnlinePlayers()) {
					playerNames.add(p.getName());
				}
				new ValueRequestBuilder(new StringListener() {

					@Override
					public void onInput(Player player, String value) {
						Object ob = PlayerUtils.getInstance().getPlayerMeta(player, "VoteSite");
						if (ob != null) {
							VoteSite site = (VoteSite) ob;
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, value, site.getServiceSite(), false);
							plugin.getVoteTimer().submit(new Runnable() {

								@Override
								public void run() {
									plugin.getServer().getPluginManager().callEvent(voteEvent);
								}
							});
						}
					}
				}, ArrayUtils.getInstance().convert(playerNames)).usingMethod(InputMethod.INVENTORY)
						.allowCustomOption(true).request(event.getWhoClicked());
			}
		});

		inv.addButton(new EditGUIButton(new EditGUIValueNumber("Priority", voteSite.getPriority()) {

			@Override
			public void setValue(Player player, Number num) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				plugin.getConfigVoteSites().setPriority(voteSite.getKey(), num.intValue());
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("ServiceSite", voteSite.getServiceSite()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setServiceSite(siteName, value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("Name", voteSite.getDisplayName()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				plugin.getConfigVoteSites().setDisplayName(voteSite.getKey(), value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("VoteURL", voteSite.getVoteURL()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setVoteURL(siteName, value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueNumber("VoteDelay", voteSite.getVoteDelay()) {

			@Override
			public void setValue(Player player, Number num) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setVoteDelay(siteName, num.intValue());
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueNumber("VoteDelayMin", voteSite.getVoteDelay()) {

			@Override
			public void setValue(Player player, Number num) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setVoteDelay(siteName, num.intValue());
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("Enabled", voteSite.isEnabled()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setEnabled(siteName, value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("ForceOffline", voteSite.isGiveOffline()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setForceOffline(siteName, value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("VoteDelayDaily", voteSite.isVoteDelayDaily()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				plugin.getConfigVoteSites().setVoteDelayDaily(siteName, value);
				plugin.reload();
			}
		}));

		if (voteSite.getSiteData().isList("Rewards")) {

			inv.addButton(
					new EditGUIButton(new EditGUIValueList("Rewards", voteSite.getSiteData().getStringList("Rewards")) {

						@Override
						public void setValue(Player p, ArrayList<String> rewards) {
							VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
							plugin.getConfigVoteSites().set(voteSite.getKey(), "Rewards", rewards);
							plugin.reload();
						}
					}));
		} else {
			inv.addButton(new BInventoryButton(new ItemBuilder(Material.DISPENSER, 1).setName("&cRewards")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(), plugin.getRewardHandler()
							.getDirectlyDefined("VoteSites." + voteSite.getKey() + ".Rewards"));
				}
			});
		}

		if (voteSite.getSiteData().isList("CoolDownEndRewards")) {

			inv.addButton(new EditGUIButton(new EditGUIValueList("CoolDownEndRewards",
					voteSite.getSiteData().getStringList("CoolDownEndRewards")) {

				@Override
				public void setValue(Player p, ArrayList<String> rewards) {
					VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
					plugin.getConfigVoteSites().set(voteSite.getKey(), "CoolDownEndRewards", rewards);
					plugin.reload();
				}
			}));
		} else {
			inv.addButton(new BInventoryButton(new ItemBuilder(Material.DISPENSER, 1).setName("&cCoolDownEndRewards")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(), plugin.getRewardHandler()
							.getDirectlyDefined("VoteSites." + voteSite.getKey() + ".CoolDownEndRewards"));
				}
			});
		}

		inv.openInventory(player);
	}
}
