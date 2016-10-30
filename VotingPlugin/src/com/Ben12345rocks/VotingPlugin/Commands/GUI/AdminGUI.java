package com.Ben12345rocks.VotingPlugin.Commands.GUI;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Commands.GUI.UserGUI;
import com.Ben12345rocks.AdvancedCore.Objects.Reward;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.ValueRequest;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.BooleanListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.NumberListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.StringListener;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class AdminGUI {
	static AdminGUI instance = new AdminGUI();

	/**
	 * Gets the single instance of Commands.
	 *
	 * @return single instance of Commands
	 */
	public static AdminGUI getInstance() {
		return instance;
	}

	/** The plugin. */
	Main plugin = Main.plugin;

	/**
	 * Instantiates a new commands.
	 */
	private AdminGUI() {
	}

	/**
	 * Open admin GUI.
	 *
	 * @param player
	 *            the player
	 */
	public void openAdminGUI(Player player) {
		BInventory inv = new BInventory("AdminGUI");
		ArrayList<String> lore = new ArrayList<String>();
		lore.add("&cOnly enabled sites are listed in this section");
		lore.add("&cMiddle Click to create");
		inv.addButton(inv.getNextSlot(), new BInventoryButton("&cVoteSites",
				Utils.getInstance().convertArray(lore), new ItemStack(
						Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					if (event.getClick().equals(ClickType.MIDDLE)) {
						player.closeInventory();
						new ValueRequest().requestString(player,
								new StringListener() {

									@Override
									public void onInput(Player player,
											String value) {
										ConfigVoteSites.getInstance()
												.generateVoteSite(value);
										player.sendMessage("Generated site");
										plugin.reload();
									}
								});
					} else {
						openAdminGUIVoteSites(player);
					}
				}
			}
		});

		lore = new ArrayList<String>();
		inv.addButton(inv.getNextSlot(), new BInventoryButton("&cConfig", Utils
				.getInstance().convertArray(lore),
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					openAdminGUIConfig(player);
				}
			}
		});

		lore = new ArrayList<String>();
		inv.addButton(inv.getNextSlot(), new BInventoryButton("&cPlayers",
				Utils.getInstance().convertArray(lore), new ItemStack(
						Material.SKULL_ITEM, 1, (short) 3)) {

			@Override
			public void onClick(ClickEvent event) {

				Player player = event.getWhoClicked();

				UserGUI.getInstance().openUsersGUI(player);

			}

		});
		lore = new ArrayList<String>();
		inv.addButton(inv.getNextSlot(), new BInventoryButton(
				"&cReload Plugin", Utils.getInstance().convertArray(lore),
				new ItemStack(Material.STONE, 1, (short) 3)) {

			@Override
			public void onClick(ClickEvent event) {
				event.getPlayer().performCommand("av reload");
			}

		});

		inv.openInventory(player);
	}

	/**
	 * Open admin GUI config.
	 *
	 * @param player
	 *            the player
	 */
	public void openAdminGUIConfig(Player player) {
		BInventory inv = new BInventory("Config");
		inv.addButton(inv.getNextSlot(), new BInventoryButton("BroadcastVote",
				new String[] { "Currently: "
						+ Config.getInstance().getBroadCastVotesEnabled() },
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				Player player = event.getWhoClicked();
				new ValueRequest().requestBoolean(player, ""
						+ Config.getInstance().getBroadCastVotesEnabled(),
						new BooleanListener() {

							@Override
							public void onInput(Player player, boolean value) {
								Config.getInstance().setBroadcastVoteEnabled(
										value);
								player.sendMessage("Value set");

							}
						});
			}
		});

		inv.openInventory(player);

	}

	/**
	 * Open admin GUI vote sites.
	 *
	 * @param player
	 *            the player
	 */
	public void openAdminGUIVoteSites(Player player) {
		BInventory inv = new BInventory("VoteSites");
		int count = 0;
		for (VoteSite voteSite : plugin.voteSites) {
			ArrayList<String> lore = new ArrayList<String>();
			lore.add("Priority: " + voteSite.getPriority());
			lore.add("ServiceSite: " + voteSite.getServiceSite());
			lore.add("VoteURL: " + voteSite.getVoteURL());
			lore.add("VoteDelay: " + voteSite.getVoteDelay());
			lore.add("Rewards: "
					+ Utils.getInstance().makeStringList(voteSite.getRewards()));
			lore.add("CumulativeVotes: " + voteSite.getCumulativeVotes());
			lore.add("CumulativeRewards: "
					+ Utils.getInstance().makeStringList(
							voteSite.getCumulativeRewards()));

			inv.addButton(count, new BInventoryButton(voteSite.getSiteName(),
					Utils.getInstance().convertArray(lore), new ItemStack(
							Material.STONE)) {

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
	 * @param player
	 *            the player
	 * @param voteSite
	 *            the vote site
	 */
	public void openAdminGUIVoteSiteSite(Player player, VoteSite voteSite) {
		BInventory inv = new BInventory("VoteSite: " + voteSite.getSiteName());
		inv.setMeta(player, "VoteSite", voteSite);
		inv.addButton(0, new BInventoryButton("SetPriority", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				Player player = event.getWhoClicked();
				new ValueRequest().requestNumber(player,
						"" + voteSite.getPriority(), null,
						new NumberListener() {

							@Override
							public void onInput(Player player, Number value) {
								VoteSite voteSite = (VoteSite) event
										.getMeta("VoteSite");
								ConfigVoteSites.getInstance().setPriority(
										voteSite.getSiteName(),
										value.intValue());
								player.sendMessage("Set Priority");
								plugin.reload();

							}
						});
			}
		});

		inv.addButton(1, new BInventoryButton("SetServiceSite", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					player.closeInventory();
					new ValueRequest().requestString(player,
							voteSite.getServiceSite(), null,
							new StringListener() {

								@Override
								public void onInput(Player player, String value) {
									VoteSite voteSite = (VoteSite) event
											.getMeta("VoteSite");
									String siteName = voteSite.getSiteName();
									ConfigVoteSites.getInstance()
											.setServiceSite(siteName, value);
									player.sendMessage("Set ServiceSite");
									plugin.reload();
								}
							});
				}

			}
		});

		inv.addButton(2, new BInventoryButton("SetVoteURL", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					player.closeInventory();
					new ValueRequest().requestString(player,
							voteSite.getVoteURL(), null, new StringListener() {

								@Override
								public void onInput(Player player, String value) {
									VoteSite voteSite = (VoteSite) event
											.getMeta("VoteSite");
									String siteName = voteSite.getSiteName();
									ConfigVoteSites.getInstance().setVoteURL(
											siteName, value);
									player.sendMessage("Set VoteURL");
									plugin.reload();

								}
							});

				}

			}
		});

		inv.addButton(3, new BInventoryButton("SetVoteDelay", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				Player player = event.getWhoClicked();
				new ValueRequest().requestNumber(player,
						"" + voteSite.getVoteDelay(), null,
						new NumberListener() {

							@Override
							public void onInput(Player player, Number value) {
								VoteSite voteSite = (VoteSite) event
										.getMeta("VoteSite");
								String siteName = voteSite.getSiteName();
								ConfigVoteSites.getInstance().setVoteDelay(
										siteName, value.intValue());
								player.sendMessage("Set VoteDelay");
								plugin.reload();

							}
						});

			}
		});
		inv.addButton(4, new BInventoryButton("SetEnabled", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {

				Player player = event.getWhoClicked();
				new ValueRequest().requestBoolean(
						player,
						""
								+ ConfigVoteSites.getInstance()
										.getVoteSiteEnabled(
												voteSite.getSiteName()),
						new BooleanListener() {

							@Override
							public void onInput(Player player, boolean value) {
								VoteSite voteSite = (VoteSite) event
										.getMeta("VoteSite");
								String siteName = voteSite.getSiteName();
								ConfigVoteSites.getInstance().setEnabled(
										siteName, value);
								player.sendMessage("Set Enabled");
								plugin.reload();

							}
						});
			}
		});

		inv.addButton(5, new BInventoryButton("Add Reward", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					VoteSite voteSite = (VoteSite) event.getMeta("VoteSite");
					String siteName = voteSite.getSiteName();
					BInventory inv = new BInventory("AddReward: " + siteName);
					int count = 0;
					for (Reward reward : RewardHandler.getInstance()
							.getRewards()) {
						inv.addButton(count,
								new BInventoryButton(reward.getRewardName(),
										new String[0], new ItemStack(
												Material.STONE)) {

									@Override
									public void onClick(ClickEvent event) {

										Player player = event.getWhoClicked();
										player.closeInventory();
										VoteSite voteSite = (VoteSite) event
												.getMeta("VoteSite");
										String siteName = voteSite
												.getSiteName();
										ArrayList<String> rewards = ConfigVoteSites
												.getInstance().getRewards(
														siteName);
										rewards.add(event.getCurrentItem()
												.getItemMeta().getDisplayName());
										ConfigVoteSites.getInstance()
												.setRewards(siteName, rewards);
										player.sendMessage("Reward added");
										plugin.reload();

									}
								});
						count++;
					}

					inv.openInventory(player);

				}

			}
		});

		inv.addButton(6, new BInventoryButton("Remove Reward", new String[0],
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				if (event.getWhoClicked() instanceof Player) {
					Player player = event.getWhoClicked();
					VoteSite voteSite = (VoteSite) event.getMeta("VoteSite");
					String siteName = voteSite.getSiteName();
					BInventory inv = new BInventory("RemoveReward: " + siteName);
					int count = 0;
					for (String rewardName : voteSite.getRewards()) {
						Reward reward = RewardHandler.getInstance().getReward(
								rewardName);
						inv.addButton(count,
								new BInventoryButton(reward.getRewardName(),
										new String[0], new ItemStack(
												Material.STONE)) {

									@Override
									public void onClick(ClickEvent event) {

										Player player = event.getWhoClicked();
										player.closeInventory();
										VoteSite voteSite = (VoteSite) event
												.getMeta("VoteSite");
										String siteName = voteSite
												.getSiteName();
										ArrayList<String> rewards = ConfigVoteSites
												.getInstance().getRewards(
														siteName);
										rewards.remove(event.getCurrentItem()
												.getItemMeta().getDisplayName());
										ConfigVoteSites.getInstance()
												.setRewards(siteName, rewards);
										player.sendMessage("Reward removed");
										plugin.reload();

									}
								});
						count++;
					}

					inv.openInventory(player);

				}

			}
		});

		inv.openInventory(player);
	}
}
