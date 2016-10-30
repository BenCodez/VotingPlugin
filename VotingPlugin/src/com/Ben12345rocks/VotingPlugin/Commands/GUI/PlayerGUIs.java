package com.Ben12345rocks.VotingPlugin.Commands.GUI;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class PlayerGUIs {
	static PlayerGUIs instance = new PlayerGUIs();

	/**
	 * Gets the single instance of Commands.
	 *
	 * @return single instance of Commands
	 */
	public static PlayerGUIs getInstance() {
		return instance;
	}

	/** The plugin. */
	Main plugin = Main.plugin;

	/**
	 * Instantiates a new commands.
	 */
	private PlayerGUIs() {
	}

	public User getSelectedPlayer(Player player) {
		User str = (User) Utils.getInstance().getPlayerMeta(player,
				"SelectedPlayerGUIs");
		return str;
	}

	public void setSelectedPlayer(Player player, User user) {
		Utils.getInstance().setPlayerMeta(player, "SelectedPlayerGUIs", user);
	}

	/**
	 * Open vote GUI.
	 *
	 * @param player
	 *            the player
	 */
	public void openVoteGUI(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteGUI: " + user.getPlayerName());

		for (String slot : ConfigGUI.getInstance().getVoteGUISlots()) {
			ItemStack item = new ItemStack(Material.STONE);

			try {
				item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteGUISlotMaterial(slot)), ConfigGUI
						.getInstance().getVoteGUISlotAmount(slot),
						(short) ConfigGUI.getInstance()
								.getVoteGUISlotData(slot));
			} catch (Exception ex) {

			}

			item = Utils.getInstance().setSkullOwner(item,
					ConfigGUI.getInstance().getVoteGUISlotSkull(player, slot));

			item = Utils.getInstance().setDurabilty(item,
					ConfigGUI.getInstance().getVoteGUISlotDurability(slot));

			String[] lore = new String[1];

			lore = Utils.getInstance().convertArray(
					ConfigGUI.getInstance().getVoteGUISlotLore(slot));
			if (slot.equalsIgnoreCase("url")) {
				lore = Commands.getInstance().voteURLs();
			} else if (slot.equalsIgnoreCase("next")) {
				lore = Commands.getInstance().voteCommandNext(user);
			} else if (slot.equalsIgnoreCase("last")) {
				lore = Commands.getInstance().voteCommandLast(user);
			} else if (slot.equalsIgnoreCase("total")) {
				lore = Commands.getInstance().voteCommandTotal(user);
			} else if (slot.equalsIgnoreCase("top")) {
				lore = TopVoter.getInstance().topVoterMonthly(1);
			} else if (slot.equalsIgnoreCase("today")) {
				lore = Commands.getInstance().voteToday();
			} else if (slot.equalsIgnoreCase("help")) {
				ArrayList<String> loreSt = new ArrayList<String>();
				loreSt = Utils.getInstance().comptoString(
						Commands.getInstance().voteHelpText(player));
				lore = Utils.getInstance().convertArray(loreSt);
			}

			inv.addButton(ConfigGUI.getInstance().getVoteGUISlotSlot(slot),
					new BInventoryButton(ConfigGUI.getInstance()
							.getVoteGUISlotName(slot), lore, item) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();
							String cmd = ConfigGUI.getInstance()
									.getVoteGUISlotCommand(slot);
							User user = getSelectedPlayer(player);
							if (!cmd.equals("")) {
								player.performCommand(cmd);
							} else {
								if (slot.equalsIgnoreCase("url")) {
									openVoteURL(player);
								} else if (slot.equalsIgnoreCase("next")) {
									openVoteNext(player, user);
								} else if (slot.equalsIgnoreCase("last")) {
									openVoteLast(player, user);
								} else if (slot.equalsIgnoreCase("total")) {
									openVoteTotal(player, user);
								} else if (slot.equalsIgnoreCase("top")) {
									openVoteTopMonthly(player);
								} else if (slot.equalsIgnoreCase("today")) {
									openVoteToday(player);
								} else if (slot.equalsIgnoreCase("help")) {
									player.performCommand("v help");
								}
							}

						}
					});
		}

		BInventory.openInventory(player, inv);
	}

	public void openVoteToday(Player player) {
		BInventory inv = new BInventory("VoteToday");
		for (User user : plugin.voteToday.keySet()) {

			for (VoteSite voteSite : plugin.voteToday.get(user).keySet()) {
				String timeString = new SimpleDateFormat(ConfigFormat
						.getInstance().getTimeFormat()).format(plugin.voteToday
						.get(user).get(voteSite));
				String msg = "&6" + user.getPlayerName() + " : "
						+ voteSite.getSiteName() + " : " + timeString;
				inv.addButton(
						inv.getNextSlot(),
						new BInventoryButton(user.getPlayerName(),
								new String[] { msg }, Utils.getInstance()
										.setSkullOwner(
												new ItemStack(
														Material.SKULL_ITEM),
												user.getPlayerName())) {

							@Override
							public void onClick(ClickEvent clickEvent) {
								User user = UserManager.getInstance()
										.getVotingPluginUser(
												clickEvent.getClickedItem()
														.getItemMeta()
														.getDisplayName());
								openVoteGUI(player, user);

							}
						});
			}
		}
		inv.openInventory(player);
	}

	public void openVoteTopMonthly(Player player) {
		BInventory inv = new BInventory("VoteTop Monthly");
		int pos = 0;
		for (Entry<User, Integer> entry : plugin.topVoterMonthly.entrySet()) {
			pos++;
			inv.addButton(
					inv.getNextSlot(),
					new BInventoryButton(pos + ": "
							+ entry.getKey().getPlayerName(),
							new String[] { "Votes: " + entry.getValue() },
							Utils.getInstance().setSkullOwner(
									new ItemStack(Material.SKULL_ITEM),
									entry.getKey().getPlayerName())) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							User user = UserManager.getInstance()
									.getVotingPluginUser(
											clickEvent.getClickedItem()
													.getItemMeta()
													.getDisplayName());
							openVoteGUI(player, user);
						}
					});
		}
		inv.openInventory(player);

	}

	/**
	 * Vote URL.
	 *
	 * @param player
	 *            the player
	 */
	public void openVoteURL(Player player) {
		BInventory inv = new BInventory("Vote Links");

		User user = UserManager.getInstance().getVotingPluginUser(player);

		int count = 0;
		if (ConfigGUI.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemStack itemAll = new ItemStack(Material.getMaterial(ConfigGUI
					.getInstance().getVoteURLAlreadyVotedItemMaterial()),
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemAmount(),
					(short) ConfigGUI.getInstance()
							.getVoteURLAlreadyVotedItemData());
			itemAll = Utils.getInstance().setDurabilty(
					itemAll,
					ConfigGUI.getInstance()
							.getVoteURLAlreadyVotedItemDurability());
			itemAll = Utils.getInstance().setSkullOwner(
					itemAll,
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemSkull(
							player));
			if (user.canVoteAll()) {
				itemAll = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteURLCanVoteItemMaterial()),
						ConfigGUI.getInstance().getVoteURLCanVoteItemAmount(),
						(short) ConfigGUI.getInstance()
								.getVoteURLCanVoteItemData());
				itemAll = Utils.getInstance().setDurabilty(
						itemAll,
						ConfigGUI.getInstance()
								.getVoteURLCanVoteItemDurability());
				itemAll = Utils.getInstance().setSkullOwner(
						itemAll,
						ConfigGUI.getInstance().getVoteURLCanVoteItemSkull(
								player));
			}

			inv.addButton(count, new BInventoryButton("&4All Voting Sites",
					new String[] { "&cClick Me" }, itemAll) {

				@Override
				public void onClick(ClickEvent event) {
					User user = UserManager.getInstance().getVotingPluginUser(
							event.getPlayer());
					Player player = event.getWhoClicked();
					player.closeInventory();
					user.sendMessage(Commands.getInstance().voteURLs());

				}
			});

			count++;
		}

		for (VoteSite voteSite : plugin.voteSites) {
			ItemStack item = new ItemStack(Material.getMaterial(ConfigGUI
					.getInstance().getVoteURLAlreadyVotedItemMaterial()),
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemAmount(),
					(short) ConfigGUI.getInstance()
							.getVoteURLAlreadyVotedItemData());
			item = Utils.getInstance().setDurabilty(
					item,
					ConfigGUI.getInstance()
							.getVoteURLAlreadyVotedItemDurability());
			item = Utils.getInstance().setSkullOwner(
					item,
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemSkull(
							player));
			ArrayList<String> lore = new ArrayList<String>();
			lore.add(ConfigGUI.getInstance().getVoteURLSeeURL());

			if (user.canVoteSite(voteSite)) {
				item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteURLCanVoteItemMaterial()),
						ConfigGUI.getInstance().getVoteURLCanVoteItemAmount(),
						(short) ConfigGUI.getInstance()
								.getVoteURLCanVoteItemData());
				item = Utils.getInstance().setDurabilty(
						item,
						ConfigGUI.getInstance()
								.getVoteURLCanVoteItemDurability());
				item = Utils.getInstance().setSkullOwner(
						item,
						ConfigGUI.getInstance().getVoteURLCanVoteItemSkull(
								player));
			} else {
				lore.add(ConfigGUI
						.getInstance()
						.getVoteURLNextVote()
						.replace(
								"%Info%",
								Commands.getInstance().voteCommandNextInfo(
										user, voteSite)));
			}

			inv.addButton(
					count,
					new BInventoryButton(ConfigGUI.getInstance()
							.getVoteURLSiteName()
							.replace("%Name%", voteSite.getSiteName()), Utils
							.getInstance().convertArray(lore), item) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();
							if (player != null) {
								player.closeInventory();
								User user = UserManager.getInstance()
										.getVotingPluginUser(player);
								user.sendMessage(voteSite.getVoteURL());

							}

						}
					});
			count++;
		}

		BInventory.openInventory(player, inv);
	}

	public void openVoteLast(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteLast: " + user.getPlayerName());
		for (VoteSite site : plugin.voteSites) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getSiteName(),
							new String[] { Commands.getInstance()
									.voteCommandLastLine(user, site) },
							new ItemStack(Material.STONE)) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							Player player = clickEvent.getPlayer();
							openVoteLast(player, getSelectedPlayer(player));
						}
					});
		}
		inv.openInventory(player);
	}

	public void openVoteNext(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteNext: " + user.getPlayerName());
		for (VoteSite site : plugin.voteSites) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getSiteName(),
							new String[] { Commands.getInstance()
									.voteCommandNextInfo(user, site) },
							new ItemStack(Material.STONE)) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							Player player = clickEvent.getPlayer();
							openVoteNext(player, getSelectedPlayer(player));
						}
					});
		}
		inv.openInventory(player);
	}

	public void openVoteTotal(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteTotal: " + user.getPlayerName());
		for (VoteSite site : plugin.voteSites) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getSiteName(),
							new String[] { Commands.getInstance()
									.voteCommandTotalLine(user, site) },
							new ItemStack(Material.STONE)) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							Player player = clickEvent.getPlayer();
							openVoteTotal(player, getSelectedPlayer(player));
						}
					});
		}
		inv.openInventory(player);
	}

	/**
	 * Vote reward.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 */
	public void voteReward(Player player, String siteName) {
		BInventory inv = new BInventory("VoteReward");

		if ((siteName == null) || (siteName == "")) {
			int count = 0;
			for (VoteSite voteSite : plugin.voteSites) {
				plugin.debug(voteSite.getSiteName());

				try {
					ItemStack item = new ItemStack(
							Material.getMaterial(ConfigGUI.getInstance()
									.getVoteSiteItemMaterial(
											voteSite.getSiteName())),
							ConfigGUI.getInstance().getVoteSiteItemAmount(
									voteSite.getSiteName()),
							(short) ConfigGUI
									.getInstance()
									.getVoteSiteItemData(voteSite.getSiteName()));
					item = Utils.getInstance().setDurabilty(
							item,
							ConfigGUI.getInstance().getVoteSiteItemDurability(
									voteSite.getSiteName()));

					item = Utils.getInstance().setSkullOwner(
							item,
							ConfigGUI.getInstance().getVoteSiteItemSkull(
									player, voteSite.getSiteName()));

					inv.addButton(
							count,
							new BInventoryButton(
									ConfigGUI.getInstance()
											.getVoteSiteItemName(
													voteSite.getSiteName()),
									Utils.getInstance()
											.convertArray(
													(ArrayList<String>) ConfigGUI
															.getInstance()
															.getVoteSiteItemLore(
																	voteSite.getSiteName())),
									item) {

								@Override
								public void onClick(ClickEvent event) {
									Player player = event.getWhoClicked();
									if (player != null) {
										player.closeInventory();
										player.performCommand("vote reward "
												+ voteSite.getSiteName());

									}

								}
							});
					count++;
				} catch (Exception ex) {

				}
			}
		} else {
			for (String itemName : ConfigGUI.getInstance().getVoteSiteItems(
					siteName)) {
				ItemStack item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteSiteItemsMaterial(siteName,
								itemName)), ConfigGUI.getInstance()
						.getVoteSiteItemsAmount(siteName, itemName),
						(short) ConfigGUI.getInstance().getVoteSiteItemsData(
								siteName, itemName));

				item = Utils.getInstance().setDurabilty(
						item,
						ConfigGUI.getInstance().getVoteSiteItemsDurability(
								siteName, itemName));

				item = Utils.getInstance().setSkullOwner(
						item,
						ConfigGUI.getInstance().getVoteSiteItemsSkull(player,
								siteName, itemName));

				inv.addButton(
						ConfigGUI.getInstance().getVoteSiteItemsSlot(siteName,
								itemName),
						new BInventoryButton(ConfigGUI.getInstance()
								.getVoteSiteItemsName(siteName, itemName),
								Utils.getInstance().convertArray(
										(ArrayList<String>) ConfigGUI
												.getInstance()
												.getVoteSiteItemsLore(siteName,
														itemName)), item) {

							@Override
							public void onClick(ClickEvent event) {
								Player player = event.getWhoClicked();
								if (player != null) {
									player.closeInventory();
								}

							}
						});
			}
		}

		BInventory.openInventory(player, inv);
	}
}
