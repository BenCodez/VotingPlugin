package com.Ben12345rocks.VotingPlugin.Commands.GUI;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;
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
		User str = (User) PlayerUtils.getInstance().getPlayerMeta(player, "SelectedPlayerGUIs");
		return str;
	}

	public void openVoteGUI(Player player, User user) {
		if (!player.getName().equals(user.getPlayerName())
				&& !player.hasPermission("VotingPlugin.Commands.Vote.GUI.Other")
				&& !player.hasPermission("VotingPlugin.Mod")) {
			player.sendMessage(AdvancedCoreHook.getInstance().getFormatNoPerms());
			return;
		}
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteGUI: " + user.getPlayerName());

		for (String slot : Config.getInstance().getVoteGUISlots()) {
			ItemBuilder builder = new ItemBuilder(Config.getInstance().getVoteGUISlotSection(slot));

			String[] lore = new String[1];

			lore = ArrayUtils.getInstance().convert(Config.getInstance().getVoteGUISlotLore(slot));
			if (lore.length == 0) {
				if (slot.equalsIgnoreCase("url")) {
					lore = Commands.getInstance().voteURLs();
				} else if (slot.equalsIgnoreCase("next")) {
					lore = Commands.getInstance().voteCommandNext(user);
				} else if (slot.equalsIgnoreCase("last")) {
					lore = Commands.getInstance().voteCommandLast(user);
				} else if (slot.equalsIgnoreCase("total")) {
					lore = Commands.getInstance().voteCommandTotal(user);
				} else if (slot.equalsIgnoreCase("top")) {
					String str = Config.getInstance().getVoteTopDefault();
					if (str.equalsIgnoreCase("monthly")) {
						lore = TopVoterHandler.getInstance().topVoterMonthly(1);
					} else if (str.equalsIgnoreCase("weekly")) {
						lore = TopVoterHandler.getInstance().topVoterWeekly(1);
					} else if (str.equalsIgnoreCase("daily")) {
						lore = TopVoterHandler.getInstance().topVoterDaily(1);
					} else {
						lore = TopVoterHandler.getInstance().topVoterAllTime(1);
					}
				} else if (slot.equalsIgnoreCase("today")) {
					lore = Commands.getInstance().voteToday();
				} else if (slot.equalsIgnoreCase("help")) {
					ArrayList<String> loreSt = new ArrayList<String>();
					loreSt = ArrayUtils.getInstance().comptoString(Commands.getInstance().voteHelpText(player));
					lore = ArrayUtils.getInstance().convert(loreSt);
				}
			}

			builder.setLore(lore);

			inv.addButton(Config.getInstance().getVoteGUISlotSlot(slot), new BInventoryButton(builder) {

				@Override
				public void onClick(ClickEvent event) {
					Player player = event.getWhoClicked();
					String cmd = Config.getInstance().getVoteGUISlotCommand(slot);
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
							openVoteTop(player);
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

	public void openVoteLast(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteLast: " + user.getPlayerName());
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getDisplayName(),
							new String[] { Commands.getInstance().voteCommandLastLine(user, site) },
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
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getDisplayName(),
							new String[] { Commands.getInstance().voteCommandNextInfo(user, site) },
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

	public void openVoteToday(Player player) {
		BInventory inv = new BInventory("VoteToday");
		for (User user : plugin.voteToday.keySet()) {

			for (VoteSite voteSite : plugin.voteToday.get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.voteToday.get(user).get(voteSite).format(formatter);
				String msg = "&6" + user.getPlayerName() + " : " + voteSite.getDisplayName() + " : " + timeString;
				inv.addButton(inv.getNextSlot(), new BInventoryButton(user.getPlayerName(), new String[] { msg },
						MiscUtils.getInstance().setSkullOwner(

								user.getPlayerName())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						User user = UserManager.getInstance()
								.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
						openVoteGUI(player, user);

					}
				});
			}
		}
		inv.openInventory(player);
	}

	public void openVoteTop(Player player) {
		String str = Config.getInstance().getVoteTopDefault();
		BInventory inv = null;
		Set<Entry<User, Integer>> users = null;
		if (str.equalsIgnoreCase("monthly")) {
			inv = new BInventory("VoteTop Monthly");
			users = plugin.topVoterMonthly.entrySet();
		} else if (str.equalsIgnoreCase("weekly")) {
			inv = new BInventory("VoteTop Weekly");
			users = plugin.topVoterWeekly.entrySet();
		} else if (str.equalsIgnoreCase("daily")) {
			inv = new BInventory("VoteTop Daily");
			users = plugin.topVoterDaily.entrySet();
		} else {
			inv = new BInventory("VoteTop AllTime");
			users = plugin.topVoterAllTime.entrySet();
		}
		int pos = 0;
		for (Entry<User, Integer> entry : users) {
			pos++;
			inv.addButton(new BInventoryButton(pos + ": " + entry.getKey().getPlayerName(),
					new String[] { "Votes: " + entry.getValue() }, MiscUtils.getInstance().setSkullOwner(

							entry.getKey().getPlayerName())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					User user = UserManager.getInstance()
							.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
					openVoteGUI(player, user);
				}
			});
		}
		inv.openInventory(player);

	}

	public void openVoteTotal(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory("VoteTotal: " + user.getPlayerName());
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(),
					new BInventoryButton(site.getDisplayName(),
							new String[] { Commands.getInstance().voteCommandTotalLine(user, site) },
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
	 * Vote URL.
	 *
	 * @param player
	 *            the player
	 */
	public void openVoteURL(Player player) {
		BInventory inv = new BInventory("Vote Links");

		User user = UserManager.getInstance().getVotingPluginUser(player);

		int count = 0;
		if (Config.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = new ItemBuilder(Config.getInstance().getVoteURLAlreadyVotedItemSection());
			if (user.canVoteAll()) {
				builderAll = new ItemBuilder(Config.getInstance().getVoteURLCanVoteItemSection());
			}

			builderAll.setName("&4All Voting Sites");
			builderAll.setLore("&cClick Me");

			inv.addButton(count, new BInventoryButton(builderAll) {

				@Override
				public void onClick(ClickEvent event) {
					User user = UserManager.getInstance().getVotingPluginUser(event.getPlayer());
					Player player = event.getWhoClicked();
					player.closeInventory();
					user.sendMessage(Commands.getInstance().voteURLs());

				}
			});

			count++;
		}

		for (VoteSite voteSite : plugin.getVoteSites()) {
			ItemBuilder builder = new ItemBuilder(Config.getInstance().getVoteURLAlreadyVotedItemSection());

			if (user.canVoteSite(voteSite)) {
				builder = new ItemBuilder(Config.getInstance().getVoteURLCanVoteItemSection());
			} else {
				builder.addLoreLine(Config.getInstance().getVoteURLNextVote().replace("%Info%",
						Commands.getInstance().voteCommandNextInfo(user, voteSite)));
			}

			builder.setName(Config.getInstance().getVoteURLSiteName().replace("%Name%", voteSite.getDisplayName()));

			inv.addButton(count, new BInventoryButton(builder) {

				@Override
				public void onClick(ClickEvent event) {
					Player player = event.getWhoClicked();
					if (player != null) {
						player.closeInventory();
						User user = UserManager.getInstance().getVotingPluginUser(player);
						user.sendMessage(voteSite.getVoteURL());

					}

				}
			});
			count++;
		}

		BInventory.openInventory(player, inv);
	}

	public void setSelectedPlayer(Player player, User user) {
		PlayerUtils.getInstance().setPlayerMeta(player, "SelectedPlayerGUIs", user);
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
			for (VoteSite voteSite : plugin.getVoteSites()) {
				try {
					ItemBuilder builder = new ItemBuilder(
							Config.getInstance().getVoteSiteItemSection(voteSite.getKey()));
					final VoteSite site = voteSite;

					inv.addButton(count, new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();
							if (player != null) {
								player.closeInventory();
								player.performCommand("vote reward " + site.getKey());

							}

						}
					});
					count++;
				} catch (Exception ex) {

				}
			}
		} else {
			for (String itemName : Config.getInstance().getVoteSiteItems(siteName)) {
				ItemBuilder builder = new ItemBuilder(Config.getInstance().getVoteSiteItemsSection(siteName, itemName));

				inv.addButton(Config.getInstance().getVoteSiteItemsSlot(siteName, itemName),
						new BInventoryButton(builder) {

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
