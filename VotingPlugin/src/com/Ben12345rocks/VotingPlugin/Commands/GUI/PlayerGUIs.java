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
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
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

	public void openVoteBest(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteBestName(), "player", user.getPlayerName()));

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestDayBestItem())
				.addPlaceholder("Best", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestWeekBestItem())
				.addPlaceholder("Best", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestMonthBestItem())
				.addPlaceholder("Best", "" + user.getBestMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.openInventory(player);
	}

	public void openVoteGUI(Player player, User user) {
		if (!player.getName().equals(user.getPlayerName())
				&& !player.hasPermission("VotingPlugin.Commands.Vote.GUI.Other")
				&& !player.hasPermission("VotingPlugin.Mod")) {
			player.sendMessage(AdvancedCoreHook.getInstance().getFormatNoPerms());
			return;
		}
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteGUIName(), "player", user.getPlayerName()));

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
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteLastName(), "player", user.getPlayerName()));
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
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteNextName(), "player", user.getPlayerName()));
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

	public void openVoteStreak(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteStreakName(), "player", user.getPlayerName()));

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakCurrentDayStreakItem())
				.addPlaceholder("Streak", "" + user.getDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakCurrentWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakCurrentMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						Player player = clickEvent.getPlayer();
						openVoteBest(player, getSelectedPlayer(player));
					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestDayStreakItem())
				.addPlaceholder("Streak", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteBest(player, getSelectedPlayer(player));
			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getBestMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						Player player = clickEvent.getPlayer();
						openVoteBest(player, getSelectedPlayer(player));
					}
				});

		inv.openInventory(player);
	}

	public void openVoteToday(Player player) {
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteTodayName());
		for (User user : plugin.voteToday.keySet()) {

			for (VoteSite voteSite : plugin.voteToday.get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.voteToday.get(user).get(voteSite).format(formatter);
				String msg = "&6" + user.getPlayerName() + " : " + voteSite.getDisplayName() + " : " + timeString;
				inv.addButton(inv.getNextSlot(), new BInventoryButton(user.getPlayerName(), new String[] { msg },
						MiscUtils.getInstance().setSkullOwner(user.getPlayerName())) {

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
		String topVoter = "";
		if (str.equalsIgnoreCase("monthly")) {
			topVoter = "Monthly";
			users = plugin.topVoterMonthly.entrySet();
		} else if (str.equalsIgnoreCase("weekly")) {
			topVoter = "Weekly";
			users = plugin.topVoterWeekly.entrySet();
		} else if (str.equalsIgnoreCase("daily")) {
			topVoter = "Daily";
			users = plugin.topVoterDaily.entrySet();
		} else {
			topVoter = "AllTime";
			users = plugin.topVoterAllTime.entrySet();
		}
		inv = new BInventory(StringUtils.getInstance().replacePlaceHolder(Config.getInstance().getGUIVoteTopName(),
				"topvoter", topVoter));
		int pos = 1;
		for (Entry<User, Integer> entry : users) {
			inv.addButton(new BInventoryButton(
					new ItemBuilder(MiscUtils.getInstance().setSkullOwner(entry.getKey().getPlayerName()))
							.setName(Config.getInstance().getGUIVoteTopItemName())
							.addLoreLine(Config.getInstance().getGUIVoteTopItemLore())
							.addPlaceholder("position", "" + pos)
							.addPlaceholder("player", entry.getKey().getPlayerName())
							.addPlaceholder("votes", "" + entry.getValue())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					User user = UserManager.getInstance()
							.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
					openVoteGUI(player, user);
				}
			});
			pos++;
		}
		inv.openInventory(player);

	}

	public void openVoteTotal(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteTotalName(), "player", user.getPlayerName()));
		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteTotalDayTotalItem())
				.addPlaceholder("Total", "" + user.getDailyTotal())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteTotal(player, getSelectedPlayer(player));
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteTotalWeekTotalItem())
				.addPlaceholder("Total", "" + user.getWeeklyTotal())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteTotal(player, getSelectedPlayer(player));
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteTotalMonthTotalItem())
				.addPlaceholder("Total", "" + user.getMonthTotal())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteTotal(player, getSelectedPlayer(player));
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteTotalAllTimeTotalItem())
				.addPlaceholder("Total", "" + user.getAllTimeTotal())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				Player player = clickEvent.getPlayer();
				openVoteTotal(player, getSelectedPlayer(player));
			}
		});
		inv.openInventory(player);
	}

	/**
	 * Vote URL.
	 *
	 * @param player
	 *            the player
	 */
	public void openVoteURL(Player player) {
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteURLName());

		User user = UserManager.getInstance().getVotingPluginUser(player);

		int count = 0;
		if (Config.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = new ItemBuilder(
					Config.getInstance().getVoteURLAlreadyVotedAllUrlsButtonItemSection());
			if (user.canVoteAll()) {
				builderAll = new ItemBuilder(Config.getInstance().getVoteURLCanVoteAllUrlsButtonItemSection());
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

	public void openVoteURL(Player player, String voteSite) {
		User user = UserManager.getInstance().getVotingPluginUser(player);
		VoteSite site = plugin.getVoteSite(voteSite);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteURLSiteName(), "site", site.getDisplayName()));
		inv.setMeta(player, "VoteSite", site);

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Material.BOW).setName("&4URL").addLoreLine("Click to see URL")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						VoteSite site = (VoteSite) clickEvent.getMeta("VoteSite");
						clickEvent.getWhoClicked().sendMessage(site.getVoteURL());
					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.COMPASS).setName("&4Next Vote")
				.addLoreLine(Commands.getInstance().voteCommandNextInfo(user, site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				openVoteURL(clickEvent.getPlayer());
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.WATCH).setName("&4Last Vote")
				.addLoreLine(Commands.getInstance().voteCommandLastLine(user, site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				openVoteURL(clickEvent.getPlayer());
			}
		});

		inv.openInventory(player);
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
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteRewardName());

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
