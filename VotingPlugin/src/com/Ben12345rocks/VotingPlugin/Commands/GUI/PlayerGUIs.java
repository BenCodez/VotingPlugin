package com.Ben12345rocks.VotingPlugin.Commands.GUI;

import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
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
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
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

	public ItemBuilder getBackButton() {
		ConfigurationSection sec = Config.getInstance().getBackButton();
		if (sec != null) {
			return new ItemBuilder(sec);
		}
		return new ItemBuilder(Material.PAPER, 1).setName("&8Back to VoteGUI");
	}

	public User getSelectedPlayer(Player player) {
		User str = (User) PlayerUtils.getInstance().getPlayerMeta(player, "SelectedPlayerGUIs");
		return str;
	}

	public void openVoteShop(Player player) {
		if (!Config.getInstance().getVoteShopEnabled()) {
			player.sendMessage(StringUtils.getInstance().colorize("&cVote shop disabled"));
			return;
		}
		BInventory inv = new BInventory(Config.getInstance().getVoteShopName());

		for (final String identifier : Config.getInstance().getIdentifiers()) {

			String perm = Config.getInstance().getVoteShopPermission(identifier);
			boolean hasPerm = false;
			if (perm.isEmpty()) {
				hasPerm = true;
			} else {
				hasPerm = player.hasPermission(perm);
			}

			int limit = Config.getInstance().getIdentifierLimit(identifier);

			boolean limitPass = true;
			if (limit > 0) {
				User user = UserManager.getInstance().getVotingPluginUser(player);
				if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
					limitPass = false;
				}
			}

			if (hasPerm && limitPass) {
				ItemBuilder builder = new ItemBuilder(Config.getInstance().getIdentifierSection(identifier));

				inv.addButton(new BInventoryButton(builder) {

					@Override
					public void onClick(ClickEvent event) {
						Player player = event.getWhoClicked();

						User user = UserManager.getInstance().getVotingPluginUser(player);
						String identifier = (String) getData("identifier");
						int limit = (int) getData("Limit");
						int points = Config.getInstance().getIdentifierCost(identifier);
						if (identifier != null) {
							HashMap<String, String> placeholders = new HashMap<String, String>();
							placeholders.put("identifier", identifier);
							placeholders.put("points", "" + points);
							placeholders.put("limit", "" + limit);
							if (user.removePoints(points)) {

								RewardHandler.getInstance().giveReward(user, Config.getInstance().getData(),
										Config.getInstance().getIdentifierRewardsPath(identifier),
										new RewardOptions().setPlaceholders(placeholders));

								user.sendMessage(StringUtils.getInstance().replacePlaceHolder(
										Config.getInstance().getFormatShopPurchaseMsg(), placeholders));
								if (limit > 0) {
									user.setVoteShopIdentifierLimit(identifier,
											user.getVoteShopIdentifierLimit(identifier) + 1);
								}
							} else {
								user.sendMessage(StringUtils.getInstance().replacePlaceHolder(
										Config.getInstance().getFormatShopFailedMsg(), placeholders));
							}
						}
					}

				}.addData("identifier", identifier).addData("Limit", limit));
			}
		}

		if (Config.getInstance().getVoteShopBackButton()) {
			inv.addButton(new BInventoryButton(PlayerGUIs.getInstance().getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					PlayerGUIs.getInstance().openVoteGUI(event.getPlayer(),
							UserManager.getInstance().getVotingPluginUser(player));
				}

			});

		}

		inv.openInventory(player);
	}

	public void openVoteBest(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteBestName(), "player", user.getPlayerName()));

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestDayBestItem())
				.addPlaceholder("Best", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestWeekBestItem())
				.addPlaceholder("Best", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteBestMonthBestItem())
				.addPlaceholder("Best", "" + user.getBestMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.openInventory(player);
	}

	public void openVoteGUI(Player player, User user) {
		if (user == null) {
			user = UserManager.getInstance().getVotingPluginUser(player);
			setSelectedPlayer(player, user);
		}
		if ((!player.getName().equals(user.getPlayerName())
				&& !player.hasPermission("VotingPlugin.Commands.Vote.GUI.Other")
				&& !player.hasPermission("VotingPlugin.Mod"))
				|| (!player.hasPermission("VotingPlugin.Commands.Vote.GUI")
						&& !player.hasPermission("VotingPlugin.Player"))) {
			player.sendMessage(
					StringUtils.getInstance().colorize(AdvancedCoreHook.getInstance().getOptions().getFormatNoPerms()));
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
					lore = Commands.getInstance().voteURLs(user);
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
					lore = new String[] { "Click to view help" };
				}
			}

			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("points", "" + user.getPoints());

			builder.setPlaceholders(placeholders);
			builder.setLore(ArrayUtils.getInstance().convert(lore));

			inv.addButton(new BInventoryButton(builder) {

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
							openVoteTop(player, null);
						} else if (slot.equalsIgnoreCase("today")) {
							openVoteToday(player);
						} else if (slot.equalsIgnoreCase("help")) {
							player.performCommand("vote help");
						} else if (slot.equalsIgnoreCase("shop")) {
							openVoteShop(player);
						}
					}

				}
			});
		}

		inv.openInventory(player);
	}

	public void openVoteLast(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteLastName(), "player", user.getPlayerName()));
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(), new BInventoryButton(site.getItem().setName(site.getDisplayName())
					.setLore(Commands.getInstance().voteCommandLastLine(user, site)).setAmountNone(1)) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}
			});
		}

		if (Config.getInstance().getGUIVoteLastBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
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
			inv.addButton(inv.getNextSlot(), new BInventoryButton(site.getItem().setName(site.getDisplayName())
					.setLore(Commands.getInstance().voteCommandNextInfo(user, site)).setAmountNone(1)) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}
			});
		}

		if (Config.getInstance().getGUIVoteNextBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
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

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakCurrentWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakCurrentMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {

					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestDayStreakItem())
				.addPlaceholder("Streak", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteStreakHighestMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getBestMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {

					}
				});

		if (Config.getInstance().getGUIVoteStreakBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
				}
			});
		}

		inv.openInventory(player);
	}

	public void openVoteToday(Player player) {
		setSelectedPlayer(player, null);
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteTodayName());
		for (User user : plugin.voteToday.keySet()) {

			for (VoteSite voteSite : plugin.voteToday.get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.voteToday.get(user).get(voteSite).format(formatter);
				String msg = "&6" + voteSite.getDisplayName() + " : " + timeString;
				inv.addButton(inv.getNextSlot(), new BInventoryButton(user.getPlayerName(), new String[] { msg },
						MiscUtils.getInstance().setSkullOwner(user.getOfflinePlayer())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						User user = UserManager.getInstance()
								.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
						openVoteGUI(player, user);

					}
				});
			}
		}

		if (Config.getInstance().getGUIVoteTodayBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
				}

			});
		}
		inv.openInventory(player);
	}

	public void openVoteTop(Player player, TopVoter top) {
		if (top == null) {
			top = TopVoter.getDefault();
		}
		Set<Entry<User, Integer>> users = null;

		String topVoter = top.getName();
		users = plugin.getTopVoter(top).entrySet();

		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteTopName(), "topvoter", topVoter));

		int pos = 1;
		for (Entry<User, Integer> entry : users) {

			inv.addButton(new BInventoryButton(
					new ItemBuilder(MiscUtils.getInstance().setSkullOwner(entry.getKey().getOfflinePlayer()))
							.setName(Config.getInstance().getGUIVoteTopItemName())
							.addLoreLine(Config.getInstance().getGUIVoteTopItemLore())
							.addPlaceholder("position", "" + pos)
							.addPlaceholder("player", entry.getKey().getPlayerName())
							.addPlaceholder("votes", "" + entry.getValue())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					// ItemBuilder item = new ItemBuilder(clickEvent.getClickedItem());
					// String name = ChatColor.stripColor(item.getName()).split(":")[1].trim();
					String name = (String) clickEvent.getButton().getData("player");

					User user = UserManager.getInstance().getVotingPluginUser(name);
					openVoteGUI(player, user);
				}
			}.addData("player", entry.getKey().getPlayerName()));
			pos++;
		}

		final TopVoter cur = top;
		inv.getPageButtons().add(new BInventoryButton(
				new ItemBuilder(Config.getInstance().getGUIVoteTopSwitchItem()).addPlaceholder("Top", topVoter)) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				if (!clickEvent.getClick().equals(ClickType.RIGHT)) {
					openVoteTop(player, cur.next());
				} else {
					openVoteTop(player, cur.prev());
				}
			}
		});

		if (Config.getInstance().getGUIVoteTopBackButton()) {
			inv.getPageButtons().add(new BInventoryButton(getBackButton().setSlot(1)) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
				}
			});
		}

		inv.setPages(true);
		inv.setMaxInvSize(Config.getInstance().getGUIVoteTopSize());
		inv.openInventory(player);

	}

	public void openVoteTotal(Player player, User user) {
		setSelectedPlayer(player, user);
		BInventory inv = new BInventory(StringUtils.getInstance()
				.replacePlaceHolder(Config.getInstance().getGUIVoteTotalName(), "player", user.getPlayerName()));

		for (TopVoter top : TopVoter.values()) {
			inv.addButton(new BInventoryButton(new ItemBuilder(Config.getInstance().getGUIVoteTotalItem(top))
					.addPlaceholder("Total", "" + user.getTotal(top)).addPlaceholder("topvoter", top.getName())) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}
			});
		}

		if (Config.getInstance().getGUIVoteTotalBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
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
		setSelectedPlayer(player, null);
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteURLName());

		User user = UserManager.getInstance().getVotingPluginUser(player);

		int count = 0;
		if (Config.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = new ItemBuilder(
					Config.getInstance().getVoteURLAlreadyVotedAllUrlsButtonItemSection());
			if (user.canVoteAll()) {
				builderAll = new ItemBuilder(Config.getInstance().getVoteURLCanVoteAllUrlsButtonItemSection());
			}

			if (!builderAll.hasCustomDisplayName()) {
				builderAll.setName("&4All Voting Sites");
			}
			if (!builderAll.hasCustomLore()) {
				builderAll.setLore("&cClick Me");
			}

			inv.addButton(count, new BInventoryButton(builderAll) {

				@Override
				public void onClick(ClickEvent event) {
					User user = UserManager.getInstance().getVotingPluginUser(event.getPlayer());
					Player player = event.getWhoClicked();
					player.closeInventory();
					user.sendMessage(Commands.getInstance().voteURLs(user));

				}
			});

			count++;
		}

		for (final VoteSite voteSite : plugin.getVoteSites()) {
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

						if (Config.getInstance().commandsVoteRewardFromVoteURL
								&& event.getClick().equals(ClickType.RIGHT)) {
							voteReward(player, voteSite.getKey());
						} else {
							User user = UserManager.getInstance().getVotingPluginUser(player);
							user.sendMessage(StringUtils.getInstance().replacePlaceHolder(StringUtils.getInstance()
									.replacePlaceHolder(StringUtils.getInstance().replacePlaceHolder(
											Config.getInstance().getGUIVoteURLURLText(), "voteurl",
											voteSite.getVoteURL()), "sitename", voteSite.getDisplayName()),
									"player", player.getName()));
						}
					}

				}
			});
			count++;
		}

		if (Config.getInstance().getGUIVoteURLBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
				}
			});
		}

		inv.openInventory(player);

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

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.CLOCK).setName("&4Last Vote")
				.addLoreLine(Commands.getInstance().voteCommandLastLine(user, site))) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				openVoteURL(clickEvent.getPlayer());
			}
		});

		if (Config.getInstance().getGUIVoteURLBackButton()) {
			inv.addButton(new BInventoryButton(getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					openVoteGUI(event.getPlayer(), getSelectedPlayer(event.getPlayer()));
				}
			});
		}
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
			for (VoteSite voteSite : plugin.getVoteSites()) {
				try {
					ItemBuilder builder = voteSite.getItem();

					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();

							voteReward(player, (String) getData("site"));
						}
					}.addData("site", voteSite.getKey()));
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

							}

						});
			}
		}

		inv.openInventory(player);
	}
}
