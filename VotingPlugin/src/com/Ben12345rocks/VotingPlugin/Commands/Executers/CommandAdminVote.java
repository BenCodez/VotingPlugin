package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Commands.GUI.UserGUI;
import com.Ben12345rocks.AdvancedCore.Configs.ConfigRewards;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Objects.Reward;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.ValueRequest;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.BooleanListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.NumberListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.StringListener;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.vexsoftware.votifier.model.Vote;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandAdminVote.
 */
public class CommandAdminVote implements CommandExecutor {

	/** The instance. */
	private static CommandAdminVote instance = new CommandAdminVote();

	/**
	 * Gets the single instance of CommandAdminVote.
	 *
	 * @return single instance of CommandAdminVote
	 */
	public static CommandAdminVote getInstance() {
		return instance;
	}

	/** The bonus reward. */
	ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	/** The config. */
	Config config = Config.getInstance();

	/** The format. */
	ConfigFormat format = ConfigFormat.getInstance();

	/** The plugin. */
	private Main plugin = Main.plugin;

	/** The vote sites. */
	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	/**
	 * Instantiates a new command admin vote.
	 */
	private CommandAdminVote() {
	}

	/**
	 * Instantiates a new command admin vote.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check update.
	 *
	 * @param sender
	 *            the sender
	 */
	public void checkUpdate(CommandSender sender) {
		plugin.updater = new Updater(plugin, 15358, false);
		final Updater.UpdateResult result = plugin.updater.getResult();
		switch (result) {
		case FAIL_SPIGOT: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cFailed to check for update for &c&l" + plugin.getName()
							+ "&c!"));
			break;
		}
		case NO_UPDATE: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&c&l" + plugin.getName()
							+ " &cis up to date! Version: &c&l"
							+ plugin.updater.getVersion()));
			break;
		}
		case UPDATE_AVAILABLE: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&c&l" + plugin.getName()
							+ " &chas an update available! Your Version: &c&l"
							+ plugin.getDescription().getVersion()
							+ " &cNew Version: &c&l"
							+ plugin.updater.getVersion()));
			break;
		}
		default: {
			break;
		}
		}
	}

	/**
	 * Check vote site.
	 *
	 * @param sender
	 *            the sender
	 * @param siteName
	 *            the site name
	 */
	public void checkVoteSite(CommandSender sender, String siteName) {
		if (!ConfigVoteSites.getInstance().isServiceSiteGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cServiceSite is invalid, votes may not work properly"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&aServiceSite is properly setup"));
		}
		if (!ConfigVoteSites.getInstance().isVoteURLGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cVoteURL is invalid"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&aVoteURL is properly setup"));
		}
	}

	/**
	 * Creates the vote site.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 */
	public void createVoteSite(CommandSender sender, String voteSite) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cCreating VoteSite..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				ConfigVoteSites.getInstance().generateVoteSite(voteSite);
				sender.sendMessage(Utils.getInstance().colorize(
						"&cCreated VoteSite: &c&l" + voteSite));
			}
		});

	}

	/**
	 * Help.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void help(CommandSender sender, int page) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendJson(Commands.getInstance().adminHelp(sender, page - 1));
		} else {
			sender.sendMessage(Utils.getInstance()
					.convertArray(
							Utils.getInstance().comptoString(
									Commands.getInstance().adminHelp(sender,
											page - 1))));
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.bukkit.command.CommandExecutor#onCommand(org.bukkit.command.CommandSender
	 * , org.bukkit.command.Command, java.lang.String, java.lang.String[])
	 */
	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /adminvote help!");

		return true;
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
					for (Reward reward : RewardHandler.getInstance().getRewards()) {
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
						Reward reward = RewardHandler.getInstance().getReward(rewardName);
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

	/**
	 * Perm list.
	 *
	 * @param sender
	 *            the sender
	 */
	public void permList(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().listPerms());

	}

	/**
	 * Reload.
	 *
	 * @param sender
	 *            the sender
	 */
	public void reload(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				sender.sendMessage(ChatColor.RED + "Reloading "
						+ plugin.getName() + "...");
				plugin.reload();
				sender.sendMessage(ChatColor.RED + plugin.getName() + " v"
						+ plugin.getDescription().getVersion() + " reloaded!");
			}
		});

	}

	/**
	 * Reset player totals.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void resetPlayerTotals(CommandSender sender, String playerName) {
		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting totals for player &c&l" + playerName));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTotalsPlayer(new User(playerName));
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting totals for &c&l" + playerName));
				plugin.update();
			}
		});
	}

	/**
	 * Reset totals.
	 *
	 * @param sender
	 *            the sender
	 */
	public void resetTotals(CommandSender sender) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting totals for all players..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTotalsMonthly();
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting totals"));
				plugin.update();
			}
		});

	}

	/**
	 * Sets the config allow unjoined.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigAllowUnjoined(CommandSender sender, boolean value) {

		Config.getInstance().setAllowUnJoined(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet AllowUnjoined to &c&l" + value));

	}

	/**
	 * Sets the config broadcast vote.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigBroadcastVote(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet BroadcastVote to &c&l" + value));

	}

	/**
	 * Sets the config debug.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigDebug(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet Debug to &c&l" + value));

	}

	/**
	 * Sets the config enable top voter awards.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigEnableTopVoterAwards(CommandSender sender,
			boolean value) {

		Config.getInstance().setTopVoterAwardsEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet DisableTopVoterAwards to &c&l" + value));

	}

	/**
	 * Sets the reward max money.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 * @param money
	 *            the money
	 */
	public void setRewardMaxMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMaxMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet maxmoney to &c&l" + money + "&c on &c&l" + reward));
	}

	/**
	 * Sets the reward message.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 * @param msg
	 *            the msg
	 */
	public void setRewardMessage(CommandSender sender, String reward, String msg) {
		ConfigRewards.getInstance().setMessagesReward(reward, msg);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet reward message to &c&l" + msg + "&c on &c&l" + reward));
	}

	/**
	 * Sets the reward min money.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 * @param money
	 *            the money
	 */
	public void setRewardMinMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMinMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet minmoney to &c&l" + money + "&c on &c&l" + reward));
	}

	/**
	 * Sets the reward money.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 * @param money
	 *            the money
	 */
	public void setRewardMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet money to &c&l" + money + "&c on &c&l" + reward));
	}

	/**
	 * Sets the reward require permission.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setRewardRequirePermission(CommandSender sender, String reward,
			boolean value) {
		ConfigRewards.getInstance().setRequirePermission(reward, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet require permission to &c&l" + value + "&c on &c&l"
						+ reward));
	}

	/**
	 * Sets the total.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotal(CommandSender sender, String playerName,
			String voteSite, int amount) {

		Data.getInstance().setTotal(new User(playerName), voteSite, amount);
		sender.sendMessage(ChatColor.GREEN + playerName + " total votes for "
				+ voteSite + " has been set to " + amount);
		plugin.update();

	}

	/**
	 * Sets the vote site enabled.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setVoteSiteEnabled(CommandSender sender, String voteSite,
			boolean value) {
		ConfigVoteSites.getInstance().setEnabled(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet votesite " + voteSite + " enabled to " + value));
	}

	/**
	 * Sets the vote site priority.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setVoteSitePriority(CommandSender sender, String voteSite,
			int value) {

		ConfigVoteSites.getInstance().setPriority(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

	}

	/**
	 * Sets the vote site service site.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param serviceSite
	 *            the service site
	 */
	public void setVoteSiteServiceSite(CommandSender sender, String voteSite,
			String serviceSite) {

		ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l"
						+ voteSite));

	}

	/**
	 * Sets the vote site vote delay.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param delay
	 *            the delay
	 */
	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite,
			int delay) {

		ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

	}

	/**
	 * Sets the vote site vote URL.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param url
	 *            the url
	 */
	public void setVoteSiteVoteURL(CommandSender sender, String voteSite,
			String url) {

		ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));

	}

	/**
	 * Site.
	 *
	 * @param sender
	 *            the sender
	 * @param site
	 *            the site
	 */
	public void site(CommandSender sender, String site) {

		sender.sendMessage(Commands.getInstance().voteCommandSiteInfo(site));

	}

	/**
	 * Sites.
	 *
	 * @param sender
	 *            the sender
	 */
	public void sites(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandSites());

	}

	/**
	 * Uuid.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void uuid(CommandSender sender, String playerName) {

		sender.sendMessage(ChatColor.GREEN + "UUID of player "
				+ ChatColor.DARK_GREEN + playerName + ChatColor.GREEN + " is: "
				+ Utils.getInstance().getUUID(playerName));

	}

	/**
	 * Version.
	 *
	 * @param sender
	 *            the sender
	 */
	public void version(CommandSender sender) {
		if (sender instanceof Player) {

			Player player = (Player) sender;
			player.performCommand("bukkit:version " + plugin.getName());

		} else {
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					"bukkit:version " + plugin.getName());
		}
	}

	/**
	 * Vote.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 * @param voteSite
	 *            the vote site
	 */
	public void Vote(CommandSender sender, String playerName, String voteSite) {

		VotiferEvent.playerVote(playerName, voteSite);

		Vote vote = new com.vexsoftware.votifier.model.Vote();
		vote.setServiceName(new VoteSite(voteSite).getServiceSite());
		vote.setUsername(playerName);

	}

}
