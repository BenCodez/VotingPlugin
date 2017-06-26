package com.Ben12345rocks.VotingPlugin.Commands;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Commands.GUI.UserGUI;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Objects.UserStorage;
import com.Ben12345rocks.AdvancedCore.Report.Report;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.ValueRequest;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.BooleanListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.NumberListener;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.StringListener;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAdminVote;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandAliases;
import com.Ben12345rocks.VotingPlugin.Commands.Executers.CommandVote;
import com.Ben12345rocks.VotingPlugin.Commands.GUI.AdminGUI;
import com.Ben12345rocks.VotingPlugin.Commands.GUI.PlayerGUIs;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AliasesTabCompleter;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.Ben12345rocks.VotingPlugin.VoteShop.VoteShop;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Table;
import com.google.common.collect.Table.Cell;
import com.mythicacraft.voteroulette.VoteRoulette;
import com.mythicacraft.voteroulette.Voter;
import com.swifteh.GAL.GAL;
import com.swifteh.GAL.GALVote;
import com.swifteh.GAL.VoteType;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandLoader.
 */
public class CommandLoader {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The instance. */
	static CommandLoader instance = new CommandLoader();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of CommandLoader.
	 *
	 * @return single instance of CommandLoader
	 */
	public static CommandLoader getInstance() {
		return instance;
	}

	/** The commands. */
	private HashMap<String, CommandHandler> commands;

	private String adminPerm = "VotingPlugin.Admin";

	private String modPerm = "VotingPlugin.Mod";

	private String playerPerm = "VotingPlugin.Player";

	/**
	 * Instantiates a new command loader.
	 */
	private CommandLoader() {
	}

	/**
	 * Instantiates a new command loader.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandLoader(Main plugin) {
		CommandLoader.plugin = plugin;
	}

	/**
	 * Gets the commands.
	 *
	 * @return the commands
	 */
	public HashMap<String, CommandHandler> getCommands() {
		return commands;
	}

	/**
	 * Load admin vote command.
	 */
	private void loadAdminVoteCommand() {
		plugin.adminVoteCommand = new ArrayList<CommandHandler>();

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "TriggerPlayerVoteEvent", "(player)", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.TriggerPlayerVoteEvent|" + adminPerm,
						"Trigger vote event, used for testing") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(args[2]),
								UserManager.getInstance().getVotingPluginUser(args[1]));
						plugin.getServer().getPluginManager().callEvent(voteEvent);

					}
				});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "ConvertFrom", "GAL" },
				"VotingPlugin.Commands.AdminVote.ConvertFrom.GAL|" + adminPerm, "Convert from GAL") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Bukkit.getServer().getPluginManager().getPlugin("GAListener") != null) {
					Table<String, Integer, Long> totals = GAL.p.db.getTotals();
					for (Cell<String, Integer, Long> entry : totals.cellSet()) {
						String name = entry.getRowKey();
						int total = entry.getColumnKey();
						User user = UserManager.getInstance().getVotingPluginUser(name);
						user.setAllTimeTotal(user.getAllTimeTotal() + total);
					}
					sender.sendMessage("Totals added");

					ListMultimap<VoteType, GALVote> votes = GAL.p.galVote;
					for (Entry<VoteType, GALVote> entry : votes.entries()) {
						if (entry.getKey().equals(VoteType.NORMAL)) {
							String msg = entry.getValue().message;
							List<String> cmds = entry.getValue().commands;
							String service = entry.getValue().key;
							if (service.equalsIgnoreCase("default")) {

							} else {
								VoteSite site = plugin.getVoteSite(service);
								if (site == null) {
									sender.sendMessage("Failed to create vote site, autogeneratesites false?");
									return;
								}
								ConfigurationSection data = configVoteSites.getData()
										.getConfigurationSection(configVoteSites.getRewardsPath(site.getKey()));
								data.set("Commands.Console", cmds);
								data.set("Messages.Player", msg);
								configVoteSites.saveData();
								sender.sendMessage("created vote site: " + site.getKey());
							}

						}
					}
				} else {
					sender.sendMessage("GAL not loaded");
				}
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "ConvertFrom", "VoteRoulette" },
				"VotingPlugin.Commands.AdminVote.ConvertFrom.GAL|" + adminPerm, "Convert from VoteRoulette") {

			@SuppressWarnings("static-access")
			@Override
			public void execute(CommandSender sender, String[] args) {
				if (Bukkit.getServer().getPluginManager().getPlugin("VoteRoulette") != null) {
					for (OfflinePlayer offPlayer : Bukkit.getOfflinePlayers()) {
						Voter voter = VoteRoulette.getInstance().getVoterManager().getVoter(offPlayer.getUniqueId(),
								offPlayer.getName());
						User user = UserManager.getInstance().getVotingPluginUser(offPlayer);
						user.setAllTimeTotal(user.getAllTimeTotal() + voter.getStatSheet().getLifetimeVotes());
						user.setMonthTotal(user.getMonthTotal() + voter.getStatSheet().getCurrentMonthVotes());
					}
				} else {
					sender.sendMessage("VoteRoulette not loaded");
				}
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "SetPoints", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetPoints|" + adminPerm, "Set players voting points") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				User user = UserManager.getInstance().getVotingPluginUser(args[1]);
				user.setPoints(Integer.parseInt(args[2]));
				sender.sendMessage(StringUtils.getInstance().colorize("&cSet " + args[1] + " points to " + args[2]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "AddPoints", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.AddPoints|" + adminPerm, "Add to players voting points") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				User user = UserManager.getInstance().getVotingPluginUser(args[1]);
				user.addPoints(Integer.parseInt(args[2]));
				sender.sendMessage(StringUtils.getInstance().colorize("&cGave " + args[1] + " " + args[2] + " points"));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Help&?" },
				"VotingPlugin.Commands.AdminVote.Help|" + adminPerm, "See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender, 1);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Help&?", "(number)" },
				"VotingPlugin.Commands.AdminVote.Help|" + adminPerm, "See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().help(sender, Integer.parseInt(args[1]));

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Report" },
				"VotingPlugin.Commands.AdminVote.Report|" + adminPerm, "Create a zip file to send for debuging") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Report.getInstance().create();
				sender.sendMessage("Created Zip File!");

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Perms" },
				"VotingPlugin.Commands.AdminVote.Perms|" + adminPerm, "List perms") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().permList(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Reload" },
				"VotingPlugin.Commands.AdminVote.Reload|" + adminPerm, "Reload plugin") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().reload(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Version" },
				"VotingPlugin.Commands.AdminVote.Version|" + adminPerm, "List version info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().version(sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Sites" },
				"VotingPlugin.Commands.AdminVote.Sites|" + adminPerm, "List VoteSites", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				AdminGUI.getInstance().openAdminGUIVoteSites((Player) sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "GUI" },
				"VotingPlugin.Commands.AdminVote.GUI|" + adminPerm, "Admin GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {

				com.Ben12345rocks.AdvancedCore.Commands.GUI.AdminGUI.getInstance().openGUI((Player) sender);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Sites", "(sitename)" },
				"VotingPlugin.Commands.AdminVote.Sites.Site|" + adminPerm, "View Site Info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					AdminGUI.getInstance().openAdminGUIVoteSiteSite((Player) sender, plugin.getVoteSite(args[1]));
				} else {
					sender.sendMessage("Must be a player to do this");
				}
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "UUID", "(player)" },
				"VotingPlugin.Commands.AdminVote.UUID|" + adminPerm, "View UUID of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().uuid(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "ClearTotal" },
				"VotingPlugin.Commands.AdminVote.ClearTotal.All|" + adminPerm, "Reset totals for all players") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					sender.sendMessage(
							StringUtils.getInstance().colorize("&cThis command can not be done from ingame"));
					return;
				}

				for (String uuid : UserManager.getInstance().getAllUUIDs()) {
					User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
					user.clearTotals();
				}
				sender.sendMessage(StringUtils.getInstance().colorize("&cCleared totals for everyone"));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "ConvertFromData", "(userstorage)" },
				"VotingPlugin.Commands.AdminVote.ConvertFromData",
				"Convert from selected user storage to current user storage") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					sender.sendMessage(StringUtils.getInstance().colorize("&cThis can not be done ingame"));
					return;
				}
				try {
					sender.sendMessage("Starting to convert");
					UserStorage prevStorage = UserStorage.valueOf(args[1].toUpperCase());
					plugin.convertDataStorage(prevStorage, AdvancedCoreHook.getInstance().getStorageType());
					sender.sendMessage("Finished converting!");
				} catch (Exception ex) {
					ex.printStackTrace();
				}
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "SetTotal", "Month", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetTotal.Month|" + adminPerm, "Set month totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				UserManager.getInstance().getVotingPluginUser(args[2]).setMonthTotal(Integer.parseInt(args[3]));
				sender.sendMessage(
						StringUtils.getInstance().colorize("&cSet month total for '" + args[2] + "' to " + args[3]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "SetTotal", "AllTime", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetTotal.AllTime|" + adminPerm, "Set alltime totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				UserManager.getInstance().getVotingPluginUser(args[2]).setAllTimeTotal(Integer.parseInt(args[3]));
				sender.sendMessage(
						StringUtils.getInstance().colorize("&cSet alltime total for '" + args[2] + "' to " + args[3]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "SetTotal", "Week", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetTotal.Week|" + adminPerm, "Set week totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				UserManager.getInstance().getVotingPluginUser(args[2]).setWeeklyTotal(Integer.parseInt(args[3]));
				sender.sendMessage(
						StringUtils.getInstance().colorize("&cSet week total for '" + args[2] + "' to " + args[3]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "SetTotal", "Day", "(player)", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetTotal.Day|" + adminPerm, "Set day totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				UserManager.getInstance().getVotingPluginUser(args[2]).setDailyTotal(Integer.parseInt(args[3]));
				sender.sendMessage(
						StringUtils.getInstance().colorize("&cSet day total for '" + args[2] + "' to " + args[3]));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "ClearTotal", "(player)" },
				"VotingPlugin.Commands.AdminVote.ClearTotal|" + adminPerm, "Clear Totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				User user = UserManager.getInstance().getVotingPluginUser(args[1]);
				user.clearTotals();
				sender.sendMessage(StringUtils.getInstance().colorize("&cCleared totals for '" + args[1] + "'"));
			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Vote", "(player)", "(Sitename)" },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().Vote(sender, args[1], args[2]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "Create" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Create VoteSite") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().createVoteSite(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "Config", "SetDebug", "(boolean)" },
				"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm, "Set Debug on or off") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandAdminVote.getInstance().setConfigDebug(sender, Boolean.parseBoolean(args[2]));

			}
		});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "SetServiceSite", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite SerivceSite") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						CommandAdminVote.getInstance().setVoteSiteServiceSite(sender, args[1], args[3]);
					}
				});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "SetVoteURL", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteURL") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						CommandAdminVote.getInstance().setVoteSiteVoteURL(sender, args[1], args[3]);
					}
				});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "SetPriority", "(number)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite Priority") {

					@Override
					public void execute(CommandSender sender, String[] args) {

						CommandAdminVote.getInstance().setVoteSitePriority(sender, args[1], Integer.parseInt(args[3]));

					}
				});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "SetVoteDelay", "(number)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteDelay") {

					@Override
					public void execute(CommandSender sender, String[] args) {

						CommandAdminVote.getInstance().setVoteSiteVoteDelay(sender, args[1], Integer.parseInt(args[3]));

					}
				});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "UpdateCheck" },
				"VotingPlugin.Commands.AdminVote.UpdateCheck|" + adminPerm, "Check for update") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

					@Override
					public void run() {
						sender.sendMessage(StringUtils.getInstance().colorize("&cChecking for update..."));
						CommandAdminVote.getInstance().checkUpdate(sender);
					}
				});

			}
		});

		plugin.adminVoteCommand
				.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "SetEnabled", "(boolean)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite Enabled") {

					@Override
					public void execute(CommandSender sender, String[] args) {

						CommandAdminVote.getInstance().setVoteSiteEnabled(sender, args[1],
								Boolean.parseBoolean(args[3]));

					}
				});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "VoteSite", "(sitename)", "Check" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Check|" + adminPerm, "Check to see if VoteSite is valid") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandAdminVote.getInstance().checkVoteSite(sender, args[1]);

			}
		});

		plugin.adminVoteCommand.add(new CommandHandler(new String[] { "BackgroundUpdate" },
				"VotingPlugin.Commands.AdminVote.BackgroundUpdate|" + adminPerm, "Force a background update") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.setUpdate(true);
				plugin.update();
				sender.sendMessage(StringUtils.getInstance().colorize("&cUpdating..."));
			}
		});

		ArrayList<CommandHandler> avCommands = com.Ben12345rocks.AdvancedCore.Commands.CommandLoader.getInstance()
				.getBasicAdminCommands("VotingPlugin");
		for (CommandHandler cmd : avCommands) {
			cmd.setPerm(cmd.getPerm() + "|" + adminPerm);
		}
		plugin.adminVoteCommand.addAll(avCommands);

	}

	/**
	 * Load aliases.
	 */
	public void loadAliases() {
		commands = new HashMap<String, CommandHandler>();
		for (CommandHandler cmdHandle : plugin.voteCommand) {
			if (cmdHandle.getArgs().length > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");
				for (String arg : args) {
					try {
						plugin.getCommand("vote" + arg).setExecutor(new CommandAliases(cmdHandle));

						plugin.getCommand("vote" + arg)
								.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle));
					} catch (Exception ex) {
						plugin.debug("Failed to load command and tab completer for /vote" + arg);
					}
				}

			}
		}

		for (CommandHandler cmdHandle : plugin.adminVoteCommand) {
			if (cmdHandle.getArgs().length > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");
				for (String arg : args) {
					try {
						plugin.getCommand("adminvote" + arg).setExecutor(new CommandAliases(cmdHandle));

						plugin.getCommand("adminvote" + arg)
								.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle));
					} catch (Exception ex) {
						plugin.debug("Failed to load command and tab completer for /adminvote" + arg + ": "
								+ ex.getMessage());

					}
				}

			}
		}
	}

	/**
	 * Load commands.
	 */
	public void loadCommands() {
		loadAdminVoteCommand();
		loadVoteCommand();
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				com.Ben12345rocks.AdvancedCore.Thread.Thread.getInstance().run(new Runnable() {

					@Override
					public void run() {
						loadTabComplete();

						UserGUI.getInstance().addPluginButton(plugin,
								new BInventoryButton("Force Vote", new String[] {}, new ItemStack(Material.STONE)) {

									@Override
									public void onClick(ClickEvent clickEvent) {
										Player player = clickEvent.getPlayer();
										ArrayList<String> voteSites = new ArrayList<String>();
										for (VoteSite voteSite : plugin.getVoteSites()) {
											voteSites.add(voteSite.getKey());
										}
										new ValueRequest().requestString(player, "",
												ArrayUtils.getInstance().convert(voteSites), true,
												new StringListener() {

													@Override
													public void onInput(Player player, String value) {

														CommandAdminVote.getInstance().Vote(player,
																UserGUI.getInstance().getCurrentPlayer(player), value);

														player.sendMessage("Forced vote for "
																+ UserGUI.getInstance().getCurrentPlayer(player)
																+ " on " + value);
													}
												});

									}
								});

						UserGUI.getInstance().addPluginButton(plugin, new BInventoryButton("Set Vote Points",
								new String[] {}, new ItemStack(Material.STONE)) {

							@Override
							public void onClick(ClickEvent clickEvent) {
								Player player = clickEvent.getPlayer();
								User user = UserManager.getInstance()
										.getVotingPluginUser(UserGUI.getInstance().getCurrentPlayer(player));
								new ValueRequest().requestNumber(player, "" + user.getPoints(),
										new Number[] { 0, 5, 100 }, true, new NumberListener() {

											@Override
											public void onInput(Player player, Number value) {
												User user = UserManager.getInstance().getVotingPluginUser(
														UserGUI.getInstance().getCurrentPlayer(player));
												user.setPoints(value.intValue());
												player.sendMessage("Points set to " + value.intValue() + " for "
														+ user.getPlayerName());

											}
										});

							}
						});
						UserGUI.getInstance().addPluginButton(plugin,
								new BInventoryButton(new ItemBuilder(Material.BOOK).setName("Set Month Total")) {

									@Override
									public void onClick(ClickEvent clickEvent) {
										String playerName = (String) clickEvent.getMeta(clickEvent.getPlayer(),
												"Player");
										User user = UserManager.getInstance().getVotingPluginUser(playerName);
										new ValueRequest().requestNumber(clickEvent.getPlayer(),
												"" + user.getMonthTotal(), new Number[] { 0, 10, 50, 100 },
												new NumberListener() {

													@Override
													public void onInput(Player player, Number value) {
														String playerName = (String) clickEvent
																.getMeta(clickEvent.getPlayer(), "Player");
														User user = UserManager.getInstance()
																.getVotingPluginUser(playerName);
														user.setMonthTotal(value.intValue());
														player.sendMessage(
																StringUtils.getInstance().colorize("&cTotal set"));
													}
												});
									}
								});
						UserGUI.getInstance().addPluginButton(plugin,
								new BInventoryButton("MileStones", new String[0], new ItemStack(Material.STONE)) {

									@Override
									public void onClick(ClickEvent event) {

										Player player = event.getWhoClicked();
										String playerName = (String) event.getMeta(player, "Player");
										BInventory inv = new BInventory("MileStones: " + playerName);
										for (String mileStoneName : Config.getInstance().getMilestoneVotes()) {
											if (StringUtils.getInstance().isInt(mileStoneName)) {
												int mileStone = Integer.parseInt(mileStoneName);

												inv.addButton(inv.getNextSlot(),
														new BInventoryButton("" + mileStone,
																new String[] {
																		"Enabled: " + Config.getInstance()
																				.getMilestoneRewardEnabled(mileStone),
																		"&cClick to set wether this has been completed or not" },
																new ItemStack(Material.STONE)) {

															@Override
															public void onClick(ClickEvent clickEvent) {
																if (StringUtils.getInstance()
																		.isInt(clickEvent.getClickedItem().getItemMeta()
																				.getDisplayName())) {
																	Player player = clickEvent.getPlayer();
																	int mileStone = Integer
																			.parseInt(clickEvent.getClickedItem()
																					.getItemMeta().getDisplayName());
																	String playerName = (String) event.getMeta(player,
																			"Player");
																	User user = UserManager.getInstance()
																			.getVotingPluginUser(playerName);
																	new ValueRequest().requestBoolean(player,
																			"" + user.hasGottenMilestone(mileStone),
																			new BooleanListener() {

																				@Override
																				public void onInput(Player player,
																						boolean value) {
																					String playerName = UserGUI
																							.getInstance()
																							.getCurrentPlayer(player);
																					User user = UserManager
																							.getInstance()
																							.getVotingPluginUser(
																									playerName);
																					user.setHasGotteMilestone(mileStone,
																							value);
																					player.sendMessage(
																							"Set milestone completetion to "
																									+ value + " on "
																									+ mileStone);

																				}
																			});
																}
															}
														});
											}
										}
									}
								});

					}
				});
			}

		});

	}

	/**
	 * Load tab complete.
	 */
	public void loadTabComplete() {
		ArrayList<String> sites = new ArrayList<String>();
		for (VoteSite site : plugin.getVoteSites()) {
			sites.add(site.getKey());
		}

		for (int i = 0; i < plugin.voteCommand.size(); i++) {
			plugin.voteCommand.get(i).addTabCompleteOption("(Sitename)", sites);

		}

		for (int i = 0; i < plugin.adminVoteCommand.size(); i++) {
			plugin.adminVoteCommand.get(i).addTabCompleteOption("(Sitename)", sites);
		}
	}

	/**
	 * Load vote command.
	 */
	private void loadVoteCommand() {
		plugin.voteCommand = new ArrayList<CommandHandler>();
		plugin.voteCommand.add(new CommandHandler(new String[] { "Help&?" },
				"VotingPlugin.Commands.Vote.Help|" + playerPerm, "View this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				CommandVote.getInstance().help(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Shop" },
				"VotingPlugin.Commands.Vote.Shop|" + playerPerm, "Open VoteShop GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VoteShop.getInstance().voteShop((Player) sender);
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "URL" },
				"VotingPlugin.Commands.Vote.URL|" + playerPerm, "Open VoteURL GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerGUIs.getInstance().openVoteURL((Player) sender);

			}
		});
		plugin.voteCommand.add(new CommandHandler(new String[] { "URL", "(SiteName)" },
				"VotingPlugin.Commands.Vote.URL.VoteSite|" + playerPerm, "Open VoteURL GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerGUIs.getInstance().openVoteURL((Player) sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Reward" },
				"VotingPlugin.Commands.Vote.Reward|" + playerPerm, "Open VoteReward GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerGUIs.getInstance().voteReward((Player) sender, "");

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Reward", "(SiteName)" },
				"VotingPlugin.Commands.Vote.Reward|" + playerPerm, "Open VoteURL GUI for VoteSIte", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerGUIs.getInstance().voteReward((Player) sender, args[1]);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last", "(player)" },
				"VotingPlugin.Commands.Vote.Last.Other|" + modPerm, "See other players last votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUILast()) {
					CommandVote.getInstance().lastOther(sender, args[1]);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteLast((Player) sender,
							UserManager.getInstance().getVotingPluginUser(args[1]));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Last" },
				"VotingPlugin.Commands.Vote.Last|" + playerPerm, "See your last votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUILast()) {
					CommandVote.getInstance().lastSelf(sender);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteLast((Player) sender,
							UserManager.getInstance().getVotingPluginUser(sender.getName()));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next", "(player)" },
				"VotingPlugin.Commands.Vote.Next.Other|" + modPerm, "See other players next votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUINext()) {
					CommandVote.getInstance().nextOther(sender, args[1]);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteNext((Player) sender,
							UserManager.getInstance().getVotingPluginUser(args[1]));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Points", "(player)" },
				"VotingPlugin.Commands.Vote.Points.Other|" + modPerm, "View pints of other player") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().pointsOther(sender, UserManager.getInstance().getVotingPluginUser(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Points", },
				"VotingPlugin.Commands.Vote.Points|" + playerPerm, "View your points") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				if (sender instanceof Player) {
					CommandVote.getInstance()
							.pointsSelf(UserManager.getInstance().getVotingPluginUser((Player) sender));
				} else {
					sender.sendMessage("Must be a player to use this!");
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Next" },
				"VotingPlugin.Commands.Vote.Next|" + playerPerm, "See your next votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUINext()) {
					CommandVote.getInstance().nextSelf(sender);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteNext((Player) sender,
							UserManager.getInstance().getVotingPluginUser(sender.getName()));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "GUI" },
				"VotingPlugin.Commands.Vote.GUI|" + playerPerm, "Open VoteGUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Player player = (Player) sender;
				PlayerGUIs.getInstance().openVoteGUI(player, UserManager.getInstance().getVotingPluginUser(player));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "GUI", "(player)" },
				"VotingPlugin.Commands.Vote.GUI.Other|" + modPerm, "Open VoteGUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Player player = (Player) sender;
				PlayerGUIs.getInstance().openVoteGUI(player, UserManager.getInstance().getVotingPluginUser(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top" },
				"VotingPlugin.Commands.Vote.Top|" + playerPerm, "Open list of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUITopVoter()) {
					Commands.getInstance().voteTop(sender, 1);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteTop((Player) sender, null);
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top", "(number)" },
				"VotingPlugin.Commands.Vote.Top|" + playerPerm, "Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Commands.getInstance().voteTop(sender, Integer.parseInt(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top", "(number)", "Monthly" },
				"VotingPlugin.Commands.Vote.Top.Monthly|" + playerPerm, "Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().topVoterMonthly(sender, Integer.parseInt(args[1]));

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top", "(number)", "All" },
				"VotingPlugin.Commands.Vote.Top.All|" + playerPerm, "Open page of Top Voters All Time") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				int page = Integer.parseInt(args[1]);
				CommandVote.getInstance().topVoterAll(sender, page);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top", "(number)", "Weekly" },
				"VotingPlugin.Commands.Vote.Top.Weekly|" + playerPerm, "Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (StringUtils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterWeekly(sender, Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(
							StringUtils.getInstance().colorize("&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Top", "(number)", "Daily" },
				"VotingPlugin.Commands.Vote.Top.Daily|" + playerPerm, "Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (StringUtils.getInstance().isInt(args[1])) {
					CommandVote.getInstance().topVoterDaily(sender, Integer.parseInt(args[1]));
				} else {
					sender.sendMessage(
							StringUtils.getInstance().colorize("&cError on " + args[1] + ", number expected"));
				}

			}
		});

		plugin.voteCommand
				.add(new CommandHandler(new String[] { "Party" }, "VotingPlugin.Commands.Vote.Party|" + playerPerm,
						"View current amount of votes and how many more needed") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteParty.getInstance().commandVoteParty(sender);
					}
				});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today", "(number)" },
				"VotingPlugin.Commands.Vote.Today|" + playerPerm, "Open page of who Voted Today") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIToday()) {
					CommandVote.getInstance().today(sender, Integer.parseInt(args[1]));
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteToday((Player) sender);
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Today" },
				"VotingPlugin.Commands.Vote.Today|" + playerPerm, "View who list of who voted today") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIToday()) {
					CommandVote.getInstance().today(sender, 1);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteToday((Player) sender);
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total", "All" },
				"VotingPlugin.Commands.Vote.Total.All|" + playerPerm, "View server total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				CommandVote.getInstance().totalAll(sender);

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total", "(player)" },
				"VotingPlugin.Commands.Vote.Total.Other|" + modPerm, "View other players total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUITotal()) {
					CommandVote.getInstance().totalOther(sender, args[1]);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteTotal((Player) sender,
							UserManager.getInstance().getVotingPluginUser(args[1]));
				}

			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Total" },
				"VotingPlugin.Commands.Vote.Total|" + playerPerm, "View your total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUITotal()) {
					CommandVote.getInstance().totalSelf(sender);
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteTotal((Player) sender,
							UserManager.getInstance().getVotingPluginUser(sender.getName()));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Best" },
				"VotingPlugin.Commands.Vote.Best|" + playerPerm, "View your best voting", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIBest()) {
					sender.sendMessage(Commands.getInstance().best(sender, sender.getName()));
				} else {
					PlayerGUIs.getInstance().openVoteBest((Player) sender,
							UserManager.getInstance().getVotingPluginUser(sender.getName()));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Best", "(player)" },
				"VotingPlugin.Commands.Vote.Best.Other|" + playerPerm, "View someone's best voting") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIBest()) {
					sender.sendMessage(Commands.getInstance().best(sender, args[1]));
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteBest((Player) sender,
							UserManager.getInstance().getVotingPluginUser(args[1]));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Streak" },
				"VotingPlugin.Commands.Vote.Streak|" + playerPerm, "View your voting streak", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIBest()) {
					sender.sendMessage(Commands.getInstance().streak(sender, sender.getName()));
				} else {
					PlayerGUIs.getInstance().openVoteStreak((Player) sender,
							UserManager.getInstance().getVotingPluginUser(sender.getName()));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] { "Streak", "(player)" },
				"VotingPlugin.Commands.Vote.Streak.Other|" + playerPerm, "View someone's voting streak") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIStreak()) {
					sender.sendMessage(Commands.getInstance().streak(sender, args[1]));
				} else if (sender instanceof Player) {
					PlayerGUIs.getInstance().openVoteStreak((Player) sender,
							UserManager.getInstance().getVotingPluginUser(args[1]));
				}
			}
		});

		plugin.voteCommand.add(new CommandHandler(new String[] {}, "", "See voting URLs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!Config.getInstance().getCommandsUseGUIVote()) {
					CommandVote.getInstance().voteURLs(sender);
				} else {
					if (sender instanceof Player) {
						PlayerGUIs.getInstance().openVoteURL((Player) sender);
					}
				}

			}
		});

		ArrayList<CommandHandler> avCommands = com.Ben12345rocks.AdvancedCore.Commands.CommandLoader.getInstance()
				.getBasicCommands("VotingPlugin");
		for (CommandHandler cmd : avCommands) {
			cmd.setPerm(cmd.getPerm() + "|" + playerPerm);
		}
		plugin.voteCommand.addAll(avCommands);
	}

	/**
	 * Sets the commands.
	 *
	 * @param commands
	 *            the commands
	 */
	public void setCommands(HashMap<String, CommandHandler> commands) {
		this.commands = commands;
	}
}
