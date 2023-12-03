package com.bencodez.votingplugin.commands;

import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permission;

import com.bencodez.advancedcore.DebugLevel;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.advancedcore.api.command.TabCompleteHandle;
import com.bencodez.advancedcore.api.command.TabCompleteHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.updater.Updater;
import com.bencodez.advancedcore.api.user.userstorage.Column;
import com.bencodez.advancedcore.api.user.userstorage.DataType;
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.listeners.BooleanListener;
import com.bencodez.advancedcore.api.valuerequest.listeners.StringListener;
import com.bencodez.advancedcore.api.yml.editor.ConfigEditor;
import com.bencodez.advancedcore.api.yml.updater.ConfigUpdater;
import com.bencodez.advancedcore.command.gui.UserGUI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.executers.CommandAliases;
import com.bencodez.votingplugin.commands.gui.AdminGUI;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteHelp;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePerms;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePlaceholders;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePlaceholdersPlayer;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteVoteParty;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteVotePlayer;
import com.bencodez.votingplugin.commands.gui.admin.cumulative.AdminVoteCumulative;
import com.bencodez.votingplugin.commands.gui.admin.milestones.AdminVoteMilestones;
import com.bencodez.votingplugin.commands.gui.admin.voteshop.AdminVoteVoteShop;
import com.bencodez.votingplugin.commands.gui.player.VoteBest;
import com.bencodez.votingplugin.commands.gui.player.VoteGUI;
import com.bencodez.votingplugin.commands.gui.player.VoteHelp;
import com.bencodez.votingplugin.commands.gui.player.VoteLast;
import com.bencodez.votingplugin.commands.gui.player.VoteNext;
import com.bencodez.votingplugin.commands.gui.player.VoteShop;
import com.bencodez.votingplugin.commands.gui.player.VoteStreak;
import com.bencodez.votingplugin.commands.gui.player.VoteToday;
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoter;
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoterLastMonth;
import com.bencodez.votingplugin.commands.gui.player.VoteTotal;
import com.bencodez.votingplugin.commands.gui.player.VoteURL;
import com.bencodez.votingplugin.commands.gui.player.VoteURLVoteSite;
import com.bencodez.votingplugin.commands.tabcompleter.AliasesTabCompleter;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandLoader.
 */
public class CommandLoader {

	private String adminPerm = "VotingPlugin.Admin";

	/** The commands. */
	private HashMap<String, CommandHandler> commands;

	private String modPerm = "VotingPlugin.Mod";

	private String playerPerm = "VotingPlugin.Player";

	private VotingPluginMain plugin;

	private Object pointLock = new Object();

	public CommandLoader(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void processSlotClick(Player player, VotingPluginUser user, String slot) {
		if (StringParser.getInstance().startsWithIgnoreCase(slot, "url")) {
			new VoteURL(plugin, player, user, true).open();
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "next")) {
			new VoteNext(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "last")) {
			new VoteLast(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "total")) {
			new VoteTotal(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "top")) {
			new VoteTopVoter(plugin, player, user, TopVoter.getDefault(), 1).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "today")) {
			new VoteToday(plugin, player, user, 1).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "help")) {
			player.performCommand("vote help");
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "shop")) {
			new VoteShop(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "lastmonth")) {
			new VoteTopVoterLastMonth(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "best")) {
			new VoteBest(plugin, player, user).open(GUIMethod.CHEST);
		} else if (StringParser.getInstance().startsWithIgnoreCase(slot, "streak")) {
			new VoteStreak(plugin, player, user).open(GUIMethod.CHEST);
		}
	}

	/**
	 * @return the adminPerm
	 */
	public String getAdminPerm() {
		return adminPerm;
	}

	public BInventoryButton getBackButton(VotingPluginUser user) {
		ConfigurationSection sec = plugin.getGui().getCHESTBackButton();
		boolean a = false;
		boolean exit = false;

		ItemBuilder item;
		if (sec != null) {
			item = new ItemBuilder(sec);
			a = sec.getBoolean("OpenVoteURL", false);
			exit = sec.getBoolean("Exit");
		} else {
			item = new ItemBuilder(Material.BARRIER, 1).setName("&8Back to VoteGUI");
		}

		final boolean openVoteURL = a;
		final boolean exitGUI = exit;
		BInventoryButton b = new BInventoryButton(item) {

			@Override
			public void onClick(ClickEvent event) {
				if (!exitGUI) {
					if (!openVoteURL) {
						new VoteGUI(plugin, event.getWhoClicked(), user)
								.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
					} else {
						new VoteURL(plugin, event.getWhoClicked(), user, true).open();
					}
				} else {
					event.closeInventory();
				}
			}

		};

		// set item to end of the GUI
		if (sec != null && sec.getBoolean("EndOfGUI")) {
			b.setSlot(-2);
		}

		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			b.dontClose();
		}

		return b;
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
	 * @return the modPerm
	 */
	public String getModPerm() {
		return modPerm;
	}

	/**
	 * @return the playerPerm
	 */
	public String getPlayerPerm() {
		return playerPerm;
	}

	public boolean hasPermission(Player player, String cmd) {
		if (cmd.startsWith("votingplugin:")) {
			cmd = cmd.substring("votingplugin:".length());
		}

		if (commands.containsKey(cmd)) {
			return commands.get(cmd).hasPerm(player);
		}

		boolean adminCommand = false;

		if (cmd.startsWith("vote")) {
			cmd = cmd.substring("vote".length());
		} else if (cmd.startsWith("v")) {
			cmd = cmd.substring(1);
		} else if (cmd.startsWith("av")) {
			adminCommand = true;
			cmd = cmd.substring("av".length());
		} else if (cmd.startsWith("adminvote")) {
			adminCommand = true;
			cmd = cmd.substring("adminvote".length());
		}
		// plugin.debug(cmd);
		if (!adminCommand) {
			for (CommandHandler handle : plugin.getVoteCommand()) {
				if (handle.isCommand(cmd)) {
					// plugin.debug("is handle " + ArrayUtils.getInstance()
					// .makeStringList(ArrayUtils.getInstance().convert(handle.getArgs())));
					if (handle.hasPerm(player)) {
						// plugin.debug("has perm");
						return true;
					} else {
						// plugin.debug("no perm " + cmd);
					}
				}
			}
		} else {
			for (CommandHandler handle : plugin.getAdminVoteCommand()) {
				if (handle.isCommand(cmd)) {
					// plugin.debug("is handle " + ArrayUtils.getInstance()
					// .makeStringList(ArrayUtils.getInstance().convert(handle.getArgs())));
					if (handle.hasPerm(player)) {
						// plugin.debug("has perm");
						return true;
					} else {
						// plugin.debug("no perm " + cmd);
					}
				}
			}
		}
		return false;
	}

	public boolean isVotingPluginCommand(Player player, String cmd) {
		if (plugin.getCommand(cmd) != null || cmd.startsWith("votingplugin")) {
			return true;
		}
		for (String str : commands.keySet()) {
			if (str.equalsIgnoreCase(cmd)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Load admin vote command.
	 */
	private void loadAdminVoteCommand() {
		plugin.setAdminVoteCommand(new ArrayList<CommandHandler>());

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "CurrentPluginTime" },
				"VotingPlugin.Commands.AdminVote.CurrentPluginTime|" + adminPerm, "Current plugin time") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				sendMessage(sender, plugin.getTimeChecker().getTime().format(formatter));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin, new String[] { "User", "(player)", "SetPoints", "(number)" },
						"VotingPlugin.Commands.AdminVote.SetPoints|" + adminPerm, "Set players voting points") {

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setPoints(Integer.parseInt(args[3]));
						sender.sendMessage(
								StringParser.getInstance().colorize("&cSet " + args[1] + " points to " + args[3]));
					}

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[3]);

						sender.sendMessage(
								StringParser.getInstance().colorize("&cSetting all players points to " + args[3]));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.dontCache();
							user.setPoints(num);
							plugin.getSpecialRewards().checkMilestone(user, null,
									plugin.getBungeeSettings().isUseBungeecoord());
						}
						sender.sendMessage(
								StringParser.getInstance().colorize("&cDone setting all players points to " + args[3]));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ResyncMilestones" },
						"VotingPlugin.Commands.AdminVote.ResyncMilestones|" + adminPerm,
						"Resync Milestones to all time total") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, "&cStarting...");
						plugin.getUserManager().copyColumnData(TopVoter.AllTime.getColumnName(), "MilestoneCount");
						sendMessage(sender, "&cFinished sync milestonecount with all time total");

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ResetMilestoneCount" },
				"VotingPlugin.Commands.AdminVote.ResetMilestoneCount|" + adminPerm, "Resets milestone count to 0") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&cStarting to clear milestonecounts...");
				plugin.getTopVoterHandler().resetMilestoneCount();
				sendMessage(sender, "&cFinished");
				plugin.setUpdate(true);
				plugin.getUserManager().getDataManager().updateCacheOnline();
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ResyncMilestonesAlreadyGiven" },
						"VotingPlugin.Commands.AdminVote.ResyncMilestonesGiven|" + adminPerm,
						"Resync Milestones already given") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, "&cStarting...");
						ArrayList<Integer> nums = new ArrayList<Integer>();

						for (String str : plugin.getSpecialRewardsConfig().getMilestoneVotes()) {
							try {
								nums.add(Integer.parseInt(str));
							} catch (Exception e) {
								plugin.getLogger().warning("Failed to get number from " + str);
							}
						}
						HashMap<UUID, ArrayList<Column>> cols = plugin.getUserManager().getAllKeys();
						for (Entry<UUID, ArrayList<Column>> playerData : cols.entrySet()) {

							String uuid = playerData.getKey().toString();
							if (plugin != null && plugin.isEnabled()) {
								if (uuid != null && !uuid.isEmpty()) {
									VotingPluginUser user = plugin.getVotingPluginUserManager()
											.getVotingPluginUser(UUID.fromString(uuid));
									user.dontCache();
									user.updateTempCacheWithColumns(playerData.getValue());
									cols.put(playerData.getKey(), null);
									int milestoneCount = user.getMilestoneCount();
									for (int num : nums) {
										if (milestoneCount >= num) {
											if (!user.hasGottenMilestone(num)) {
												sendMessage(sender, "&cMilestone " + num + " for "
														+ user.getPlayerName()
														+ " not already given when it should be, Current AllTimeTotal: "
														+ user.getTotal(TopVoter.AllTime) + ", Current MileStoneCount: "
														+ user.getMilestoneCount());
												user.setHasGotteMilestone(num, true);
											}
										} else {
											if (user.hasGottenMilestone(num)) {
												sendMessage(sender, "&cMilestone " + num + " for "
														+ user.getPlayerName()
														+ " already given when it shouldn't be, Current AllTimeTotal: "
														+ user.getTotal(TopVoter.AllTime) + ", Current MileStoneCount: "
														+ user.getMilestoneCount());
												user.setHasGotteMilestone(num, false);
											}

										}
									}
									user.clearTempCache();
								}
							}
						}
						cols.clear();
						cols = null;
						sendMessage(sender, "&cFinished");

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ResetPoints" },
						"VotingPlugin.Commands.AdminVote.ResetPoints|" + adminPerm, "Clears all points of all players",
						true, false) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, "&cStarting...");
						plugin.getUserManager().removeAllKeyValues("Points", DataType.INTEGER);
						plugin.getUserManager().getDataManager().clearCache();
						sendMessage(sender, "&cFinished");

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ResyncMilestones", "(player)" },
						"VotingPlugin.Commands.AdminVote.SetResyncMilestones|" + adminPerm,
						"Resync Milestones to alltimetotal for player") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setMilestoneCount(user.getTotal(TopVoter.AllTime));

						sendMessage(sender, "&cResynced milestones for " + args[1]);

					}
				});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin, new String[] { "User", "(player)", "AddPoints", "(number)" },
						"VotingPlugin.Commands.AdminVote.AddPoints|" + adminPerm, "Add to players voting points") {

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						synchronized (pointLock) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
							user.cache();
							int newTotal = 0;
							newTotal = user.addPoints(Integer.parseInt(args[3]));
							sender.sendMessage(StringParser.getInstance().colorize("&cGave " + args[1] + " " + args[3]
									+ " points" + ", " + args[1] + " now has " + newTotal + " points"));
						}
					}

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[3]);

						sender.sendMessage(StringParser.getInstance()
								.colorize("&cGiving " + "all players" + " " + args[3] + " points"));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.dontCache();
							user.addPoints(num);
							plugin.getSpecialRewards().checkMilestone(user, null,
									plugin.getBungeeSettings().isUseBungeecoord());
						}
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cGave " + "all players" + " " + args[3] + " points"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "RemovePoints", "(number)" },
						"VotingPlugin.Commands.AdminVote.RemovePoints|" + adminPerm, "Remove voting points") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.cache();
						user.removePoints(Integer.parseInt(args[3]));
						sender.sendMessage(StringParser.getInstance().colorize("&cRemoved " + args[3] + " points from "
								+ args[1] + ", " + args[1] + " now has " + user.getPoints() + " points"));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Help&?" },
				"VotingPlugin.Commands.AdminVote.Help|" + adminPerm, "See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteHelp(plugin, sender, 1).open();
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ServiceSites&Status" },
						"VotingPlugin.Commands.AdminVote.ServiceSites|" + adminPerm,
						"See a list of all service sites the server got") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						ArrayList<String> serviceSites = plugin.getServerData().getServiceSites();
						if (!serviceSites.isEmpty()) {
							sendMessage(sender, "&cEvery service site the server has gotten from votifier:");
							for (String serviceSite : serviceSites) {
								boolean hasSite = plugin.hasVoteSite(serviceSite);
								if (hasSite) {
									String siteName = plugin.getVoteSiteName(true, serviceSite);
									sendMessage(sender, serviceSite + " : Current site = " + siteName);
								} else {
									sendMessage(sender, serviceSite
											+ " : No site with this service site, did you do something wrong?");
								}
							}
						} else {
							sendMessage(sender, "&cNo votes have been received. Please check your votifier settings.");
						}
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Help&?", "(number)" },
				"VotingPlugin.Commands.AdminVote.Help|" + adminPerm, "See this page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				int page = Integer.parseInt(args[1]);
				new AdminVoteHelp(plugin, sender, page).open();

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Edit", "BungeeSettings" },
				"VotingPlugin.Commands.AdminVote.Edit.BungeeSettings", "Edit BungeeSettings.yml", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new ConfigEditor(plugin, plugin.getBungeeSettings()).open((Player) sender);
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Edit", "VoteShop" },
				"VotingPlugin.Commands.AdminVote.Edit.VoteShop", "Edit VoteShop", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteVoteShop(plugin, sender).open(GUIMethod.CHEST);
			}
		});
		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Edit", "MileStones" },
				"VotingPlugin.Commands.AdminVote.Edit.MileStones", "Edit milestones rewards", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteMilestones(plugin, sender).open(GUIMethod.CHEST);
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Edit", "Cumulative" },
				"VotingPlugin.Commands.AdminVote.Edit.Cumulative", "Edit cumulative rewards", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteCumulative(plugin, sender).open(GUIMethod.CHEST);
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Edit", "VoteParty" },
				"VotingPlugin.Commands.AdminVote.Edit.VoteParty", "Edit voteparty", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteVoteParty(plugin, sender).open(GUIMethod.CHEST);
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Perms" },
				"VotingPlugin.Commands.AdminVote.Perms|" + adminPerm, "List permissions") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVotePerms(plugin, sender, 1).open();
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Perms", "(Number)" },
				"VotingPlugin.Commands.AdminVote.Perms|" + adminPerm, "List permissions") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVotePerms(plugin, sender, Integer.parseInt(args[1])).open();
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "PermsPlayer", "(Player)" },
						"VotingPlugin.Commands.AdminVote.Perms.Other|" + adminPerm,
						"List permissions from the plugin the player has") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						new AdminVotePerms(plugin, sender, 1, args[1]).open();
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "PermsPlayer", "(Player)", "(Number)" },
						"VotingPlugin.Commands.AdminVote.Perms.Other|" + adminPerm,
						"List permissions from the plugin the player has") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						new AdminVotePerms(plugin, sender, Integer.parseInt(args[2]), args[1]).open();
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Perms", "(Number)", "(Player)" },
						"VotingPlugin.Commands.AdminVote.Perms.Other|" + adminPerm,
						"List permissions from the plugin the specificed player has") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						new AdminVotePerms(plugin, sender, Integer.parseInt(args[1]), args[2]).open();
					}
				});

		if (plugin.getOptions().getDebug().equals(DebugLevel.DEV)) {
			plugin.getAdminVoteCommand()
					.add(new CommandHandler(plugin, new String[] { "PermsDebug" },
							"VotingPlugin.Commands.AdminVote.PermsDebug",
							"Dev permission list, generate this list, requires dev debug") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							sendMessage(sender, ArrayUtils.getInstance()
									.convert(new AdminVotePerms(plugin, sender, 0).listPermsDev(sender)));
						}
					});
		}

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Reload" },
				"VotingPlugin.Commands.AdminVote.Reload|" + adminPerm, "Reload plugin, will not reload user storage") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&4" + "Reloading " + plugin.getName() + "...");
				plugin.reload();
				if (plugin.isYmlError()) {
					sendMessage(sender, "&3Detected yml error, please check server log for details");
				}
				if (plugin.getProfile().equals("dev")) {
					sendMessage(sender, "&cDetected using dev build, there could be bugs, use at your own risk");
				}
				sendMessage(sender, "&4" + plugin.getName() + " v" + plugin.getDescription().getVersion()
						+ " reloaded! Note: User storage has not been reloaded");
				if (plugin.getServerData().getServiceSites().size() == 0) {
					sendMessage(sender, "&c"
							+ "Detected that server hasn't received any votes from votifier, please check votifier connection");
				}
				if (!plugin.getConfigFile().isDisableUpdateChecking()
						&& plugin.getUpdater().getResult().equals(Updater.UpdateResult.UPDATE_AVAILABLE)) {
					sendMessage(sender,
							"&3Plugin has update available! https://www.spigotmc.org/resources/votingplugin.15358/");
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ReloadAll" },
				"VotingPlugin.Commands.AdminVote.Reload|" + adminPerm, "Reload plugin, including user storage") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&4" + "Reloading " + plugin.getName() + "...");
				plugin.reloadAll();
				if (plugin.isYmlError()) {
					sendMessage(sender, "&3Detected yml error, please check server log for details");
				}
				if (plugin.getProfile().equals("dev")) {
					sendMessage(sender, "&cDetected using dev build, there could be bugs, use at your own risk");
				}
				sendMessage(sender,
						"&4" + plugin.getName() + " v" + plugin.getDescription().getVersion() + " reloaded!");
				if (plugin.getServerData().getServiceSites().size() == 0) {
					sendMessage(sender, "&c"
							+ "Detected that server hasn't received any votes from votifier, please check votifier connection");
				}
				if (!plugin.getConfigFile().isDisableUpdateChecking()
						&& plugin.getUpdater().getResult().equals(Updater.UpdateResult.UPDATE_AVAILABLE)) {
					sendMessage(sender,
							"&3Plugin has update available! https://www.spigotmc.org/resources/votingplugin.15358/");
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Version" },
				"VotingPlugin.Commands.AdminVote.Version|" + adminPerm, "List version info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					Player player = (Player) sender;
					plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							player.performCommand("bukkit:version " + plugin.getName());
						}
					});

				} else {
					plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

						@Override
						public void run() {
							Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
									"bukkit:version " + plugin.getName());
						}
					});

				}

				sendMessage(sender,
						"Using AdvancedCore " + plugin.getVersion() + "' built on '" + plugin.getBuildTime());
				if (!plugin.getAdvancedCoreBuildNumber().equals("NOTSET")) {
					sendMessage(sender, "AdvancedCore Jenkins build number: " + plugin.getAdvancedCoreBuildNumber());
				}
				if (!plugin.getBuildNumber().equals("NOTSET")) {
					sendMessage(sender, "Using votingplugin jenkins build: " + plugin.getBuildNumber());
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Sites" },
				"VotingPlugin.Commands.AdminVote.Sites|" + adminPerm, "List VoteSites", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminGUI(plugin).openAdminGUIVoteSites((Player) sender);

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "GUI" },
				"VotingPlugin.Commands.AdminVote.GUI|" + adminPerm, "Admin GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				com.bencodez.advancedcore.command.gui.AdminGUI.getInstance().openGUI((Player) sender);
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Sites", "(sitename)" },
				"VotingPlugin.Commands.AdminVote.Sites.Site|" + adminPerm, "View Site Info") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					new AdminGUI(plugin).openAdminGUIVoteSiteSite((Player) sender, plugin.getVoteSite(args[1], false));
				} else {
					sender.sendMessage("Must be a player to do this");
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "UUID", "(player)" },
				"VotingPlugin.Commands.AdminVote.UUID|" + adminPerm, "View UUID of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sender.sendMessage(ChatColor.GREEN + "UUID of player " + ChatColor.DARK_GREEN + args[1]
						+ ChatColor.GREEN + " is: " + PlayerUtils.getInstance().getUUID(args[1]));

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "PlayerName", "(uuid)" },
				"VotingPlugin.Commands.AdminVote.PlayerName|" + adminPerm, "View PlayerName of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				try {
					sender.sendMessage(ChatColor.GREEN + "PlayerName of player " + ChatColor.DARK_GREEN + args[1]
							+ ChatColor.GREEN + " is: "
							+ PlayerUtils.getInstance().getPlayerName(
									plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(args[1])),
									args[1]));
				} catch (IllegalArgumentException e) {
					sendMessage(sender, "&cInvalid uuid");
				}

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ClearTotal" },
				"VotingPlugin.Commands.AdminVote.ClearTotal.All|" + adminPerm, "Reset totals for all players") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					sender.sendMessage(
							StringParser.getInstance().colorize("&cThis command can not be done from ingame"));
					return;
				}

				for (TopVoter top : TopVoter.values()) {
					plugin.getUserManager().removeAllKeyValues(top.getColumnName(), DataType.INTEGER);
				}
				plugin.getUserManager().getDataManager().clearCache();
				plugin.setUpdate(true);
				sender.sendMessage(StringParser.getInstance().colorize("&cCleared totals for everyone"));
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ClearOfflineVoteRewards" },
				"VotingPlugin.Commands.AdminVote.ClearOfflineVoteRewards|" + adminPerm, "Reset offline votes/rewards") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					sender.sendMessage(
							StringParser.getInstance().colorize("&cThis command can not be done from ingame"));
					return;
				}
				plugin.getUserManager().removeAllKeyValues("OfflineVotes", DataType.STRING);
				plugin.getUserManager().removeAllKeyValues(plugin.getUserManager().getOfflineRewardsPath(),
						DataType.STRING);
				plugin.getUserManager().getDataManager().clearCache();
				sender.sendMessage(StringParser.getInstance().colorize("&cCleared offline votes/rewards"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "SetVoteStreak", "DAY", "(number)" },
						"VotingPlugin.Commands.AdminVote.SetVoteStreak.Day|" + adminPerm, "Set votestreak for player") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
								.setDayVoteStreak(Integer.parseInt(args[4]));
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet votestreak day for '" + args[1] + "' to " + args[4]));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin,
				new String[] { "User", "(player)", "SetVoteStreak", "WEEK", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetVoteStreak.Week|" + adminPerm, "Set votestreak for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
						.setWeekVoteStreak(Integer.parseInt(args[4]));
				sender.sendMessage(StringParser.getInstance()
						.colorize("&cSet votestreak week for '" + args[1] + "' to " + args[4]));
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin,
				new String[] { "User", "(player)", "SetVoteStreak", "MONTH", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetVoteStreak.Month|" + adminPerm, "Set votestreak for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
						.setMonthVoteStreak(Integer.parseInt(args[4]));
				sender.sendMessage(StringParser.getInstance()
						.colorize("&cSet votestreak month for '" + args[1] + "' to " + args[4]));
			}
		});

		for (final TopVoter top : TopVoter.values()) {
			plugin.getAdminVoteCommand()
					.add(new CommandHandler(plugin,
							new String[] { "User", "(player)", "SetTotal", top.toString(), "(number)" },
							"VotingPlugin.Commands.AdminVote.SetTotal." + top.toString() + "|" + adminPerm,
							"Set " + top.toString() + " totals for player") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]).setTotal(top,
									Integer.parseInt(args[4]));
							sender.sendMessage(StringParser.getInstance().colorize(
									"&cSet " + top.toString() + " total for '" + args[1] + "' to " + args[4]));
						}
					});

			plugin.getAdminVoteCommand()
					.add(new CommandHandler(plugin,
							new String[] { "User", "(player)", "AddTotal", top.toString(), "(number)" },
							"VotingPlugin.Commands.AdminVote.AddTotal." + top.toString() + "|" + adminPerm,
							"Add " + top.toString() + " totals for player") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
							user.setTotal(top, user.getTotal(top) + Integer.parseInt(args[4]));
							sender.sendMessage(StringParser.getInstance()
									.colorize("&cAdded " + top.toString() + " total for " + args[1]));
						}
					});
		}

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "User", "(player)", "ClearTotal" },
				"VotingPlugin.Commands.AdminVote.ClearTotal|" + adminPerm, "Clear Totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
				user.clearTotals();
				sender.sendMessage(StringParser.getInstance().colorize("&cCleared totals for '" + args[1] + "'"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ResetAllVotedSites" },
						"VotingPlugin.Commands.AdminVote.ResetAllVotedSites|" + adminPerm, "Resets all voted") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.resetLastVoted();
						plugin.getCoolDownCheck().check(user);
						plugin.getCoolDownCheck().checkPerSite(user);
						sender.sendMessage(
								StringParser.getInstance().colorize("&cVoted sites reset for '" + args[1] + "'"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ResetVotedSite", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.ResetVotedSite|" + adminPerm,
						"Resets last voted for specific site") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.resetLastVoted(plugin.getVoteSite(args[3], false));
						plugin.getCoolDownCheck().checkPerSite(user);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cVoted site reset for '" + args[1] + "'" + " on " + args[3]));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin,
						new String[] { "User", "(player)", "AddMilestoneCount", "(number)" },
						"VotingPlugin.Commands.AdminVote.AddMilestoneCount|" + adminPerm, "Add milestonecount") {

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setMilestoneCount(user.getMilestoneCount() + Integer.parseInt(args[3]));
						plugin.getSpecialRewards().checkMilestone(user, null,
								plugin.getBungeeSettings().isUseBungeecoord());
						sender.sendMessage(
								StringParser.getInstance().colorize("&cAdded milestonecount for " + args[1]));
					}

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						int toAdd = Integer.parseInt(args[3]);

						sender.sendMessage(
								StringParser.getInstance().colorize("&cAdding milestonecount for all players..."));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.dontCache();
							user.setMilestoneCount(user.getMilestoneCount() + toAdd);
							plugin.getSpecialRewards().checkMilestone(user, null,
									plugin.getBungeeSettings().isUseBungeecoord());
						}
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cFinished adding milestonecount for all players"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "SetMilestoneCount", "(number)" },
						"VotingPlugin.Commands.AdminVote.SetMilestoneCount|" + adminPerm, "Set milestonecount") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setMilestoneCount(Integer.parseInt(args[3]));
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet milestonecount for " + args[1] + " to " + args[3]));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin, new String[] { "User", "(player)", "ClearGottenMilestones" },
						"VotingPlugin.Commands.AdminVote.ClearGottenMilestones|" + adminPerm,
						"Clears received milestones") {

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setHasGottenMilestone(new HashMap<String, Boolean>());
						sender.sendMessage(
								StringParser.getInstance().colorize("&cClearing gotten milestones for " + args[1]));
					}

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						String path = "GottenMileStones";
						if (plugin.getBungeeSettings().isPerServerMilestones()) {
							path = plugin.getBungeeSettings().getServerNameStorage() + "_" + "GottenMilestones";
						}
						sender.sendMessage(
								StringParser.getInstance().colorize("&cClearing gotten milestones for all players..."));
						plugin.getUserManager().removeAllKeyValues(path, DataType.STRING);
						for (Player p : Bukkit.getOnlinePlayers()) {
							plugin.getUserManager().getDataManager().cacheUser(p.getUniqueId());
						}
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cFinished clearing gotten milestones for all players"));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Vote", "(player)", "All" },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&cTriggering vote for all voting sites...");
				for (VoteSite site : plugin.getVoteSites()) {
					plugin.getVoteTimer().submit(new Runnable() {

						@Override
						public void run() {
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, args[1], site.getServiceSite(),
									false);
							if (voteEvent.getVoteSite() != null) {
								if (!voteEvent.getVoteSite().isVaidServiceSite()) {
									sendMessage(sender,
											"&cPossible issue with service site, has the server gotten the vote from "
													+ voteEvent.getServiceSite() + "?");
								}
							}
							plugin.getServer().getPluginManager().callEvent(voteEvent);
						}
					});
				}

				if (plugin.isYmlError()) {
					sendMessage(sender, "&3Detected yml error, please check server log for details");
				}

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Vote", "(player)", "(Sitename)" },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(args[2], true), args[1], args[2],
						false);
				if (voteEvent.getVoteSite() != null) {
					if (!voteEvent.getVoteSite().isVaidServiceSite()) {
						sendMessage(sender, "&cPossible issue with service site, has the server gotten the vote from "
								+ voteEvent.getServiceSite() + "?");
					}
					sendMessage(sender, "&cTriggering vote...");

					plugin.getVoteTimer().submit(new Runnable() {

						@Override
						public void run() {
							plugin.getServer().getPluginManager().callEvent(voteEvent);
						}
					});

					if (plugin.isYmlError()) {
						sendMessage(sender, "&3Detected yml error, please check server log for details");
					}
				}

			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteExact", "(PlayerExact)", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.VoteExact|" + adminPerm,
						"Trigger manual vote with exact player name") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(args[2], true), args[1],
								args[2], false);
						sendMessage(sender, "&cTriggering vote...");
						if (voteEvent.getVoteSite() != null) {
							if (!voteEvent.getVoteSite().isVaidServiceSite()) {
								sendMessage(sender,
										"&cPossible issue with service site, has the server gotten the vote from "
												+ voteEvent.getServiceSite() + "?");
							}
						}
						plugin.getVoteTimer().submit(new Runnable() {

							@Override
							public void run() {
								plugin.getServer().getPluginManager().callEvent(voteEvent);
							}
						});

						if (plugin.isYmlError()) {
							sendMessage(sender, "&3Detected yml error, please check server log for details");
						}

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Vote", "(player)", },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote via GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteVotePlayer(plugin, sender, args[1]).open();
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Vote" },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Manual vote syntax") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&aUse /av vote (player) (site)");
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(Player)", "ForceVote", "All" },
						"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, "&cTriggering vote for all voting sites...");
						for (VoteSite site : plugin.getVoteSites()) {
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, args[1], site.getServiceSite(),
									false);
							if (voteEvent.getVoteSite() != null) {
								if (!voteEvent.getVoteSite().isVaidServiceSite()) {
									sendMessage(sender,
											"&cPossible issue with service site, has the server gotten the vote from "
													+ voteEvent.getServiceSite() + "?");
								}
							}
							plugin.getServer().getPluginManager().callEvent(voteEvent);
						}

						if (plugin.isYmlError()) {
							sendMessage(sender, "&3Detected yml error, please check server log for details");
						}

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(Player)", "ForceVote", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(args[3], true), args[1],
								args[3], false);
						sendMessage(sender, "&cTriggering vote...");
						if (voteEvent.getVoteSite() != null) {
							if (!voteEvent.getVoteSite().isVaidServiceSite()) {
								sendMessage(sender,
										"&cPossible issue with service site, has the server gotten the vote from "
												+ voteEvent.getServiceSite() + "?");
							}
						}
						plugin.getServer().getPluginManager().callEvent(voteEvent);

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "User", "(Player)", "ForceCoolDownEndRewards", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.ForceCoolDownEndRewards|" + adminPerm,
						"Trigger CoolDownEndRewards manually") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						PlayerVoteSiteCoolDownEndEvent event = new PlayerVoteSiteCoolDownEndEvent(user,
								plugin.getVoteSite(args[3], true));
						plugin.getServer().getPluginManager().callEvent(event);
						sendMessage(sender, "&cCoolDownEndRewards forced on votesite " + args[3]);

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "Create" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Create VoteSite") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sender.sendMessage(StringParser.getInstance().colorize("&cCreating VoteSite..." + args[1]));

				plugin.getConfigVoteSites().generateVoteSite(args[1]);
				sender.sendMessage(StringParser.getInstance().colorize("&cCreated VoteSite: &c&l" + args[1]));

			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Config", "TempDebug" },
						"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm,
						"Enable debug, effective until reload/restart") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginMain.plugin.getOptions().setDebug(DebugLevel.INFO);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Config", "TempExtraDebug" },
						"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm,
						"Enable extra debug, effective until reload/restart") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginMain.plugin.getOptions().setDebug(DebugLevel.EXTRA);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Config", "Update" },
						"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm,
						"Force update Config.yml with new options") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						try {
							ConfigUpdater.update(plugin, "Config.yml", plugin.getConfigFile().getdFile(), Arrays
									.asList("VoteReminding", "MySQL", "CustomCommands", "CustomPlaceholderReturns"));
							plugin.getConfigFile().reloadData();
						} catch (IOException e) {
							e.printStackTrace();
						}
						sendMessage(sender, "&aUpdated config");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetServiceSite", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite SerivceSite") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteName(true, args[1]);
						String serviceSite = args[3];
						plugin.getConfigVoteSites().setServiceSite(voteSite, serviceSite);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l" + voteSite));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "CheckVoteCoolDownRewards" },
						"VotingPlugin.Commands.AdminVote.VoteSite.CheckVoteCoolDownRewards|" + adminPerm,
						"Force check all vote cooldown rewards") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getCoolDownCheck().getTimer().schedule(new Runnable() {

							@Override
							public void run() {
								for (VoteSite site : plugin.getVoteSites()) {
									if (site.isVoteDelayDaily()) {
										plugin.getCoolDownCheck().checkAllVoteSite(site);
									}
								}
							}
						}, 5, TimeUnit.SECONDS);
						sender.sendMessage(
								StringParser.getInstance().colorize("&cForce checking on vote cooldown rewards"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetVoteURL", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteURL") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteName(true, args[1]);
						String url = args[3];
						plugin.getConfigVoteSites().setVoteURL(voteSite, url);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetPriority", "(number)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite Priority") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteName(true, args[1]);
						int value = Integer.parseInt(args[3]);
						plugin.getConfigVoteSites().setPriority(voteSite, value);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetVoteDelay", "(number)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteDelay") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteName(true, args[1]);
						int delay = Integer.parseInt(args[3]);
						plugin.getConfigVoteSites().setVoteDelay(voteSite, delay);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "UpdateCheck" },
				"VotingPlugin.Commands.AdminVote.UpdateCheck|" + adminPerm, "Check for update") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {

					@Override
					public void run() {
						sender.sendMessage(StringParser.getInstance().colorize("&cChecking for update..."));
						plugin.setUpdater(new Updater(plugin, 15358, false));
						final Updater.UpdateResult result = plugin.getUpdater().getResult();
						switch (result) {
						case FAIL_SPIGOT: {
							sender.sendMessage(StringParser.getInstance()
									.colorize("&cFailed to check for update for &c&l" + plugin.getName() + "&c!"));
							break;
						}
						case NO_UPDATE: {
							sender.sendMessage(StringParser.getInstance().colorize("&c&l" + plugin.getName()
									+ " &cis up to date! Version: &c&l" + plugin.getUpdater().getVersion()));
							break;
						}
						case UPDATE_AVAILABLE: {
							sender.sendMessage(StringParser.getInstance().colorize(
									"&c&l" + plugin.getName() + " &chas an update available! Your Version: &c&l"
											+ plugin.getDescription().getVersion() + " &cNew Version: &c&l"
											+ plugin.getUpdater().getVersion()));
							break;
						}
						default: {
							break;
						}
						}
					}
				});

			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetEnabled", "(boolean)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite Enabled") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteName(false, args[1]);
						boolean value = Boolean.parseBoolean(args[3]);

						plugin.getConfigVoteSites().setEnabled(voteSite, value);
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cSet votesite " + voteSite + " enabled to " + value));

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "Check" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Check|" + adminPerm, "Check to see if VoteSite is valid") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				String siteName = args[1];
				if (!plugin.getConfigVoteSites().isServiceSiteGood(siteName)) {
					sender.sendMessage(StringParser.getInstance()
							.colorize("&cServiceSite is invalid, votes may not work properly"));
				} else {
					String service = plugin.getConfigVoteSites().getServiceSite(siteName);
					if (plugin.getServerData().getServiceSites().contains(service)) {
						sender.sendMessage(StringParser.getInstance().colorize("&aServiceSite is properly setup"));
					} else {
						sender.sendMessage(StringParser.getInstance()
								.colorize("&cService may not be valid, haven't recieved a vote from " + service
										+ ", see /av servicesites"));
					}

				}
				if (!plugin.getConfigVoteSites().isVoteURLGood(siteName)) {
					sender.sendMessage(StringParser.getInstance().colorize("&cVoteURL is invalid"));
				} else {
					sender.sendMessage(StringParser.getInstance().colorize("&aVoteURL is properly setup"));
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "BackgroundUpdate" },
				"VotingPlugin.Commands.AdminVote.BackgroundUpdate|" + adminPerm, "Force a background update") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.setUpdate(true);
				plugin.update();
				sender.sendMessage(StringParser.getInstance().colorize("&cUpdating..."));
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ClearOfflineVotes" },
				"VotingPlugin.Commands.AdminVote.ClearOfflineVotes|" + adminPerm, "Clear all offline votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getUserManager().removeAllKeyValues("OfflineVotes", DataType.STRING);
				plugin.getUserManager().getDataManager().clearCache();
				sender.sendMessage(StringParser.getInstance().colorize("&cOffline votes Cleared"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Test", "(Player)", "(sitename)", "(number)" },
						"VotingPlugin.Commands.AdminVote.Test|" + adminPerm, "Test voting speed, for debug") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVoteTester().testVotes(Integer.parseInt(args[3]), args[1], args[2]);
						if (isPlayer(sender)) {
							sendMessage(sender, "&cSee console for details");
						}
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "TestSpam", "(Player)", "(sitename)", "(number)" },
						"VotingPlugin.Commands.AdminVote.TestSpam|" + adminPerm, "Test voting spam speed, for debug") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVoteTester().testSpam(Integer.parseInt(args[3]), args[1], args[2]);
						if (isPlayer(sender)) {
							sendMessage(sender, "&cSee console for details");
						}
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "TestReward", "(Player)", "(reward)", "(number)" },
						"VotingPlugin.Commands.AdminVote.TestReward|" + adminPerm, "Test reward speed, for debug") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVoteTester().testRewards(Integer.parseInt(args[3]), args[1], args[2]);
						if (isPlayer(sender)) {
							sendMessage(sender, "&cSee console for details");
						}
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Placeholders" },
						"VotingPlugin.Commands.AdminVote.Placeholders|" + adminPerm,
						"See possible placeholderapi placeholders") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						new AdminVotePlaceholders(plugin, sender).open();
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteParty", "Force" },
						"VotingPlugin.Commands.AdminVote.VoteParty.Force|" + adminPerm,
						"Force a voteparty reward, resets vote count") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVoteParty().giveRewards(null, plugin.getBungeeSettings().isUseBungeecoord());
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteParty", "SetVoteCount", "(Number)" },
						"VotingPlugin.Commands.AdminVote.VoteParty.SetVoteCount|" + adminPerm, "Set voteparty count") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[2]);
						if (num == 0) {
							plugin.getVoteParty().reset(true);
							sendMessage(sender, "&cVoteparty totals have been set to 0 and all been reset");
						} else {
							plugin.getVoteParty().setTotalVotes(num);
							sendMessage(sender, "&cVoteparty total votes has been set to " + args[2]);
						}
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteParty", "AddVoteCount", "(Number)" },
						"VotingPlugin.Commands.AdminVote.VoteParty.SetVoteCount|" + adminPerm, "Add voteparty count") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						int num = plugin.getVoteParty().getTotalVotes() + Integer.parseInt(args[2]);
						plugin.getVoteParty().setTotalVotes(num);
						sendMessage(sender, "&cVoteparty total votes has been set to " + num);
					}
				});
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteParty", "SetExtraRequired", "(Number)" },
						"VotingPlugin.Commands.AdminVote.VoteParty.SetExtraRequired|" + adminPerm,
						"Set VotePartyExtraRequired value") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getServerData().setVotePartyExtraRequired(Integer.parseInt(args[2]));
						sendMessage(sender, "&cSet VotePartyExtraRequired to " + args[2]);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceVoteShop", "(VoteShop)" },
						"VotingPlugin.Commands.AdminVote.ForceVoteShop|" + adminPerm, "Force a voteshop reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getRewardHandler().giveReward(user, plugin.getConfigFile().getData(),
								plugin.getGui().getChestShopIdentifierRewardsPath(args[3]), new RewardOptions());
						sendMessage(sender, "&cVoteShop " + args[3] + " forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceMilestone", "(Number)" },
						"VotingPlugin.Commands.AdminVote.ForceMilestone|" + adminPerm, "Force a milestone") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveMilestoneVoteReward(user, user.isOnline(), parseInt(args[3]),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cMilestone " + args[3] + " forced");
					}
				});

		for (final TopVoter top : TopVoter.valuesMinusAllTime()) {
			plugin.getAdminVoteCommand()
					.add(new CommandHandler(plugin,
							new String[] { "User", "(player)", "ForceTopVoter", top.toString(), "(Number)" },
							"VotingPlugin.Commands.AdminVote.ForceTopVoter." + top.toString() + "|" + adminPerm,
							"Force a top voter reward") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
							int place = parseInt(args[4]);
							switch (top) {
							case Daily:
								user.giveDailyTopVoterAward(place, args[4]);
								break;
							case Monthly:
								user.giveMonthlyTopVoterAward(place, args[4]);
								break;
							case Weekly:
								user.giveWeeklyTopVoterAward(place, args[4]);
								break;
							default:
								break;

							}
							sendMessage(sender, "&cTopVoter " + top.toString() + " " + args[4] + " forced");
						}
					});

			String text = "";
			switch (top) {
			case Daily:
				text = "Day";
				break;
			case Monthly:
				text = "Month";
				break;
			case Weekly:
				text = "Week";
				break;
			default:
				break;

			}

			final String str = text;

			plugin.getAdminVoteCommand()
					.add(new CommandHandler(plugin,
							new String[] { "User", "(player)", "ForceVoteStreak", str, "(Number)" },
							"VotingPlugin.Commands.AdminVote.ForceVoteStreak|" + adminPerm,
							"Force a votestreak reward for " + str) {

						@Override
						public void execute(CommandSender sender, String[] args) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
							plugin.getSpecialRewards().giveVoteStreakReward(user, user.isOnline(), str, args[4],
									parseInt(args[4]), plugin.getBungeeSettings().isUseBungeecoord());
							sendMessage(sender, "&cVoteStreak " + str + " " + args[4] + " forced");
						}
					});
		}

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceCumulative", "(Number)" },
						"VotingPlugin.Commands.AdminVote.ForceCumulative|" + adminPerm, "Force a cumulative reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveCumulativeVoteReward(user, user.isOnline(), parseInt(args[3]),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cCumulative " + args[3] + " forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceAllSites" },
						"VotingPlugin.Commands.AdminVote.ForceAllSites|" + adminPerm, "Force a allsites reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveAllSitesRewards(user, user.isOnline(),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cAllSites forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceAlmostAllSites" },
						"VotingPlugin.Commands.AdminVote.ForceAllSites|" + adminPerm, "Force a almostallsites reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveAlmostAllSitesRewards(user, user.isOnline(),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cAlmostAllSites forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceFirstVote" },
						"VotingPlugin.Commands.AdminVote.ForceFirstVote|" + adminPerm, "Force a firstvote reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveFirstVoteRewards(user, user.isOnline(),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cFirstVote forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ForceFirstVoteToday" },
						"VotingPlugin.Commands.AdminVote.ForceFirstVoteToday|" + adminPerm,
						"Force a firstvotetoday reward") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						plugin.getSpecialRewards().giveFirstVoteTodayRewards(user, user.isOnline(),
								plugin.getBungeeSettings().isUseBungeecoord());
						sendMessage(sender, "&cFirstVoteToday forced");
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Placeholders", "(player)" },
						"VotingPlugin.Commands.AdminVote.Placeholders.Players|" + adminPerm,
						"See possible placeholderapi placeholders with player values") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						new AdminVotePlaceholdersPlayer(plugin, sender,
								plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])).open();

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] {},
				"VotingPlugin.Commands.AdminVote|" + adminPerm, "Base command") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&cInvalid command, see /adminvote help");
			}
		});

		ArrayList<CommandHandler> avCommands = com.bencodez.advancedcore.command.CommandLoader.getInstance()
				.getBasicAdminCommands("VotingPlugin");
		for (CommandHandler cmd : avCommands) {
			cmd.setPerm(cmd.getPerm() + "|" + adminPerm);
		}
		plugin.getAdminVoteCommand().addAll(avCommands);

	}

	/**
	 * Load aliases.
	 */
	public void loadAliases() {
		commands = new HashMap<String, CommandHandler>();
		if (!plugin.getConfigFile().isLoadCommandAliases()) {
			return;
		}
		for (CommandHandler cmdHandle : plugin.getVoteCommand()) {
			int argLength = cmdHandle.getArgs().length;
			String arg0 = "";
			if (argLength > 0) {
				arg0 = cmdHandle.getArgs()[0];
			}
			String[] perms = cmdHandle.getPerm().split(Pattern.quote("|"));
			try {
				if (perms.length > 1) {
					// has another perm
					plugin.devDebug("Adding child perm " + perms[0] + " to " + perms[1] + " from /vote" + arg0);
					Permission p = Bukkit.getPluginManager().getPermission(perms[1]);
					p.getChildren().put(perms[0], true);
					p.recalculatePermissibles();
				}
			} catch (Exception e) {
				plugin.debug("Failed to set permission for /vote" + arg0);
			}
			if (argLength > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");

				for (String arg : args) {
					commands.put("vote" + arg, cmdHandle);

					try {
						plugin.getCommand("vote" + arg).setExecutor(new CommandAliases(cmdHandle, false));

						plugin.getCommand("vote" + arg)
								.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle, false));

						String currentPerm = plugin.getCommand("vote" + arg).getPermission();
						if (currentPerm == null || currentPerm.length() > perms[0].length()) {
							plugin.getCommand("vote" + arg).setPermission(perms[0]);
						}

						for (String str : plugin.getCommand("vote" + arg).getAliases()) {
							commands.put(str, cmdHandle);
						}
					} catch (Exception ex) {
						plugin.devDebug("Failed to load command and tab completer for /vote" + arg);
					}
				}
			}
		}

		for (CommandHandler cmdHandle : plugin.getAdminVoteCommand()) {
			int argLength = cmdHandle.getArgs().length;
			String arg0 = "";
			if (argLength > 0) {
				arg0 = cmdHandle.getArgs()[0];
			}
			String[] perms = cmdHandle.getPerm().split(Pattern.quote("|"));
			try {
				if (perms.length > 1) {
					// has another perm
					plugin.devDebug("Adding child perm " + perms[0] + " to " + perms[1] + " from /adminvote" + arg0);
					Permission p = Bukkit.getPluginManager().getPermission(perms[1]);
					p.getChildren().put(perms[0], true);
					p.recalculatePermissibles();
				}
			} catch (Exception e) {
				plugin.debug("Failed to set permission for /adminvote" + arg0);
			}

			if (argLength > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");
				for (String arg : args) {
					commands.put("adminvote" + arg, cmdHandle);

					try {
						plugin.getCommand("adminvote" + arg).setExecutor(new CommandAliases(cmdHandle, true));

						plugin.getCommand("adminvote" + arg)
								.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle, true));

						String currentPerm = plugin.getCommand("adminvote" + arg).getPermission();
						if (currentPerm == null || currentPerm.length() > perms[0].length()) {
							plugin.getCommand("adminvote" + arg).setPermission(perms[0]);
						}

						for (String str : plugin.getCommand("adminvote" + arg).getAliases()) {
							commands.put(str, cmdHandle);
						}
					} catch (Exception ex) {
						plugin.devDebug("Failed to load command and tab completer for /adminvote" + arg + ": "
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
		plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {

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
										ArrayUtils.getInstance().convert(voteSites), true, new StringListener() {

											@Override
											public void onInput(Player player, String value) {
												PlayerVoteEvent voteEvent = new PlayerVoteEvent(
														plugin.getVoteSite(value, true),
														UserGUI.getInstance().getCurrentPlayer(player),
														plugin.getVoteSiteServiceSite(value), false);
												plugin.getServer().getPluginManager().callEvent(voteEvent);

												player.sendMessage("Forced vote for "
														+ UserGUI.getInstance().getCurrentPlayer(player) + " on "
														+ value);
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
								for (String mileStoneName : plugin.getSpecialRewardsConfig().getMilestoneVotes()) {
									if (StringParser.getInstance().isInt(mileStoneName)) {
										int mileStone = Integer.parseInt(mileStoneName);

										inv.addButton(inv.getNextSlot(),
												new BInventoryButton("" + mileStone, new String[] {
														"Enabled: " + plugin.getSpecialRewardsConfig()
																.getMilestoneRewardEnabled(mileStone),
														"&cClick to set wether this has been completed or not" },
														new ItemStack(Material.STONE)) {

													@Override
													public void onClick(ClickEvent clickEvent) {
														if (StringParser.getInstance().isInt(clickEvent.getClickedItem()
																.getItemMeta().getDisplayName())) {
															Player player = clickEvent.getPlayer();
															int mileStone = Integer.parseInt(clickEvent.getClickedItem()
																	.getItemMeta().getDisplayName());
															String playerName = (String) event.getMeta(player,
																	"Player");
															VotingPluginUser user = plugin.getVotingPluginUserManager()
																	.getVotingPluginUser(playerName);
															new ValueRequest().requestBoolean(player,
																	"" + user.hasGottenMilestone(mileStone),
																	new BooleanListener() {

																		@Override
																		public void onInput(Player player,
																				boolean value) {
																			String playerName = UserGUI.getInstance()
																					.getCurrentPlayer(player);
																			VotingPluginUser user = plugin
																					.getVotingPluginUserManager()
																					.getVotingPluginUser(playerName);
																			user.setHasGotteMilestone(mileStone, value);
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

	/**
	 * Load tab complete.
	 */
	public void loadTabComplete() {
		ArrayList<String> sites = new ArrayList<String>();
		for (VoteSite site : plugin.getVoteSites()) {
			sites.add(site.getKey());
		}

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(Sitename)", sites) {

			@Override
			public void reload() {
				ArrayList<String> sites = new ArrayList<String>();
				for (VoteSite site : plugin.getVoteSites()) {
					sites.add(site.getKey());
				}
				setReplace(sites);
			}

			@Override
			public void updateReplacements() {
			}
		});

		ArrayList<String> topVoter = new ArrayList<String>();
		for (TopVoter top : TopVoter.values()) {
			topVoter.add(top.toString());
		}

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(topvoter)", topVoter) {

			@Override
			public void reload() {
				ArrayList<String> topVoter = new ArrayList<String>();
				for (TopVoter top : TopVoter.values()) {
					topVoter.add(top.toString());
				}
				setReplace(topVoter);

			}

			@Override
			public void updateReplacements() {
			}
		});

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(VoteShop)", sites) {

			@Override
			public void reload() {
				ArrayList<String> sites = new ArrayList<String>();
				for (String str : plugin.getGui().getChestShopIdentifiers()) {
					sites.add(str);
				}
				setReplace(sites);
			}

			@Override
			public void updateReplacements() {
			}
		});
	}

	/**
	 * Load vote command.
	 */
	private void loadVoteCommand() {
		plugin.setVoteCommand(new ArrayList<CommandHandler>());
		if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders()) {
			plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "SetPrimaryAccount", "(player)" },
					"VotingPlugin.Commands.Vote.SetPrimaryAccount|" + modPerm, "Set primary account", false) {

				@Override
				public void execute(CommandSender sender, String[] args) {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(sender.getName());
					if (args[1].equals("none")) {
						user.setPrimaryAccount(null);
						sendMessage(sender, "&cRemoved primary account");
					} else {
						try {
							user.setPrimaryAccount(
									java.util.UUID.fromString(PlayerUtils.getInstance().getUUID(args[1])));
							sendMessage(sender, "&cPrimary account set");
						} catch (Exception e) {
							e.printStackTrace();
							sendMessage(sender, "Failed to set primary account: " + e.getMessage());
						}
					}
				}
			});
		}
		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Help&?" },
				"VotingPlugin.Commands.Vote.Help|" + playerPerm, "View help page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteHelp(plugin, sender, 1).open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Help&?", "(number)" },
				"VotingPlugin.Commands.Vote.Help|" + playerPerm, "View help page") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteHelp(plugin, sender, Integer.parseInt(args[1])).open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "ToggleReminders" },
				"VotingPlugin.Commands.Vote.ToggleReminders", "Enable/disable vote reminders", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				Player p = (Player) sender;
				boolean value = false;
				if (plugin.getVoteReminding().getRemindersEnabled().containsKey(p.getUniqueId())) {
					value = !plugin.getVoteReminding().getRemindersEnabled().get(p.getUniqueId());
				}
				plugin.getVoteReminding().getRemindersEnabled().put(p.getUniqueId(), value);
				plugin.getPlaceholders().onUpdate(
						plugin.getVotingPluginUserManager().getVotingPluginUser(p.getUniqueId(), p.getName()), true);

				if (value) {
					sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleRemindersEnabled());
				} else {
					sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleRemindersDisabled());
				}
			}
		});

		if (plugin.getGui().isChestVoteShopEnabled()) {
			plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Shop" },
					"VotingPlugin.Commands.Vote.Shop|" + playerPerm, "Open VoteShop GUI", false) {

				@Override
				public void execute(CommandSender sender, String[] args) {
					new VoteShop(plugin, sender,
							plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender)).open();
				}
			});
			plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Shop", "(Text)" },
					"VotingPlugin.Commands.Vote.Shop|" + playerPerm, "Open VoteShop GUI", false) {

				@Override
				public void execute(CommandSender sender, String[] args) {
					if (!plugin.getGui().isChestVoteShopEnabled()) {
						sender.sendMessage(StringParser.getInstance().colorize("&cVote shop disabled"));
						return;
					}

					String identifier = args[1];
					Set<String> identifiers = plugin.getGui().getChestShopIdentifiers();
					if (ArrayUtils.getInstance().containsIgnoreCase(identifiers, identifier)) {
						for (String ident : identifiers) {
							if (ident.equalsIgnoreCase(args[1])) {
								identifier = ident;
							}
						}

						String perm = plugin.getGui().getChestVoteShopPermission(identifier);
						boolean hasPerm = false;
						if (perm.isEmpty()) {
							hasPerm = true;
						} else {
							hasPerm = sender.hasPermission(perm);
						}

						int limit = plugin.getGui().getChestShopIdentifierLimit(identifier);

						VotingPluginUser user = plugin.getVotingPluginUserManager()
								.getVotingPluginUser(sender.getName());
						boolean limitPass = true;
						if (limit > 0) {

							if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
								limitPass = false;
							}
						}

						if (!plugin.getGui().getChestVoteShopNotBuyable(identifier)) {
							if (hasPerm) {
								if (plugin.getConfigFile().isExtraVoteShopCheck()) {
									user.cache();
								}
								int points = plugin.getGui().getChestShopIdentifierCost(identifier);
								if (identifier != null) {

									if (limitPass) {
										HashMap<String, String> placeholders = new HashMap<String, String>();
										placeholders.put("identifier", identifier);
										placeholders.put("points", "" + points);
										placeholders.put("limit", "" + limit);
										if (user.removePoints(points, true)) {

											plugin.getRewardHandler().giveReward(user, plugin.getGui().getData(),
													plugin.getGui().getChestShopIdentifierRewardsPath(identifier),
													new RewardOptions().setPlaceholders(placeholders));

											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													plugin.getConfigFile().getFormatShopPurchaseMsg(), placeholders));
											if (limit > 0) {
												user.setVoteShopIdentifierLimit(identifier,
														user.getVoteShopIdentifierLimit(identifier) + 1);
											}
										} else {
											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
										}
									} else {
										user.sendMessage(plugin.getGui().getChestVoteShopLimitReached());
									}
								}

							}
						}
					} else {
						sendMessage(sender, "&cWrong voteshop item");
					}
				}
			});
		}

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "URL", "(SiteName)" },
				"VotingPlugin.Commands.Vote.URL.VoteSite", "Open VoteURL GUI for VoteSite", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteURLVoteSite(plugin, sender,
						plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender), args[1]).open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "URL" },
				"VotingPlugin.Commands.Vote.URL|" + playerPerm, "Open VoteURL GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteURL(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender),
						true).open();

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Last", "(player)" },
				"VotingPlugin.Commands.Vote.Last.Other|" + modPerm, "See other players last votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteLast(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Last" },
				"VotingPlugin.Commands.Vote.Last|" + playerPerm, "See your last votes", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteLast(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		plugin.getVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ToggleBroadcast" },
						"VotingPlugin.Commands.Vote.ToggleBroadcast|" + playerPerm,
						"Toggle whether or not you will recieve vote broadcasts", false) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager()
								.getVotingPluginUser((Player) sender);
						boolean value = !user.getDisableBroadcast();
						user.setDisableBroadcast(value);
						if (!value) {
							sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleBroadcastEnabled());
						} else {
							sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleBroadcastDisabled());
						}
					}
				});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Next", "(player)" },
				"VotingPlugin.Commands.Vote.Next.Other|" + modPerm, "See other players next votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteNext(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Points", "(player)" },
				"VotingPlugin.Commands.Vote.Points.Other|" + modPerm, "View pints of other player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
					String msg = plugin.getConfigFile().getFormatCommandsVotePoints()
							.replace("%Player%", user.getPlayerName()).replace("%Points%", "" + user.getPoints());
					if (sender instanceof Player) {
						plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender).sendMessage(msg);
					} else {
						sender.sendMessage(StringParser.getInstance().colorize(msg));
					}
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Points" },
				"VotingPlugin.Commands.Vote.Points|" + playerPerm, "View your points", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
				String msg = plugin.getConfigFile().getFormatCommandsVotePoints()
						.replace("%Player%", user.getPlayerName()).replace("%Points%", "" + user.getPoints());
				user.sendMessage(msg);
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Next" },
				"VotingPlugin.Commands.Vote.Next|" + playerPerm, "See your next votes", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteNext(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "GUI", "(player)" },
				"VotingPlugin.Commands.Vote.GUI.Other|" + modPerm, "Open VoteGUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteGUI(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "GUI" },
				"VotingPlugin.Commands.Vote.GUI|" + playerPerm, "Open VoteGUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteGUI(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		if (plugin.getGui().isLastMonthGUI() || plugin.getOptions().getDebug().equals(DebugLevel.DEV)) {
			plugin.debug("Loading last month top");
			plugin.getVoteCommand()
					.add(new CommandHandler(plugin, new String[] { "LastMonthTop" },
							"VotingPlugin.Commands.Vote.LastMonthTop|" + playerPerm,
							"Open list of Top Voters from last month") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							new VoteTopVoterLastMonth(plugin, sender,
									plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender)).open();
						}
					});
		}

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Top" },
				"VotingPlugin.Commands.Vote.Top|" + playerPerm, "Open list of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteTopVoter(plugin, sender, null, TopVoter.getDefault(), 1)
						.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTopVoter().toUpperCase()));
			}
		});

		for (final TopVoter top : TopVoter.values()) {
			String argName = top.toString();
			String perm = top.toString();
			if (argName.equals(TopVoter.AllTime.toString())) {
				argName = "All";
				perm = "All";
			}

			plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Top", argName },
					"VotingPlugin.Commands.Vote.Top." + perm + "|" + playerPerm, "Open page of Top Voters") {

				@Override
				public void execute(CommandSender sender, String[] args) {
					new VoteTopVoter(plugin, sender, null, top, 1)
							.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTopVoter().toUpperCase()));
				}
			});

			plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Top", argName, "(number)" },
					"VotingPlugin.Commands.Vote.Top." + perm + "|" + playerPerm, "Open page of Top Voters") {

				@Override
				public void execute(CommandSender sender, String[] args) {
					int page = Integer.parseInt(args[2]);
					new VoteTopVoter(plugin, sender, null, top, page)
							.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTopVoter().toUpperCase()));
				}
			});
		}

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Top", "(number)" },
				"VotingPlugin.Commands.Vote.Top|" + playerPerm, "Open page of Top Voters") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				int page = Integer.parseInt(args[1]);

				new VoteTopVoter(plugin, sender, null, TopVoter.getDefault(), page)
						.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTopVoter().toUpperCase()));
			}
		});

		plugin.getVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Party" },
						"VotingPlugin.Commands.Vote.Party|" + playerPerm,
						"View current amount of votes and how many more needed") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVoteParty().commandVoteParty(sender);
					}
				});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Today", "(number)" },
				"VotingPlugin.Commands.Vote.Today|" + playerPerm, "Open page of who Voted Today") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				int page = Integer.parseInt(args[1]);
				new VoteToday(plugin, sender, null, page).open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Today" },
				"VotingPlugin.Commands.Vote.Today|" + playerPerm, "View who list of who voted today") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteToday(plugin, sender, null, 1).open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Total", "All" },
				"VotingPlugin.Commands.Vote.Total.All|" + playerPerm, "View server total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				ArrayList<String> msg = new ArrayList<String>();

				int daily = 0;
				int weekly = 0;
				int month = 0;
				int all = 0;

				for (TopVoter top : TopVoter.values()) {
					int cTotal = 0;
					ArrayList<Integer> nums = plugin.getUserManager().getNumbersInColumn(top.getColumnName());
					for (Integer num : nums) {
						cTotal += num.intValue();
					}
					switch (top) {
					case AllTime:
						all = cTotal;
						break;
					case Daily:
						daily = cTotal;
						break;
					case Monthly:
						month = cTotal;
						break;
					case Weekly:
						weekly = cTotal;
						break;
					default:
						break;

					}
				}

				for (String s : plugin.getConfigFile().getFormatCommandsVoteTotalAll()) {
					String str = StringParser.getInstance().replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
					str = StringParser.getInstance().replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
					str = StringParser.getInstance().replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
					str = StringParser.getInstance().replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
					msg.add(str);
				}

				msg = ArrayUtils.getInstance().colorize(msg);
				sendMessage(sender, msg);

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Total", "(player)" },
				"VotingPlugin.Commands.Vote.Total.Other|" + modPerm, "View other players total votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteTotal(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Total" },
				"VotingPlugin.Commands.Vote.Total|" + playerPerm, "View your total votes", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteTotal(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Best" },
				"VotingPlugin.Commands.Vote.Best|" + playerPerm, "View your best voting", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteBest(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Best", "(player)" },
				"VotingPlugin.Commands.Vote.Best.Other|" + modPerm, "View someone's best voting") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteBest(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Streak" },
				"VotingPlugin.Commands.Vote.Streak|" + playerPerm, "View your voting streak", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteStreak(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender))
						.open();
			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Streak", "(player)" },
				"VotingPlugin.Commands.Vote.Streak.Other|" + modPerm, "View someone's voting streak") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getUserManager().userExist(args[1])) {
					new VoteStreak(plugin, sender, plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]))
							.open();
				} else {
					sendMessage(sender, StringParser.getInstance()
							.replacePlaceHolder(plugin.getConfigFile().getFormatUserNotExist(), "player", args[1]));
				}
			}
		});

		if (plugin.getConfigFile().isAllowVotePointTransfers()) {
			plugin.getVoteCommand()
					.add(new CommandHandler(plugin, new String[] { "GivePoints", "(player)", "(number)" },
							"VotingPlugin.Commands.Vote.GivePoints", "Give someone points from your points", false) {

						@Override
						public void execute(CommandSender sender, String[] args) {
							if (plugin.getConfigFile().isAllowVotePointTransfers()) {
								VotingPluginUser cPlayer = plugin.getVotingPluginUserManager()
										.getVotingPluginUser((Player) sender);
								cPlayer.cache();

								if (plugin.getUserManager().userExist(args[1])) {
									VotingPluginUser user = plugin.getVotingPluginUserManager()
											.getVotingPluginUser(args[1]);
									if (!user.isOnline()) {
										user.dontCache();
									}
									int pointsToGive = Integer.parseInt(args[2]);
									if (pointsToGive > 0) {
										if (cPlayer.getPoints() >= pointsToGive) {
											user.addPoints(pointsToGive);
											cPlayer.removePoints(pointsToGive);
											HashMap<String, String> placeholders = new HashMap<String, String>();
											placeholders.put("transfer", "" + pointsToGive);
											placeholders.put("touser", "" + user.getPlayerName());
											placeholders.put("fromuser", "" + cPlayer.getPlayerName());
											sendMessage(sender,
													StringParser.getInstance()
															.replacePlaceHolder(plugin.getConfigFile()
																	.getFormatCommandsVoteGivePointsTransferFrom(),
																	placeholders));
											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													plugin.getConfigFile().getFormatCommandsVoteGivePointsTransferTo(),
													placeholders));
										} else {
											sendMessage(sender, plugin.getConfigFile()
													.getFormatCommandsVoteGivePointsNotEnoughPoints());
										}
									} else {
										sendMessage(sender, plugin.getConfigFile()
												.getFormatCommandsVoteGivePointsNumberLowerThanZero());
									}
								} else {
									sendMessage(sender,
											StringParser.getInstance().replacePlaceHolder(
													plugin.getConfigFile()
															.getFormatCommandsVoteGivePointsNotJoinedServer(),
													"player", args[1]));
								}
							}
						}
					});
		}

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] {},
				"VotingPlugin.Commands.Vote|" + playerPerm, "See voting URLs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VotingPluginUser user = null;
				if (sender instanceof Player) {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
				}
				if (plugin.getConfigFile().isUseVoteGUIMainCommand() && sender instanceof Player) {
					// vote gui command
					new VoteGUI(plugin, sender, user).open();
				} else {
					new VoteURL(plugin, sender, user, true).open();
				}

			}
		});

		plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "List&All" },
				"VotingPlugin.Commands.Vote.List|" + playerPerm, "See voting URLs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new VoteURL(plugin, sender, null, true).open(GUIMethod.CHAT);
			}
		});

		if (plugin.getConfigFile().isAddCustomCommands()) {
			for (String ident : plugin.getConfigFile().getCustomCommands()) {
				ConfigurationSection section = plugin.getConfigFile().getCustomCommands(ident);
				@SuppressWarnings("unchecked")
				String[] args = ArrayUtils.getInstance()
						.convert((ArrayList<String>) section.getList("Args", new ArrayList<String>()));
				plugin.getVoteCommand().add(new CommandHandler(plugin, args, section.getString("Permission", ""),
						section.getString("HelpMessage", "")) {

					@SuppressWarnings("unchecked")
					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, section.getString("Message", ""));
						for (String str : (ArrayList<String>) section.getList("Commands", new ArrayList<String>())) {
							plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

								@Override
								public void run() {
									Bukkit.getServer().dispatchCommand(sender, str);
								}
							});

						}
					}
				});
			}
		}

		ArrayList<CommandHandler> avCommands = com.bencodez.advancedcore.command.CommandLoader.getInstance()
				.getBasicCommands("VotingPlugin");
		for (CommandHandler cmd : avCommands) {
			cmd.setPerm(cmd.getPerm() + "|" + playerPerm);
		}
		plugin.getVoteCommand().addAll(avCommands);

		ArrayList<CommandHandler> list = plugin.getVoteCommand();
		ArrayList<String> disabledCommands = plugin.getConfigFile().getDisabledCommands();
		for (int i = list.size() - 1; i >= 0; i--) {
			boolean remove = false;
			for (String disabled : disabledCommands) {
				CommandHandler handle = list.get(i);
				if (handle.getPerm().contains(disabled)) {
					remove = true;
				}
			}

			if (remove) {
				plugin.debug("Disabling: " + ArrayUtils.getInstance()
						.makeStringList(ArrayUtils.getInstance().convert(list.get(i).getArgs())));
				list.remove(i);
			}
		}
		ArrayList<String> disabledDefaultPerms = plugin.getConfigFile().getDisabledDefaultPermissions();
		for (CommandHandler cmd : list) {
			boolean contains = false;
			for (String dis : disabledDefaultPerms) {
				if (cmd.getPerm().contains(dis + "|")) {
					contains = true;
				}
			}
			if (contains) {
				cmd.setPerm(cmd.getPerm().replace("|" + playerPerm, ""));
				plugin.debug("Disabling VotingPlugin.Player permission on " + cmd.getPerm());

			}
		}
	}

	/**
	 * Sets the commands.
	 *
	 * @param commands the commands
	 */
	public void setCommands(HashMap<String, CommandHandler> commands) {
		this.commands = commands;
	}
}
