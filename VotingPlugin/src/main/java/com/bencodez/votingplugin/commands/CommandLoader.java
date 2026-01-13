package com.bencodez.votingplugin.commands;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permission;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.listeners.StringListener;
import com.bencodez.advancedcore.api.yml.editor.ConfigEditor;
import com.bencodez.advancedcore.command.gui.UserGUI;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.command.TabCompleteHandle;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.simpleapi.debug.DebugLevel;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.updater.Updater;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.executers.CommandAliases;
import com.bencodez.votingplugin.commands.gui.AdminGUI;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteHelp;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePerms;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePlaceholders;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePlaceholdersPlayer;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteTopPoints;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteVoteParty;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteVotePlayer;
import com.bencodez.votingplugin.commands.gui.admin.cumulative.AdminVoteCumulative;
import com.bencodez.votingplugin.commands.gui.admin.milestones.AdminVoteMilestones;
import com.bencodez.votingplugin.commands.gui.admin.votelog.AdminVoteLogMenu;
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
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoterPreviousMonths;
import com.bencodez.votingplugin.commands.gui.player.VoteTotal;
import com.bencodez.votingplugin.commands.gui.player.VoteURL;
import com.bencodez.votingplugin.commands.gui.player.VoteURLVoteSite;
import com.bencodez.votingplugin.commands.tabcompleter.AliasesTabCompleter;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesManager;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.var;

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

	public CommandLoader(VotingPluginMain plugin) {
		this.plugin = plugin;
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
		plugin.setAdminVoteCommand(new ArrayList<>());

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
					public void executeAll(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[3]);

						sender.sendMessage(MessageAPI.colorize("&cSetting all players points to " + args[3]));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
							user.setPoints(num);
						}
						sender.sendMessage(MessageAPI.colorize("&cDone setting all players points to " + args[3]));
						plugin.getPlaceholders().onUpdate();
					}

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.setPoints(Integer.parseInt(args[3]));
						sender.sendMessage(MessageAPI.colorize("&cSet " + args[1] + " points to " + args[3]));
						plugin.getPlaceholders().onUpdate(user, false);
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

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "TopPoints" },
				"VotingPlugin.Commands.AdminVote.TopPoints|" + adminPerm, "Open the top points GUI", false) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&cLoading top points, please wait...");

				new AdminVoteTopPoints(plugin, sender,
						plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender), 1).open();
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "PauseRewards" },
				"VotingPlugin.Commands.AdminVote.PauseRewards|" + adminPerm, "Pause rewards globally") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (!plugin.getOptions().isPauseRewards()) {
					plugin.getOptions().setPauseRewards(true);
					sendMessage(sender,
							"&cRewards paused, note server restart will reset pause, resume with /av resumerewards");
				} else {
					sendMessage(sender, "&cRewards already paused, use /av resumerewards to resume");
				}

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ResumeRewards" },
				"VotingPlugin.Commands.AdminVote.ResumeRewards|" + adminPerm, "Resume rewards globally") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (plugin.getOptions().isPauseRewards()) {
					plugin.getOptions().setPauseRewards(false);
					for (Player p : Bukkit.getOnlinePlayers()) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
						user.offVote();
						user.checkOfflineRewards();
					}
					plugin.setUpdate(true);
					sendMessage(sender, "&aRewards resumed");
				} else {
					sendMessage(sender, "&aRewards already resumed");
				}

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
						plugin.setUpdate(true);

					}
				});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin, new String[] { "User", "(player)", "AddPoints", "(number)" },
						"VotingPlugin.Commands.AdminVote.AddPoints|" + adminPerm, "Add to players voting points") {

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[3]);

						sender.sendMessage(
								MessageAPI.colorize("&cGiving " + "all players" + " " + args[3] + " points"));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
							user.addPoints(num);
						}
						sender.sendMessage(MessageAPI.colorize("&cGave " + "all players" + " " + args[3] + " points"));

						plugin.getPlaceholders().onUpdate();
					}

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.cache();
						int newTotal = 0;
						newTotal = user.addPoints(Integer.parseInt(args[3]));
						sender.sendMessage(MessageAPI.colorize("&cGave " + args[1] + " " + args[3] + " points" + ", "
								+ args[1] + " now has " + newTotal + " points"));

						plugin.getPlaceholders().onUpdate(user, false);

					}
				});

		plugin.getAdminVoteCommand()
				.add(new PlayerCommandHandler(plugin, new String[] { "User", "(player)", "RemovePoints", "(number)" },
						"VotingPlugin.Commands.AdminVote.RemovePoints|" + adminPerm, "Remove voting points") {

					@Override
					public void executeAll(CommandSender sender, String[] args) {
						int num = Integer.parseInt(args[3]);

						sender.sendMessage(
								MessageAPI.colorize("&cGiving " + "all players" + " " + args[3] + " points"));
						for (String uuidStr : plugin.getUserManager().getAllUUIDs()) {
							UUID uuid = UUID.fromString(uuidStr);
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
							user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
							user.removePoints(num);
						}
						sender.sendMessage(MessageAPI.colorize("&cGave " + "all players" + " " + args[3] + " points"));

						plugin.getPlaceholders().onUpdate();
					}

					@Override
					public void executeSinglePlayer(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.cache();
						user.removePoints(Integer.parseInt(args[3]));
						sender.sendMessage(MessageAPI.colorize("&cRemoved " + args[3] + " points from " + args[1] + ", "
								+ args[1] + " now has " + user.getPoints() + " points"));
						plugin.getPlaceholders().onUpdate(user, false);
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
								boolean hasSite = plugin.getVoteSiteManager().hasVoteSite(serviceSite);
								if (hasSite) {
									String siteName = plugin.getVoteSiteManager().getVoteSiteName(true, serviceSite);
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

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "CorrectServiceSites" },
						"VotingPlugin.Commands.AdminVote.CorrectServiceSites|" + adminPerm,
						"Attempt to correct invalid services sites") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						int invalid = 0;
						int fixed = 0;
						for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
							if (!site.isVaidServiceSite()) {
								invalid++;
								if (plugin.getServiceSiteHandler().contains(site.getServiceSite())) {
									fixed++;
									plugin.getConfigVoteSites().setServiceSite(site.getKey(),
											plugin.getServiceSiteHandler().match(site.getServiceSite()));
									sendMessage(sender,
											"&aChanging '" + site.getServiceSite() + "' to '"
													+ plugin.getServiceSiteHandler().match(site.getServiceSite())
													+ "' on site '" + site.getKey() + "'");
								} else {
									sendMessage(sender, "&cCouldn't find an valid service site for '"
											+ site.getServiceSite() + "' on site '" + site.getKey() + "'");
								}
							}
						}
						if (fixed > 0) {
							plugin.reload();
						}
						sendMessage(sender,
								"&aDetected " + invalid + " service sites possibly invalid, fixed " + fixed);
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
							sendMessage(sender,
									ArrayUtils.convert(new AdminVotePerms(plugin, sender, 0).listPermsDev(sender)));
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
						&& !plugin.getDescription().getVersion().endsWith("SNAPSHOT") && plugin.getUpdater() != null
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
						&& !plugin.getDescription().getVersion().endsWith("SNAPSHOT") && plugin.getUpdater() != null
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

				StringBuilder msg = new StringBuilder();

				// ---- VotingPlugin ----
				msg.append("VotingPlugin ").append(plugin.getDescription().getVersion());

				String vpBuild = plugin.getBuildNumber();
				if (vpBuild != null && !vpBuild.isEmpty() && !vpBuild.equalsIgnoreCase("NOTSET")) {
					msg.append(" (build ").append(vpBuild).append(")");
				}

				msg.append(" | ");

				// ---- AdvancedCore ----
				String acVersion = plugin.getAdvancedCoreVersion();
				msg.append("AdvancedCore ");
				msg.append((acVersion != null && !acVersion.isEmpty()) ? acVersion : "unknown");

				String acBuild = plugin.getAdvancedCoreBuildNumber();
				if (acBuild != null && !acBuild.isEmpty() && !acBuild.equalsIgnoreCase("NOTSET")) {
					msg.append(" (build ").append(acBuild).append(")");
				}

				sendMessage(sender, msg.toString());
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
					new AdminGUI(plugin).openAdminGUIVoteSiteSite((Player) sender,
							plugin.getVoteSiteManager().getVoteSite(args[1], false));
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
						+ ChatColor.GREEN + " is: " + UuidLookup.getInstance().getUUID(args[1]));

			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "PlayerName", "(uuid)" },
				"VotingPlugin.Commands.AdminVote.PlayerName|" + adminPerm, "View PlayerName of player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				try {
					sender.sendMessage(ChatColor.GREEN + "PlayerName of player " + ChatColor.DARK_GREEN + args[1]
							+ ChatColor.GREEN + " is: " + UuidLookup.getInstance().getPlayerName(
									plugin.getUserManager().getUser(UUID.fromString(args[1]), false), args[1]));
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
					sender.sendMessage(MessageAPI.colorize("&cThis command can not be done from ingame"));
					return;
				}
				for (TopVoter top : TopVoter.values()) {
					plugin.getUserManager().removeAllKeyValues(top.getColumnName(), DataType.INTEGER);
				}
				plugin.getUserManager().getDataManager().clearCache();
				plugin.setUpdate(true);
				sender.sendMessage(MessageAPI.colorize("&cCleared totals for everyone"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ResetTotal", "(TopVoter)" },
						"VotingPlugin.Commands.AdminVote.ResetTotal.All|" + adminPerm,
						"Reset specific totals for all players (DAY/WEEK/MONTH)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						if (sender instanceof Player) {
							sender.sendMessage(MessageAPI.colorize("&cThis command can not be done from ingame"));
							return;
						}

						TopVoter top = TopVoter.getTopVoter(args[1]);
						if (top.equals(TopVoter.AllTime)) {
							sendMessage(sender, "&cCan't reset all time total or invalid argument: " + args[1]);
							return;
						}
						plugin.getUserManager().removeAllKeyValues(top.getColumnName(), DataType.INTEGER);

						plugin.getUserManager().getDataManager().clearCache();
						plugin.setUpdate(true);
						sender.sendMessage(MessageAPI.colorize("&cCleared totals of " + top.getColumnName()));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ClearOfflineVoteRewards" },
				"VotingPlugin.Commands.AdminVote.ClearOfflineVoteRewards|" + adminPerm, "Reset offline votes/rewards") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				if (sender instanceof Player) {
					sender.sendMessage(MessageAPI.colorize("&cThis command can not be done from ingame"));
					return;
				}
				plugin.getUserManager().removeAllKeyValues("OfflineVotes", DataType.STRING);
				plugin.getUserManager().removeAllKeyValues(plugin.getUserManager().getOfflineRewardsPath(),
						DataType.STRING);
				plugin.getUserManager().getDataManager().clearCache();
				sender.sendMessage(MessageAPI.colorize("&cCleared offline votes/rewards"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "SetVoteStreak", "DAY", "(number)" },
						"VotingPlugin.Commands.AdminVote.SetVoteStreak.Day|" + adminPerm, "Set votestreak for player") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
								.setDayVoteStreak(Integer.parseInt(args[4]));
						sender.sendMessage(
								MessageAPI.colorize("&cSet votestreak day for '" + args[1] + "' to " + args[4]));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin,
				new String[] { "User", "(player)", "SetVoteStreak", "WEEK", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetVoteStreak.Week|" + adminPerm, "Set votestreak for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
						.setWeekVoteStreak(Integer.parseInt(args[4]));
				sender.sendMessage(MessageAPI.colorize("&cSet votestreak week for '" + args[1] + "' to " + args[4]));
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin,
				new String[] { "User", "(player)", "SetVoteStreak", "MONTH", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetVoteStreak.Month|" + adminPerm, "Set votestreak for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getVotingPluginUserManager().getVotingPluginUser(args[1])
						.setMonthVoteStreak(Integer.parseInt(args[4]));
				sender.sendMessage(MessageAPI.colorize("&cSet votestreak month for '" + args[1] + "' to " + args[4]));
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
							sender.sendMessage(MessageAPI.colorize(
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
							sender.sendMessage(
									MessageAPI.colorize("&cAdded " + top.toString() + " total for " + args[1]));
						}
					});
		}

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "User", "(player)", "ClearTotal" },
				"VotingPlugin.Commands.AdminVote.ClearTotal|" + adminPerm, "Clear Totals for player") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
				user.clearTotals();
				sender.sendMessage(MessageAPI.colorize("&cCleared totals for '" + args[1] + "'"));
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
						sender.sendMessage(MessageAPI.colorize("&cVoted sites reset for '" + args[1] + "'"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ResetVotedSite", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.ResetVotedSite|" + adminPerm,
						"Resets last voted for specific site") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
						user.resetLastVoted(plugin.getVoteSiteManager().getVoteSite(args[3], false));
						plugin.getCoolDownCheck().checkPerSite(user);
						sender.sendMessage(
								MessageAPI.colorize("&cVoted site reset for '" + args[1] + "'" + " on " + args[3]));
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "Vote", "(player)", "All" },
				"VotingPlugin.Commands.AdminVote.Vote|" + adminPerm, "Trigger manual vote") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sendMessage(sender, "&cTriggering vote for all voting sites...");
				for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
					plugin.getVoteTimer().submit(new Runnable() {

						@Override
						public void run() {
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, args[1], site.getServiceSite(),
									false);
							if (voteEvent.getVoteSite() != null) {
								if (!voteEvent.getVoteSite().isVaidServiceSite()
										&& !plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
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
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSiteManager().getVoteSite(args[2], true),
						args[1], args[2], false);
				if (voteEvent.getVoteSite() != null) {
					if (!voteEvent.getVoteSite().isVaidServiceSite()
							&& !plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
						sendMessage(sender, "&cPossible issue with service site, has the server gotten the vote from "
								+ voteEvent.getServiceSite() + "?");
					}
					if (!plugin.getConfigFile().isDisableNoServiceSiteMessage() && !isPlayer(sender)) {
						sendMessage(sender, "&cTriggering vote...");
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

			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteExact", "(PlayerExact)", "(Sitename)" },
						"VotingPlugin.Commands.AdminVote.VoteExact|" + adminPerm,
						"Trigger manual vote with exact player name") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						PlayerVoteEvent voteEvent = new PlayerVoteEvent(
								plugin.getVoteSiteManager().getVoteSite(args[2], true), args[1], args[2], false);
						if (!plugin.getConfigFile().isDisableNoServiceSiteMessage() && !isPlayer(sender)) {
							sendMessage(sender, "&cTriggering vote...");
						}
						if (voteEvent.getVoteSite() != null) {
							if (!voteEvent.getVoteSite().isVaidServiceSite()
									&& !plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
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
						for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, args[1], site.getServiceSite(),
									false);
							if (voteEvent.getVoteSite() != null) {
								if (!voteEvent.getVoteSite().isVaidServiceSite()
										&& !plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
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
						PlayerVoteEvent voteEvent = new PlayerVoteEvent(
								plugin.getVoteSiteManager().getVoteSite(args[3], true), args[1], args[3], false);
						if (!plugin.getConfigFile().isDisableNoServiceSiteMessage() && !isPlayer(sender)) {
							sendMessage(sender, "&cTriggering vote...");
						}
						if (voteEvent.getVoteSite() != null) {
							if (!voteEvent.getVoteSite().isVaidServiceSite()
									&& !plugin.getConfigFile().isDisableNoServiceSiteMessage()) {
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
								plugin.getVoteSiteManager().getVoteSite(args[3], true));
						plugin.getServer().getPluginManager().callEvent(event);
						sendMessage(sender, "&cCoolDownEndRewards forced on votesite " + args[3]);

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "Create" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Create VoteSite") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				sender.sendMessage(MessageAPI.colorize("&cCreating VoteSite..." + args[1]));

				plugin.getConfigVoteSites().generateVoteSite(args[1]);
				sender.sendMessage(MessageAPI.colorize("&cCreated VoteSite: &c&l" + args[1]));

			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Config", "TempDebug" },
						"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm,
						"Enable debug, effective until reload/restart") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getOptions().setDebug(DebugLevel.INFO);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "Config", "TempExtraDebug" },
						"VotingPlugin.Commands.AdminVote.Config.Edit|" + adminPerm,
						"Enable extra debug, effective until reload/restart") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.getOptions().setDebug(DebugLevel.EXTRA);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetServiceSite", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite SerivceSite") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteManager().getVoteSiteName(true, args[1]);
						String serviceSite = args[3];
						plugin.getConfigVoteSites().setServiceSite(voteSite, serviceSite);
						sender.sendMessage(MessageAPI
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
								for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
									if (site.isVoteDelayDaily()) {
										plugin.getCoolDownCheck().checkAllVoteSite(site);
									}
								}
							}
						}, 5, TimeUnit.SECONDS);
						sender.sendMessage(MessageAPI.colorize("&cForce checking on vote cooldown rewards"));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetVoteURL", "(string)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteURL") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteManager().getVoteSiteName(true, args[1]);
						String url = args[3];
						plugin.getConfigVoteSites().setVoteURL(voteSite, url);
						sender.sendMessage(
								MessageAPI.colorize("&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetPriority", "(number)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite Priority") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteManager().getVoteSiteName(true, args[1]);
						int value = Integer.parseInt(args[3]);
						plugin.getConfigVoteSites().setPriority(voteSite, value);
						sender.sendMessage(
								MessageAPI.colorize("&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "SetVoteDelay", "(TEXT)" },
						"VotingPlugin.Commands.AdminVote.VoteSite.Edit|" + adminPerm, "Set VoteSite VoteDelay") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String voteSite = plugin.getVoteSiteManager().getVoteSiteName(true, args[1]);
						plugin.getConfigVoteSites().setVoteDelay(voteSite, args[3]);
						sender.sendMessage(
								MessageAPI.colorize("&cSet VoteDelay to &c&l" + args[3] + "&c on &c&l" + voteSite));

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "UpdateCheck" },
				"VotingPlugin.Commands.AdminVote.UpdateCheck|" + adminPerm, "Check for update") {

			@Override
			public void execute(CommandSender sender, String[] args) {

				plugin.getBukkitScheduler().runTaskAsynchronously(plugin, new Runnable() {

					@Override
					public void run() {
						sender.sendMessage(MessageAPI.colorize("&cChecking for update..."));
						plugin.setUpdater(new Updater(plugin, 15358, false));
						final Updater.UpdateResult result = plugin.getUpdater().getResult();
						switch (result) {
						case FAIL_SPIGOT: {
							sender.sendMessage(MessageAPI
									.colorize("&cFailed to check for update for &c&l" + plugin.getName() + "&c!"));
							break;
						}
						case NO_UPDATE: {
							sender.sendMessage(MessageAPI.colorize("&c&l" + plugin.getName()
									+ " &cis up to date! Version: &c&l" + plugin.getUpdater().getVersion()));
							break;
						}
						case UPDATE_AVAILABLE: {
							sender.sendMessage(MessageAPI.colorize(
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
						String voteSite = plugin.getVoteSiteManager().getVoteSiteName(false, args[1]);
						boolean value = Boolean.parseBoolean(args[3]);

						plugin.getConfigVoteSites().setEnabled(voteSite, value);
						sender.sendMessage(MessageAPI.colorize("&cSet votesite " + voteSite + " enabled to " + value));

					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteSite", "(sitename)", "Check" },
				"VotingPlugin.Commands.AdminVote.VoteSite.Check|" + adminPerm, "Check to see if VoteSite is valid") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				String siteName = args[1];
				if (!plugin.getConfigVoteSites().isServiceSiteGood(siteName)) {
					sender.sendMessage(MessageAPI.colorize("&cServiceSite is invalid, votes may not work properly"));
				} else {
					String service = plugin.getConfigVoteSites().getServiceSite(siteName);
					if (plugin.getServerData().getServiceSites().contains(service)) {
						sender.sendMessage(MessageAPI.colorize("&aServiceSite is properly setup"));
					} else {
						sender.sendMessage(
								MessageAPI.colorize("&cService may not be valid, haven't recieved a vote from "
										+ service + ", see /av servicesites"));
					}

				}
				if (!plugin.getConfigVoteSites().isVoteURLGood(siteName)) {
					sender.sendMessage(MessageAPI.colorize("&cVoteURL is invalid"));
				} else {
					sender.sendMessage(MessageAPI.colorize("&aVoteURL is properly setup"));
				}
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "BackgroundUpdate" },
				"VotingPlugin.Commands.AdminVote.BackgroundUpdate|" + adminPerm, "Force a background update") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.setUpdate(true);
				plugin.update();
				sender.sendMessage(MessageAPI.colorize("&cUpdating..."));
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "ClearOfflineVotes" },
				"VotingPlugin.Commands.AdminVote.ClearOfflineVotes|" + adminPerm, "Clear all offline votes") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getUserManager().removeAllKeyValues("OfflineVotes", DataType.STRING);
				plugin.getUserManager().getDataManager().clearCache();
				sender.sendMessage(MessageAPI.colorize("&cOffline votes Cleared"));
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
								plugin.getShopFile().getShopIdentifierRewardsPath(args[3]), new RewardOptions());
						sendMessage(sender, "&cVoteShop " + args[3] + " forced");
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
							new String[] { "User", "(player)", "ForceVoteStreak", str, "(Text)" },
							"VotingPlugin.Commands.AdminVote.ForceVoteStreak|" + adminPerm,
							"Force a votestreak reward for " + str) {

						@Override
						public void execute(CommandSender sender, String[] args) {
							String num = args[4];
							if (num.contains("-")) {
								num = num.replaceAll("-", "");
							}
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(args[1]);
							plugin.getSpecialRewards().giveVoteStreakReward(null, user, user.isOnline(), str, args[4],
									parseInt(num), plugin.getBungeeSettings().isUseBungeecoord());
							sendMessage(sender, "&cVoteStreak " + str + " " + args[4] + " forced");
						}
					});
		}

		// /av votemilestone forcegroup <player> <group>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "ForceGroup", "(player)", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.ForceGroup|" + adminPerm,
						"Force execute VoteMilestones for a group (skips totals, respects limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String group = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						int executed = vm.forceGroup(user, group, false, null);

						sendMessage(sender, "&aForced VoteMilestonesGroup (limits ON) for &e" + player + "&a group=&e"
								+ group + "&a executed=&e" + executed);
					}
				});

		// /av votemilestone forcegroupnolimits <player> <group>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "ForceGroupNoLimits", "(player)", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.ForceGroupNoLimits|" + adminPerm,
						"Force execute VoteMilestones for a group (skips totals, bypasses limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String group = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						int executed = vm.forceGroup(user, group, true, null);

						sendMessage(sender, "&aForced VoteMilestonesGroup (limits OFF) for &e" + player + "&a group=&e"
								+ group + "&a executed=&e" + executed);
					}
				});

		// /av votemilestone force <player> <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteMilestone&VM", "Force", "(player)", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.Force|" + adminPerm,
						"Force execute a specific VoteMilestone (skips totals, respects limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						boolean ok = vm.forceMilestone(user, milestoneId, false, null);

						sendMessage(sender,
								ok ? "&aForced VoteMilestone (limits ON) for &e" + player + "&a milestone=&e"
										+ milestoneId
										: "&cFailed to force milestone (not found/disabled/blocked by limit): &e"
												+ milestoneId);
					}
				});

		// /av votemilestone forcenolimits <player> <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "ForceNoLimits", "(player)", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.ForceNoLimits|" + adminPerm,
						"Force execute a specific VoteMilestone (skips totals, bypasses limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						boolean ok = vm.forceMilestone(user, milestoneId, true, null);

						sendMessage(sender,
								ok ? "&aForced VoteMilestone (limits OFF) for &e" + player + "&a milestone=&e"
										+ milestoneId
										: "&cFailed to force milestone (not found/disabled): &e" + milestoneId);
					}
				});

		// /av votemilestone previewmilestone <player> <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "PreviewMilestone", "(player)", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.PreviewMilestone|" + adminPerm,
						"Preview a specific VoteMilestone (shows total/match/limit; does not execute)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						String group = vm.getGroupForMilestoneId(milestoneId);
						if (group == null) {
							sendMessage(sender, "&cUnknown milestone: &e" + milestoneId);
							return;
						}

						sendMessage(sender, "&aVoteMilestone preview for &e" + player + "&a milestone=&e" + milestoneId
								+ "&a (group=&e" + group + "&a)");
						for (String line : vm.previewGroup(user, group, null)) {
							if (line == null) {
								continue;
							}
							if (line.startsWith("group=") || line.toLowerCase().startsWith(milestoneId.toLowerCase())) {
								sendMessage(sender, "&7- &f" + line);
							}
						}
					}
				});

		// /av votemilestone statusmilestone <player> <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "StatusMilestone", "(player)", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.StatusMilestone|" + adminPerm,
						"Show status for a specific VoteMilestone (given/missing/due/pending; does not execute)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						String group = vm.getGroupForMilestoneId(milestoneId);
						if (group == null) {
							sendMessage(sender, "&cUnknown milestone: &e" + milestoneId);
							return;
						}

						sendMessage(sender, "&aVoteMilestone status for &e" + player + "&a milestone=&e" + milestoneId
								+ "&a (group=&e" + group + "&a)");
						java.util.List<String> lines = vm.statusGroup(user, group, null);
						for (String line : lines) {
							if (line == null) {
								continue;
							}
							if (line.toLowerCase().startsWith(milestoneId.toLowerCase())) {
								sendMessage(sender, "&7- &f" + line);
							}
						}
					}
				});

		// /av user <player> forcevotemilestone <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "User", "(player)", "ForceVoteMilestone", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.User.ForceVoteMilestone|" + adminPerm,
						"Force execute a specific VoteMilestone (skips totals, respects limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[1];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						boolean ok = vm.forceMilestone(user, milestoneId, false, null);

						sendMessage(sender,
								ok ? "&aForced VoteMilestone (limits ON) for &e" + player + "&a milestone=&e"
										+ milestoneId
										: "&cFailed to force milestone (not found/disabled/blocked by limit): &e"
												+ milestoneId);
					}
				});

		// /av user <player> forcevotemilestonenolimits <milestoneId>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "User", "(player)", "ForceVoteMilestoneNoLimits", "(milestone)" },
						"VotingPlugin.Commands.AdminVote.User.ForceVoteMilestoneNoLimits|" + adminPerm,
						"Force execute a specific VoteMilestone (skips totals, bypasses limits)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[1];
						String milestoneId = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						boolean ok = vm.forceMilestone(user, milestoneId, true, null);

						sendMessage(sender,
								ok ? "&aForced VoteMilestone (limits OFF) for &e" + player + "&a milestone=&e"
										+ milestoneId
										: "&cFailed to force milestone (not found/disabled): &e" + milestoneId);
					}
				});

		// /av user <player> resetvotemilestonelimits [group]
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "User", "(player)", "ResetVoteMilestoneLimits", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.User.ResetVoteMilestoneLimits|" + adminPerm,
						"Reset VoteMilestone limits for a player (group)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[1];
						String group = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						int removed = vm.resetLimits(user, group);

						sendMessage(sender, "&aReset VoteMilestone limits for &e" + player + "&a group=&e" + group
								+ "&a removed=&e" + removed);
					}
				});

		// /av user <player> resetvotemilestonelimits (no group)
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "User", "(player)", "ResetVoteMilestoneLimits" },
						"VotingPlugin.Commands.AdminVote.User.ResetVoteMilestoneLimits|" + adminPerm,
						"Reset ALL VoteMilestone limits for a player") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[1];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
						int removed = vm.resetLimits(user, null);

						sendMessage(sender,
								"&aReset ALL VoteMilestone limits for &e" + player + "&a cleared=&e" + removed);
					}
				});

		// /av votemilestone preview <player> <group>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "Preview", "(player)", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.Preview|" + adminPerm,
						"Preview VoteMilestones for a group (shows totals/matches; does not execute)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String group = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);

						sendMessage(sender, "&aVoteMilestone preview for &e" + player + "&a group=&e" + group);
						for (String line : vm.previewGroup(user, group, null)) {
							sendMessage(sender, "&7- &f" + line);
						}
					}
				});

		// /av votemilestone status <player> <group>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin,
						new String[] { "VoteMilestone&VM", "Status", "(player)", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.Status|" + adminPerm,
						"Show VoteMilestone status for a group (current total; does not execute)") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String player = args[2];
						String group = args[3];

						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);

						java.util.List<String> lines = vm.statusGroup(user, group, null);
						sendMessage(sender, "&aVoteMilestone status for &e" + player + "&a group=&e" + group);
						for (String line : lines) {
							sendMessage(sender, "&7- &f" + line);
						}
					}
				});

		// /av votemilestone list <group>
		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "VoteMilestone&VM", "List", "(milestonegroup)" },
						"VotingPlugin.Commands.AdminVote.VoteMilestone.List|" + adminPerm,
						"List VoteMilestones in a group") {

					@Override
					public void execute(CommandSender sender, String[] args) {
						VoteMilestonesManager vm = plugin.getVoteMilestonesManager();
						if (vm == null) {
							sendMessage(sender, "&cVoteMilestonesManager is not available");
							return;
						}

						String group = args[2];

						sendMessage(sender, "&aVoteMilestones in group &e" + group + "&a:");
						for (String line : vm.list(group)) {
							sendMessage(sender, "&7- &f" + line);
						}
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "PurgeNoData" },
				"VotingPlugin.PurgeNoData",
				"Purge players from database with no data and haven't been online for a number of days (Set in Config.yml)") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				plugin.getVotingPluginUserManager().purgeOldPlayersNowNoData();
				sendMessage(sender, "&cPurged players with no data");
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "RemoveOfflineUUIDs" },
						"VotingPlugin.Commands.AdminVote.RemoveOfflineUUIDs|" + adminPerm,
						"Purges database of offline UUIDs, keeps online UUIDs", true, true) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						if (!plugin.getOptions().isOnlineMode()) {
							sendMessage(sender, "&cNot in online mode!");
							return;
						}
						int amount = 0;
						for (String uuid : plugin.getUserManager().getAllUUIDs()) {
							if (uuid.charAt(14) == '3') {
								plugin.getUserManager().removeUUID(UUID.fromString(uuid));
								amount++;
							}

						}
						sendMessage(sender, "&cOffline UUIDs purged: " + amount);
					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "RemoveOnlineUUIDs" },
						"VotingPlugin.Commands.AdminVote.RemoveOnlineUUIDs|" + adminPerm,
						"Purges database of online UUIDs, keeps proper offline UUIDs", true, true) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						if (plugin.getOptions().isOnlineMode()) {
							sendMessage(sender, "&cNot in offle mode!");
							return;
						}
						int amount = 0;
						for (String uuid : plugin.getUserManager().getAllUUIDs()) {
							if (uuid.charAt(14) != '3') {
								plugin.getUserManager().removeUUID(UUID.fromString(uuid));
								amount++;
								continue;
							}

							String name = UuidLookup.getInstance()
									.getPlayerName(plugin.getUserManager().getUser(UUID.fromString(uuid)), uuid, true);

							if (name == null) {
								continue;
							}

							if (!UuidLookup.getInstance().getUUID(name).toString().equals(uuid)) {
								plugin.getUserManager().removeUUID(UUID.fromString(uuid));
								amount++;
								continue;
							}

						}
						sendMessage(sender, "&cOffline UUIDs purged: " + amount);
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

		// /av MergeOfflineUUIDs [true/false]
		// - false (default) = dry run (prints what would change)
		// - true = perform fixes + merges
		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "MergeOfflineUUIDs", "(Boolean)" },
				"VotingPlugin.Commands.AdminVote.MergeOfflineUUIDs|" + adminPerm,
				"Offline-mode data repair: fixes incorrect UUIDs for names, merges duplicates, and normalizes PlayerName casing. Use /av MergeOfflineUUIDs true to apply.",
				true, true) {

			@Override
			public void execute(CommandSender sender, String[] args) {
				boolean apply = args.length > 1 && args[1] != null && args[1].equalsIgnoreCase("true");

				if (plugin.getOptions().isOnlineMode()) {
					sendMessage(sender,
							"&cServer is in online-mode. This command is intended for offline-mode UUID repairs.");
					return;
				}

				ArrayList<String> allUuids = plugin.getUserManager().getAllUUIDs();

				int scanned = 0;
				int invalidUuidStrings = 0;

				int rowsNeedingMove = 0; // uuid doesn't match expected offline uuid for name
				int duplicateGroups = 0; // multiple uuids for the same normalized name
				int rowsRemoved = 0; // rows deleted
				int canonicalEnsured = 0; // canonical rows created/ensured
				int groupsApplied = 0; // groups repaired

				// normName -> list of uuids (as strings) that claim to be that player
				Map<String, List<String>> byNormName = new HashMap<>();

				for (String uuid : allUuids) {
					if (uuid == null) {
						continue;
					}
					uuid = uuid.trim();
					if (uuid.isEmpty()) {
						continue;
					}
					scanned++;

					UUID parsed;
					try {
						parsed = UUID.fromString(uuid);
					} catch (Exception e) {
						invalidUuidStrings++;
						continue;
					}

					// Load user and read PlayerName
					var user = plugin.getUserManager().getUser(parsed);
					if (user == null) {
						continue;
					}

					user.userDataFetechMode(com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);

					String name = user.getData().getString("PlayerName",
							com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
					if (name == null) {
						name = "";
					}
					name = name.trim();
					if (name.isEmpty()) {
						continue;
					}

					String norm = name.toLowerCase(java.util.Locale.ROOT);

					byNormName.computeIfAbsent(norm, k -> new ArrayList<>()).add(uuid);
				}

				// Helpers
				java.util.function.Function<String, String> offlineUuidForNormName = (normName) -> java.util.UUID
						.nameUUIDFromBytes(
								("OfflinePlayer:" + normName).getBytes(java.nio.charset.StandardCharsets.UTF_8))
						.toString();

				java.util.function.Function<String, Integer> getIntSafe = (s) -> {
					try {
						return Integer.parseInt(s);
					} catch (Exception e) {
						return 0;
					}
				};

				for (Map.Entry<String, List<String>> entry : byNormName.entrySet()) {
					String normName = entry.getKey();
					List<String> uuids = entry.getValue();
					if (uuids == null || uuids.isEmpty()) {
						continue;
					}

					String canonicalUuid = offlineUuidForNormName.apply(normName);

					// Count groups with dupes (for reporting)
					if (uuids.size() > 1) {
						duplicateGroups++;
					}

					// Determine if any uuid in this name-group is NOT the canonical uuid
					boolean needsFix = false;
					for (String u : uuids) {
						if (!u.equalsIgnoreCase(canonicalUuid)) {
							needsFix = true;
							break;
						}
					}

					if (!needsFix) {
						// Still optionally normalize PlayerName casing to match normName (optional)
						// but we won't touch anything if dry run and no changes needed.
						continue;
					}

					// This group has either duplicates or "wrong uuid for name"
					rowsNeedingMove += (int) uuids.stream().filter(u -> !u.equalsIgnoreCase(canonicalUuid)).count();

					if (!apply) {
						sendMessage(sender, "&e[DRY] &7Repair for &f" + normName + "&7 -> canonical &a" + canonicalUuid
								+ "&7, found &f" + uuids.size() + "&7 rows: &f" + uuids);
						continue;
					}

					try {
						// Ensure canonical user exists/loaded
						var canonicalUser = plugin.getUserManager().getUser(UUID.fromString(canonicalUuid));
						canonicalUser.userDataFetechMode(com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
						canonicalEnsured++;

						// Merge accumulators
						int allTime = 0, month = 0, week = 0, day = 0, points = 0;
						Map<String, Long> lastVotes = new HashMap<>();

						// Merge ALL rows in this group into canonical (including canonical if present)
						for (String u : uuids) {
							UUID uid;
							try {
								uid = UUID.fromString(u);
							} catch (Exception e) {
								continue;
							}

							var fromUser = plugin.getUserManager().getUser(uid);
							if (fromUser == null) {
								continue;
							}
							fromUser.userDataFetechMode(com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);

							allTime += fromUser.getData().getInt("AllTimeTotal",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
							month += fromUser.getData().getInt("MonthTotal",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
							week += fromUser.getData().getInt("WeeklyTotal",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
							day += fromUser.getData().getInt("DailyTotal",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
							points += fromUser.getData().getInt("Points",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);

							String lv = fromUser.getData().getString("LastVotes",
									com.bencodez.advancedcore.api.user.UserDataFetchMode.NO_CACHE);
							if (lv != null && !lv.trim().isEmpty()) {
								for (String line : lv.split(java.util.regex.Pattern.quote("%line%"))) {
									String[] parts = line.split(java.util.regex.Pattern.quote("//"));
									if (parts.length != 2) {
										continue;
									}
									String site = parts[0];
									long t;
									try {
										t = Long.parseLong(parts[1]);
									} catch (Exception ex) {
										continue;
									}
									lastVotes.merge(site, t, Math::max);
								}
							}
						}

						// Write merged values into canonical
						canonicalUser.getData().setInt("AllTimeTotal", allTime);
						canonicalUser.getData().setInt("MonthTotal", month);
						canonicalUser.getData().setInt("WeeklyTotal", week);
						canonicalUser.getData().setInt("DailyTotal", day);
						canonicalUser.getData().setInt("Points", points);

						// Rebuild LastVotes string
						if (!lastVotes.isEmpty()) {
							StringBuilder sb = new StringBuilder();
							boolean first = true;
							for (Map.Entry<String, Long> lv : lastVotes.entrySet()) {
								if (!first) {
									sb.append("%line%");
								}
								first = false;
								sb.append(lv.getKey()).append("//").append(lv.getValue());
							}
							canonicalUser.getData().setString("LastVotes", sb.toString());
						} else {
							// optional: clear if none
							// canonicalUser.getData().setString("LastVotes", "");
						}

						// Normalize stored PlayerName (your choice):
						// - If you want to preserve "nice" casing, you could pick a representative name
						// instead.
						// - For strict consistency, store lowercased (normName) so future comparisons
						// are stable.
						canonicalUser.getData().setString("PlayerName", normName);

						// Delete ALL non-canonical UUID rows for this normName
						for (String u : uuids) {
							if (u.equalsIgnoreCase(canonicalUuid)) {
								continue;
							}
							try {
								plugin.getUserManager().removeUUID(UUID.fromString(u));
								rowsRemoved++;
							} catch (Exception e) {
								plugin.debug(e);
							}
						}

						groupsApplied++;
						sendMessage(sender, "&aRepaired &f" + normName + " &a-> &f" + canonicalUuid + " &a(merged "
								+ uuids.size() + " rows, removed " + (uuids.size() - 1) + ")");

					} catch (Exception e) {
						sendMessage(sender, "&cFailed repairing group for &f" + normName + "&c: " + e.getMessage());
						plugin.debug(e);
					}
				}

				sendMessage(sender,
						"&7Scanned: &f" + scanned + " &7InvalidUUIDStrings: &f" + invalidUuidStrings
								+ " &7GroupsWithDupes: &f" + duplicateGroups + " &7RowsNeedingMove: &f"
								+ rowsNeedingMove + " &7CanonicalEnsured: &f" + canonicalEnsured + " &7RowsRemoved: &f"
								+ rowsRemoved + " &7GroupsApplied: &f" + groupsApplied + (apply ? "" : " &e(DRY RUN)"));
			}
		});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "MergeDataFrom", "(UserStorage)" },
						"VotingPlugin.Commands.AdminVote.MergeDataFrom|" + adminPerm,
						"Merge player totals from other storage type", true, true) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						plugin.loadUserAPI(UserStorage.value(args[1]));

						if (plugin.getMysql() != null) {
							plugin.getMysql().clearCacheBasic();
						}

						HashMap<UUID, ArrayList<Column>> cols = plugin.getUserManager()
								.getAllKeys(UserStorage.value(args[1]));
						Queue<Entry<UUID, ArrayList<Column>>> players = new LinkedList<>(cols.entrySet());
						ArrayList<String> uuids = plugin.getUserManager().getAllUUIDs();

						while (players.size() > 0) {
							Entry<UUID, ArrayList<Column>> entry = players.poll();
							VotingPluginUser user = plugin.getVotingPluginUserManager()
									.getVotingPluginUser(entry.getKey(), false);
							user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
							if (uuids.contains(entry.getKey().toString())) {
								user.mergeData(user.getData().convert(entry.getValue()));
							} else {
								user.getData().setValues(plugin.getStorageType(),
										user.getData().convert(entry.getValue()));
							}

							sendMessage(sender, "Finished merge for " + user.getUUID() + ", " + players.size()
									+ " more left to go!");

							if (players.size() % 50 == 0) {
								sendMessage(sender,
										"Working on converting data, about " + players.size() + " left to go!");
							}
						}
						plugin.getUserManager().getDataManager().clearCache();
						sendMessage(sender, "Merge finished");

					}
				});

		plugin.getAdminVoteCommand()
				.add(new CommandHandler(plugin, new String[] { "ClearDiscordMessageID", "(topvoter)" },
						"VotingPlugin.Commands.AdminVote.ClearDiscordMessageID",
						"Clear discord message ID for top voter", true, true) {

					@Override
					public void execute(CommandSender sender, String[] args) {
						String topVoter = args[1];
						TopVoter top = TopVoter.getTopVoter(topVoter);
						plugin.getServerData().setTopVoterMessageId(top, 0);
						plugin.getDiscordHandler().getTopVoterMessageIds().put(top, 0L);
						sendMessage(sender, "&cCleared discord message ID for " + top.toString());
					}
				});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteLog" },
				"VotingPlugin.Commands.AdminVote.VoteLog|" + adminPerm, "See vote logs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteLogMenu(plugin, sender, plugin.getVoteLogMysqlTable(), 60).open();
			}
		});

		plugin.getAdminVoteCommand().add(new CommandHandler(plugin, new String[] { "VoteLog", "(Number)" },
				"VotingPlugin.Commands.AdminVote.VoteLog|" + adminPerm, "See vote logs") {

			@Override
			public void execute(CommandSender sender, String[] args) {
				new AdminVoteLogMenu(plugin, sender, plugin.getVoteLogMysqlTable(), Integer.parseInt(args[1])).open();
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

	private final Set<String> aliasCommandNames = new HashSet<>();

	public Set<String> getAliasCommandNames() {
		return aliasCommandNames;
	}

	/**
	 * Load aliases.
	 */
	public void loadAliases() {
		commands = new HashMap<>();
		aliasCommandNames.clear();

		// If false: still wire permissions, but don't wire alias executors/tab
		// completers.
		final boolean enableAliases = plugin.getConfigFile().isLoadCommandAliases();

		// ---------------------------
		// /vote aliases
		// ---------------------------
		for (CommandHandler cmdHandle : plugin.getVoteCommand()) {
			int argLength = cmdHandle.getArgs().length;
			String arg0 = "";
			if (argLength > 0) {
				arg0 = cmdHandle.getArgs()[0];
			}

			String[] perms = cmdHandle.getPerm().split(Pattern.quote("|"));
			String basePerm = perms[0];

			// Parent/child permission wiring (always done)
			try {
				if (perms.length > 1) {
					plugin.devDebug("Adding child perm " + perms[0] + " to " + perms[1] + " from /vote" + arg0);
					Permission p = Bukkit.getPluginManager().getPermission(perms[1]);
					if (p != null) {
						p.getChildren().put(perms[0], true);
						p.recalculatePermissibles();
					} else {
						plugin.debug("Parent permission " + perms[1] + " not found for /vote" + arg0);
					}
				}
			} catch (Exception e) {
				plugin.debug("Failed to set permission for /vote" + arg0);
			}

			if (argLength > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");

				for (String arg : args) {
					String commandName = "vote" + arg;
					commands.put(commandName, cmdHandle);
					aliasCommandNames.add(commandName.toLowerCase(Locale.ROOT));

					try {
						PluginCommand command = plugin.getCommand(commandName);
						if (command == null) {
							plugin.devDebug("Command " + commandName + " not found in plugin.yml");
							continue;
						}

						// Ensure the base permission is set on the command
						String currentPerm = command.getPermission();
						if (currentPerm == null || currentPerm.length() > basePerm.length()) {
							command.setPermission(basePerm);
						}

						if (enableAliases) {
							// Normal behavior: executor + tab completer
							command.setExecutor(new CommandAliases(cmdHandle, false));
							command.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle, false));

							// Track aliases from plugin.yml too
							for (String str : command.getAliases()) {
								commands.put(str, cmdHandle);
								aliasCommandNames.add(str.toLowerCase(Locale.ROOT));
							}
						} else {
							// Disabled: keep perms, but stub executor.
							// Tab hiding is done in PlayerCommandSendEvent.
							command.setExecutor((sender, cmd, label, args1) -> {
								sender.sendMessage(ChatColor.RED + "This command is currently disabled.");
								return true;
							});
							command.setTabCompleter(null);
						}
					} catch (Exception ex) {
						plugin.devDebug(
								"Failed to load command and tab completer for /vote" + arg + ": " + ex.getMessage());
					}
				}
			}
		}

		// ---------------------------
		// /adminvote aliases
		// ---------------------------
		for (CommandHandler cmdHandle : plugin.getAdminVoteCommand()) {
			int argLength = cmdHandle.getArgs().length;
			String arg0 = "";
			if (argLength > 0) {
				arg0 = cmdHandle.getArgs()[0];
			}

			String[] perms = cmdHandle.getPerm().split(Pattern.quote("|"));
			String basePerm = perms[0];

			// Parent/child permission wiring (always done)
			try {
				if (perms.length > 1) {
					plugin.devDebug("Adding child perm " + perms[0] + " to " + perms[1] + " from /adminvote" + arg0);
					Permission p = Bukkit.getPluginManager().getPermission(perms[1]);
					if (p != null) {
						p.getChildren().put(perms[0], true);
						p.recalculatePermissibles();
					} else {
						plugin.debug("Parent permission " + perms[1] + " not found for /adminvote" + arg0);
					}
				}
			} catch (Exception e) {
				plugin.debug("Failed to set permission for /adminvote" + arg0);
			}

			if (argLength > 0) {
				String[] args = cmdHandle.getArgs()[0].split("&");

				for (String arg : args) {
					String commandName = "adminvote" + arg;
					commands.put(commandName, cmdHandle);
					aliasCommandNames.add(commandName.toLowerCase(Locale.ROOT));

					try {
						PluginCommand command = plugin.getCommand(commandName);
						if (command == null) {
							plugin.devDebug("Command " + commandName + " not found in plugin.yml");
							continue;
						}

						// Ensure the base permission is set on the command
						String currentPerm = command.getPermission();
						if (currentPerm == null || currentPerm.length() > basePerm.length()) {
							command.setPermission(basePerm);
						}

						if (enableAliases) {
							// Normal behavior
							command.setExecutor(new CommandAliases(cmdHandle, true));
							command.setTabCompleter(new AliasesTabCompleter().setCMDHandle(cmdHandle, true));

							// Track aliases from plugin.yml too
							for (String str : command.getAliases()) {
								commands.put(str, cmdHandle);
								aliasCommandNames.add(str.toLowerCase(Locale.ROOT));
							}
						} else {
							// Disabled: stub executor; hiding from tab is done by listener
							command.setExecutor((sender, cmd, label, args1) -> {
								sender.sendMessage(ChatColor.RED + "This command is currently disabled.");
								return true;
							});
							command.setTabCompleter(null);
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
								ArrayList<String> voteSites = new ArrayList<>();
								for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
									voteSites.add(voteSite.getKey());
								}
								new ValueRequest().requestString(player, "", ArrayUtils.convert(voteSites), true,
										new StringListener() {

											@Override
											public void onInput(Player player, String value) {
												PlayerVoteEvent voteEvent = new PlayerVoteEvent(
														plugin.getVoteSiteManager().getVoteSite(value, true),
														UserGUI.getInstance().getCurrentPlayer(player),
														plugin.getVoteSiteManager().getVoteSiteServiceSite(value),
														false);
												plugin.getServer().getPluginManager().callEvent(voteEvent);

												player.sendMessage("Forced vote for "
														+ UserGUI.getInstance().getCurrentPlayer(player) + " on "
														+ value);
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
		ArrayList<String> sites = new ArrayList<>();
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			sites.add(site.getKey());
		}

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(Sitename)", sites) {

			@Override
			public void reload() {
				ArrayList<String> sites = new ArrayList<>();
				for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
					sites.add(site.getKey());
				}
				setReplace(sites);
			}

			@Override
			public void updateReplacements() {
			}
		});

		ArrayList<String> topVoter = new ArrayList<>();
		for (TopVoter top : TopVoter.values()) {
			topVoter.add(top.toString());
		}

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(topvoter)", topVoter) {

			@Override
			public void reload() {
				ArrayList<String> topVoter = new ArrayList<>();
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
				ArrayList<String> sites = new ArrayList<>();
				for (String str : plugin.getShopFile().getShopIdentifiers()) {
					sites.add(str);
				}
				setReplace(sites);
			}

			@Override
			public void updateReplacements() {
			}
		});

		ArrayList<String> milestones = new ArrayList<>();
		plugin.getVoteMilestonesManager().getConfig().getMilestones().values().forEach(milestone -> {
			milestones.add(milestone.getId());
		});

		TabCompleteHandler.getInstance().addTabCompleteOption(new TabCompleteHandle("(milestone)", milestones) {

			@Override
			public void reload() {
				ArrayList<String> milestones = new ArrayList<>();
				plugin.getVoteMilestonesManager().getConfig().getMilestones().values().forEach(milestone -> {
					milestones.add(milestone.getId());
				});
				setReplace(milestones);
			}

			@Override
			public void updateReplacements() {
			}
		});

		ArrayList<String> milestoneGroups = new ArrayList<>();
		plugin.getVoteMilestonesManager().getGroupModes().keySet().forEach(milestone -> {
			milestoneGroups.add(milestone);
		});

		TabCompleteHandler.getInstance()
				.addTabCompleteOption(new TabCompleteHandle("(milestonegroup)", milestoneGroups) {

					@Override
					public void reload() {
						ArrayList<String> milestoneGroups = new ArrayList<>();
						plugin.getVoteMilestonesManager().getGroupModes().keySet().forEach(milestone -> {
							milestoneGroups.add(milestone);
						});
						setReplace(milestoneGroups);
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
		plugin.setVoteCommand(new ArrayList<>());
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
				plugin.getVoteReminding().saveReminds();

				if (value) {
					sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleRemindersEnabled());
				} else {
					sendMessage(sender, plugin.getConfigFile().getFormatCommandsVoteToggleRemindersDisabled());
				}
			}
		});

		if (plugin.getShopFile().isVoteShopEnabled()) {
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
					if (!plugin.getShopFile().isVoteShopEnabled()) {
						sender.sendMessage(MessageAPI.colorize("&cVote shop disabled"));
						return;
					}

					String identifier = args[1];
					Set<String> identifiers = plugin.getShopFile().getShopIdentifiers();
					if (ArrayUtils.containsIgnoreCase(identifiers, identifier)) {
						for (String ident : identifiers) {
							if (ident.equalsIgnoreCase(args[1])) {
								identifier = ident;
							}
						}

						String perm = plugin.getShopFile().getVoteShopPermission(identifier);
						boolean hasPerm = false;
						if (perm.isEmpty()) {
							hasPerm = true;
						} else {
							hasPerm = sender.hasPermission(perm);
						}

						int limit = plugin.getShopFile().getShopIdentifierLimit(identifier);

						VotingPluginUser user = plugin.getVotingPluginUserManager()
								.getVotingPluginUser(sender.getName());
						boolean limitPass = true;
						if (limit > 0) {

							if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
								limitPass = false;
							}
						}

						if (!plugin.getShopFile().getVoteShopNotBuyable(identifier)) {
							if (hasPerm) {
								if (plugin.getConfigFile().isExtraVoteShopCheck()) {
									user.cache();
								}
								int points = plugin.getShopFile().getShopIdentifierCost(identifier);
								if (identifier != null) {

									if (limitPass) {
										HashMap<String, String> placeholders = new HashMap<>();
										placeholders.put("identifier", identifier);
										placeholders.put("points", "" + points);
										placeholders.put("limit", "" + limit);
										if (user.removePoints(points, true)) {

											plugin.getRewardHandler().giveReward(user, plugin.getShopFile().getData(),
													plugin.getShopFile().getShopIdentifierRewardsPath(identifier),
													new RewardOptions().setPlaceholders(placeholders));

											user.sendMessage(PlaceholderUtils.replacePlaceHolder(
													plugin.getConfigFile().getFormatShopPurchaseMsg(), placeholders));
											if (limit > 0) {
												user.setVoteShopIdentifierLimit(identifier,
														user.getVoteShopIdentifierLimit(identifier) + 1);
											}
										} else {
											user.sendMessage(PlaceholderUtils.replacePlaceHolder(
													plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
										}
									} else {
										user.sendMessage(plugin.getShopFile().getVoteShopLimitReached());
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
					sendMessage(sender, PlaceholderUtils
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
					sendMessage(sender, PlaceholderUtils
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
						sender.sendMessage(MessageAPI.colorize(msg));
					}
				} else {
					sendMessage(sender, PlaceholderUtils
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
					sendMessage(sender, PlaceholderUtils
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

		if (plugin.getConfigFile().isStoreMonthTotalsWithDate()
				|| plugin.getOptions().getDebug().equals(DebugLevel.DEV)) {
			plugin.getVoteCommand()
					.add(new CommandHandler(plugin, new String[] { "PreviousMonthsTotals" },
							"VotingPlugin.Commands.Vote.PreviousMonthsTotals",
							"Open list of Top Voters from all known previous months") {

						@Override
						public void execute(CommandSender sender, String[] args) {
							new VoteTopVoterPreviousMonths(plugin, sender,
									plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender), 0).open();
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
				ArrayList<String> msg = new ArrayList<>();

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
					String str = MessageAPI.replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
					str = MessageAPI.replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
					str = MessageAPI.replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
					str = MessageAPI.replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
					msg.add(str);
				}

				msg = ArrayUtils.colorize(msg);
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
					sendMessage(sender, PlaceholderUtils
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
					sendMessage(sender, PlaceholderUtils
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
					sendMessage(sender, PlaceholderUtils
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
										user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
									}
									int pointsToGive = Integer.parseInt(args[2]);
									if (pointsToGive > 0) {
										if (cPlayer.getPoints() >= pointsToGive) {
											user.addPoints(pointsToGive);
											cPlayer.removePoints(pointsToGive);
											HashMap<String, String> placeholders = new HashMap<>();
											placeholders.put("transfer", "" + pointsToGive);
											placeholders.put("touser", "" + user.getPlayerName());
											placeholders.put("fromuser", "" + cPlayer.getPlayerName());
											sendMessage(sender,
													PlaceholderUtils.replacePlaceHolder(
															plugin.getConfigFile()
																	.getFormatCommandsVoteGivePointsTransferFrom(),
															placeholders));
											user.sendMessage(PlaceholderUtils.replacePlaceHolder(
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
											PlaceholderUtils.replacePlaceHolder(
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
				String[] args = ArrayUtils.convert((ArrayList<String>) section.getList("Args", new ArrayList<>()));
				plugin.getVoteCommand().add(new CommandHandler(plugin, args, section.getString("Permission", ""),
						section.getString("HelpMessage", "")) {

					@SuppressWarnings("unchecked")
					@Override
					public void execute(CommandSender sender, String[] args) {
						sendMessage(sender, section.getString("Message", ""));
						for (String str : (ArrayList<String>) section.getList("Commands", new ArrayList<>())) {
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
				plugin.debug("Disabling: " + ArrayUtils.makeStringList(ArrayUtils.convert(list.get(i).getArgs())));
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

	public void processSlotClick(Player player, VotingPluginUser user, String slot) {
		if (MessageAPI.startsWithIgnoreCase(slot, "url")) {
			new VoteURL(plugin, player, user, true).open();
		} else if (MessageAPI.startsWithIgnoreCase(slot, "next")) {
			new VoteNext(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "last")) {
			new VoteLast(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "total")) {
			new VoteTotal(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "top")) {
			new VoteTopVoter(plugin, player, user, TopVoter.getDefault(), 1).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "today")) {
			new VoteToday(plugin, player, user, 1).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "help")) {
			player.performCommand("vote help");
		} else if (MessageAPI.startsWithIgnoreCase(slot, "shop")) {
			new VoteShop(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "lastmonth")) {
			new VoteTopVoterLastMonth(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "best")) {
			new VoteBest(plugin, player, user).open(GUIMethod.CHEST);
		} else if (MessageAPI.startsWithIgnoreCase(slot, "streak")) {
			new VoteStreak(plugin, player, user).open(GUIMethod.CHEST);
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