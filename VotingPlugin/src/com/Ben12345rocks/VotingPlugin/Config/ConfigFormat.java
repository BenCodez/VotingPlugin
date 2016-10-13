package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

import com.Ben12345rocks.AdvancedCore.Util.Files.FilesManager;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigFormat.
 */
public class ConfigFormat {

	/** The instance. */
	static ConfigFormat instance = new ConfigFormat();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigFormat.
	 *
	 * @return single instance of ConfigFormat
	 */
	public static ConfigFormat getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new config format.
	 */
	private ConfigFormat() {
	}

	/**
	 * Instantiates a new config format.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public ConfigFormat(Main plugin) {
		ConfigFormat.plugin = plugin;
	}

	public String getShopPurchaseMsg() {
		String msg = getData().getString("Format.ShopPurchase",
				"&aYou bought the %Identifier% for %Points% Points!");

		return msg;

	}

	public String getShopFailedMsg() {
		String msg = getData()
				.getString("Format.ShopFailed", "&cYou do not have %Points% points to purhcase this!");

		return msg;

	}

	/**
	 * Gets the broad cast msg.
	 *
	 * @return the broad cast msg
	 */
	public String getBroadCastMsg() {
		String str = getData().getString("Format.BroadcastMsg");
		if (str != null) {
			return str;
		} else {
			return "&6[&4Broadcast&6] &2Thanks &c%player% &2for voting on %SiteName%";
		}
	}

	/**
	 * Gets the broadcast when online.
	 *
	 * @return the broadcast when online
	 */
	public boolean getBroadcastWhenOnline() {
		return getData().getBoolean("Format.BroadcastWhenOnline");
	}

	/**
	 * Gets the commands vote auto input sites.
	 *
	 * @return the commands vote auto input sites
	 */
	public boolean getCommandsVoteAutoInputSites() {
		return getData().getBoolean("Format.Commands.Vote.AutoInputSites");
	}

	/**
	 * Gets the commands vote help line.
	 *
	 * @return the commands vote help line
	 */
	public String getCommandsVoteHelpLine() {

		String str = getData().getString("Format.Commands.Vote.Help.Line");
		if (str != null) {
			return str;
		} else {
			return "&3&l%Command% - &3%HelpMessage%";
		}

	}

	/**
	 * Gets the commands vote help require permission.
	 *
	 * @return the commands vote help require permission
	 */
	public boolean getCommandsVoteHelpRequirePermission() {
		return getData().getBoolean(
				"Format.Commands.Vote.Help.RequirePermission");
	}

	/**
	 * Gets the commands vote help title.
	 *
	 * @return the commands vote help title
	 */
	public String getCommandsVoteHelpTitle() {
		String str = getData().getString("Format.Commands.Vote.Help.Title");
		if (str != null) {
			return str;
		} else {
			return "Voting Player Help";
		}

	}

	/**
	 * Gets the commands vote last line.
	 *
	 * @return the commands vote last line
	 */
	public String getCommandsVoteLastLine() {
		String str = getData().getString("Format.Commands.Vote.Last.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%time%";
		}
	}

	/**
	 * Gets the commands vote last title.
	 *
	 * @return the commands vote last title
	 */
	public String getCommandsVoteLastTitle() {
		String str = getData().getString("Format.Commands.Vote.Last.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Last Vote Times:";
		}
	}

	/**
	 * Gets the commands vote next info can vote.
	 *
	 * @return the commands vote next info can vote
	 */
	public String getCommandsVoteNextInfoCanVote() {
		String str = getData().getString(
				"Format.Commands.Vote.Next.Info.CanVote");
		if (str != null) {
			return str;
		} else {
			return "Go Vote!";
		}
	}

	/**
	 * Gets the commands vote next info error.
	 *
	 * @return the commands vote next info error
	 */
	public String getCommandsVoteNextInfoError() {
		String str = getData()
				.getString("Format.Commands.Vote.Next.Info.Error");
		if (str != null) {
			return str;
		} else {
			return "";
		}
	}

	/**
	 * Gets the commands vote next info time.
	 *
	 * @return the commands vote next info time
	 */
	public String getCommandsVoteNextInfoTime() {
		String str = getData().getString(
				"Format.Commands.Vote.Next.Info.TimeUntilVote");
		if (str != null) {
			return str;
		} else {
			return "&cCould not caculate time until next vote!";
		}
	}

	/**
	 * Gets the commands vote next layout.
	 *
	 * @return the commands vote next layout
	 */
	public String getCommandsVoteNextLayout() {
		String str = getData().getString("Format.Commands.Vote.Next.Layout");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%info%";
		}
	}

	/**
	 * Gets the commands vote next title.
	 *
	 * @return the commands vote next title
	 */
	public String getCommandsVoteNextTitle() {
		String str = getData().getString("Format.Commands.Vote.Next.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Next Votes:";
		}
	}

	/**
	 * Gets the commands vote party.
	 *
	 * @return the commands vote party
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsVoteParty() {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"Format.Commands.Vote.Party");
			if (list != null) {
				return list;
			}
			ArrayList<String> msg = new ArrayList<String>();
			msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
			return msg;
		} catch (Exception ex) {
			ArrayList<String> msg = new ArrayList<String>();
			msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
			return msg;
		}
	}

	/**
	 * Gets the commands vote text.
	 *
	 * @return the commands vote text
	 */
	@SuppressWarnings("unchecked")
	public List<String> getCommandsVoteText() {
		List<String> str;
		try {
			str = (List<String>) getData().getList("Format.Commands.Vote.Text");
			if (str != null) {
				return str;
			} else {
				str = new ArrayList<String>();
				str.add("&4&lVote for our server!");
				return str;
			}
		} catch (Exception ex) {
			str = new ArrayList<String>();
			str.add("&4&lVote for our server!");
			return str;
		}
	}

	/**
	 * Gets the commands vote total all line.
	 *
	 * @return the commands vote total all line
	 */
	public String getCommandsVoteTotalAllLine() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName% &6%Total%";
		}
	}

	/**
	 * Gets the commands vote total all title.
	 *
	 * @return the commands vote total all title
	 */
	public String getCommandsVoteTotalAllTitle() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&lAll Votes Total:";
		}
	}

	/**
	 * Gets the commands vote total all total.
	 *
	 * @return the commands vote total all total
	 */
	public String getCommandsVoteTotalAllTotal() {
		String str = getData().getString("Format.Commands.Vote.TotalAll.Total");
		if (str != null) {
			return str;
		} else {
			return "&3&lTotal: &6&l%Totals%";
		}
	}

	/**
	 * Gets the commands vote total line.
	 *
	 * @return the commands vote total line
	 */
	public String getCommandsVoteTotalLine() {
		String str = getData().getString("Format.Commands.Vote.Total.Line");
		if (str != null) {
			return str;
		} else {
			return "&3%SiteName%: &6%Total%";
		}
	}

	/**
	 * Gets the commands vote total title.
	 *
	 * @return the commands vote total title
	 */
	public String getCommandsVoteTotalTitle() {
		String str = getData().getString("Format.Commands.Vote.Total.Title");
		if (str != null) {
			return str;
		} else {
			return "&3&l%player% Total Votes:";
		}
	}

	/**
	 * Gets the commands vote total total.
	 *
	 * @return the commands vote total total
	 */
	public String getCommandsVoteTotalTotal() {
		String str = getData().getString("Format.Commands.Vote.Total.Total");
		if (str != null) {
			return str;
		} else {
			return "&3&lTotal: &6&l%Totals%";
		}
	}

	/**
	 * Gets the commands vote URLS.
	 *
	 * @return the commands vote URLS
	 */
	public String getCommandsVoteURLS() {
		String str = getData().getString("Format.Commands.Vote.Sites");
		if (str != null) {
			return str;
		} else {
			return "&4%num%: &c&l%SiteName% - &c%url%";
		}
	}

	/**
	 * Gets the command vote points.
	 *
	 * @return the command vote points
	 */
	public String getCommandVotePoints() {
		String str = getData().getString("Format.Commands.Vote.Points");
		if (str != null) {
			return str;
		} else {
			return "&a%Player% currently has &a&l%Points%&a Points!";
		}
	}

	/**
	 * Gets the command vote top line.
	 *
	 * @return the command vote top line
	 */
	public String getCommandVoteTopLine() {
		String str = getData().getString("Format.Commands.Vote.Top.Line");
		if (str != null) {
			return str;
		} else {
			return "&c%num%: &6%player%, %votes%";
		}
	}

	/**
	 * Gets the command vote top title.
	 *
	 * @return the command vote top title
	 */
	public String getCommandVoteTopTitle() {
		String str = getData().getString("Format.Commands.Vote.Top.Title");
		if (str != null) {
			return str;
		} else {
			return "&3Top Voters %page%/%maxpages%";
		}
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public FileConfiguration getData() {
		return data;
	}

	/**
	 * Gets the login msg.
	 *
	 * @return the login msg
	 */
	public String getLoginMsg() {
		String str = getData().getString("Format.LoginMsg");
		if (str != null) {
			return str;
		} else {
			return "&cRemember to vote!";
		}
	}

	/**
	 * Gets the no perms.
	 *
	 * @return the no perms
	 */
	public String getNoPerms() {
		String str = getData().getString("Format.NoPerms");
		if (str != null) {
			return str;
		} else {
			return "&cYou do not have enough permission!";
		}
	}

	/**
	 * Gets the not number.
	 *
	 * @return the not number
	 */
	public String getNotNumber() {
		String str = getData().getString("Format.NotNumber");
		if (str != null) {
			return str;
		} else {
			return "&cError on &6%arg%&c, number expected!";
		}
	}

	/**
	 * Gets the page size.
	 *
	 * @return the page size
	 */
	public int getPageSize() {
		return 10;
	}

	/**
	 * Gets the reward msg.
	 *
	 * @return the reward msg
	 */
	public String getRewardMsg() {
		String str = getData().getString("Format.DefaultRewardMsg");
		if (str != null) {
			return str;
		} else {
			return "&aYou were given rewards!";
		}
	}

	/**
	 * Gets the sign top voter sign line 1.
	 *
	 * @return the sign top voter sign line 1
	 */
	public String getSignTopVoterSignLine1() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line1");
		if (str != null) {
			return str;
		} else {
			return "TopVoter: %SiteName%";
		}
	}

	/**
	 * Gets the sign top voter sign line 2.
	 *
	 * @return the sign top voter sign line 2
	 */
	public String getSignTopVoterSignLine2() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line2");
		if (str != null) {
			return str;
		} else {
			return "#%position%";
		}
	}

	/**
	 * Gets the sign top voter sign line 3.
	 *
	 * @return the sign top voter sign line 3
	 */
	public String getSignTopVoterSignLine3() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line3");
		if (str != null) {
			return str;
		} else {
			return "%player%";
		}
	}

	/**
	 * Gets the sign top voter sign line 4.
	 *
	 * @return the sign top voter sign line 4
	 */
	public String getSignTopVoterSignLine4() {
		String str = getData().getString("Format.Signs.TopVoterSign.Line4");
		if (str != null) {
			return str;
		} else {
			return "%votes% Votes";
		}
	}

	/**
	 * Gets the sign top voter sign right click message.
	 *
	 * @return the sign top voter sign right click message
	 */
	public String getSignTopVoterSignRightClickMessage() {
		String str = getData().getString("Format.Signs.RightClickMessage");
		if (str != null) {
			return str;
		} else {
			return "&c&l%player% &cis &c&l%position% &cwith &c&l%votes% &cin &c&l%SiteName%";
		}
	}

	/**
	 * Gets the time format.
	 *
	 * @return the time format
	 */
	public String getTimeFormat() {
		String string = getData().getString("Format.TimeFormat");
		if (string == null) {
			return "EEE, d MMM yyyy HH:mm";
		}
		return string;
	}

	/**
	 * Gets the time zone.
	 *
	 * @return the time zone
	 */
	public String getTimeZone() {
		String str = getData().getString("Format.TimeZone");
		if (str != null) {
			return str;
		}
		return "UTC";
	}

	/**
	 * Gets the top voter reward msg.
	 *
	 * @return the top voter reward msg
	 */
	public String getTopVoterRewardMsg() {
		String msg = getData().getString("Format.TopVoterAwardMsg");
		if (msg != null) {
			return msg;
		} else {
			return "&aYou came in %place% in top voters of the month! Here is an award!";
		}
	}

	/**
	 * Gets the vote help.
	 *
	 * @return the vote help
	 */
	@SuppressWarnings("unchecked")
	public List<String> getVoteHelp() {
		return (List<String>) getData().getList(
				"Format.Commands.Vote.Help.Lines");
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	/**
	 * Save data.
	 */
	public void saveData() {
		FilesManager.getInstance().editFile(dFile, data);

	}

	/**
	 * Sets the up.
	 *
	 * @param p
	 *            the new up
	 */
	public void setup(Plugin p) {
		if (!p.getDataFolder().exists()) {
			p.getDataFolder().mkdir();
		}

		dFile = new File(p.getDataFolder(), "Format.yml");

		if (!dFile.exists()) {
			try {
				dFile.createNewFile();
				plugin.saveResource("Format.yml", true);
			} catch (IOException e) {
				Bukkit.getServer().getLogger()
						.severe(ChatColor.RED + "Could not create Format.yml!");
			}
		}

		data = YamlConfiguration.loadConfiguration(dFile);
	}
}
