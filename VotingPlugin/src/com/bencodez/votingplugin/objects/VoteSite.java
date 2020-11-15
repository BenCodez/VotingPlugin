package com.bencodez.votingplugin.objects;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.bungeeapi.pluginmessage.PluginMessage;
import com.bencodez.votingplugin.BungeeHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

import lombok.Getter;
import lombok.Setter;

/**
 * The Class VoteSite.
 */
public class VoteSite {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	@Setter
	private String voteURL;

	@Getter
	@Setter
	private String serviceSite;

	@Getter
	@Setter
	private String key;

	@Getter
	@Setter
	private String displayName;

	@Getter
	@Setter
	private double voteDelay;

	@Getter
	@Setter
	private double voteDelayMin;

	@Getter
	@Setter
	private double timeOffSet;

	@Getter
	@Setter
	private boolean enabled;

	@Getter
	@Setter
	private boolean voteDelayDaily;

	@Getter
	@Setter
	private int priority;

	@Setter
	private ConfigurationSection item;

	@Getter
	@Setter
	private boolean giveOffline;

	@Getter
	@Setter
	private boolean waitUntilVoteDelay;

	/**
	 * Instantiates a new vote site.
	 *
	 * @param plugin the plugin
	 */
	public VoteSite(VotingPluginMain plugin) {
		VoteSite.plugin = plugin;
	}

	/**
	 * Instantiates a new vote site.
	 *
	 * @param siteName the site name
	 */
	public VoteSite(String siteName) {
		key = siteName.replace(".", "_");
		init();
	}

	public void broadcastVote(VotingPluginUser user) {
		broadcastVote(user, true);
	}

	/**
	 * Broadcast vote.
	 *
	 * @param user        the user
	 * @param checkBungee check bungee broadcast
	 */
	public void broadcastVote(VotingPluginUser user, boolean checkBungee) {
		if (!user.isVanished()) {
			if (checkBungee && BungeeSettings.getInstance().isBungeeBroadcast()
					&& BungeeSettings.getInstance().isUseBungeecoord()) {
				if (BungeeHandler.getInstance().getMethod().equals(BungeeMethod.SOCKETS)) {
					BungeeHandler.getInstance().sendData("Broadcast", getServiceSite(), user.getPlayerName());
				} else if (BungeeHandler.getInstance().getMethod().equals(BungeeMethod.MYSQL)
						|| BungeeHandler.getInstance().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
					String uuid = user.getUUID();
					String service = getServiceSite();

					if (Bukkit.getOnlinePlayers().size() > 0) {
						PluginMessage.getInstance().sendPluginMessage(PlayerUtils.getInstance().getRandomOnlinePlayer(),
								"VoteBroadcast", uuid, service);
					}
				}

			} else {
				String playerName = user.getPlayerName();
				if (config.getVotingBroadcastBlacklist().contains(playerName)) {
					plugin.getLogger().info("Not broadcasting for " + playerName + ", in blacklist");
					return;
				}
				String bc = StringParser.getInstance().colorize(config.getFormatBroadCastMsg());
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("player", playerName);
				placeholders.put("nickname",
						(user.getPlayer() != null) ? user.getPlayer().getDisplayName() : user.getPlayerName());
				placeholders.put("sitename", getDisplayName());
				placeholders.put("servicesite", getServiceSite());
				bc = StringParser.getInstance().replacePlaceHolder(bc, placeholders);
				bc = StringParser.getInstance().replacePlaceHolders(user.getOfflinePlayer(), bc);
				ArrayList<Player> players = new ArrayList<Player>();
				for (Player p : Bukkit.getOnlinePlayers()) {
					if (!UserManager.getInstance().getVotingPluginUser(p).getDisableBroadcast()) {
						players.add(p);
					}
				}

				MiscUtils.getInstance().broadcast(bc, players);
			}
		} else {
			plugin.debug(user.getPlayerName() + " is vanished, not broadcasting");
		}
	}

	/**
	 * @return the item
	 */
	public ItemBuilder getItem() {
		if (item == null) {
			plugin.getLogger().warning("Invalid item section in site: " + key);
			return new ItemBuilder(Material.STONE, 1).setName("&cInvalid item for site: " + key)
					.setLore("&cInvalid item for site: " + key);
		} else {
			return new ItemBuilder(item);
		}
	}

	public ConfigurationSection getSiteData() {
		return configVoteSites.getData(key);
	}

	public String getVoteURLJsonStrip() {
		String url = ChatColor.stripColor(StringParser.getInstance()
				.colorize(StringParser.getInstance().parseJson(getVoteURL(false)).toPlainText()));
		if (!url.startsWith("http")) {
			if (!url.startsWith("www.")) {
				url = "https://www." + url;
			} else {
				url = "https://" + url;
			}
		}
		return url;
	}

	public String getVoteURL() {
		return getVoteURL(true);
	}

	public String getVoteURL(boolean json) {
		if (!Config.getInstance().isFormatCommandsVoteForceLinks() || !json) {
			return voteURL;
		} else {
			if (!voteURL.startsWith("http")) {
				return "[Text=\"" + voteURL + "\",url=\"http://" + voteURL + "\"]";
			}
			return "[Text=\"" + voteURL + "\",url=\"" + voteURL + "\"]";
		}
	}

	public void giveRewards(VotingPluginUser user, boolean online, boolean bungee) {
		new RewardBuilder(configVoteSites.getData(), configVoteSites.getRewardsPath(key)).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSite()).withPlaceHolder("SiteName", getDisplayName())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL())
				.setServer(bungee).send(user);

		new RewardBuilder(configVoteSites.getData(), configVoteSites.getEverySiteRewardPath()).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSite()).withPlaceHolder("SiteName", getDisplayName())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL())
				.setServer(bungee).send(user);
	}

	public boolean hasRewards() {
		return RewardHandler.getInstance().hasRewards(configVoteSites.getData(), configVoteSites.getRewardsPath(key));
	}

	/**
	 * Inits the.
	 */
	public void init() {
		setVoteURL(configVoteSites.getVoteURL(key));
		setServiceSite(configVoteSites.getServiceSite(key));
		setVoteDelay(configVoteSites.getVoteDelay(key));
		setVoteDelayMin(configVoteSites.getVoteDelayMin(key));
		setEnabled(configVoteSites.getVoteSiteEnabled(key));
		setPriority(configVoteSites.getPriority(key));
		displayName = configVoteSites.getDisplayName(key);
		if (displayName == null || displayName.equals("")) {
			displayName = key;
		}
		item = configVoteSites.getItem(key);
		voteDelayDaily = configVoteSites.getVoteSiteResetVoteDelayDaily(key);
		giveOffline = configVoteSites.getVoteSiteGiveOffline(key);
		waitUntilVoteDelay = configVoteSites.getWaitUntilVoteDelay(key);
		timeOffSet = configVoteSites.getTimeOffSet(key);
	}

	public boolean isVaidServiceSite() {
		return ArrayUtils.getInstance().containsIgnoreCase(ServerData.getInstance().getServiceSites(),
				getServiceSite());
	}

}
