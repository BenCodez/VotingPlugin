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
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

/**
 * The Class VoteSite.
 */
public class VoteSite {
	@Getter
	@Setter
	private String displayName;

	@Getter
	@Setter
	private boolean enabled;

	@Getter
	@Setter
	private boolean giveOffline;

	@Getter
	@Setter
	private boolean hidden;

	@Getter
	@Setter
	private boolean ignoreCanVote;

	@Setter
	private ConfigurationSection item;

	@Getter
	@Setter
	private String key;

	private VotingPluginMain plugin;

	@Getter
	@Setter
	private int priority;

	@Getter
	@Setter
	private String serviceSite;

	@Getter
	@Setter
	private double timeOffSet;

	@Getter
	@Setter
	private double voteDelay;

	@Getter
	@Setter
	private boolean voteDelayDaily;

	@Getter
	@Setter
	private double voteDelayMin;

	@Setter
	private String voteURL;

	@Getter
	@Setter
	private boolean waitUntilVoteDelay;

	public VoteSite(VotingPluginMain plugin, String siteName) {
		this.plugin = plugin;
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
		if (plugin.getConfigFile().isFormatAlternateBroadcastEnabled()) {
			return;
		}
		if (!user.isVanished()) {
			if (checkBungee && plugin.getBungeeSettings().isBungeeBroadcast()
					&& plugin.getBungeeSettings().isUseBungeecoord()) {
				if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.SOCKETS)) {
					plugin.getBungeeHandler().sendData("Broadcast", getServiceSite(), user.getPlayerName());
				} else if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.MYSQL)
						|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
					String uuid = user.getUUID();
					String service = getServiceSite();

					if (Bukkit.getOnlinePlayers().size() > 0) {
						plugin.getPluginMessaging().sendPluginMessage("VoteBroadcast", uuid, service);
					}
				}

			} else {
				String playerName = user.getPlayerName();
				if (plugin.getConfigFile().getVotingBroadcastBlacklist().contains(playerName)) {
					plugin.getLogger().info("Not broadcasting for " + playerName + ", in blacklist");
					return;
				}
				String bc = StringParser.getInstance().colorize(plugin.getConfigFile().getFormatBroadCastMsg());
				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("player", playerName);
				placeholders.put("nickname",
						(user.getPlayer() != null) ? user.getPlayer().getDisplayName() : user.getPlayerName());
				placeholders.put("sitename", getDisplayName());
				placeholders.put("servicesite", getServiceSite());
				bc = StringParser.getInstance().replacePlaceHolder(bc, placeholders);
				bc = StringParser.getInstance().replacePlaceHolders(user.getOfflinePlayer(), bc);
				ArrayList<Player> players = new ArrayList<>();
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
			plugin.getLogger().warning("Invalid display item section in site: " + key);
			return new ItemBuilder(Material.STONE, 1).setName("&cInvalid display item for site: " + key)
					.setLore("&cInvalid display item for site: " + key);
		} else {
			return new ItemBuilder(item);
		}
	}

	public ConfigurationSection getSiteData() {
		return plugin.getConfigVoteSites().getData(key);
	}

	public String getVoteURL() {
		return getVoteURL(true);
	}

	public String getVoteURL(boolean json) {
		if (!plugin.getConfigFile().isFormatCommandsVoteForceLinks() || !json) {
			return voteURL;
		} else {
			if (!voteURL.startsWith("http")) {
				return "[Text=\"" + voteURL + "\",url=\"http://" + voteURL + "\"]";
			}
			return "[Text=\"" + voteURL + "\",url=\"" + voteURL + "\"]";
		}
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

	public void giveRewards(VotingPluginUser user, boolean online, boolean bungee) {
		new RewardBuilder(plugin.getConfigVoteSites().getData(), plugin.getConfigVoteSites().getRewardsPath(key))
				.setOnline(online).withPlaceHolder("ServiceSite", getServiceSite())
				.withPlaceHolder("SiteName", getDisplayName()).withPlaceHolder("VoteDelay", "" + getVoteDelay())
				.withPlaceHolder("VoteURL", getVoteURL()).setServer(bungee).send(user);

		new RewardBuilder(plugin.getConfigVoteSites().getData(), plugin.getConfigVoteSites().getEverySiteRewardPath())
				.setOnline(online).withPlaceHolder("ServiceSite", getServiceSite())
				.withPlaceHolder("SiteName", getDisplayName()).withPlaceHolder("VoteDelay", "" + getVoteDelay())
				.withPlaceHolder("VoteURL", getVoteURL()).setServer(bungee).send(user);
	}

	public boolean hasRewards() {
		return RewardHandler.getInstance().hasRewards(plugin.getConfigVoteSites().getData(),
				plugin.getConfigVoteSites().getRewardsPath(key));
	}

	/**
	 * Inits the.
	 */
	public void init() {
		setVoteURL(plugin.getConfigVoteSites().getVoteURL(key));
		setServiceSite(plugin.getConfigVoteSites().getServiceSite(key));
		setVoteDelay(plugin.getConfigVoteSites().getVoteDelay(key));
		setVoteDelayMin(plugin.getConfigVoteSites().getVoteDelayMin(key));
		setEnabled(plugin.getConfigVoteSites().getVoteSiteEnabled(key));
		setPriority(plugin.getConfigVoteSites().getPriority(key));
		displayName = plugin.getConfigVoteSites().getDisplayName(key);
		if (displayName == null || displayName.equals("")) {
			displayName = key;
		}
		item = plugin.getConfigVoteSites().getItem(key);
		voteDelayDaily = plugin.getConfigVoteSites().getVoteSiteResetVoteDelayDaily(key);
		giveOffline = plugin.getConfigVoteSites().getVoteSiteGiveOffline(key);
		waitUntilVoteDelay = plugin.getConfigVoteSites().getWaitUntilVoteDelay(key);
		timeOffSet = plugin.getConfigVoteSites().getTimeOffSet(key);
		hidden = plugin.getConfigVoteSites().getVoteSiteHidden(key);
		ignoreCanVote = plugin.getConfigVoteSites().getVoteSiteIgnoreCanVote(key);
	}

	public boolean isVaidServiceSite() {
		return ArrayUtils.getInstance().containsIgnoreCase(plugin.getServerData().getServiceSites(), getServiceSite());
	}

}
