package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

/**
 * The Class VoteSite.
 */
public class VoteSite {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/** The vote URL. */
	private String voteURL;

	/** The service site. */
	private String serviceSite;

	/** The site name. */
	private String key;

	private String displayName;

	/** The vote delay. */
	private double voteDelay;

	/** The enabled. */
	private boolean enabled;

	private boolean voteDelayDaily;

	/** The priority. */
	private int priority;

	private ConfigurationSection item;

	private boolean giveOffline;

	/**
	 * Instantiates a new vote site.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VoteSite(Main plugin) {
		VoteSite.plugin = plugin;
	}

	/**
	 * Instantiates a new vote site.
	 *
	 * @param siteName
	 *            the site name
	 */
	public VoteSite(String siteName) {
		key = siteName.replace(".", "_");
		init();
	}

	/**
	 * Broadcast vote.
	 *
	 * @param user
	 *            the user
	 */
	public void broadcastVote(User user) {
		if (!user.isVanished()) {
			String playerName = user.getPlayerName();
			String bc = StringUtils.getInstance().colorize(config.getFormatBroadCastMsg());
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("player", playerName);
			placeholders.put("nickname", user.getPlayer().getDisplayName());
			placeholders.put("sitename", getDisplayName());
			bc = StringUtils.getInstance().replacePlaceHolder(bc, placeholders);
			ArrayList<Player> players = new ArrayList<Player>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				if (!UserManager.getInstance().getVotingPluginUser(p).getDisableBroadcast()) {
					players.add(p);
				}
			}

			MiscUtils.getInstance().broadcast(bc, players);
		}
	}

	/**
	 * @return the displayName
	 */
	public String getDisplayName() {
		return displayName;
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

	/**
	 * @return the key
	 */
	public String getKey() {
		return key;
	}

	/**
	 * Gets the priority.
	 *
	 * @return the priority
	 */
	public int getPriority() {
		return priority;
	}

	/**
	 * Gets the service site.
	 *
	 * @return the service site
	 */
	public String getServiceSite() {
		return serviceSite;
	}

	/**
	 * Gets the vote delay.
	 *
	 * @return the vote delay
	 */
	public double getVoteDelay() {
		return voteDelay;
	}

	/**
	 * Gets the vote URL.
	 *
	 * @return the vote URL
	 */
	public String getVoteURL() {
		return voteURL;
	}

	/**
	 * Give rewards.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 */
	public void giveRewards(User user, boolean online) {
		new RewardBuilder(configVoteSites.getData(), configVoteSites.getRewardsPath(key)).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSite()).withPlaceHolder("SiteName", getDisplayName())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL()).send(user);

		new RewardBuilder(configVoteSites.getData(), configVoteSites.getEverySiteRewardPath()).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSite()).withPlaceHolder("SiteName", getDisplayName())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL()).send(user);
	}

	/**
	 * Give site reward.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 */
	public void giveSiteReward(User user, boolean online) {
		giveRewards(user, online);
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
		setEnabled(configVoteSites.getVoteSiteEnabled(key));
		setPriority(configVoteSites.getPriority(key));
		displayName = configVoteSites.getDisplayName(key);
		if (displayName == null || displayName.equals("")) {
			displayName = key;
		}
		item = configVoteSites.getItem(key);
		voteDelayDaily = configVoteSites.getVoteSiteResetVoteDelayDaily(key);
		giveOffline = configVoteSites.getVoteSiteGiveOffline(key);
	}

	/**
	 * Checks if is enabled.
	 *
	 * @return true, if is enabled
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * @return the giveOffline
	 */
	public boolean isGiveOffline() {
		return giveOffline;
	}

	/**
	 * @return the voteDelayDaily
	 */
	public boolean isVoteDelayDaily() {
		return voteDelayDaily;
	}

	/**
	 * @param displayName
	 *            the displayName to set
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled
	 *            the new enabled
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	/**
	 * @param giveOffline
	 *            the giveOffline to set
	 */
	public void setGiveOffline(boolean giveOffline) {
		this.giveOffline = giveOffline;
	}

	/**
	 * @param item
	 *            the item to set
	 */
	public void setItem(ConfigurationSection item) {
		this.item = item;
	}

	/**
	 * @param key
	 *            the key to set
	 */
	public void setKey(String key) {
		this.key = key;
	}

	/**
	 * Sets the priority.
	 *
	 * @param priority
	 *            the new priority
	 */
	public void setPriority(int priority) {
		this.priority = priority;
	}

	/**
	 * Sets the service site.
	 *
	 * @param serviceSite
	 *            the new service site
	 */
	public void setServiceSite(String serviceSite) {
		this.serviceSite = serviceSite;
	}

	/**
	 * Sets the vote delay.
	 *
	 * @param voteDelay
	 *            the new vote delay
	 */
	public void setVoteDelay(double voteDelay) {
		this.voteDelay = voteDelay;
	}

	/**
	 * @param voteDelayDaily
	 *            the voteDelayDaily to set
	 */
	public void setVoteDelayDaily(boolean voteDelayDaily) {
		this.voteDelayDaily = voteDelayDaily;
	}

	/**
	 * Sets the vote URL.
	 *
	 * @param voteURL
	 *            the new vote URL
	 */
	public void setVoteURL(String voteURL) {
		this.voteURL = voteURL;
	}

}
