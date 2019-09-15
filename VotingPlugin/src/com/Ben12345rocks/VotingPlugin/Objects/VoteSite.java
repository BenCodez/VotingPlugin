package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

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
	static Main plugin = Main.plugin;

	@Getter
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
			placeholders.put("nickname",
					(user.getPlayer() != null) ? user.getPlayer().getDisplayName() : user.getPlayerName());
			placeholders.put("sitename", getDisplayName());
			placeholders.put("servicesite", getServiceSite());
			bc = StringUtils.getInstance().replacePlaceHolder(bc, placeholders);
			ArrayList<Player> players = new ArrayList<Player>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				if (!UserManager.getInstance().getVotingPluginUser(p).getDisableBroadcast()) {
					players.add(p);
				}
			}

			MiscUtils.getInstance().broadcast(bc, players);
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
		waitUntilVoteDelay = configVoteSites.getWaitUntilVoteDelay(key);
	}

	public boolean isVaidServiceSite() {
		return ArrayUtils.getInstance().containsIgnoreCase(ServerData.getInstance().getServiceSites(),
				getServiceSite());
	}

}
