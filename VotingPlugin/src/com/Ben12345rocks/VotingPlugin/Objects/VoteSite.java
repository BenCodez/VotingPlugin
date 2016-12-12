package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;

import org.bukkit.Bukkit;

import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

// TODO: Auto-generated Javadoc
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
	private String siteName;

	/** The vote delay. */
	private int voteDelay;

	/** The enabled. */
	private boolean enabled;

	/** The rewards. */
	private ArrayList<String> rewards;

	/** The priority. */
	private int priority;

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
		String org = siteName;
		siteName = siteName.replace(".", "_");
		setSiteName(siteName);
		if (!configVoteSites.getVoteSitesNames().contains(siteName)) {
			if (Config.getInstance().getAutoCreateVoteSites()) {
				configVoteSites.generateVoteSite(org);
			}
			init();
			plugin.loadVoteSites();
		} else {
			init();
		}
	}

	/**
	 * Broadcast vote.
	 *
	 * @param user
	 *            the user
	 */
	public void broadcastVote(User user) {
		String playerName = user.getPlayerName();
		String bc = StringUtils.getInstance().colorize(config.getFormatBroadCastMsg());
		bc = bc.replace("%player%", playerName).replace("%SiteName%", siteName);
		final String str = bc;
		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				Bukkit.broadcastMessage(str);
			}
		});

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
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	public ArrayList<String> getRewards() {
		return rewards;
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
	 * Gets the site name.
	 *
	 * @return the site name
	 */
	public String getSiteName() {
		return siteName;
	}

	/**
	 * Gets the vote delay.
	 *
	 * @return the vote delay
	 */
	public int getVoteDelay() {
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
		for (String reward : getRewards()) {

			RewardHandler.getInstance().giveReward(user, reward, online);

		}
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

	/**
	 * Inits the.
	 */
	public void init() {
		setVoteURL(configVoteSites.getVoteURL(siteName));
		setServiceSite(configVoteSites.getServiceSite(siteName));
		setVoteDelay(configVoteSites.getVoteDelay(siteName));
		setEnabled(configVoteSites.getVoteSiteEnabled(siteName));
		setRewards(configVoteSites.getRewards(siteName));
		setPriority(configVoteSites.getPriority(siteName));
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
	 * Sets the enabled.
	 *
	 * @param enabled
	 *            the new enabled
	 */
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
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
	 * Sets the rewards.
	 *
	 * @param rewards
	 *            the new rewards
	 */
	public void setRewards(ArrayList<String> rewards) {
		this.rewards = rewards;
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
	 * Sets the site name.
	 *
	 * @param siteName
	 *            the new site name
	 */
	public void setSiteName(String siteName) {
		this.siteName = siteName;
	}

	/**
	 * Sets the vote delay.
	 *
	 * @param voteDelay
	 *            the new vote delay
	 */
	public void setVoteDelay(int voteDelay) {
		this.voteDelay = voteDelay;
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
