package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;

import org.bukkit.Bukkit;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;

public class VoteSite {
	static Config config = Config.getInstance();
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();
	static ConfigFormat format = ConfigFormat.getInstance();

	static Main plugin = Main.plugin;

	private String voteURL;
	private String serviceSite;
	private String siteName;
	private int voteDelay;
	private boolean enabled;

	private ArrayList<String> rewards;

	private int cumulativeVotes;
	private ArrayList<String> cumulativeRewards;

	public VoteSite(Main plugin) {
		VoteSite.plugin = plugin;
	}

	/**
	 * New VoteSite
	 *
	 * @param siteName
	 *            Sitename
	 */
	public VoteSite(String siteName) {
		setSiteName(siteName);
		if (!configVoteSites.getVoteSiteFile(siteName).exists()) {
			if (!Config.getInstance().getDisableAutoCreateVoteSites()) {
				configVoteSites.generateVoteSite(siteName);
			}
			init();
			plugin.loadVoteSites();
		} else {
			init();
		}
	}

	/**
	 * Broad cast a vote
	 *
	 * @param user
	 *            User to broadcast with
	 */
	public void broadcastVote(User user) {
		String playerName = user.getPlayerName();
		String bc = Utils.getInstance().colorize(format.getBroadCastMsg());
		bc = bc.replace("%player%", playerName).replace("%SiteName%", siteName);
		Bukkit.broadcastMessage(bc);
	}

	public ArrayList<String> getCumulativeRewards() {
		return cumulativeRewards;
	}

	public int getCumulativeVotes() {
		return cumulativeVotes;
	}

	public ArrayList<String> getRewards() {
		return rewards;
	}

	public String getServiceSite() {
		return serviceSite;
	}

	public String getSiteName() {
		return siteName;
	}

	public int getVoteDelay() {
		return voteDelay;
	}

	public String getVoteURL() {
		return voteURL;
	}

	public void giveCulumativeRewards(User user) {
		for (String reward : getCumulativeRewards()) {
			ConfigRewards.getInstance().getReward(reward).giveReward(user);
		}
	}

	public void giveRewards(User user) {
		for (String reward : getRewards()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

	public void giveSiteReward(User user) {
		giveRewards(user);

		try {

			user.addCumulativeReward(this);

			if ((user.getCumulativeReward(this) >= configVoteSites
					.getCumulativeRewardVotesAmount(siteName))
					&& (configVoteSites
							.getCumulativeRewardVotesAmount(siteName) != 0)) {

				giveCulumativeRewards(user);

				user.setCumulativeReward(this, 0);

				user.sendMessage(Utils
						.getInstance()
						.replaceIgnoreCase(
								ConfigFormat.getInstance()
								.getCumulativeRewardMsg(),
								"%votes%",
								""
										+ configVoteSites
										.getCumulativeRewardVotesAmount(siteName)));
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void init() {
		setVoteURL(configVoteSites.getVoteURL(siteName));
		setServiceSite(configVoteSites.getServiceSite(siteName));
		setVoteDelay(configVoteSites.getVoteDelay(siteName));
		setEnabled(configVoteSites.getVoteSiteEnabled(siteName));
		setRewards(configVoteSites.getRewards(siteName));
		setCumulativeVotes(configVoteSites
				.getCumulativeRewardVotesAmount(siteName));
		setCumulativeRewards(configVoteSites.getCumulativeRewards(siteName));
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setCumulativeRewards(ArrayList<String> cumulativeRewards) {
		this.cumulativeRewards = cumulativeRewards;
	}

	public void setCumulativeVotes(int cumulativeVotes) {
		this.cumulativeVotes = cumulativeVotes;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public void setRewards(ArrayList<String> rewards) {
		this.rewards = rewards;
	}

	public void setServiceSite(String serviceSite) {
		this.serviceSite = serviceSite;
	}

	public void setSiteName(String siteName) {
		this.siteName = siteName;
	}

	public void setVoteDelay(int voteDelay) {
		this.voteDelay = voteDelay;
	}

	public void setVoteURL(String voteURL) {
		this.voteURL = voteURL;
	}

}
