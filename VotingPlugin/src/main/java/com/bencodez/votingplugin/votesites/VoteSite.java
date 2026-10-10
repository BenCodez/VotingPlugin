package com.bencodez.votingplugin.votesites;

import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

import lombok.Getter;
import lombok.Setter;

public class VoteSite {
	@Getter
	private String displayName;
	private boolean displayNameFallback;

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
	private int voteDelayDailyHour;

	@Getter
	@Setter
	private boolean voteDelayDaily;

	@Getter
	private ParsedDuration voteDelay;

	@Setter
	private String voteURL;

	@Getter
	@Setter
	private String permissionToView;

	@Getter
	@Setter
	private boolean waitUntilVoteDelay;

	public VoteSite(VotingPluginMain plugin, String siteName) {
		this.plugin = plugin;
		key = siteName.replace(".", "_");
		init();
	}

	/**
	 * @return the item
	 */
	public ItemBuilder getItem() {
		if (item == null) {
			plugin.getLogger().warning("Invalid display item section in site: " + key);
			return new ItemBuilder(Material.STONE, 1).setName("&cInvalid display item for site: " + key)
					.setLore("&cInvalid display item for site: " + key);
		}
		return new ItemBuilder(item);
	}

	public ConfigurationSection getSiteData() {
		return plugin.getConfigVoteSites().getData(key);
	}

	public String getVoteURL() {
		return getVoteURL(true);
	}

	public String getVoteURL(boolean json) {
		if (!plugin.getConfigFile().isFormatCommandsVoteForceLinks() || !json || MessageAPI.containsJson(voteURL)) {
			return voteURL;
		}
		if (!voteURL.startsWith("http")) {
			return "[Text=\"" + voteURL + "\",url=\"http://" + voteURL + "\"]";
		}
		return "[Text=\"" + voteURL + "\",url=\"" + voteURL + "\"]";
	}

	public String getVoteURLJsonStrip() {
		String url = ChatColor
				.stripColor(MessageAPI.colorize(PlaceholderUtils.parseJson(getVoteURL(false)).toPlainText()));
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
		createRewardBuilder(plugin.getConfigVoteSites().getEverySiteRewardPath(), online, bungee).send(user);

		createRewardBuilder(plugin.getConfigVoteSites().getRewardsPath(key), online, bungee).send(user);

	}

	/**
	 * Gives rewards configured for a vote rejected by WaitUntilVoteDelay.
	 *
	 * @param user the voting user
	 * @param online whether the player was online when the vote was received
	 * @param bungee whether the vote came through the proxy
	 */
	public void giveWaitUntilVoteDelayRewards(VotingPluginUser user, boolean online, boolean bungee) {
		createRewardBuilder(plugin.getConfigVoteSites().getWaitUntilVoteDelayRewardsPath(key), online, bungee)
				.send(user);
	}

	/**
	 * Await both vote-site reward phases; AdvancedCore marshals configured
	 * player/world actions to their owning platform scheduler.
	 */
	public java.util.concurrent.CompletionStage<Void> giveRewardsAsync(VotingPluginUser user,
			boolean online, boolean bungee) {
		return createRewardBuilder(plugin.getConfigVoteSites().getEverySiteRewardPath(), online, bungee)
				.sendAsync(user).thenCompose(ignored ->
						createRewardBuilder(plugin.getConfigVoteSites().getRewardsPath(key), online, bungee)
								.sendAsync(user));
	}

	private RewardBuilder createRewardBuilder(String path, boolean online, boolean bungee) {
		// Reward placeholders also feed commands and other exact-value actions. The
		// service identifier is already validated at ingress, so preserve it here;
		// display-only callers use getServiceSiteForFormatting() instead.
		return new RewardBuilder(plugin.getConfigVoteSites().getData(), path).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSite()).withPlaceHolder("SiteName", getDisplayName())
				.withDisplayPlaceHolder("ServiceSite", getServiceSiteForFormatting())
				.withDisplayPlaceHolder("SiteName", getDisplayNameForFormatting())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL())
				.setServer(bungee);
	}

	public boolean hasRewards() {
		return plugin.getRewardHandler().hasRewards(plugin.getConfigVoteSites().getData(),
				plugin.getConfigVoteSites().getRewardsPath(key));
	}

	/** Returns the configured service identifier guarded for trusted templates. */
	public String getServiceSiteForFormatting() {
		return ServiceSiteValidator.inertForFormatting(getServiceSite());
	}

	/** Sets an administrator-controlled display name. */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
		this.displayNameFallback = false;
	}

	/** Returns the site label guarded only when it falls back to an external site key. */
	public String getDisplayNameForFormatting() {
		return displayNameFallback ? ServiceSiteValidator.inertForFormatting(getDisplayName()) : getDisplayName();
	}

	/**
	 * Inits the.
	 */
	public void init() {
		setVoteURL(plugin.getConfigVoteSites().getVoteURL(key));
		setServiceSite(plugin.getConfigVoteSites().getServiceSite(key));
		this.voteDelay = plugin.getConfigVoteSites().getVoteDelay(key);
		setEnabled(plugin.getConfigVoteSites().getVoteSiteEnabled(key));
		setPriority(plugin.getConfigVoteSites().getPriority(key));
		displayName = plugin.getConfigVoteSites().getDisplayName(key);
		displayNameFallback = displayName == null || displayName.equals("");
		if (displayNameFallback) {
			displayName = key;
		}
		item = plugin.getConfigVoteSites().getItem(key);
		voteDelayDaily = plugin.getConfigVoteSites().getVoteSiteResetVoteDelayDaily(key);
		giveOffline = plugin.getConfigVoteSites().getVoteSiteGiveOffline(key);
		waitUntilVoteDelay = plugin.getConfigVoteSites().getWaitUntilVoteDelay(key);
		voteDelayDailyHour = plugin.getConfigVoteSites().getVoteDelayDailyHour(key);
		hidden = plugin.getConfigVoteSites().getVoteSiteHidden(key);
		ignoreCanVote = plugin.getConfigVoteSites().getVoteSiteIgnoreCanVote(key);
		permissionToView = plugin.getConfigVoteSites().getPermissionToView(key);
	}

	public boolean isVaidServiceSite() {
		return ArrayUtils.containsIgnoreCase(plugin.getServerData().getServiceSites(), getServiceSite());
	}

	public String loadingDebug() {
		String str = "Loading votesite key: " + key;
		str += ", Displayname: " + displayName;
		str += ", VoteDelay: " + getVoteDelay();
		str += ", VoteDelayDaily: " + isVoteDelayDaily();
		str += ", IsWaitUntilVoteDelay: " + isWaitUntilVoteDelay();
		str += ", ServiceSite: " + getServiceSite();
		str += ", VoteDelayDailyHour:" + getVoteDelayDailyHour();
		str += ", Url: " + getVoteURL();
		str += ", IgnoreCanVote: " + isIgnoreCanVote();
		str += ", Hidden: " + isHidden();
		return str;
	}

}
