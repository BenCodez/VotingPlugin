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
import com.bencodez.votingplugin.util.RewardActionTemplateGuard;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

import lombok.Getter;
import lombok.Setter;

public class VoteSite {
	@Getter
	private String displayName;
	private boolean displayNameFallback;
	private boolean automaticallyCreatedVoteSite;
	private boolean serviceSiteFromAutomaticCreation;

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

	private RewardBuilder createRewardBuilder(String path, boolean online, boolean bungee) {
		// Service-site values can originate at the Votifier trust boundary. Preserve
		// ordinary identifiers exactly, but break placeholder/color token syntax before
		// reward actions (including console commands) consume externally supplied text.
		ConfigurationSection configuredData = plugin.getConfigVoteSites().getData();
		ConfigurationSection rewardData = rewardDataForActions(configuredData, path);
		RewardBuilder builder = new RewardBuilder(rewardData, path).setOnline(online)
				.withPlaceHolder("ServiceSite", getServiceSiteForActions())
				.withPlaceHolder("SiteName", getDisplayNameForActions())
				.withDisplayPlaceHolder("ServiceSite", getServiceSiteForFormatting())
				.withDisplayPlaceHolder("SiteName", getDisplayNameForFormatting())
				.withPlaceHolder("VoteDelay", "" + getVoteDelay()).withPlaceHolder("VoteURL", getVoteURL())
				.setServer(bungee);
		// A registered direct reward points at the original section. Only the isolated
		// guarded copy may execute when an external value can complete template syntax.
		if (rewardData != configuredData) builder.withSuffix(null);
		return builder;
	}

	public boolean hasRewards() {
		return plugin.getRewardHandler().hasRewards(plugin.getConfigVoteSites().getData(),
				plugin.getConfigVoteSites().getRewardsPath(key));
	}

	/** Returns external auto-created service text guarded for reward actions. */
	public String getServiceSiteForActions() {
		return getServiceSiteForActions(false);
	}

	private String getServiceSiteForActions(boolean leadingBoundary) {
		return serviceSiteFromAutomaticCreation
				? ServiceSiteValidator.inertForActions(getServiceSite(), leadingBoundary)
				: getServiceSite();
	}

	/** Returns the configured service identifier guarded for trusted templates. */
	public String getServiceSiteForFormatting() {
		return ServiceSiteValidator.inertForFormatting(getServiceSite());
	}

	/** Keeps administrator display names exact, while guarding external fallback names in reward actions. */
	public String getDisplayNameForActions() {
		return getDisplayNameForActions(false);
	}

	private String getDisplayNameForActions(boolean leadingBoundary) {
		return displayNameFallback && automaticallyCreatedVoteSite
				? ServiceSiteValidator.inertForActions(getDisplayName(), leadingBoundary) : getDisplayName();
	}

	private ConfigurationSection rewardDataForActions(ConfigurationSection root, String path) {
		java.util.LinkedHashMap<String, String> untrusted = new java.util.LinkedHashMap<>();
		if (serviceSiteFromAutomaticCreation) untrusted.put("ServiceSite", getServiceSite());
		if (displayNameFallback && automaticallyCreatedVoteSite) untrusted.put("SiteName", getDisplayName());
		return RewardActionTemplateGuard.isolate(root, path, untrusted);
	}

	public boolean isDisplayNameFromAutomaticCreation() {
		return displayNameFallback && automaticallyCreatedVoteSite;
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
		automaticallyCreatedVoteSite = plugin.getConfigVoteSites().isAutomaticallyCreatedVoteSite(key);
		serviceSiteFromAutomaticCreation = plugin.getConfigVoteSites().isAutoGeneratedServiceSite(key);
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
