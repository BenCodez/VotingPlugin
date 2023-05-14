package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.yml.YMLFile;
import com.bencodez.advancedcore.api.yml.annotation.AnnotationHandler;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataBoolean;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataConfigurationSection;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataListString;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;

import lombok.Getter;

public class GUI extends YMLFile {

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.AlreadyVotedColor")
	@Getter
	private String bookVoteURLBookGUIAlreadyVotedColor = "RED";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.AlreadyVotedText")
	@Getter
	private String bookVoteURLBookGUIAlreadyVotedText = "Click me";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.CanVoteColor")
	@Getter
	private String bookVoteURLBookGUICanVoteColor = "GREEN";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.CanVoteText")
	@Getter
	private String bookVoteURLBookGUICanVoteText = "Click me";

	@ConfigDataListString(path = "BOOK.VoteURLBookGUI.Layout")
	@Getter
	private ArrayList<String> bookVoteURLBookGUILayout = ArrayUtils.getInstance()
			.convert(new String[] { "&c%Sitename%", "[UrlText]" });

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.Title")
	@Getter
	private String bookVoteURLBookGUITitle = "&cVoteURL";

	@Getter
	@ConfigDataConfigurationSection(path = "CHEST.ShopConfirmPurchase.NoItem")
	private ConfigurationSection chestShopConfirmPurchaseNoItem;

	@Getter
	@ConfigDataString(path = "CHEST.ShopConfirmPurchase.Title")
	private String chestShopConfirmPurchaseTitle = "Confirm Purchase?";

	@Getter
	@ConfigDataConfigurationSection(path = "CHEST.ShopConfirmPurchase.YesItem")
	private ConfigurationSection chestShopConfirmPurchaseYesItem;

	@Getter
	@ConfigDataString(path = "CHEST.VoteShopDisabled")
	private String chestVoteShopDisabled = "&cVote shop disabled";

	@ConfigDataBoolean(path = "CHEST.VoteShopHideLimitedReached")
	@Getter
	private boolean chestVoteShopHideLimitedReached = true;

	@ConfigDataString(path = "CHEST.VoteShopLimitReached")
	@Getter
	private String chestVoteShopLimitReached = "&aYou reached your limit";

	@ConfigDataBoolean(path = "CHEST.VoteShopRequireConfirmation")
	@Getter
	private boolean chestVoteShopRequireConfirmation = false;

	@ConfigDataBoolean(path = "CHEST.VoteBest.BackButton")
	@Getter
	private boolean chestVoteBestBackButton = true;

	@ConfigDataString(path = "CHEST.VoteToday.IconTitle")
	@Getter
	private String chestVoteTodayIconTitle = "%player%";

	@ConfigDataString(path = "CHEST.VoteToday.Line")
	@Getter
	private String chestVoteTodayLine = "&6%VoteSite% : %Time%";

	@ConfigDataConfigurationSection(path = "CHEST.VoteToday.PlayerItem")
	@Getter
	private ConfigurationSection chestVoteTodayPlayerItem;

	@ConfigDataBoolean(path = "CHEST.VoteToday.UseSkull", defaultValue = true)
	@Getter
	private boolean chestVoteTodayUseSkull = true;

	@ConfigDataString(path = "CHEST.VoteTop.PlayerItem.Material")
	@Getter
	private String chestVoteTopPlayerItemMaterial = "PAPER";

	@ConfigDataBoolean(path = "CHEST.VoteTop.UseSkull")
	@Getter
	private boolean chestVoteTopUseSkull = true;

	@ConfigDataBoolean(path = "CHEST.VoteURL.AllUrlsButton.RequireAllSitesVoted")
	@Getter
	private boolean chestVoteURLAllUrlsButtonrequireAllSitesVoted = true;

	@ConfigDataString(path = "GUIMethod.Best")
	@Getter
	private String guiMethodBest = "CHEST";

	@ConfigDataString(path = "GUIMethod.GUI")
	@Getter
	private String guiMethodGUI = "CHEST";

	@ConfigDataString(path = "GUIMethod.Last")
	@Getter
	private String guiMethodLast = "CHEST";

	@ConfigDataString(path = "GUIMethod.Next")
	@Getter
	private String guiMethodNext = "CHEST";

	@ConfigDataString(path = "GUIMethod.Streak")
	@Getter
	private String guiMethodStreak = "CHEST";

	@ConfigDataString(path = "GUIMethod.Today")
	@Getter
	private String guiMethodToday = "CHEST";

	@ConfigDataString(path = "GUIMethod.TopVoter")
	@Getter
	private String guiMethodTopVoter = "CHEST";

	@ConfigDataString(path = "GUIMethod.Total")
	@Getter
	private String guiMethodTotal = "CHEST";

	@ConfigDataString(path = "GUIMethod.URL")
	@Getter
	private String guiMethodURL = "CHEST";

	@ConfigDataBoolean(path = "LastMonthGUI")
	@Getter
	private boolean lastMonthGUI = false;

	@ConfigDataBoolean(path = "CHEST.VoteLast.ClickableLinks")
	@Getter
	private boolean chestVoteLastClickableLinks = true;

	private VotingPluginMain plugin;

	public GUI(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "GUI.yml"));
		this.plugin = plugin;
	}

	public void createShop(String value) {
		ConfigurationSection shopData = getData().createSection("CHEST.Shop." + value);
		shopData.set("Identifier_Name", value);
		shopData.set("Material", "STONE");
		shopData.set("Amount", 1);
		shopData.set("Name", "&cPlaceholder item");
		shopData.set("Cost", 1);
		shopData.set("Permission", "");
		shopData.set("CloseGUI", true);
		shopData.set("RequireConfirmation", false);

		shopData.set("Rewards.Items.Item1.Material", "STONE");
		shopData.set("Rewards.Items.Item1.Amount", 1);
		saveData();
	}

	public ConfigurationSection getCHESTBackButton() {
		return getData().getConfigurationSection("CHEST.BackButton");
	}

	public int getChestShopIdentifierCost(String identifier) {
		return getData().getInt("CHEST.Shop." + identifier + ".Cost");
	}

	public String getChestShopIdentifierIdentifierName(String identifier) {
		return getData().getString("CHEST.Shop." + identifier + ".Identifier_Name", identifier);
	}

	public int getChestShopIdentifierLimit(String identifier) {
		return getData().getInt("CHEST.Shop." + identifier + ".Limit", -1);
	}

	public String getChestShopIdentifierRewardsPath(String identifier) {
		return "CHEST.Shop." + identifier + ".Rewards";
	}

	public Set<String> getChestShopIdentifiers() {
		ConfigurationSection shop = getData().getConfigurationSection("CHEST.Shop");
		if (shop != null) {
			return shop.getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getChestShopIdentifierSection(String identifier) {
		return getData().getConfigurationSection("CHEST.Shop." + identifier);
	}

	public ConfigurationSection getChestVoteBestDayBestItem() {
		return getData().getConfigurationSection("CHEST.VoteBest.DayBest.Item");
	}

	public ConfigurationSection getChestVoteBestMonthBestItem() {
		return getData().getConfigurationSection("CHEST.VoteBest.MonthBest.Item");
	}

	public String getChestVoteBestName() {
		return getData().getString("CHEST.VoteBest.Name", "VoteBest: %player%");
	}

	public ConfigurationSection getChestVoteBestWeekBestItem() {
		return getData().getConfigurationSection("CHEST.VoteBest.WeekBest.Item");
	}

	public String getChestVoteGUIName() {
		return getData().getString("CHEST.VoteGUIName", "&cVoteGUI: &c&l%player%");
	}

	/**
	 * Gets the vote GUI slot command.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot command
	 */

	public String getChestVoteGUISlotCommand(String slot) {
		return getData().getString("CHEST.VoteGUI." + slot + ".Command", "");
	}

	/**
	 * Gets the vote GUI slot lore.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot lore
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getChestVoteGUISlotLore(String slot) {
		return (ArrayList<String>) getData().getList("CHEST.VoteGUI." + slot + ".Item.Lore", new ArrayList<String>());
	}

	public String getChestVoteGUISlotRewardsPath(String slot, String lastPath) {
		return "CHEST.VoteGUI." + slot + "." + lastPath;
	}

	/**
	 * Gets the vote GUI slots.
	 *
	 * @return the vote GUI slots
	 */
	public Set<String> getChestVoteGUISlots() {
		try {
			return getData().getConfigurationSection("CHEST.VoteGUI").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public ConfigurationSection getChestVoteGUISlotSection(String slot) {
		return getData().getConfigurationSection("CHEST.VoteGUI." + slot + ".Item");
	}

	public ConfigurationSection getChestVoteTopCustomization() {
		return getData().getConfigurationSection("CHEST.VoteTop.Customization");
	}

	/**
	 * Gets the vote GUI slot slot.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot slot
	 */
	public int getChestVoteGUISlotSlot(String slot) {
		return getData().getInt("CHEST.VoteGUI." + slot + ".Slot", -1);
	}

	public boolean getChestVoteLastBackButton() {
		return getData().getBoolean("CHEST.VoteLast.BackButton");
	}

	public String getChestVoteLastLine() {
		return getData().getString("CHEST.VoteLast.Line", "&6%timeSince%");
	}

	public String getChestVoteLastName() {
		return getData().getString("CHEST.VoteLast.Name", "VoteLast: %player%");
	}

	public boolean getChestVoteNextBackButton() {
		return getData().getBoolean("CHEST.VoteNext.BackButton");
	}

	public String getChestVoteNextName() {
		return getData().getString("CHEST.VoteNext.Name", "VoteNext: %player%");
	}
	
	public String getChestVoteNextLine() {
		return getData().getString("CHEST.VoteNext.Line", "%time%");
	}
	
	public String getChestVoteNextCustomSiteNamesDisplays(String site) {
		return getData().getString("CHEST.VoteNext.CustomSiteNamesDisplays." + site, "");
	}

	public String getChestVoteRewardName() {
		return getData().getString("CHEST.VoteRewardName", "VoteReward");
	}

	public boolean getChestVoteShopBackButton() {
		return getData().getBoolean("CHEST.VoteShopBackButton", true);
	}
	
	public boolean getChestVoteShopReopenGUIOnPurchase() {
		return getData().getBoolean("CHEST.VoteShopReopenGUIOnPurchase", false);
	}

	public boolean getChestVoteShopCloseGUI(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".CloseGUI", true);
	}

	public boolean getChestVoteShopEnabled() {
		return getData().getBoolean("CHEST.VoteShopEnabled", true);
	}

	public String getChestVoteShopName() {
		return getData().getString("CHEST.VoteShopName", "VoteShop");
	}

	public boolean getChestVoteShopNotBuyable(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".NotBuyable", false);
	}

	public String getChestVoteShopPermission(String ident) {
		return getData().getString("CHEST.Shop." + ident + ".Permission", "");
	}

	public String getCHESTVoteShopPurchase(String identifier) {
		return getData().getString("CHEST.Shop." + identifier + ".PurchaseMessage",
				plugin.getConfigFile().getFormatShopPurchaseMsg());
	}

	public boolean getChestVoteShopResetDaily(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".Reset.Daily", false);
	}

	public boolean getChestVoteShopResetMonthly(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".Reset.Monthly", false);
	}

	public boolean getChestVoteShopResetWeekly(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".Reset.Weekly", false);
	}

	public boolean getChestVoteStreakBackButton() {
		return getData().getBoolean("CHEST.VoteStreak.BackButton");
	}

	public ConfigurationSection getChestVoteStreakCurrentDayStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.CurrentDayStreak.Item");
	}

	public ConfigurationSection getChestVoteStreakCurrentMonthStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.CurrentMonthStreak.Item");
	}

	public ConfigurationSection getChestVoteStreakCurrentWeekStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.CurrentWeekStreak.Item");
	}

	public ConfigurationSection getChestVoteStreakHighestDayStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.HighestDayStreak.Item");
	}

	public ConfigurationSection getChestVoteStreakHighestMonthStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.HighestMonthStreak.Item");
	}

	public ConfigurationSection getChestVoteStreakHighestWeekStreakItem() {
		return getData().getConfigurationSection("CHEST.VoteStreak.HighestWeekStreak.Item");
	}

	public String getChestVoteStreakName() {
		return getData().getString("CHEST.VoteStreak.Name", "VoteStreak");
	}

	public boolean getChestVoteTodayBackButton() {
		return getData().getBoolean("CHEST.VoteToday.BackButton");
	}

	public String getChestVoteTodayName() {
		return getData().getString("CHEST.VoteToday.Name", "VoteToday");
	}

	public boolean getChestVoteTopBackButton() {
		return getData().getBoolean("CHEST.VoteTop.BackButton");
	}

	public String getChestVoteTopItemLore() {
		return getData().getString("CHEST.VoteTop.Item.Lore", "&3&lVotes: &3%votes%");
	}

	public String getChestVoteTopItemName() {
		return getData().getString("CHEST.VoteTop.Item.Name", "&3&l%position%: &3%player%");
	}

	public String getChestVoteTopName() {
		return getData().getString("CHEST.VoteTop.Name", "VoteTop %topvoter%");
	}

	public boolean getChestVoteTopOpenMainGUIOnClick() {
		return getData().getBoolean("CHEST.VoteTop.OpenMainGUIOnClick", true);
	}

	public boolean getChestVoteTopCloseGUIOnClick() {
		return getData().getBoolean("CHEST.VoteTop.CloseGUIOnClick", true);
	}

	public int getChestVoteTopSize() {
		return getData().getInt("CHEST.VoteTop.Size", 27);
	}

	public ConfigurationSection getChestVoteTopSwitchItem() {
		return getData().getConfigurationSection("CHEST.VoteTop.SwitchItem");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getChestVoteTopSwitchItemTopVoters() {
		return (ArrayList<String>) getData().getList("CHEST.VoteTop.SwitchItem.TopVoters", new ArrayList<String>());
	}

	public ConfigurationSection getChestVoteTotalAllTimeTotalItem() {
		return getData().getConfigurationSection("CHEST.VoteTotal.AllTimeTotal.Item");
	}

	public boolean getChestVoteTotalBackButton() {
		return getData().getBoolean("CHEST.VoteTotal.BackButton");
	}

	public ConfigurationSection getChestVoteTotalDayTotalItem() {
		return getData().getConfigurationSection("CHEST.VoteTotal.DayTotal.Item");
	}

	public ConfigurationSection getChestVoteTotalItem(TopVoter top) {
		switch (top) {
		case AllTime:
			return getChestVoteTotalAllTimeTotalItem();
		case Daily:
			return getChestVoteTotalDayTotalItem();
		case Monthly:
			return getChestVoteTotalMonthTotalItem();
		case Weekly:
			return getChestVoteTotalWeekTotalItem();
		default:
			return getChestVoteTotalAllTimeTotalItem();

		}
	}

	public ConfigurationSection getChestVoteTotalMonthTotalItem() {
		return getData().getConfigurationSection("CHEST.VoteTotal.MonthTotal.Item");
	}

	public String getChestVoteTotalName() {
		return getData().getString("CHEST.VoteTotal.Name", "VoteTotal: %player%");
	}

	public ConfigurationSection getChestVoteTotalWeekTotalItem() {
		return getData().getConfigurationSection("CHEST.VoteTotal.WeekTotal.Item");
	}

	public int getChestVoteURLAllUrlsButtonSlot() {
		return getData().getInt("CHEST.VoteURL.AllUrlsButton.Slot", -1);
	}

	public int getChestVoteURLAllUrlsButtonStartSlot() {
		return getData().getInt("CHEST.VoteURL.StartSlot", -1);
	}

	public ConfigurationSection getChestVoteURLAlreadyVotedAllUrlsButtonItemSection() {
		if (getData().isConfigurationSection("CHEST.VoteURL.AllUrlsButton.AlreadyVotedItem")) {
			return getData().getConfigurationSection("CHEST.VoteURL.AllUrlsButton.AlreadyVotedItem");
		} else {
			return getData().getConfigurationSection("CHEST.VoteURL.AlreadyVotedItem");
		}
	}

	public ConfigurationSection getChestVoteURLAlreadyVotedItemSection() {
		return getData().getConfigurationSection("CHEST.VoteURL.AlreadyVotedItem");
	}

	public boolean getChestVoteURLBackButton() {
		return getData().getBoolean("CHEST.VoteURL.BackButton");
	}

	public ConfigurationSection getChestVoteURLCanVoteAllUrlsButtonItemSection() {
		if (getData().isConfigurationSection("CHEST.VoteURL.AllUrlsButton.CanVoteItem")) {
			return getData().getConfigurationSection("CHEST.VoteURL.AllUrlsButton.CanVoteItem");
		} else {
			return getData().getConfigurationSection("CHEST.VoteURL.CanVoteItem");
		}
	}

	public ConfigurationSection getChestVoteURLCanVoteItemSection() {
		return getData().getConfigurationSection("CHEST.VoteURL.CanVoteItem");
	}

	public Set<String> getChestVoteURLExtraItems() {
		if (getData().isConfigurationSection("CHEST.VoteURL.ExtraItems")) {
			return getData().getConfigurationSection("CHEST.VoteURL.ExtraItems").getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getChestVoteURLExtraItemsItem(String item) {
		return getData().getConfigurationSection("CHEST.VoteURL.ExtraItems." + item);
	}

	public Set<String> getChestGUIExtraItems(String gui) {
		if (getData().isConfigurationSection("CHEST." + gui + ".ExtraItems")) {
			return getData().getConfigurationSection("CHEST." + gui + ".ExtraItems").getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getChestGUIVoteShopExtraItems(String item) {
		return getData().getConfigurationSection("CHEST.VoteShopExtraItems." + item);
	}

	public Set<String> getChestGUIVoteShopExtraItems() {
		if (getData().isConfigurationSection("CHEST.VoteShopExtraItems")) {
			return getData().getConfigurationSection("CHEST.VoteShopExtraItems").getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getChestGUIExtraItemsItem(String gui, String item) {
		return getData().getConfigurationSection("CHEST." + gui + ".ExtraItems." + item);
	}

	/**
	 * Gets the vote URL site name.
	 *
	 * @return the vote URL site name
	 */
	public String getChestVoteURLGUISiteName() {
		return getData().getString("CHEST.VoteURL.SiteName", "&c%Name%");
	}

	public String getChestVoteURLGUISiteNameCanVote() {
		return getData().getString("CHEST.VoteURL.SiteNameCanVote", "&a%Name%");
	}

	public String getChestVoteURLName() {
		return getData().getString("CHEST.VoteURL.Name", "&cVoteURL");
	}

	/**
	 * Gets the vote URL next vote.
	 *
	 * @return the vote URL next vote
	 */
	public String getChestVoteURLNextVote() {
		return getData().getString("CHEST.VoteURL.NextVote", "&cCan Vote In: %Info%");

	}

	/**
	 * Gets the vote URL see URL.
	 *
	 * @return the vote URL see URL
	 */
	public String getChestVoteURLSeeURL() {
		return getData().getString("CHEST.VoteURL.SeeURL", "&cClick to see URL");
	}

	public String getChestVoteURLSiteName() {
		return getData().getString("CHEST.VoteURLSite.Name", "VoteSite %site%");
	}

	public String getChestVoteURLURLText() {
		return getData().getString("CHEST.VoteURL.URLText", "%VoteUrl%");
	}

	/**
	 * Gets the vote URL view all urls button enabled.
	 *
	 * @return the vote URL view all urls button enabled
	 */
	public boolean getChestVoteURLViewAllUrlsButtonEnabled() {
		return getData().getBoolean("CHEST.VoteURL.ViewAllUrlsButtonEnabled");
	}

	public boolean isChestVoteShopRequireConfirmation(String identifier) {
		return getData().getBoolean("CHEST.Shop." + identifier + ".RequireConfirmation",
				isChestVoteShopRequireConfirmation());
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("GUI.yml", true);
	}

	public void removeShop(String value) {
		getData().set("CHEST.Shop." + value, null);
		saveData();
	}
}
