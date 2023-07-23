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
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataInt;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataKeys;
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

	@ConfigDataBoolean(path = "BOOK.VoteURLBookGUI.Manual")
	@Getter
	private boolean bookVoteURLBookGUIManual = false;

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.CanVoteText")
	@Getter
	private String bookVoteURLBookGUICanVoteText = "Click me";

	@ConfigDataListString(path = "BOOK.VoteURLBookGUI.Layout")
	@Getter
	private ArrayList<String> bookVoteURLBookGUILayout = ArrayUtils.getInstance()
			.convert(new String[] { "&c%Sitename%", "[UrlText]" });

	@ConfigDataListString(path = "BOOK.VoteURLBookGUI.TopLayout")
	@Getter
	private ArrayList<String> bookVoteURLBookGUITopLayout = new ArrayList<String>();

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

	@ConfigDataConfigurationSection(path = "CHEST.BackButton")
	@Getter
	private ConfigurationSection CHESTBackButton;

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

	@ConfigDataConfigurationSection(path = "CHEST.VoteBest.DayBest.Item")
	@Getter
	private ConfigurationSection chestVoteBestDayBestItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteBest.MonthBest.Item")
	@Getter
	private ConfigurationSection chestVoteBestMonthBestItem;

	@ConfigDataString(path = "CHEST.VoteBest.Name")
	@Getter
	private String chestVoteBestName = "VoteBest: %player%";

	@ConfigDataConfigurationSection(path = "CHEST.VoteBest.WeekBest.Item")
	@Getter
	private ConfigurationSection chestVoteBestWeekBestItem;

	@ConfigDataString(path = "CHEST.VoteGUIName")
	@Getter
	private String chestVoteGUIName = "&cVoteGUI: &c&l%player%";

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

	@ConfigDataKeys(path = "CHEST.VoteGUI")
	@Getter
	private Set<String> chestVoteGUISlots = new HashSet<String>();

	public ConfigurationSection getChestVoteGUISlotSection(String slot) {
		return getData().getConfigurationSection("CHEST.VoteGUI." + slot + ".Item");
	}

	@ConfigDataConfigurationSection(path = "CHEST.VoteTop.Customization")
	@Getter
	private ConfigurationSection chestVoteTopCustomization;

	/**
	 * Gets the vote GUI slot slot.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot slot
	 */
	public int getChestVoteGUISlotSlot(String slot) {
		return getData().getInt("CHEST.VoteGUI." + slot + ".Slot", -1);
	}

	@ConfigDataBoolean(path = "CHEST.VoteLast.BackButton")
	@Getter
	private boolean chestVoteLastBackButton = false;

	@ConfigDataString(path = "CHEST.VoteLast.Line")
	@Getter
	private String chestVoteLastLine = "&6%timeSince%";

	@ConfigDataString(path = "CHEST.VoteLast.Name")
	@Getter
	private String chestVoteLastName = "VoteLast: %player%";

	@ConfigDataBoolean(path = "CHEST.VoteNext.BackButton")
	@Getter
	private boolean chestVoteNextBackButton = false;

	@ConfigDataString(path = "CHEST.VoteNext.Name")
	@Getter
	private String chestVoteNextName = "VoteNext: %player%";

	@ConfigDataString(path = "CHEST.VoteNext.Line")
	@Getter
	private String chestVoteNextLine = "%time%";

	public String getChestVoteNextCustomSiteNamesDisplays(String site) {
		return getData().getString("CHEST.VoteNext.CustomSiteNamesDisplays." + site, "");
	}

	@ConfigDataString(path = "CHEST.VoteRewardName")
	@Getter
	private String chestVoteRewardName = "VoteReward";

	@ConfigDataBoolean(path = "CHEST.VoteShopBackButton")
	@Getter
	private boolean chestVoteShopBackButton = true;

	@ConfigDataBoolean(path = "CHEST.VoteShopReopenGUIOnPurchase")
	@Getter
	private boolean chestVoteShopReopenGUIOnPurchase = true;

	public boolean getChestVoteShopCloseGUI(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".CloseGUI", true);
	}

	@ConfigDataBoolean(path = "CHEST.VoteShopEnabled")
	@Getter
	private boolean chestVoteShopEnabled = true;

	@ConfigDataString(path = "CHEST.VoteShopName")
	@Getter
	private String chestVoteShopName = "VoteShop";

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

	@ConfigDataBoolean(path = "CHEST.VoteStreak.BackButton")
	@Getter
	private boolean chestVoteStreakBackButton = false;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.CurrentDayStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakCurrentDayStreakItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.CurrentMonthStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakCurrentMonthStreakItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.CurrentWeekStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakCurrentWeekStreakItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.HighestDayStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakHighestDayStreakItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.HighestMonthStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakHighestMonthStreakItem;

	@ConfigDataConfigurationSection(path = "CHEST.VoteStreak.HighestWeekStreak.Item")
	@Getter
	private ConfigurationSection chestVoteStreakHighestWeekStreakItem;

	@ConfigDataString(path = "CHEST.VoteStreak.Name")
	@Getter
	private String chestVoteStreakName = "VoteStreak";

	@ConfigDataBoolean(path = "CHEST.VoteToday.BackButton")
	@Getter
	private boolean chestVoteTodayBackButton = false;

	@ConfigDataString(path = "CHEST.VoteToday.Name")
	@Getter
	private String chestVoteTodayName = "VoteToday";

	@ConfigDataBoolean(path = "CHEST.VoteTop.BackButton")
	@Getter
	private boolean chestVoteTopBackButton = false;

	@ConfigDataString(path = "CHEST.VoteTop.Item.Lore")
	@Getter
	private String chestVoteTopItemLore = "&3&lVotes: &3%votes%";

	@ConfigDataString(path = "CHEST.VoteTop.Item.Name")
	@Getter
	private String chestVoteTopItemName = "&3&l%position%: &3%player%";

	@ConfigDataString(path = "CHEST.VoteTop.Name")
	@Getter
	private String chestVoteTopName = "VoteTop %topvoter%";

	@ConfigDataBoolean(path = "CHEST.VoteTop.OpenMainGUIOnClick")
	@Getter
	private boolean chestVoteTopOpenMainGUIOnClick = true;

	@ConfigDataBoolean(path = "CHEST.VoteTop.CloseGUIOnClick")
	@Getter
	private boolean chestVoteTopCloseGUIOnClick = true;

	@ConfigDataInt(path = "CHEST.VoteTop.Size")
	@Getter
	private int chestVoteTopSize = 27;

	@ConfigDataConfigurationSection(path = "CHEST.VoteTop.SwitchItem")
	@Getter
	private ConfigurationSection chestVoteTopSwitchItem;

	@ConfigDataListString(path = "CHEST.VoteTop.SwitchItem.TopVoters")
	@Getter
	private ArrayList<String> chestVoteTopSwitchItemTopVoters = new ArrayList<String>();

	@ConfigDataConfigurationSection(path = "CHEST.VoteTotal.AllTimeTotal.Item")
	@Getter
	private ConfigurationSection chestVoteTotalAllTimeTotalItem;

	@ConfigDataBoolean(path = "CHEST.VoteTotal.BackButton")
	@Getter
	private boolean chestVoteTotalBackButton = false;

	@ConfigDataConfigurationSection(path = "CHEST.VoteTotal.DayTotal.Item")
	@Getter
	private ConfigurationSection chestVoteTotalDayTotalItem;

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

	@ConfigDataConfigurationSection(path = "CHEST.VoteTotal.MonthTotal.Item")
	@Getter
	private ConfigurationSection chestVoteTotalMonthTotalItem;

	@ConfigDataString(path = "CHEST.VoteTotal.Name")
	@Getter
	private String chestVoteTotalName = "VoteTotal: %player%";

	@ConfigDataConfigurationSection(path = "CHEST.VoteTotal.WeekTotal.Item")
	@Getter
	private ConfigurationSection chestVoteTotalWeekTotalItem;

	@ConfigDataInt(path = "CHEST.VoteURL.AllUrlsButton.Slot")
	@Getter
	private int chestVoteURLAllUrlsButtonSlot = -1;

	@ConfigDataInt(path = "CHEST.VoteURL.StartSlot")
	@Getter
	private int chestVoteURLAllUrlsButtonStartSlot = -1;

	@ConfigDataConfigurationSection(path = "CHEST.VoteURL.AllUrlsButton.AlreadyVotedItem", secondPath = "CHEST.VoteURL.AlreadyVotedItem")
	@Getter
	private ConfigurationSection chestVoteURLAlreadyVotedAllUrlsButtonItemSection;

	@ConfigDataConfigurationSection(path = "CHEST.VoteURL.AlreadyVotedItem")
	@Getter
	private ConfigurationSection chestVoteURLAlreadyVotedItemSection;

	@ConfigDataBoolean(path = "CHEST.VoteURL.BackButton")
	@Getter
	private boolean chestVoteURLBackButton = false;

	@ConfigDataConfigurationSection(path = "CHEST.VoteURL.AllUrlsButton.CanVoteItem", secondPath = "CHEST.VoteURL.CanVoteItem")
	@Getter
	private ConfigurationSection chestVoteURLCanVoteAllUrlsButtonItemSection;

	@ConfigDataConfigurationSection(path = "CHEST.VoteURL.CanVoteItem")
	@Getter
	private ConfigurationSection chestVoteURLCanVoteItemSection;

	@ConfigDataKeys(path = "CHEST.VoteURL.ExtraItems")
	@Getter
	private Set<String> chestVoteURLExtraItems = new HashSet<String>();

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

	@ConfigDataKeys(path = "CHEST.VoteShopExtraItems")
	@Getter
	private Set<String> chestGUIVoteShopExtraItems = new HashSet<String>();

	public ConfigurationSection getChestGUIExtraItemsItem(String gui, String item) {
		return getData().getConfigurationSection("CHEST." + gui + ".ExtraItems." + item);
	}

	@ConfigDataString(path = "CHEST.VoteURL.SiteName")
	@Getter
	private String chestVoteURLGUISiteName = "&c%Name%";

	@ConfigDataString(path = "CHEST.VoteURL.SiteNameCanVote")
	@Getter
	private String chestVoteURLGUISiteNameCanVote = "&a%Name%";

	@ConfigDataString(path = "CHEST.VoteURL.Name")
	@Getter
	private String chestVoteURLName = "&cVoteURL";

	@ConfigDataString(path = "CHEST.VoteURL.NextVote")
	@Getter
	private String chestVoteURLNextVote = "&cCan Vote In: %Info%";

	@ConfigDataString(path = "CHEST.VoteURL.SeeURL")
	@Getter
	private String chestVoteURLSeeURL = "&cClick to see URL";

	@ConfigDataString(path = "CHEST.VoteURLSite.Name")
	@Getter
	private String chestVoteURLSiteName = "VoteSite %site%";

	@ConfigDataString(path = "CHEST.VoteURL.URLText")
	@Getter
	private String chestVoteURLURLText = "%VoteUrl%";

	@ConfigDataBoolean(path = "CHEST.VoteURL.ViewAllUrlsButtonEnabled")
	@Getter
	private boolean chestVoteURLViewAllUrlsButtonEnabled = false;

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
