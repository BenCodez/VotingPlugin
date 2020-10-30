package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.Ben12345rocks.AdvancedCore.Util.Annotation.AnnotationHandler;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataBoolean;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataConfigurationSection;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataListString;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataString;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

import lombok.Getter;

public class GUI extends YMLFile {
	/** The instance. */
	static GUI instance = new GUI();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static GUI getInstance() {
		return instance;
	}

	public GUI() {
		super(new File(Main.plugin.getDataFolder(), "GUI.yml"));
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("GUI.yml", true);
	}

	@ConfigDataString(path = "GUIMethod.Today")
	@Getter
	private String guiMethodToday = "CHEST";

	@ConfigDataString(path = "GUIMethod.TopVoter")
	@Getter
	private String guiMethodTopVoter = "CHEST";

	@ConfigDataString(path = "GUIMethod.Last")
	@Getter
	private String guiMethodLast = "CHEST";

	@ConfigDataString(path = "GUIMethod.Next")
	@Getter
	private String guiMethodNext = "CHEST";

	@ConfigDataString(path = "GUIMethod.Total")
	@Getter
	private String guiMethodTotal = "CHEST";

	@ConfigDataString(path = "GUIMethod.URL")
	@Getter
	private String guiMethodURL = "CHEST";

	@ConfigDataString(path = "GUIMethod.Best")
	@Getter
	private String guiMethodBest = "CHEST";

	@ConfigDataString(path = "GUIMethod.Streak")
	@Getter
	private String guiMethodStreak = "CHEST";

	@ConfigDataString(path = "GUIMethod.GUI")
	@Getter
	private String guiMethodGUI = "CHEST";

	@ConfigDataBoolean(path = "LastMonthGUI")
	@Getter
	private boolean lastMonthGUI = false;

	@ConfigDataString(path = "CHEST.VoteToday.Line")
	@Getter
	private String chestVoteTodayLine = "&6%VoteSite% : %Time%";

	@ConfigDataBoolean(path = "CHEST.VoteToday.UseSkull", defaultValue = true)
	@Getter
	private boolean chestVoteTodayUseSkull = true;

	@ConfigDataConfigurationSection(path = "CHEST.VoteToday.PlayerItem")
	@Getter
	private ConfigurationSection chestVoteTodayPlayerItem;

	@ConfigDataString(path = "CHEST.VoteToday.IconTitle")
	@Getter
	private String chestVoteTodayIconTitle = "%player%";

	@ConfigDataBoolean(path = "CHEST.VoteShopHideLimitedReached")
	@Getter
	private boolean chestVoteShopHideLimitedReached = true;

	@ConfigDataString(path = "CHEST.VoteShopLimitReached")
	@Getter
	private String chestVoteShopLimitReached = "&aYou reached your limit";

	@ConfigDataBoolean(path = "CHEST.VoteTop.UseSkull")
	@Getter
	private boolean chestVoteTopUseSkull = true;

	@ConfigDataString(path = "CHEST.VoteTop.PlayerItem.Material")
	@Getter
	private String chestVoteTopPlayerItemMaterial = "PAPER";

	@Getter
	@ConfigDataString(path = "CHEST.VoteShopDisabled")
	private String chestVoteShopDisabled = "&cVote shop disabled";

	@ConfigDataBoolean(path = "CHEST.VoteURL.AllUrlsButton.RequireAllSitesVoted")
	@Getter
	private boolean chestVoteURLAllUrlsButtonrequireAllSitesVoted = true;

	@ConfigDataBoolean(path = "CHEST.VoteShopRequireConfirmation")
	@Getter
	private boolean chestVoteShopRequireConfirmation = false;

	@ConfigDataListString(path = "BOOK.VoteURLBookGUI.Layout")
	@Getter
	private ArrayList<String> bookVoteURLBookGUILayout = ArrayUtils.getInstance()
			.convert(new String[] { "&c%Sitename%", "[UrlText]" });

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.Title")
	@Getter
	private String bookVoteURLBookGUITitle = "&cVoteURL";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.CanVoteText")
	@Getter
	private String bookVoteURLBookGUICanVoteText = "Click me";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.AlreadyVotedText")
	@Getter
	private String bookVoteURLBookGUIAlreadyVotedText = "Click me";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.CanVoteColor")
	@Getter
	private String bookVoteURLBookGUICanVoteColor = "GREEN";

	@ConfigDataString(path = "BOOK.VoteURLBookGUI.AlreadyVotedColor")
	@Getter
	private String bookVoteURLBookGUIAlreadyVotedColor = "RED";

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

	public boolean getChestVoteLastBackButton() {
		return getData().getBoolean("CHEST.VoteLast.BackButton");
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

	public String getChestVoteRewardName() {
		return getData().getString("CHEST.VoteRewardName", "VoteReward");
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
		return getData().getBoolean("CHEST.VoteToday.BackButton");
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

	public int getChestVoteTopSize() {
		return getData().getInt("CHEST.VoteTop.Size", 27);
	}

	public ConfigurationSection getChestVoteTopSwitchItem() {
		return getData().getConfigurationSection("CHEST.VoteTop.SwitchItem");
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

	public boolean getChestVoteURLBackButton() {
		return getData().getBoolean("CHEST.VoteURL.BackButton");
	}

	public String getChestVoteURLName() {
		return getData().getString("CHEST.VoteURL.Name", "&cVoteURL");
	}

	public String getChestVoteURLSiteName() {
		return getData().getString("CHEST.VoteURLSite.Name", "VoteSite %site%");
	}

	public String getChestVoteURLURLText() {
		return getData().getString("CHEST.VoteURL.URLText", "%VoteUrl%");
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

	/**
	 * Gets the vote GUI slot slot.
	 *
	 * @param slot the slot
	 * @return the vote GUI slot slot
	 */
	public int getChestVoteGUISlotSlot(String slot) {
		return getData().getInt("CHEST.VoteGUI." + slot + ".Slot", -1);
	}

	public boolean getChestVoteShopBackButton() {
		return getData().getBoolean("CHEST.VoteShopBackButton", true);
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

	public boolean getChestVoteShopCloseGUI(String shop) {
		return getData().getBoolean("CHEST.Shop." + shop + ".CloseGUI", true);
	}

	public String getChestVoteShopPermission(String ident) {
		return getData().getString("CHEST.Shop." + ident + ".Permission", "");
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

	public ConfigurationSection getChestVoteURLAlreadyVotedAllUrlsButtonItemSection() {
		if (getData().isConfigurationSection("CHEST.VoteURL.AllUrlsButton.AlreadyVotedItem")) {
			return getData().getConfigurationSection("CHEST.VoteURL.AllUrlsButton.AlreadyVotedItem");
		} else {
			return getData().getConfigurationSection("CHEST.VoteURL.AlreadyVotedItem");
		}
	}

	@Getter
	@ConfigDataString(path = "CHEST.ShopConfirmPurchase.Title")
	private String chestShopConfirmPurchaseTitle = "Confirm Purchase?";

	@Getter
	@ConfigDataConfigurationSection(path = "CHEST.ShopConfirmPurchase.YesItem")
	private ConfigurationSection chestShopConfirmPurchaseYesItem;

	@Getter
	@ConfigDataConfigurationSection(path = "CHEST.ShopConfirmPurchase.NoItem")
	private ConfigurationSection chestShopConfirmPurchaseNoItem;

	public ConfigurationSection getChestVoteURLAlreadyVotedItemSection() {
		return getData().getConfigurationSection("CHEST.VoteURL.AlreadyVotedItem");
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

	/**
	 * Gets the vote URL site name.
	 *
	 * @return the vote URL site name
	 */
	public String getChestVoteURLGUISiteName() {
		return getData().getString("CHEST.VoteURL.SiteName", "&c%Name%");
	}

	/**
	 * Gets the vote URL view all urls button enabled.
	 *
	 * @return the vote URL view all urls button enabled
	 */
	public boolean getChestVoteURLViewAllUrlsButtonEnabled() {
		return getData().getBoolean("CHEST.VoteURL.ViewAllUrlsButtonEnabled");
	}

	public ConfigurationSection getCHESTBackButton() {
		return getData().getConfigurationSection("CHEST.BackButton");
	}
}
