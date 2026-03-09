package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.file.YMLFile;
import com.bencodez.simpleapi.file.annotation.AnnotationHandler;
import com.bencodez.simpleapi.file.annotation.ConfigDataBoolean;
import com.bencodez.simpleapi.file.annotation.ConfigDataConfigurationSection;
import com.bencodez.simpleapi.file.annotation.ConfigDataKeys;
import com.bencodez.simpleapi.file.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

/**
 * The ShopFile class manages the vote shop configuration.
 */
@Getter
@Setter
public class ShopFile extends YMLFile {

	@ConfigDataConfigurationSection(path = "ShopConfirmPurchase.NoItem")
	private ConfigurationSection shopConfirmPurchaseNoItem;

	@ConfigDataBoolean(path = "ShopConfirmPurchase.UseDialog")
	private boolean shopConfirmPurchaseUseDialog = false;

	@ConfigDataString(path = "ShopConfirmPurchase.Title")
	private String shopConfirmPurchaseTitle = "Confirm Purchase?";

	@ConfigDataConfigurationSection(path = "ShopConfirmPurchase.YesItem")
	private ConfigurationSection shopConfirmPurchaseYesItem;

	@ConfigDataString(path = "VoteShop.Disabled")
	private String voteShopDisabled = "&cVote shop disabled";

	@ConfigDataBoolean(path = "VoteShop.HideLimitedReached")
	private boolean voteShopHideLimitedReached = true;

	@ConfigDataString(path = "VoteShop.LimitReached")
	private String voteShopLimitReached = "&aYou reached your limit";

	@ConfigDataBoolean(path = "VoteShop.RequireConfirmation")
	private boolean voteShopRequireConfirmation = false;

	private VotingPluginMain plugin;

	@ConfigDataBoolean(path = "VoteShop.BackButton")
	private boolean voteShopBackButton = true;

	@ConfigDataBoolean(path = "VoteShop.ReopenGUIOnPurchase")
	private boolean voteShopReopenGuiOnPurchase = true;

	@ConfigDataBoolean(path = "VoteShop.Enabled")
	private boolean voteShopEnabled = true;

	@ConfigDataString(path = "VoteShop.Name")
	private String voteShopName = "VoteShop";

	@ConfigDataKeys(path = "ExtraItems")
	private Set<String> voteShopExtraItems = new HashSet<String>();

	/**
	 * Constructs a new ShopFile.
	 *
	 * @param plugin the main plugin instance
	 */
	public ShopFile(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "Shop.yml"));
		setIgnoreCase(plugin.getConfigFile().isCaseInsensitiveYMLFiles());
		this.plugin = plugin;
	}

	/**
	 * Converts vote shop data from the GUI file to this shop file.
	 */
	public void convertFromGUIFile() {
		setValue("VoteShop.Enabled", plugin.getGui().getData().getBoolean("CHEST.VoteShopEnabled"));
		setValue("VoteShop.BackButton", plugin.getGui().getData().getBoolean("CHEST.VoteShopBackButton"));
		setValue("VoteShop.HideLimitReached", plugin.getGui().getData().getBoolean("CHEST.VoteShopHideLimitedReached"));
		setValue("VoteShop.RequireConfirmation",
				plugin.getGui().getData().getBoolean("CHEST.VoteShopRequireConfirmation"));
		setValue("VoteShop.ReopenGUIOnPurchase",
				plugin.getGui().getData().getBoolean("CHEST.VoteShopReopenGUIOnPurchase"));
		setValue("VoteShop.Name", plugin.getGui().getData().getString("CHEST.VoteShopName"));
		setValue("VoteShop.LimitReached", plugin.getGui().getData().getString("CHEST.VoteShopLimitReached"));
		setValue("VoteShop.Disabled", plugin.getGui().getData().getString("CHEST.VoteShopDisabled"));
		setValue("Shop", plugin.getGui().getData().getConfigurationSection("CHEST.Shop"));
		setValue("ExtraItems", plugin.getGui().getData().getConfigurationSection("CHEST.VoteShopExtraItems"));
		setValue("ShopConfirmPurchase", plugin.getGui().getData().getConfigurationSection("CHEST.ShopConfirmPurchase"));
		saveData();
	}

	/**
	 * Creates a new legacy shop item.
	 *
	 * @param value the identifier
	 */
	public void createShop(String value) {
		ConfigurationSection shopData = getData().createSection("Shop." + value);
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

	/**
	 * Gets the categories section.
	 *
	 * @return the categories section
	 */
	public ConfigurationSection getCategoriesSection() {
		return getData().getConfigurationSection("Categories");
	}

	/**
	 * Gets a category section.
	 *
	 * @param categoryId the category id
	 * @return the category section
	 */
	public ConfigurationSection getCategorySection(String categoryId) {
		return getData().getConfigurationSection("Categories." + categoryId);
	}

	/**
	 * Gets a category shop section.
	 *
	 * @param categoryId the category id
	 * @return the category shop section
	 */
	public ConfigurationSection getCategoryShopSection(String categoryId) {
		return getData().getConfigurationSection("Categories." + categoryId + ".Shop");
	}

	/**
	 * Checks if a category back button is enabled.
	 *
	 * @param category the category id
	 * @return true if enabled
	 */
	public boolean isCategoryBackButtonEnabled(String category) {
		return getData().getBoolean("Categories." + category + ".BackButton", false);
	}

	/**
	 * Gets the category back button item section.
	 *
	 * @param category the category id
	 * @return configuration section or null
	 */
	public ConfigurationSection getCategoryBackButtonItem(String category) {
		String path = "Categories." + category + ".BackButtonItem";

		if (getData().isConfigurationSection(path)) {
			return getData().getConfigurationSection(path);
		}

		return null;
	}

	/**
	 * Gets the default VoteShop back button item.
	 *
	 * @return configuration section or null
	 */
	public ConfigurationSection getDefaultBackButtonItem() {
		if (getData().isConfigurationSection("VoteShop.BackButtonItem")) {
			return getData().getConfigurationSection("VoteShop.BackButtonItem");
		}
		return null;
	}

	/**
	 * Gets the display section for an entry, preferring DisplayItem when present.
	 *
	 * @param section the entry section
	 * @return the display section
	 */
	public ConfigurationSection getDisplaySection(ConfigurationSection section) {
		if (section == null) {
			return null;
		}
		ConfigurationSection displaySection = section.getConfigurationSection("DisplayItem");
		if (displaySection != null) {
			return displaySection;
		}
		return section;
	}

	/**
	 * Gets the configuration for a vote shop extra item.
	 *
	 * @param item the item identifier
	 * @return the configuration section
	 */
	public ConfigurationSection getGUIVoteShopExtraItems(String item) {
		return getData().getConfigurationSection("ExtraItems." + item);
	}

	/**
	 * Gets the cost of a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the cost
	 */
	public int getShopIdentifierCost(String identifier) {
		return getData().getInt("Shop." + identifier + ".Cost");
	}

	/**
	 * Gets the identifier name for a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the identifier name
	 */
	public String getShopIdentifierIdentifierName(String identifier) {
		return getData().getString("Shop." + identifier + ".Identifier_Name", identifier);
	}

	/**
	 * Gets the purchase limit for a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the purchase limit
	 */
	public int getShopIdentifierLimit(String identifier) {
		return getData().getInt("Shop." + identifier + ".Limit", -1);
	}

	/**
	 * Gets the rewards path for a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the rewards path
	 */
	public String getShopIdentifierRewardsPath(String identifier) {
		return "Shop." + identifier + ".Rewards";
	}

	/**
	 * Gets all shop identifiers.
	 *
	 * @return the set of shop identifiers
	 */
	public Set<String> getShopIdentifiers() {
		ConfigurationSection shop = getData().getConfigurationSection("Shop");
		if (shop != null) {
			return shop.getKeys(false);
		}
		return new HashSet<String>();
	}

	/**
	 * Gets the shop root section.
	 *
	 * @return the shop section
	 */
	public ConfigurationSection getShopSection() {
		return getData().getConfigurationSection("Shop");
	}

	/**
	 * Gets the configuration section for a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the configuration section
	 */
	public ConfigurationSection getShopIdentifierSection(String identifier) {
		return getData().getConfigurationSection("Shop." + identifier);
	}

	/**
	 * Checks if the GUI should close on purchase.
	 *
	 * @param shop the shop identifier
	 * @return true if GUI should close
	 */
	public boolean getVoteShopCloseGUI(String shop) {
		return getData().getBoolean("Shop." + shop + ".CloseGUI", true);
	}

	/**
	 * Checks if the shop item should be hidden when player has no permission.
	 *
	 * @param shop the shop identifier
	 * @return true if should hide
	 */
	public boolean getVoteShopHideOnNoPermission(String shop) {
		return getData().getBoolean("Shop." + shop + ".HideOnNoPermission", true);
	}

	/**
	 * Checks if the shop item is not buyable.
	 *
	 * @param shop the shop identifier
	 * @return true if not buyable
	 */
	public boolean getVoteShopNotBuyable(String shop) {
		return getData().getBoolean("Shop." + shop + ".NotBuyable", false);
	}

	/**
	 * Gets the permission required for a shop item.
	 *
	 * @param ident the shop identifier
	 * @return the permission
	 */
	public String getVoteShopPermission(String ident) {
		return getData().getString("Shop." + ident + ".Permission", "");
	}

	/**
	 * Gets the purchase message for a shop item.
	 *
	 * @param identifier the shop identifier
	 * @return the purchase message
	 */
	public String getVoteShopPurchase(String identifier) {
		return getData().getString("Shop." + identifier + ".PurchaseMessage",
				plugin.getConfigFile().getFormatShopPurchaseMsg());
	}

	/**
	 * Checks if the shop item resets daily.
	 *
	 * @param shop the shop identifier
	 * @return true if resets daily
	 */
	public boolean getVoteShopResetDaily(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Daily", false);
	}

	/**
	 * Checks if the shop item resets monthly.
	 *
	 * @param shop the shop identifier
	 * @return true if resets monthly
	 */
	public boolean getVoteShopResetMonthly(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Monthly", false);
	}

	/**
	 * Checks if the shop item resets weekly.
	 *
	 * @param shop the shop identifier
	 * @return true if resets weekly
	 */
	public boolean getVoteShopResetWeekly(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Weekly", false);
	}

	/**
	 * Checks if the shop item requires purchase confirmation.
	 *
	 * @param identifier the shop identifier
	 * @return true if requires confirmation
	 */
	public boolean isVoteShopRequireConfirmation(String identifier) {
		return getData().getBoolean("Shop." + identifier + ".RequireConfirmation", isVoteShopRequireConfirmation());
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("Shop.yml", true);
	}

	/**
	 * Removes a shop item.
	 *
	 * @param value the shop identifier
	 */
	public void removeShop(String value) {
		getData().set("Shop." + value, null);
		saveData();
	}
}