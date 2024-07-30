package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.yml.YMLFile;
import com.bencodez.simpleapi.file.annotation.AnnotationHandler;
import com.bencodez.simpleapi.file.annotation.ConfigDataBoolean;
import com.bencodez.simpleapi.file.annotation.ConfigDataConfigurationSection;
import com.bencodez.simpleapi.file.annotation.ConfigDataKeys;
import com.bencodez.simpleapi.file.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class ShopFile extends YMLFile {

	@Getter
	@ConfigDataConfigurationSection(path = "ShopConfirmPurchase.NoItem")
	private ConfigurationSection shopConfirmPurchaseNoItem;

	@Getter
	@ConfigDataString(path = "ShopConfirmPurchase.Title")
	private String shopConfirmPurchaseTitle = "Confirm Purchase?";

	@Getter
	@ConfigDataConfigurationSection(path = "ShopConfirmPurchase.YesItem")
	private ConfigurationSection shopConfirmPurchaseYesItem;

	@Getter
	@ConfigDataString(path = "VoteShop.Disabled")
	private String voteShopDisabled = "&cVote shop disabled";

	@ConfigDataBoolean(path = "VoteShop.HideLimitedReached")
	@Getter
	private boolean voteShopHideLimitedReached = true;

	@ConfigDataString(path = "VoteShop.LimitReached")
	@Getter
	private String voteShopLimitReached = "&aYou reached your limit";

	@ConfigDataBoolean(path = "VoteShop.RequireConfirmation")
	@Getter
	private boolean voteShopRequireConfirmation = false;

	private VotingPluginMain plugin;

	public ShopFile(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "Shop.yml"));
		this.plugin = plugin;
	}

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

	public int getShopIdentifierCost(String identifier) {
		return getData().getInt("Shop." + identifier + ".Cost");
	}

	public String getShopIdentifierIdentifierName(String identifier) {
		return getData().getString("Shop." + identifier + ".Identifier_Name", identifier);
	}

	public int getShopIdentifierLimit(String identifier) {
		return getData().getInt("Shop." + identifier + ".Limit", -1);
	}

	public String getShopIdentifierRewardsPath(String identifier) {
		return "Shop." + identifier + ".Rewards";
	}

	public Set<String> getShopIdentifiers() {
		ConfigurationSection shop = getData().getConfigurationSection("Shop");
		if (shop != null) {
			return shop.getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getShopIdentifierSection(String identifier) {
		return getData().getConfigurationSection("Shop." + identifier);
	}

	@ConfigDataBoolean(path = "VoteShop.BackButton")
	@Getter
	private boolean voteShopBackButton = true;

	@ConfigDataBoolean(path = "VoteShop.ReopenGUIOnPurchase")
	@Getter
	private boolean voteShopReopenGUIOnPurchase = true;

	public boolean getVoteShopCloseGUI(String shop) {
		return getData().getBoolean("Shop." + shop + ".CloseGUI", true);
	}

	public boolean getVoteShopHideOnNoPermission(String shop) {
		return getData().getBoolean("Shop." + shop + ".HideOnNoPermission", true);
	}

	@ConfigDataBoolean(path = "VoteShop.Enabled")
	@Getter
	private boolean voteShopEnabled = true;

	@ConfigDataString(path = "VoteShop.Name")
	@Getter
	private String voteShopName = "VoteShop";

	public boolean getVoteShopNotBuyable(String shop) {
		return getData().getBoolean("Shop." + shop + ".NotBuyable", false);
	}

	public String getVoteShopPermission(String ident) {
		return getData().getString("Shop." + ident + ".Permission", "");
	}

	public String getVoteShopPurchase(String identifier) {
		return getData().getString("Shop." + identifier + ".PurchaseMessage",
				plugin.getConfigFile().getFormatShopPurchaseMsg());
	}

	public boolean getVoteShopResetDaily(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Daily", false);
	}

	public boolean getVoteShopResetMonthly(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Monthly", false);
	}

	public boolean getVoteShopResetWeekly(String shop) {
		return getData().getBoolean("Shop." + shop + ".Reset.Weekly", false);
	}

	public ConfigurationSection getGUIVoteShopExtraItems(String item) {
		return getData().getConfigurationSection("ExtraItems." + item);
	}

	@ConfigDataKeys(path = "ExtraItems")
	@Getter
	private Set<String> voteShopExtraItems = new HashSet<String>();

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

	public void convertFromGUIFile() {
		// booleans
		setValue("VoteShop.Enabled", plugin.getGui().getData().getBoolean("CHEST.VoteShopEnabled"));
		setValue("VoteShop.BackButton", plugin.getGui().getData().getBoolean("CHEST.VoteShopBackButton"));
		setValue("VoteShop.HideLimitReached", plugin.getGui().getData().getBoolean("CHEST.VoteShopHideLimitedReached"));
		setValue("VoteShop.RequireConfirmation",
				plugin.getGui().getData().getBoolean("CHEST.VoteShopRequireConfirmation"));
		setValue("VoteShop.ReopenGUIOnPurchase",
				plugin.getGui().getData().getBoolean("CHEST.VoteShopReopenGUIOnPurchase"));

		// strings
		setValue("VoteShop.Name", plugin.getGui().getData().getString("CHEST.VoteShopName"));
		setValue("VoteShop.LimitReached", plugin.getGui().getData().getString("CHEST.VoteShopLimitReached"));
		setValue("VoteShop.Disabled", plugin.getGui().getData().getString("CHEST.VoteShopDisabled"));

		// sections
		setValue("Shop", plugin.getGui().getData().getConfigurationSection("CHEST.Shop"));
		setValue("ExtraItems", plugin.getGui().getData().getConfigurationSection("CHEST.VoteShopExtraItems"));
		setValue("ShopConfirmPurchase", plugin.getGui().getData().getConfigurationSection("CHEST.ShopConfirmPurchase"));
		saveData();

	}

	public void removeShop(String value) {
		getData().set("Shop." + value, null);
		saveData();
	}
}
