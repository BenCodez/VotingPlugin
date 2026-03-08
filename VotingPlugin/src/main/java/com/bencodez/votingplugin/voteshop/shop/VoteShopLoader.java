package com.bencodez.votingplugin.voteshop.shop;

import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.config.ShopFile;

/**
 * Loads vote shop data from ShopFile.
 */
public class VoteShopLoader {

	/**
	 * Loads the vote shop definition.
	 *
	 * @param shopFile the shop file
	 * @return the definition
	 */
	public VoteShopDefinition load(ShopFile shopFile) {
		VoteShopDefinition definition = new VoteShopDefinition();
		definition.setEnabled(shopFile.isVoteShopEnabled());
		definition.setTitle(shopFile.getVoteShopName());
		definition.setBackButton(shopFile.isVoteShopBackButton());
		definition.setHideLimitedReached(shopFile.isVoteShopHideLimitedReached());
		definition.setRequireConfirmation(shopFile.isVoteShopRequireConfirmation());
		definition.setReopenGuiOnPurchase(shopFile.isVoteShopReopenGuiOnPurchase());
		definition.setDisabledMessage(shopFile.getVoteShopDisabled());
		definition.setLimitReachedMessage(shopFile.getVoteShopLimitReached());

		loadMainEntries(shopFile, definition);
		loadCategories(shopFile, definition);
		loadExtraItems(shopFile, definition);
		return definition;
	}

	/**
	 * Loads main shop entries.
	 *
	 * @param shopFile the shop file
	 * @param definition the definition
	 */
	protected void loadMainEntries(ShopFile shopFile, VoteShopDefinition definition) {
		ConfigurationSection section = shopFile.getShopSection();
		if (section == null) {
			return;
		}

		for (String identifier : section.getKeys(false)) {
			ConfigurationSection entrySection = section.getConfigurationSection(identifier);
			if (entrySection == null) {
				continue;
			}

			VoteShopEntry entry = loadEntry(entrySection, identifier, "Shop." + identifier + ".Rewards",
					definition.isRequireConfirmation());
			definition.getMainEntries().put(identifier, entry);
		}
	}

	/**
	 * Loads categories.
	 *
	 * @param shopFile the shop file
	 * @param definition the definition
	 */
	protected void loadCategories(ShopFile shopFile, VoteShopDefinition definition) {
		ConfigurationSection categoriesSection = shopFile.getCategoriesSection();
		if (categoriesSection == null) {
			return;
		}

		for (String categoryId : categoriesSection.getKeys(false)) {
			ConfigurationSection categorySection = categoriesSection.getConfigurationSection(categoryId);
			if (categorySection == null) {
				continue;
			}

			VoteShopCategory category = new VoteShopCategory();
			category.setId(categoryId);
			category.setName(categorySection.getString("Name", categoryId));
			category.setBackButton(categorySection.getBoolean("BackButton", true));

			ConfigurationSection shopSection = categorySection.getConfigurationSection("Shop");
			if (shopSection != null) {
				for (String identifier : shopSection.getKeys(false)) {
					ConfigurationSection entrySection = shopSection.getConfigurationSection(identifier);
					if (entrySection == null) {
						continue;
					}

					VoteShopEntry entry = loadEntry(entrySection, identifier,
							"Categories." + categoryId + ".Shop." + identifier + ".Rewards",
							definition.isRequireConfirmation());
					category.getEntries().put(identifier, entry);
				}
			}

			definition.getCategories().put(categoryId, category);
		}
	}

	/**
	 * Loads extra items.
	 *
	 * @param shopFile the shop file
	 * @param definition the definition
	 */
	protected void loadExtraItems(ShopFile shopFile, VoteShopDefinition definition) {
		Set<String> extraItems = shopFile.getVoteShopExtraItems();
		for (String identifier : extraItems) {
			VoteShopExtraItem item = new VoteShopExtraItem();
			item.setIdentifier(identifier);
			item.setDisplaySection(shopFile.getGUIVoteShopExtraItems(identifier));
			definition.getExtraItems().put(identifier, item);
		}
	}

	/**
	 * Loads one shop entry.
	 *
	 * @param entrySection the entry section
	 * @param identifier the identifier
	 * @param rewardsPath the rewards path
	 * @param defaultRequireConfirmation default confirmation state
	 * @return the entry
	 */
	protected VoteShopEntry loadEntry(ConfigurationSection entrySection, String identifier, String rewardsPath,
			boolean defaultRequireConfirmation) {
		String categoryId = entrySection.getString("Category", "");
		if (!categoryId.isEmpty()) {
			return loadCategoryButton(entrySection, identifier, categoryId);
		}
		return loadItem(entrySection, identifier, rewardsPath, defaultRequireConfirmation);
	}

	/**
	 * Loads a shop item.
	 *
	 * @param entrySection the entry section
	 * @param identifier the identifier
	 * @param rewardsPath the rewards path
	 * @param defaultRequireConfirmation default confirmation state
	 * @return the item
	 */
	protected VoteShopItem loadItem(ConfigurationSection entrySection, String identifier, String rewardsPath,
			boolean defaultRequireConfirmation) {
		VoteShopItem item = new VoteShopItem();
		item.setIdentifier(identifier);
		item.setIdentifierName(entrySection.getString("Identifier_Name", identifier));
		item.setCost(entrySection.getInt("Cost", 0));
		item.setLimit(entrySection.getInt("Limit", -1));
		item.setPermission(entrySection.getString("Permission", ""));
		item.setHideOnNoPermission(entrySection.getBoolean("HideOnNoPermission", true));
		item.setCloseGUI(entrySection.getBoolean("CloseGUI", true));
		item.setRequireConfirmation(entrySection.getBoolean("RequireConfirmation", defaultRequireConfirmation));
		item.setNotBuyable(entrySection.getBoolean("NotBuyable", false));
		item.setPurchaseMessage(entrySection.getString("PurchaseMessage", ""));
		item.setResetDaily(entrySection.getBoolean("Reset.Daily", false));
		item.setResetWeekly(entrySection.getBoolean("Reset.Weekly", false));
		item.setResetMonthly(entrySection.getBoolean("Reset.Monthly", false));
		item.setRewardsPath(rewardsPath);
		item.setDisplaySection(getDisplaySection(entrySection));
		return item;
	}

	/**
	 * Loads a category button.
	 *
	 * @param entrySection the entry section
	 * @param identifier the identifier
	 * @param categoryId the category id
	 * @return the category button
	 */
	protected VoteShopCategoryButton loadCategoryButton(ConfigurationSection entrySection, String identifier,
			String categoryId) {
		VoteShopCategoryButton button = new VoteShopCategoryButton();
		button.setIdentifier(identifier);
		button.setCategoryId(categoryId);
		button.setPermission(entrySection.getString("Permission", ""));
		button.setHideOnNoPermission(entrySection.getBoolean("HideOnNoPermission", true));
		button.setDisplaySection(getDisplaySection(entrySection));
		return button;
	}

	/**
	 * Gets the display section for an entry.
	 *
	 * @param entrySection the entry section
	 * @return the display section
	 */
	protected ConfigurationSection getDisplaySection(ConfigurationSection entrySection) {
		ConfigurationSection displaySection = entrySection.getConfigurationSection("DisplayItem");
		if (displaySection != null) {
			return displaySection;
		}
		return entrySection;
	}
}
