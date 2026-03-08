package com.bencodez.votingplugin.voteshop.shop;

import java.util.LinkedHashMap;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

/**
 * Full vote shop definition.
 */
@Getter
@Setter
public class VoteShopDefinition {

	private String title;

	private boolean enabled;

	private boolean backButton;

	private boolean hideLimitedReached;

	private boolean requireConfirmation;

	private boolean reopenGuiOnPurchase;

	private String disabledMessage;

	private String limitReachedMessage;

	private final Map<String, VoteShopEntry> mainEntries = new LinkedHashMap<String, VoteShopEntry>();

	private final Map<String, VoteShopCategory> categories = new LinkedHashMap<String, VoteShopCategory>();

	private final Map<String, VoteShopExtraItem> extraItems = new LinkedHashMap<String, VoteShopExtraItem>();

	/**
	 * Gets a category.
	 *
	 * @param categoryId the category id
	 * @return the category or null
	 */
	public VoteShopCategory getCategory(String categoryId) {
		if (categoryId == null) {
			return null;
		}
		return categories.get(categoryId);
	}
}
