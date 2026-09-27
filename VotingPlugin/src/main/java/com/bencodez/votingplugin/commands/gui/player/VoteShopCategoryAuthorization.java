package com.bencodez.votingplugin.commands.gui.player;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopEntry;

/** Keeps VoteShop category visibility separate from authorization. */
final class VoteShopCategoryAuthorization {
	private VoteShopCategoryAuthorization() {
	}

	static boolean mayDisplay(VoteShopCategoryButton button, Predicate<String> permissionCheck) {
		return authorized(button, permissionCheck) || !button.isHideOnNoPermission();
	}

	static boolean authorized(VoteShopCategoryButton button, Predicate<String> permissionCheck) {
		return button != null && permissionCheck.test(button.getPermission());
	}

	/** Returns true only when an authorized path from the main shop reaches the category. */
	static boolean canOpen(VoteShopDefinition definition, String targetCategory,
			Predicate<String> permissionCheck) {
		if (definition == null || targetCategory == null || targetCategory.isEmpty()) return false;
		return reaches(definition, definition.getMainEntries().values(), targetCategory,
				permissionCheck, new HashSet<>());
	}

	static boolean canPurchase(VoteShopDefinition definition, VoteShopCategory category,
			Predicate<String> permissionCheck) {
		return category == null || canOpen(definition, category.getId(), permissionCheck);
	}

	private static boolean reaches(VoteShopDefinition definition, Iterable<VoteShopEntry> entries,
			String targetCategory, Predicate<String> permissionCheck, Set<String> visited) {
		for (VoteShopEntry entry : entries) {
			if (!(entry instanceof VoteShopCategoryButton button) || !authorized(button, permissionCheck)) continue;
			String categoryId = button.getCategoryId();
			if (categoryId == null) continue;
			VoteShopCategory category = definition.getCategory(categoryId);
			if (category == null) continue;
			if (targetCategory.equals(categoryId)) return true;
			if (!visited.add(categoryId)) continue;
			if (reaches(definition, category.getEntries().values(), targetCategory,
					permissionCheck, visited)) return true;
		}
		return false;
	}
}
