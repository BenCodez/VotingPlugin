package com.bencodez.votingplugin.commands.gui.player;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopEntry;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

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

	static boolean canPurchase(VoteShopDefinition definition, VoteShopCategory category, VoteShopItem item,
			Predicate<String> permissionCheck) {
		if (definition == null || item == null) return false;
		if (category == null) return containsSameEntry(definition.getMainEntries().values(), item);
		VoteShopCategory currentCategory = definition.getCategory(category.getId());
		return currentCategory == category && containsSameEntry(currentCategory.getEntries().values(), item)
				&& canOpen(definition, category.getId(), permissionCheck);
	}

	private static boolean containsSameEntry(Iterable<VoteShopEntry> entries, VoteShopEntry expected) {
		for (VoteShopEntry entry : entries) if (entry == expected) return true;
		return false;
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
