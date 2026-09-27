package com.bencodez.votingplugin.commands.gui.player;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

class VoteShopCategoryAuthorizationTest {

	@Test
	void visibleUnauthorizedButtonStillCannotOpenCategory() {
		VoteShopCategoryButton button = button("restricted", "shop.category", false);
		VoteShopDefinition definition = definition(button);

		assertTrue(VoteShopCategoryAuthorization.mayDisplay(button, permission -> false));
		assertFalse(VoteShopCategoryAuthorization.authorized(button, permission -> false));
		assertFalse(VoteShopCategoryAuthorization.canOpen(definition, "restricted", permission -> false));
	}

	@Test
	void hideFlagControlsVisibilityWithoutChangingAuthorization() {
		VoteShopCategoryButton hidden = button("restricted", "shop.category", true);

		assertFalse(VoteShopCategoryAuthorization.mayDisplay(hidden, permission -> false));
		assertTrue(VoteShopCategoryAuthorization.mayDisplay(hidden, permission -> true));
	}

	@Test
	void directAndNestedOpeningRequiresAnAuthorizedPath() {
		VoteShopDefinition definition = new VoteShopDefinition();
		VoteShopCategoryButton parent = button("parent", "shop.parent", false);
		definition.getMainEntries().put("parent", parent);
		VoteShopCategory parentCategory = new VoteShopCategory();
		parentCategory.setId("parent");
		parentCategory.getEntries().put("child", button("child", "shop.child", false));
		definition.getCategories().put("parent", parentCategory);
		VoteShopCategory child = new VoteShopCategory();
		child.setId("child");
		definition.getCategories().put("child", child);

		assertFalse(VoteShopCategoryAuthorization.canOpen(definition, "child",
				Set.of("shop.child")::contains));
		assertFalse(VoteShopCategoryAuthorization.canOpen(definition, "child",
				Set.of("shop.parent")::contains));
		assertTrue(VoteShopCategoryAuthorization.canOpen(definition, "child",
				Set.of("shop.parent", "shop.child")::contains));
	}

	@Test
	void purchaseRechecksCurrentCategoryPermission() {
		VoteShopCategoryButton button = button("restricted", "shop.category", false);
		VoteShopDefinition definition = definition(button);
		VoteShopCategory category = definition.getCategory("restricted");
		VoteShopItem item = item("item");
		category.getEntries().put("item", item);
		AtomicBoolean permitted = new AtomicBoolean(true);

		assertTrue(VoteShopCategoryAuthorization.canPurchase(definition, category, item,
				permission -> permitted.get()));
		permitted.set(false);
		assertFalse(VoteShopCategoryAuthorization.canPurchase(definition, category, item,
				permission -> permitted.get()));
		VoteShopItem mainItem = item("main");
		definition.getMainEntries().put("main", mainItem);
		assertTrue(VoteShopCategoryAuthorization.canPurchase(definition, null, mainItem, permission -> false));
	}

	@Test
	void purchaseRejectsCategoryRemovedByReload() {
		VoteShopDefinition definition = definition(button("restricted", "shop.category", false));
		VoteShopCategory staleCategory = definition.getCategory("restricted");
		VoteShopItem item = item("item");
		staleCategory.getEntries().put("item", item);

		definition.getCategories().remove("restricted");

		assertFalse(VoteShopCategoryAuthorization.canPurchase(definition, staleCategory, item,
				permission -> true));
	}

	@Test
	void purchaseRejectsCategoryAndItemObjectsRetainedAcrossReload() {
		VoteShopDefinition definition = definition(button("restricted", "shop.category", false));
		VoteShopCategory staleCategory = definition.getCategory("restricted");
		VoteShopItem staleItem = item("item");
		staleCategory.getEntries().put("item", staleItem);
		VoteShopCategory replacement = new VoteShopCategory();
		replacement.setId("restricted");
		replacement.getEntries().put("item", item("item"));
		definition.getCategories().put("restricted", replacement);

		assertFalse(VoteShopCategoryAuthorization.canPurchase(definition, staleCategory, staleItem,
				permission -> true));
	}

	private static VoteShopDefinition definition(VoteShopCategoryButton button) {
		VoteShopDefinition definition = new VoteShopDefinition();
		definition.getMainEntries().put("category", button);
		VoteShopCategory category = new VoteShopCategory();
		category.setId(button.getCategoryId());
		definition.getCategories().put(category.getId(), category);
		return definition;
	}

	private static VoteShopCategoryButton button(String categoryId, String permission, boolean hide) {
		VoteShopCategoryButton button = new VoteShopCategoryButton();
		button.setCategoryId(categoryId);
		button.setPermission(permission);
		button.setHideOnNoPermission(hide);
		return button;
	}

	private static VoteShopItem item(String identifier) {
		VoteShopItem item = new VoteShopItem();
		item.setIdentifier(identifier);
		return item;
	}
}
