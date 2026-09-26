package com.bencodez.votingplugin.commands.gui.player;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;

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
}
