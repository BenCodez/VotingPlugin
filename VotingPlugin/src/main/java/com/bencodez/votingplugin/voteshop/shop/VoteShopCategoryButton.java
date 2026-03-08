package com.bencodez.votingplugin.voteshop.shop;

import lombok.Getter;
import lombok.Setter;

/**
 * Vote shop entry that opens a category.
 */
@Getter
@Setter
public class VoteShopCategoryButton extends VoteShopEntry {

	private String categoryId;

	private String permission = "";

	private boolean hideOnNoPermission = true;
}
