package com.bencodez.votingplugin.voteshop.service;

import lombok.Getter;

/**
 * Result of a vote shop purchase validation or execution.
 */
@Getter
public enum VoteShopPurchaseResult {

	SUCCESS,
	SHOP_DISABLED,
	ITEM_NOT_FOUND,
	NO_PERMISSION,
	NOT_BUYABLE,
	LIMIT_REACHED,
	NOT_ENOUGH_POINTS,
	INVALID_ENTRY,
	CATEGORY_NOT_FOUND;
}
