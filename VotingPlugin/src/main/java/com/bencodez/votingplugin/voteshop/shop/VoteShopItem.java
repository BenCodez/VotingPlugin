package com.bencodez.votingplugin.voteshop.shop;

import lombok.Getter;
import lombok.Setter;

/**
 * Purchasable vote shop item.
 */
@Getter
@Setter
public class VoteShopItem extends VoteShopEntry {

	private String identifierName;

	private int cost;

	private int limit = -1;

	private String permission = "";

	private boolean hideOnNoPermission = true;

	private boolean closeGUI = true;

	private boolean requireConfirmation = false;

	private boolean notBuyable = false;

	private String purchaseMessage = "";

	private boolean resetDaily = false;

	private boolean resetWeekly = false;

	private boolean resetMonthly = false;

	private String rewardsPath;
}
