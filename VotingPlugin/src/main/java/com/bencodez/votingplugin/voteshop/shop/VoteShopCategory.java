package com.bencodez.votingplugin.voteshop.shop;

import java.util.LinkedHashMap;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

/**
 * Vote shop category.
 */
@Getter
@Setter
public class VoteShopCategory {

	private String id;

	private String name;

	private boolean backButton = true;

	private final Map<String, VoteShopEntry> entries = new LinkedHashMap<String, VoteShopEntry>();
}
