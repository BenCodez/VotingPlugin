package com.bencodez.votingplugin.voteshop.shop;

import org.bukkit.configuration.ConfigurationSection;

import lombok.Getter;
import lombok.Setter;

/**
 * Extra vote shop item.
 */
@Getter
@Setter
public class VoteShopExtraItem {

	private String identifier;

	private ConfigurationSection displaySection;
}
