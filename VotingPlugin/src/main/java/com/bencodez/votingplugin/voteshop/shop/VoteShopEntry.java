package com.bencodez.votingplugin.voteshop.shop;

import org.bukkit.configuration.ConfigurationSection;

import lombok.Getter;
import lombok.Setter;

/**
 * Base vote shop entry.
 */
@Getter
@Setter
public abstract class VoteShopEntry {

	private String identifier;

	private ConfigurationSection displaySection;
}
