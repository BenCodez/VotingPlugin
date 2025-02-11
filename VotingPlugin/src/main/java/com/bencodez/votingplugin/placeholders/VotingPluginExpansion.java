package com.bencodez.votingplugin.placeholders;

import org.bukkit.OfflinePlayer;

import com.bencodez.votingplugin.VotingPluginMain;

import me.clip.placeholderapi.expansion.PlaceholderExpansion;

public class VotingPluginExpansion extends PlaceholderExpansion {

	private VotingPluginMain plugin;

	public VotingPluginExpansion(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public String getAuthor() {
		return "bencodez";
	}

	@Override
	public String getIdentifier() {
		return "votingplugin";
	}

	@Override
	public String getVersion() {
		return "1.6";
	}

	@Override
	public String onRequest(OfflinePlayer p, String indendifier) {
		return plugin.getPlaceholders().getPlaceHolder(p, indendifier);
	}

	@Override
	public boolean persist() {
		return true;
	}
}
