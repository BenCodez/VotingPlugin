package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import org.bukkit.OfflinePlayer;

import com.Ben12345rocks.VotingPlugin.Objects.User;

public abstract class PlaceHolder {
	private String identifier;

	/**
	 * @return the identifier
	 */
	public String getIdentifier() {
		return identifier;
	}

	public PlaceHolder(String identifier) {
		this.identifier = identifier;
	}

	public abstract String placeholderRequest(OfflinePlayer p, User user, String identifier);

}
