package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import org.bukkit.OfflinePlayer;

import com.Ben12345rocks.VotingPlugin.Objects.User;

public abstract class PlaceHolder {
	private String identifier;
	private boolean useStartsWith = false;

	/**
	 * @return the identifier
	 */
	public String getIdentifier() {
		return identifier;
	}

	public PlaceHolder(String identifier) {
		this.identifier = identifier;
	}

	public PlaceHolder(String identifier, boolean useStartsWith) {
		this.identifier = identifier;
		this.useStartsWith = useStartsWith;
	}

	public PlaceHolder useStartsWith() {
		useStartsWith = true;
		return this;
	}

	/**
	 * @return the useStartsWith
	 */
	public boolean isUseStartsWith() {
		return useStartsWith;
	}

	public abstract String placeholderRequest(OfflinePlayer p, User user, String identifier);

}
