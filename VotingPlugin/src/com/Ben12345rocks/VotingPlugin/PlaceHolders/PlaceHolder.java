package com.Ben12345rocks.VotingPlugin.PlaceHolders;

import org.bukkit.OfflinePlayer;

import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
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

	public boolean matches(String identifier) {
		if (isUseStartsWith()) {
			if (StringUtils.getInstance().startsWithIgnoreCase(identifier, getIdentifier())) {
				return true;
			}
		} else {
			if (getIdentifier().equalsIgnoreCase(identifier)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @return the useStartsWith
	 */
	public boolean isUseStartsWith() {
		return useStartsWith;
	}

	public abstract String placeholderRequest(OfflinePlayer p, User user, String identifier);

}
