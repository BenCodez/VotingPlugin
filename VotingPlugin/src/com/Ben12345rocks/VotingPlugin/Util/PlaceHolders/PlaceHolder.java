package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import org.bukkit.OfflinePlayer;

import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Objects.User;

import lombok.Getter;

public abstract class PlaceHolder {
	@Getter
	private String identifier;
	@Getter
	private boolean useStartsWith = false;
	@Getter
	private String description;

	public PlaceHolder(String identifier) {
		this.identifier = identifier;
	}

	public PlaceHolder(String identifier, boolean useStartsWith) {
		this.identifier = identifier;
		this.useStartsWith = useStartsWith;
	}

	public boolean hasDescription() {
		return description != null;
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

	public abstract String placeholderRequest(OfflinePlayer p, User user, String identifier);

	public PlaceHolder useStartsWith() {
		useStartsWith = true;
		return this;
	}

	public PlaceHolder withDescription(String desc) {
		description = desc;
		return this;
	}

}
