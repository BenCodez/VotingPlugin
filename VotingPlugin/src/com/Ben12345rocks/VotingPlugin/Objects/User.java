package com.Ben12345rocks.VotingPlugin.Objects;

import org.bukkit.entity.Player;

import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

@Deprecated
public class User {
	private VotingPluginUser user;

	public User(Player p) {
		user = UserManager.getInstance().getVotingPluginUser(p);
	}

	public void addPoints(int points) {
		user.addPoints(points);
	}

	public int getPoints() {
		return user.getPoints();
	}

	public boolean removePoints(int points) {
		return user.removePoints(points);
	}

	public void setPoints(int points) {
		user.setPoints(points);
	}
}