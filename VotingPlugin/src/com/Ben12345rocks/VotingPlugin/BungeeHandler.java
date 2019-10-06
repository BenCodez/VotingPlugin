package com.Ben12345rocks.VotingPlugin;

import java.util.ArrayList;
import java.util.UUID;

import com.Ben12345rocks.AdvancedCore.Util.PluginMessage.PluginMessage;
import com.Ben12345rocks.AdvancedCore.Util.PluginMessage.PluginMessageHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class BungeeHandler {
	private static BungeeHandler instance = new BungeeHandler();

	public static BungeeHandler getInstance() {
		return instance;
	}

	public void load() {
		PluginMessage.getInstance().add(new PluginMessageHandler("bungeevote") {

			@Override
			public void onRecieve(String subchannel, ArrayList<String> data) {
				String uuid = data.get(0);
				Main.plugin.getMysql().clearCache(uuid);
				User user = UserManager.getInstance().getVotingPluginUser(UUID.fromString(uuid));
				user.bungeeVote();
			}
		});

		PluginMessage.getInstance().add(new PluginMessageHandler("Broadcast") {

			@Override
			public void onRecieve(String subchannel, ArrayList<String> data) {
				VoteSite site = Main.plugin.getVoteSite(data.get(0));
				String p = data.get(1);
				User user = UserManager.getInstance().getVotingPluginUser(p);
				site.broadcastVote(user, false);

			}
		});
	}
}
