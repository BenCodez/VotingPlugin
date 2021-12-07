package com.bencodez.votingplugin.bungee;

import com.imaginarycode.minecraft.redisbungee.RedisBungeeAPI;
import com.imaginarycode.minecraft.redisbungee.events.PlayerJoinedNetworkEvent;

import lombok.Getter;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.api.plugin.Listener;
import net.md_5.bungee.event.EventHandler;

public class RedisBungee implements Listener {
	@Getter
	private boolean enabled;
	private VotingPluginBungee plugin;

	public RedisBungee(VotingPluginBungee plugin) {
		this.plugin = plugin;
	}

	public RedisBungee load() {
		if (plugin.getConfig().getRedisSupport()) {
			plugin.getProxy().getPluginManager().registerListener(plugin, this);
			plugin.getProxy().getLogger().info("Loaded expiremental redis support");
			enabled = true;
		} else {
			enabled = false;
		}
		return this;
	}

	@EventHandler
	public void PlayerJoinedNetworkEvent(PlayerJoinedNetworkEvent event) {
		plugin.login(plugin.getProxy().getPlayer(event.getUuid()));
	}

	public boolean isOnline(ProxiedPlayer p) {
		return RedisBungeeAPI.getRedisBungeeApi().isPlayerOnline(p.getUniqueId());
	}

	public void destroy() {
		if (!enabled) {
			return;
		}
		plugin.getProxy().getPluginManager().unregisterListener(this);
	}
}
