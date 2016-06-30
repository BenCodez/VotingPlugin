package com.Ben12345rocks.VotingPlugin.VoteReminding;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteReminding;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteReminding {

	static VoteReminding instance = new VoteReminding();

	static Main plugin = Main.plugin;

	public static VoteReminding getInstance() {
		return instance;
	}

	private VoteReminding() {
	}

	public VoteReminding(Main plugin) {
		VoteReminding.plugin = plugin;
	}

	public void checkRemind(User user) {
		String playerName = user.getPlayerName();

		if (Utils.getInstance().hasPermission(playerName,
				"VotingPlugin.Login.RemindVotes")
				|| Utils.getInstance().hasPermission(playerName,
						"VotingPlugin.Player")) {
			if (user.canVoteAll()) {
				Player player = Bukkit.getPlayer(playerName);
				if (player != null) {
					if (!ConfigVoteReminding.getInstance().getRemindOnlyOnce()) {
						runRemind(user);
						user.setReminded(true);
					} else if (!user.getReminded()) {
						runRemind(user);
						user.setReminded(true);
					}
				}

			}

		}
	}

	public void loadRemindChecking() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(
				plugin,
				new Runnable() {

					@Override
					public void run() {
						for (Player player : Bukkit.getServer()
								.getOnlinePlayers()) {
							User user = new User(player);
							checkRemind(user);
						}
					}
				}, 10,
				ConfigVoteReminding.getInstance().getRemindDelay() * 20 * 60);
	}

	public void playEffect(User user) {
		if (ConfigVoteReminding.getInstance().getEffectEnabled()) {
			user.playParticleEffect(ConfigVoteReminding.getInstance()
					.getEffectEffect(), ConfigVoteReminding.getInstance()
					.getEffectData(), ConfigVoteReminding.getInstance()
					.getEffectParticles(), ConfigVoteReminding.getInstance()
					.getEffectRadius());
		}
	}

	public void playSound(User user) {
		if (ConfigVoteReminding.getInstance().getSoundEnabled()) {
			try {
				user.playSound(ConfigVoteReminding.getInstance()
						.getSoundSound(), ConfigVoteReminding.getInstance()
						.getSoundVolume(), ConfigVoteReminding.getInstance()
						.getSoundPitch());
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	public void runCommands(User user) {
		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = ConfigVoteReminding.getInstance()
				.getCommandsConsole();

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = ConfigVoteReminding.getInstance()
				.getCommandsPlayer();

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	public void runRemind(User user) {
		if (ConfigVoteReminding.getInstance().getEnabled()) {
			runCommands(user);
			runTitle(user);
			playSound(user);
			playEffect(user);
			sendMessage(user);
		}
	}

	public void runTitle(User user) {
		if (ConfigVoteReminding.getInstance().getTitleEnabled()) {
			user.sendTitle(ConfigVoteReminding.getInstance().getTitleTitle(),
					ConfigVoteReminding.getInstance().getTitleTitleColor(),
					ConfigVoteReminding.getInstance().getTitleSubTitle(),
					ConfigVoteReminding.getInstance().getTitleSubTitleColor(),
					ConfigVoteReminding.getInstance().getTitleFadeIn(),
					ConfigVoteReminding.getInstance().getTitleShowTime(),
					ConfigVoteReminding.getInstance().getTitleFadeOut());
		}
	}

	public void sendMessage(User user) {
		String remindMsg = ConfigVoteReminding.getInstance()
				.getMessagesRemind();
		if (remindMsg != null) {
			user.sendMessage(remindMsg);
		}
	}
}
