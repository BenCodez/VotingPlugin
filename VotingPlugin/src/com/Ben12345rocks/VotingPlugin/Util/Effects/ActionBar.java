package com.Ben12345rocks.VotingPlugin.Util.Effects;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.scheduler.BukkitRunnable;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

/**
 * The Class ActionBar.
 */
public class ActionBar {
	
	/** The works. */
	public boolean works = true;
	
	/** The nmsver. */
	public String nmsver;
	
	/** The use old methods. */
	private boolean useOldMethods = false;
	
	/** The duration. */
	private int duration;
	
	/** The msg. */
	private String msg;

	/**
	 * Send action bar.
	 *
	 * @param player
	 *            the player
	 * @param message
	 *            the message
	 */
	public void sendActionBar(Player player, String message) {
		try {
			Class<?> c1 = Class.forName("org.bukkit.craftbukkit." + nmsver
					+ ".entity.CraftPlayer");
			Object p = c1.cast(player);
			Object ppoc;
			Class<?> c4 = Class.forName("net.minecraft.server." + nmsver
					+ ".PacketPlayOutChat");
			Class<?> c5 = Class.forName("net.minecraft.server." + nmsver
					+ ".Packet");
			if (useOldMethods) {
				Class<?> c2 = Class.forName("net.minecraft.server." + nmsver
						+ ".ChatSerializer");
				Class<?> c3 = Class.forName("net.minecraft.server." + nmsver
						+ ".IChatBaseComponent");
				Method m3 = c2.getDeclaredMethod("a", String.class);
				Object cbc = c3.cast(m3.invoke(c2, "{\"text\": \"" + message
						+ "\"}"));
				ppoc = c4.getConstructor(new Class<?>[] { c3, byte.class })
						.newInstance(cbc, (byte) 2);
			} else {
				Class<?> c2 = Class.forName("net.minecraft.server." + nmsver
						+ ".ChatComponentText");
				Class<?> c3 = Class.forName("net.minecraft.server." + nmsver
						+ ".IChatBaseComponent");
				Object o = c2.getConstructor(new Class<?>[] { String.class })
						.newInstance(message);
				ppoc = c4.getConstructor(new Class<?>[] { c3, byte.class })
						.newInstance(o, (byte) 2);
			}
			Method m1 = c1.getDeclaredMethod("getHandle");
			Object h = m1.invoke(p);
			Field f1 = h.getClass().getDeclaredField("playerConnection");
			Object pc = f1.get(h);
			Method m5 = pc.getClass().getDeclaredMethod("sendPacket", c5);
			m5.invoke(pc, ppoc);
		} catch (Exception ex) {
			ex.printStackTrace();
			works = false;
		}
	}

	/**
	 * Send action bar.
	 *
	 * @param player
	 *            the player
	 * @param message
	 *            the message
	 * @param duration
	 *            the duration
	 */
	public void sendActionBar(final Player player, final String message,
			int duration) {
		sendActionBar(player, message);

		if (duration >= 0) {
			// Sends empty message at the end of the duration. Allows messages
			// shorter than 3 seconds, ensures precision.
			new BukkitRunnable() {
				@Override
				public void run() {
					sendActionBar(player, "");
				}
			}.runTaskLater(Main.plugin, duration + 1);
		}

		// Re-sends the messages every 3 seconds so it doesn't go away from the
		// player's screen.
		while (duration > 60) {
			duration -= 60;
			int sched = duration % 60;
			new BukkitRunnable() {
				@Override
				public void run() {
					sendActionBar(player, message);
				}
			}.runTaskLater(Main.plugin, (long) sched);
		}
	}

	/**
	 * Send action bar to all players.
	 *
	 * @param message
	 *            the message
	 */
	public void sendActionBarToAllPlayers(String message) {
		sendActionBarToAllPlayers(message, -1);
	}

	/**
	 * Send action bar to all players.
	 *
	 * @param message
	 *            the message
	 * @param duration
	 *            the duration
	 */
	public void sendActionBarToAllPlayers(String message, int duration) {
		for (Player p : Bukkit.getOnlinePlayers()) {
			sendActionBar(p, message, duration);
		}
	}

	/**
	 * Send.
	 *
	 * @param players
	 *            the players
	 */
	public void send(Player... players) {
		for (Player player : players) {
			sendActionBar(player, getMsg(), getDuration());
		}
	}

	/**
	 * Instantiates a new action bar.
	 *
	 * @param msg
	 *            the msg
	 * @param duration
	 *            the duration
	 */
	public ActionBar(String msg, int duration) {
		setMsg(Utils.getInstance().colorize(msg));
		setDuration(duration);

		nmsver = Bukkit.getServer().getClass().getPackage().getName();
		nmsver = nmsver.substring(nmsver.lastIndexOf(".") + 1);

		if (nmsver.equalsIgnoreCase("v1_8_R1")
				|| nmsver.equalsIgnoreCase("v1_7_")) { // Not sure if 1_7 works
														// // for the protocol
														// // hack?
			useOldMethods = true;
		}
	}

	/**
	 * Gets the msg.
	 *
	 * @return the msg
	 */
	public String getMsg() {
		return msg;
	}

	/**
	 * Sets the msg.
	 *
	 * @param msg
	 *            the new msg
	 */
	public void setMsg(String msg) {
		this.msg = msg;
	}

	/**
	 * Gets the duration.
	 *
	 * @return the duration
	 */
	public int getDuration() {
		return duration;
	}

	/**
	 * Sets the duration.
	 *
	 * @param duration
	 *            the new duration
	 */
	public void setDuration(int duration) {
		this.duration = duration;
	}
}
