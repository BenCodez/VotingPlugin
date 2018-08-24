package com.Ben12345rocks.VotingPlugin.Signs;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class SignHandler.
 */
public class SignHandler {

	/** The sign. */
	private String sign;

	/** The location. */
	private Location location;

	/** The data. */
	private String data;

	/** The lines. */
	private ArrayList<String> lines;

	/** The position. */
	private int position;

	/** The plugin. */
	public Main plugin = Main.plugin;

	/** The player name. */
	private String playerName;

	/** The votes. */
	private int votes;

	/** The is valid. */
	private boolean isValid;

	/**
	 * Instantiates a new sign handler.
	 *
	 * @param sign
	 *            the sign
	 * @param location
	 *            the location
	 * @param data
	 *            the data
	 * @param position
	 *            the position
	 */
	public SignHandler(String sign, Location location, String data, int position) {
		setSign(sign);
		setLocation(location);
		setData(data);
		this.position = position;
		setValid(true);
		lines = new ArrayList<String>();
		checkValidSign();
		playerName = "";
	}

	@SuppressWarnings("deprecation")
	private boolean checkSkull(Block block) {
		if (block.getState() instanceof Skull) {
			Skull skull = (Skull) block.getState();
			skull.setOwner(playerName);
			skull.update();
			return true;
		}
		return false;
	}

	/**
	 * Check skulls.
	 */
	public void checkSkulls() {
		if (playerName.equalsIgnoreCase("No Player") || playerName.equals("")) {
			return;
		}
		Location loc = location;
		Location loc1 = new Location(loc.getWorld(), loc.getBlockX() - 1, loc.getBlockY() - 1, loc.getBlockZ() - 1);
		Location loc2 = new Location(loc.getWorld(), loc.getBlockX() + 1, loc.getBlockY() + 1, loc.getBlockZ() + 1);
		updateSkulls(loc1, loc2);
	}

	/**
	 * Check valid sign.
	 */
	public void checkValidSign() {
		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				try {
					setValid(getLocation().getBlock().getState() instanceof Sign);
				} catch (Exception ex) {
					setValid(false);
				}
			}
		});

	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public String getData() {
		return data;
	}

	/**
	 * Gets the location.
	 *
	 * @return the location
	 */
	public Location getLocation() {
		return location;
	}

	/**
	 * Gets the right click message.
	 *
	 * @return the right click message
	 */
	public String getRightClickMessage() {
		String msg = Config.getInstance().getFormatSignTopVoterSignRightClickMessage();
		msg = msg.replace("%player%", playerName);
		msg = msg.replace("%position%", "" + position);
		msg = msg.replace("%votes%", "" + votes);
		msg = msg.replace("%SiteName%", data);
		return msg;
	}

	/**
	 * Gets the sign.
	 *
	 * @return the sign
	 */
	public String getSign() {
		return sign;
	}

	/**
	 * Checks if is location same.
	 *
	 * @param loc
	 *            the loc
	 * @return true, if is location same
	 */
	public boolean isLocationSame(Location loc) {
		return loc.equals(getLocation());
	}

	/**
	 * Checks if is valid.
	 *
	 * @return true, if is valid
	 */
	public boolean isValid() {
		return isValid;
	}

	/**
	 * Removes the sign.
	 */
	public void removeSign() {
		ServerData.getInstance().removeSign(sign);
	}

	/**
	 * Sets the data.
	 *
	 * @param data
	 *            the new data
	 */
	public void setData(String data) {
		this.data = data;
	}

	/**
	 * Sets the location.
	 *
	 * @param location
	 *            the new location
	 */
	public void setLocation(Location location) {
		this.location = location;
	}

	/**
	 * Sets the sign.
	 *
	 * @param sign
	 *            the new sign
	 */
	public void setSign(String sign) {
		this.sign = sign;
	}

	/**
	 * Sets the valid.
	 *
	 * @param isValid
	 *            the new valid
	 */
	public void setValid(boolean isValid) {
		this.isValid = isValid;
	}

	/**
	 * Store sign.
	 */
	public void storeSign() {
		ServerData.getInstance().setSign(sign, location, data, position);
	}

	/**
	 * Update lines.
	 */
	public void updateLines() {
		lines = new ArrayList<String>();
		checkValidSign();
		if (position != 0) {
			String line1 = Config.getInstance().getFormatSignTopVoterSignLine1();
			String line2 = Config.getInstance().getFormatSignTopVoterSignLine2();
			String line3 = Config.getInstance().getFormatSignTopVoterSignLine3();
			String line4 = Config.getInstance().getFormatSignTopVoterSignLine4();
			lines.add(line1);
			lines.add(line2);
			lines.add(line3);
			lines.add(line4);

			ArrayList<User> users = null;
			if (data.equalsIgnoreCase("all")) {
				users = plugin.convertSet(plugin.topVoterAllTime.keySet());
			} else if (data.equalsIgnoreCase("monthly")) {
				users = plugin.convertSet(plugin.topVoterMonthly.keySet());
			} else if (data.equalsIgnoreCase("weekly")) {
				users = plugin.convertSet(plugin.topVoterWeekly.keySet());
			} else if (data.equalsIgnoreCase("daily")) {
				users = plugin.convertSet(plugin.topVoterDaily.keySet());
			}

			if (users != null && users.size() >= position) {
				User user = users.get(position - 1);
				playerName = user.getPlayerName();

				votes = 0;
				if (data.equalsIgnoreCase("all")) {
					votes = plugin.topVoterAllTime.get(user);
				} else if (data.equalsIgnoreCase("monthly")) {
					votes = plugin.topVoterMonthly.get(user);
				} else if (data.equalsIgnoreCase("weekly")) {
					votes = plugin.topVoterWeekly.get(user);
				} else if (data.equalsIgnoreCase("daily")) {
					votes = plugin.topVoterDaily.get(user);
				}

				for (int j = 0; j < lines.size(); j++) {
					lines.set(j, lines.get(j).replace("%votes%", "" + votes).replace("%player%", playerName));
				}
			} else {
				playerName = "No Player";
				votes = 0;
				for (int j = 0; j < lines.size(); j++) {
					lines.set(j, lines.get(j).replace("%votes%", "" + votes).replace("%player%", playerName));
				}
			}

			for (int j = 0; j < lines.size(); j++) {
				lines.set(j, lines.get(j).replace("%SiteName%", data).replace("%position%", "" + position));
			}

			lines = ArrayUtils.getInstance().colorize(lines);

		}

	}

	/**
	 * Update sign.
	 *
	 * @param delay
	 *            the delay
	 */
	public void updateSign(int delay) {
		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				try {
					BlockState state = getLocation().getBlock().getState();
					if (state instanceof Sign) {
						Sign s = (Sign) state;

						for (int j = 0; j < lines.size() && j < 4; j++) {
							s.setLine(j, lines.get(j));
						}
						s.update();
						checkSkulls();
					}

				} catch (Exception ex) {
					AdvancedCoreHook.getInstance().debug(ex);
				}
			}
		}, delay);
	}

	public void updateSkulls(Location loc1, Location loc2) {
		BlockState state = getLocation().getBlock().getState();
		if (state instanceof Sign) {
			org.bukkit.material.Sign s = (org.bukkit.material.Sign) state.getData();
			Block b = location.getBlock().getRelative(s.getAttachedFace());
			Block above = b.getRelative(BlockFace.UP);
			if (checkSkull(above)) {
				return;
			}

		}

		for (Block block : MiscUtils.getInstance().getRegionBlocks(location.getWorld(), loc1, loc2)) {
			checkSkull(block);
		}
	}
}
