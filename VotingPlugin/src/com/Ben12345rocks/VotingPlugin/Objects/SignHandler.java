package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

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

	/**
	 * Check skulls.
	 */
	@SuppressWarnings("deprecation")
	public void checkSkulls() {
		Location loc = location;
		Location loc1 = new Location(loc.getWorld(), loc.getBlockX() - 1,
				loc.getBlockY() - 1, loc.getBlockZ() - 1);
		Location loc2 = new Location(loc.getWorld(), loc.getBlockX() + 1,
				loc.getBlockY() + 1, loc.getBlockZ() + 1);
		for (Block block : Utils.getInstance().getRegionBlocks(loc.getWorld(),
				loc1, loc2)) {
			if (block.getState() instanceof Skull) {
				Skull skull = (Skull) block.getState();
				skull.setOwner(playerName);
				skull.update();
			}
		}
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
		String msg = ConfigFormat.getInstance()
				.getSignTopVoterSignRightClickMessage();
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
			String line1 = ConfigFormat.getInstance()
					.getSignTopVoterSignLine1();
			String line2 = ConfigFormat.getInstance()
					.getSignTopVoterSignLine2();
			String line3 = ConfigFormat.getInstance()
					.getSignTopVoterSignLine3();
			String line4 = ConfigFormat.getInstance()
					.getSignTopVoterSignLine4();
			lines.add(line1);
			lines.add(line2);
			lines.add(line3);
			lines.add(line4);

			if (data.equalsIgnoreCase("All")) {
				ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
						.getInstance().convertSet(
								plugin.topVoterMonthly.keySet());

				if (users.size() >= position) {
					playerName = users.get(position - 1).getPlayerName();
					votes = users.get(position - 1).getTotalVotes();
					for (int j = 0; j < lines.size(); j++) {
						lines.set(j,
								lines.get(j).replace("%votes%", "" + votes)
										.replace("%player%", playerName));
					}
				} else {
					playerName = "No Player";
					votes = 0;
					for (int j = 0; j < lines.size(); j++) {
						lines.set(j,
								lines.get(j).replace("%votes%", "" + votes)
										.replace("%player%", playerName));
					}
				}

				for (int j = 0; j < lines.size(); j++) {
					lines.set(j, lines.get(j).replace("%SiteName%", data)
							.replace("%position%", "" + position));
				}

				lines = Utils.getInstance().colorize(lines);

			} else {
				for (VoteSite voteSite : plugin.voteSites) {
					if (data.equalsIgnoreCase(voteSite.getSiteName())) {
						ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
								.getInstance().convertSet(
										TopVoter.getInstance()
												.topVotersSortedVoteSite(
														voteSite).keySet());

						if (users.size() >= position) {
							playerName = users.get(position - 1)
									.getPlayerName();
							votes = users.get(position - 1).getTotal(voteSite);
							for (int j = 0; j < lines.size(); j++) {
								lines.set(
										j,
										lines.get(j)
												.replace("%votes%", "" + votes)
												.replace("%player%", playerName));
							}
						} else {
							playerName = "No Player";
							votes = 0;
							for (int j = 0; j < lines.size(); j++) {
								lines.set(
										j,
										lines.get(j)
												.replace("%votes%", "" + votes)
												.replace("%player%", playerName));
							}
						}

						for (int j = 0; j < lines.size(); j++) {
							lines.set(
									j,
									lines.get(j)
											.replace("%SiteName%", data)
											.replace("%position%",
													"" + position));
						}

						lines = Utils.getInstance().colorize(lines);
					}
				}
			}
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
					}

				} catch (Exception ex) {
					if (com.Ben12345rocks.AdvancedCore.Configs.Config
							.getInstance().getDebugEnabled()) {
						ex.printStackTrace();
					}
				}
			}
		}, delay);
	}
}
