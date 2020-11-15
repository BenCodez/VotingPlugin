package com.bencodez.votingplugin.signs;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.block.data.Directional;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.nms.NMSManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class SignHandler.
 */
public class SignHandler {

	@Getter
	@Setter
	private String sign;

	@Getter
	@Setter
	private Location location;

	@Getter
	@Setter
	private Location skullLocation;

	@Getter
	@Setter
	private String data;

	/** The lines. */
	private ArrayList<String> lines;

	/** The position. */
	private int position;

	/** The plugin. */
	public VotingPluginMain plugin = VotingPluginMain.plugin;

	/** The player name. */
	private String playerName;

	/** The votes. */
	private int votes;

	@Getter
	@Setter
	private boolean valid;

	public SignHandler(String sign, Location location, Location skullLocation, String data, int position) {
		setSign(sign);
		setLocation(location);
		setSkullLocation(skullLocation);
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
		if (skullLocation != null) {
			checkSkull(skullLocation.getBlock());
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
	 * Checks if is location same.
	 *
	 * @param loc
	 *            the loc
	 * @return true, if is location same
	 */
	public boolean isLocationSame(Location loc) {
		return loc.equals(getLocation());
	}

	public boolean isSkullSet() {
		return skullLocation != null;
	}

	/**
	 * Removes the sign.
	 */
	public void removeSign() {
		ServerData.getInstance().removeSign(sign);
	}

	/**
	 * Store sign.
	 */
	public void storeSign() {
		ServerData.getInstance().setSign(sign, location, skullLocation, data, position);
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

			ArrayList<VotingPluginUser> users = null;
			if (data.equalsIgnoreCase("all")) {
				users = plugin.convertSet(plugin.getTopVoter(TopVoter.AllTime).keySet());
			} else if (data.equalsIgnoreCase("monthly")) {
				users = plugin.convertSet(plugin.getTopVoter(TopVoter.Monthly).keySet());
			} else if (data.equalsIgnoreCase("weekly")) {
				users = plugin.convertSet(plugin.getTopVoter(TopVoter.Weekly).keySet());
			} else if (data.equalsIgnoreCase("daily")) {
				users = plugin.convertSet(plugin.getTopVoter(TopVoter.Daily).keySet());
			}

			if (users != null && users.size() >= position) {
				VotingPluginUser user = users.get(position - 1);
				playerName = user.getPlayerName();

				votes = 0;
				if (data.equalsIgnoreCase("all")) {
					votes = plugin.getTopVoter(TopVoter.AllTime).get(user);
				} else if (data.equalsIgnoreCase("monthly")) {
					votes = plugin.getTopVoter(TopVoter.Monthly).get(user);
				} else if (data.equalsIgnoreCase("weekly")) {
					votes = plugin.getTopVoter(TopVoter.Weekly).get(user);
				} else if (data.equalsIgnoreCase("daily")) {
					votes = plugin.getTopVoter(TopVoter.Daily).get(user);
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
					VotingPluginMain.plugin.debug(ex);
				}
			}
		}, delay);
	}

	public void updateSkulls(Location loc1, Location loc2) {
		if (!NMSManager.getInstance().isVersion("1.12")) {
			BlockState state = getLocation().getBlock().getState();
			if (state instanceof Sign && state.getBlockData() instanceof Directional) {
				Directional s = (Directional) state.getBlockData();
				Block b = location.getBlock().getRelative(s.getFacing());
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
}
