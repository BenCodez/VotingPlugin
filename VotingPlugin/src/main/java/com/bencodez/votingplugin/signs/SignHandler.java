package com.bencodez.votingplugin.signs;

import java.util.ArrayList;
import java.util.UUID;

import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;
import org.bukkit.block.data.Directional;
import org.bukkit.inventory.meta.SkullMeta;

import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.nms.NMSManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class SignHandler.
 */
public class SignHandler {

	@Getter
	@Setter
	private String data;

	/** The lines. */
	private ArrayList<String> lines;

	@Getter
	@Setter
	private Location location;

	private String playerName;

	private UUID uuid;

	private VotingPluginMain plugin;

	private int position;

	@Getter
	@Setter
	private String sign;

	@Getter
	@Setter
	private Location skullLocation;

	@Getter
	@Setter
	private boolean valid;

	private int votes;

	public SignHandler(VotingPluginMain plugin, String sign, Location location, Location skullLocation, String data,
			int position) {
		this.plugin = plugin;
		setSign(sign);
		setLocation(location);
		setSkullLocation(skullLocation);
		setData(data);
		this.position = position;
		setValid(true);
		lines = new ArrayList<>();
		checkValidSign();
		playerName = "";
	}

	@SuppressWarnings("deprecation")
	private boolean checkSkull(Block block) {
		if (block.getState() instanceof Skull) {
			if (!playerName.equals("No Player")) {
				Skull skull = (Skull) block.getState();
				try {
					SkullMeta meta = (SkullMeta) plugin.getSkullCacheHandler().getSkull(uuid, playerName).getItemMeta();
					skull.setOwnerProfile(meta.getOwnerProfile());
					skull.update(true, false);
				} catch (Exception e) {
					plugin.debug("Failed to set skull for " + playerName);
					plugin.debug(e);
					skull.setOwner(playerName);
					skull.update(true, false);
				}

			}
			return true;
		}
		return false;
	}

	/**
	 * Check skulls.
	 */
	public void checkSkulls() {
		if (noNameAvailable()) {
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
		plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				try {
					setValid(getLocation().getBlock().getState() instanceof Sign);
				} catch (Exception ex) {
					setValid(false);
				}
			}
		}, getLocation());

	}

	/**
	 * Gets the right click message.
	 *
	 * @return the right click message
	 */
	public String getRightClickMessage() {
		String msg = plugin.getConfigFile().getFormatSignTopVoterSignRightClickMessage();
		msg = msg.replace("%player%", playerName);
		msg = msg.replace("%position%", "" + position);
		msg = msg.replace("%votes%", "" + votes);
		msg = msg.replace("%SiteName%", data);
		return msg;
	}

	/**
	 * Checks if is location same.
	 *
	 * @param loc the loc
	 * @return true, if is location same
	 */
	public boolean isLocationSame(Location loc) {
		return loc.equals(getLocation());
	}

	public boolean isSkullSet() {
		return skullLocation != null;
	}

	public boolean noNameAvailable() {
		return playerName.equalsIgnoreCase("No Player") || playerName.equals("");
	}

	/**
	 * Removes the sign.
	 */
	public void removeSign() {
		plugin.getServerData().removeSign(sign);
	}

	/**
	 * Store sign.
	 */
	public void storeSign() {
		plugin.getServerData().setSign(sign, location, skullLocation, data, position);
	}

	/**
	 * Update lines.
	 */
	public void updateLines() {
		lines = new ArrayList<>();
		checkValidSign();
		if (position != 0) {
			String line1 = plugin.getConfigFile().getFormatSignTopVoterSignLine1();
			String line2 = plugin.getConfigFile().getFormatSignTopVoterSignLine2();
			String line3 = plugin.getConfigFile().getFormatSignTopVoterSignLine3();
			String line4 = plugin.getConfigFile().getFormatSignTopVoterSignLine4();
			lines.add(line1);
			lines.add(line2);
			lines.add(line3);
			lines.add(line4);

			ArrayList<TopVoterPlayer> users = null;
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
				TopVoterPlayer user = users.get(position - 1);
				playerName = user.getPlayerName();
				uuid = user.getUuid();

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

			lines = ArrayUtils.colorize(lines);

		}

	}

	/**
	 * Update sign.
	 *
	 * @param delay the delay
	 */
	public void updateSign(int delay) {
		plugin.getBukkitScheduler().runTaskLater(plugin, new Runnable() {

			@SuppressWarnings("deprecation")
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
					plugin.debug("Failed to update sign at " + getLocation().toString());
					VotingPluginMain.plugin.debug(ex);
				}
			}
		}, delay, getLocation());
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
