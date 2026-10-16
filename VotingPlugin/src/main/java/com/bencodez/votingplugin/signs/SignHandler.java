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

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

import lombok.Getter;
import lombok.Setter;

public class SignHandler {

	@Getter
	@Setter
	private volatile String data;

	/** The lines. */
	private ArrayList<String> lines;

	@Getter
	@Setter
	private volatile Location location;

	private String playerName;

	private UUID uuid;

	private VotingPluginMain plugin;

	private int position;

	@Getter
	@Setter
	private String sign;

	@Getter
	@Setter
	private volatile Location skullLocation;

	@Getter
	@Setter
	private boolean valid;

	private int votes;

	private record Render(ArrayList<String> lines, String name, UUID identity, int votes) { }
	private record DisplayUpdate(long generation, Location location, String data) { }
	private DisplayUpdate preparing;
	private long skullSequence;
	private record SkullFrame(long sequence, long generation, Location origin, Location selected, String data,
			String name, UUID identity) { }

	private synchronized SkullFrame skullFrame(String name, UUID identity) {
		return new SkullFrame(++skullSequence, plugin.getTopVoterDisplayGeneration(), location.clone(),
				skullLocation == null ? null : skullLocation.clone(), data, name, identity);
	}

	private boolean current(SkullFrame frame) {
		if (!plugin.isEnabled() || plugin.getTopVoterDisplayGeneration() != frame.generation()) return false;
		synchronized (this) {
			return skullSequence == frame.sequence() && java.util.Objects.equals(location, frame.origin())
					&& java.util.Objects.equals(skullLocation, frame.selected())
					&& java.util.Objects.equals(data, frame.data());
		}
	}

	/** Only coordinates cross regions; every block access occurs inside its own location task. */
	private void onOwner(SkullFrame frame, Location target, Runnable action) {
		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> {
				if (current(frame)) action.run();
			}, target);
		} catch (RuntimeException rejected) { plugin.debug("Unable to schedule skull display"); }
	}

	private void updateSkull(SkullFrame frame, Location target, Runnable absent) {
		onOwner(frame, target, () -> {
			String name = frame.name(); UUID identity = frame.identity();
			// Queued region work may outlive the evidence used by the sign render.
			TopVoter period = displayPeriod();
			if (period != null && position > 0 && plugin.getConfigFile().isHideTopVoterIfNotPlayedBefore()) {
				var rows = new ArrayList<>(plugin.getTopVoterDisplay(period).keySet());
				if (rows.size() >= position) {
					name = rows.get(position - 1).getPlayerName(); identity = rows.get(position - 1).getUuid();
				} else { name = "No Player"; identity = null; }
			}
			if (!current(frame)) return;
			if (!checkSkull(target.getBlock(), name, identity, frame) && absent != null) absent.run();
		});
	}


	/**
	 * Constructs a new SignHandler.
	 *
	 * @param plugin        the main plugin instance
	 * @param sign          the sign identifier
	 * @param location      the sign location
	 * @param skullLocation the skull location
	 * @param data          the sign data
	 * @param position      the sign position
	 */
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
	private boolean checkSkull(Block block, String name, UUID identity, SkullFrame frame) {
		if (block.getState() instanceof Skull skull) {
			if (name.isEmpty() || name.equalsIgnoreCase("No Player")) {
				if (plugin.getConfigFile().isHideTopVoterIfNotPlayedBefore()) {
					if (!current(frame)) return true;
					skull.setOwnerProfile(null);
					skull.update(true, false);
				}
				return true;
			}
			if (!name.equals("No Player")) {
				try {
					SkullMeta meta = (SkullMeta) plugin.getSkullCacheHandler().getSkull(identity, name).getItemMeta();
					if (!current(frame)) return true;
					skull.setOwnerProfile(meta.getOwnerProfile());
					skull.update(true, false);
				} catch (Exception e) {
					plugin.debug("Failed to set skull for " + name);
					plugin.debug(e);
					if (!current(frame)) return true;
					skull.setOwner(name);
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
		Render rendered = currentRender();
		checkSkulls(rendered.name(), rendered.identity());
	}

	private void checkSkulls(String name, UUID identity) {
		if ((name.equalsIgnoreCase("No Player") || name.isEmpty())
				&& !plugin.getConfigFile().isHideTopVoterIfNotPlayedBefore()) {
			return;
		}
		SkullFrame frame = skullFrame(name, identity);
		if (frame.selected() != null) {
			updateSkull(frame, frame.selected(), null);
			return;
		}
		Location loc = frame.origin();
		Location loc1 = new Location(loc.getWorld(), loc.getBlockX() - 1, loc.getBlockY() - 1, loc.getBlockZ() - 1);
		Location loc2 = new Location(loc.getWorld(), loc.getBlockX() + 1, loc.getBlockY() + 1, loc.getBlockZ() + 1);
		updateSkulls(loc1, loc2, frame);
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

	/**
	 * Checks if skull location is set.
	 *
	 * @return true if skull is set
	 */
	public boolean isSkullSet() {
		return skullLocation != null;
	}

	/**
	 * Checks if no player name is available.
	 *
	 * @return true if no name available
	 */
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
		Render rendered = renderLines();
		synchronized (this) { applyRender(rendered); }
	}

	private TopVoter displayPeriod() {
		if ("all".equalsIgnoreCase(data)) return TopVoter.AllTime;
		if ("monthly".equalsIgnoreCase(data)) return TopVoter.Monthly;
		if ("weekly".equalsIgnoreCase(data)) return TopVoter.Weekly;
		if ("daily".equalsIgnoreCase(data)) return TopVoter.Daily;
		return null;
	}

	private Render renderLines() {
		var rendered = new ArrayList<String>();
		checkValidSign();
		if (position == 0) {
			synchronized (this) { return new Render(rendered, playerName, uuid, votes); }
		}
		rendered.add(plugin.getConfigFile().getFormatSignTopVoterSignLine1());
		rendered.add(plugin.getConfigFile().getFormatSignTopVoterSignLine2());
		rendered.add(plugin.getConfigFile().getFormatSignTopVoterSignLine3());
		rendered.add(plugin.getConfigFile().getFormatSignTopVoterSignLine4());
		TopVoter period = displayPeriod();
		java.util.Map<TopVoterPlayer, Integer> board = period == null ? java.util.Map.of()
				: plugin.getTopVoterDisplay(period);
		var users = new ArrayList<>(board.keySet());
		String name = "No Player";
		UUID identity = null;
		int amount = 0;
		if (users.size() >= position) {
			TopVoterPlayer row = users.get(position - 1);
			name = row.getPlayerName(); identity = row.getUuid(); amount = board.get(row);
		}
		for (int index = 0; index < rendered.size(); index++) {
			rendered.set(index, rendered.get(index).replace("%votes%", Integer.toString(amount))
					.replace("%player%", name).replace("%SiteName%", data)
					.replace("%position%", Integer.toString(position)));
		}
		return new Render(ArrayUtils.colorize(rendered), name, identity, amount);
	}

	/** Called under this monitor; never call storage/provider APIs from this scope. */
	private void applyRender(Render rendered) {
		lines = rendered.lines(); playerName = rendered.name(); uuid = rendered.identity(); votes = rendered.votes();
	}

	private Render currentRender() {
		synchronized (this) { return new Render(new ArrayList<>(lines), playerName, uuid, votes); }
	}

	/** Update the sign on its owning scheduler, waiting once for admitted presence evidence when enabled. */
	public void updateSign(int delay) {
		TopVoter period = displayPeriod();
		if (position != 0 && period != null && plugin.getConfigFile() != null
				&& plugin.getConfigFile().isHideTopVoterIfNotPlayedBefore()) {
			prepareDisplay(period, delay);
			return;
		}
		plugin.getBukkitScheduler().runTaskLater(plugin, () -> writeSign(currentRender(), getLocation()),
				delay, getLocation());
	}

	private void prepareDisplay(TopVoter period, int delay) {
		long generation = plugin.getTopVoterDisplayGeneration();
		Location target = getLocation().clone();
		DisplayUpdate update;
		synchronized (this) {
			if (preparing != null && preparing.generation() == generation
					&& preparing.location().equals(target) && java.util.Objects.equals(preparing.data(), data)) return;
			update = new DisplayUpdate(generation, target, data);
			preparing = update;
		}
		try {
			var ready = plugin.getTopVoterDisplayAsync(period);
			if (ready == null) schedulePreparedDisplay(update, delay);
			else ready.whenComplete((ignored, failure) -> schedulePreparedDisplay(update, delay));
		} catch (RuntimeException unavailable) {
			schedulePreparedDisplay(update, delay);
		}
	}

	private boolean current(DisplayUpdate update) {
		return plugin.isEnabled() && plugin.getTopVoterDisplayGeneration() == update.generation()
				&& java.util.Objects.equals(update.location(), getLocation())
				&& java.util.Objects.equals(update.data(), data);
	}

	private void schedulePreparedDisplay(DisplayUpdate update, int delay) {
		synchronized (this) { if (preparing != update) return; }
		try {
			plugin.getBukkitScheduler().runTaskLater(plugin, () -> {
				try {
					synchronized (SignHandler.this) { if (preparing != update) return; }
					if (!current(update)) return;
					// Resolve current evidence outside the sign monitor. Completion may reenter from the SDK worker.
					Render rendered = renderLines();
					if (!current(update)) return;
					synchronized (SignHandler.this) {
						if (preparing != update) return;
						applyRender(rendered);
					}
					writeSign(rendered, update.location());
				} finally {
					synchronized (SignHandler.this) { if (preparing == update) preparing = null; }
				}
			}, delay, update.location());
		} catch (RuntimeException rejected) {
			synchronized (this) { if (preparing == update) preparing = null; }
		}
	}

	@SuppressWarnings("deprecation")
	private void writeSign(Render rendered, Location target) {
		try {
			BlockState state = target.getBlock().getState();
			if (state instanceof Sign signState) {
				for (int index = 0; index < rendered.lines().size() && index < 4; index++)
					signState.setLine(index, rendered.lines().get(index));
				signState.update();
				checkSkulls(rendered.name(), rendered.identity());
			}
		} catch (Exception failure) {
			plugin.debug("Failed to update sign display");
			plugin.debug(failure);
		}
	}

	/**
	 * Updates skulls in the specified region.
	 *
	 * @param loc1 the first location
	 * @param loc2 the second location
	 */
	public void updateSkulls(Location loc1, Location loc2) {
		Render rendered = currentRender();
		updateSkulls(loc1, loc2, rendered.name(), rendered.identity());
	}

	private void updateSkulls(Location loc1, Location loc2, String name, UUID identity) {
		updateSkulls(loc1, loc2, skullFrame(name, identity));
	}

	private void updateSkulls(Location loc1, Location loc2, SkullFrame frame) {
		Runnable nearby = () -> {
			for (int x = Math.min(loc1.getBlockX(), loc2.getBlockX()); x <= Math.max(loc1.getBlockX(), loc2.getBlockX()); x++)
				for (int y = Math.min(loc1.getBlockY(), loc2.getBlockY()); y <= Math.max(loc1.getBlockY(), loc2.getBlockY()); y++)
					for (int z = Math.min(loc1.getBlockZ(), loc2.getBlockZ()); z <= Math.max(loc1.getBlockZ(), loc2.getBlockZ()); z++) {
						if (!current(frame)) return;
						updateSkull(frame, new Location(frame.origin().getWorld(), x, y, z), null);
					}
		};
		onOwner(frame, frame.origin(), () -> {
			BlockState state = frame.origin().getBlock().getState();
			if (state instanceof Sign && state.getBlockData() instanceof Directional direction) {
				BlockFace facing = direction.getFacing();
				Location preferred = new Location(frame.origin().getWorld(),
						frame.origin().getBlockX() + facing.getModX(),
						frame.origin().getBlockY() + facing.getModY() + 1,
						frame.origin().getBlockZ() + facing.getModZ());
				updateSkull(frame, preferred, nearby);
			} else nearby.run();
		});
	}
}
