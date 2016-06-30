package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

public class SignHandler {
	private String sign;
	private Location location;
	private String data;
	private ArrayList<String> lines;
	private int position;
	public Main plugin = Main.plugin;
	private String playerName;
	private int votes;
	private boolean isValid;

	public SignHandler(String sign, Location location, String data, int position) {
		this.setSign(sign);
		this.setLocation(location);
		this.setData(data);
		this.position = position;
	}

	public Location getLocation() {
		return location;
	}

	public void setLocation(Location location) {
		this.location = location;
	}

	public String getData() {
		return data;
	}

	public void setData(String data) {
		this.data = data;
	}

	public void storeSign() {
		ServerData.getInstance().setSign(sign, location, data, position);
	}

	public void updateLines() {
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
				ArrayList<User> users = TopVoter.getInstance()
						.topVotersSortedAll();

				if (users.size() >= position) {
					String playerName = users.get(position - 1).getPlayerName();
					votes = users.get(position - 1).getTotalVotes();
					for (int j = 0; j < lines.size(); j++) {
						lines.set(j,
								lines.get(j).replace("%votes%", "" + votes)
										.replace("%player%", playerName));
					}
				} else {
					playerName = "No Player";
					int votes = 0;
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
						ArrayList<User> users = TopVoter.getInstance()
								.topVotersSortedVoteSite(voteSite);

						if (users.size() >= position) {
							String playerName = users.get(position - 1)
									.getPlayerName();
							votes = users.get(position - 1).getTotalVotes();
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

	public void updateSign(int delay) {
		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				BlockState state = getLocation().getBlock().getState();
				if (state instanceof Sign) {
					Sign s = (Sign) state;

					for (int j = 0; j < lines.size(); j++) {
						s.setLine(j, lines.get(j));
					}
					s.update();
				}

			}

		}, delay);
	}

	public void checkValidSign() {
		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				setValid(getLocation().getBlock().getState() instanceof Sign);
			}
		});

	}

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

	public String getSign() {
		return sign;
	}

	public void removeSign() {
		ServerData.getInstance().removeSign(sign);
	}

	public void setSign(String sign) {
		this.sign = sign;
	}

	public boolean isLocationSame(Location loc) {
		return loc.equals(getLocation());
	}

	public String getRightClickMessage() {
		String msg = ConfigFormat.getInstance()
				.getSignTopVoterSignRightClickMessage();
		msg = msg.replace("%player%", playerName);
		msg = msg.replace("%position%", "" + position);
		msg = msg.replace("%votes%", "" + votes);
		msg = msg.replace("%SiteName%", data);
		return msg;
	}

	public boolean isValid() {
		return isValid;
	}

	public void setValid(boolean isValid) {
		this.isValid = isValid;
	}

}
