package com.Ben12345rocks.VotingPlugin.Signs;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

public class Signs {

	static ConfigFormat format = ConfigFormat.getInstance();

	static Signs instance = new Signs();

	static Main plugin = Main.plugin;

	public static Signs getInstance() {
		return instance;
	}

	private Signs() {
	}

	public Signs(Main plugin) {
		Signs.plugin = plugin;
	}

	@SuppressWarnings("deprecation")
	public void checkSkulls(Location loc, String playerName) {
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

	public String getSignFromLocation(Location loc) {
		for (String sign : ServerData.getInstance().getSigns()) {
			if (ServerData.getInstance().getSignLocation(sign).equals(loc)) {
				return sign;
			}
		}

		return null;
	}

	public void refreshSigns() {
		Set<String> signs = ServerData.getInstance().getSigns();
		if (signs != null) {

			int i = 0;
			for (String sign : signs) {
				Location loc = ServerData.getInstance().getSignLocation(sign);
				int position = ServerData.getInstance().getSignPosition(sign);
				String data = ServerData.getInstance().getSignData(sign);
				if (position != 0) {
					String line1 = ConfigFormat.getInstance()
							.getSignTopVoterSignLine1();
					String line2 = ConfigFormat.getInstance()
							.getSignTopVoterSignLine2();
					String line3 = ConfigFormat.getInstance()
							.getSignTopVoterSignLine3();
					String line4 = ConfigFormat.getInstance()
							.getSignTopVoterSignLine4();

					ArrayList<String> lines = new ArrayList<String>();

					lines.add(line1);
					lines.add(line2);
					lines.add(line3);
					lines.add(line4);

					if (data.equalsIgnoreCase("All")) {
						ArrayList<User> users = TopVoter.getInstance()
								.topVotersSortedAll();

						if (users.size() >= position) {
							String playerName = users.get(position - 1)
									.getPlayerName();
							int votes = users.get(position - 1).getTotalVotes();
							for (int j = 0; j < lines.size(); j++) {
								lines.set(
										j,
										lines.get(j)
										.replace("%votes%", "" + votes)
										.replace("%player%", playerName));
							}
						} else {
							String playerName = "No Player";
							int votes = 0;
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

						ServerData.getInstance().setLines(sign, lines);

						Bukkit.getScheduler().runTaskLater(plugin,
								new Runnable() {

							@Override
							public void run() {
								BlockState state = loc.getBlock()
										.getState();
								if (state instanceof Sign) {
									Sign s = (Sign) state;

									List<String> lines = ServerData
											.getInstance().getLines(
													sign);

									for (int j = 0; j < lines.size(); j++) {
										s.setLine(j, lines.get(j));
									}
									s.update();

									if (users.size() >= position) {
										String playerName = users.get(
												position - 1)
												.getPlayerName();
										checkSkulls(loc, playerName);
									}
								}

							}

						}, 10l + i);
					} else {
						for (VoteSite voteSite : plugin.voteSites) {
							if (data.equalsIgnoreCase(voteSite.getSiteName())) {
								ArrayList<User> users = TopVoter.getInstance()
										.topVotersSortedVoteSite(voteSite);

								if (users.size() >= position) {
									String playerName = users.get(position - 1)
											.getPlayerName();
									int votes = users.get(position - 1)
											.getTotalVotes();
									for (int j = 0; j < lines.size(); j++) {
										lines.set(
												j,
												lines.get(j)
												.replace("%votes%",
														"" + votes)
														.replace("%player%",
																playerName));
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

								ServerData.getInstance().setLines(sign, lines);

								Bukkit.getScheduler().runTaskLater(plugin,
										new Runnable() {

									@Override
									public void run() {
										BlockState state = loc
												.getBlock().getState();
										if (state instanceof Sign) {
											Sign s = (Sign) state;

											List<String> lines = ServerData
													.getInstance()
													.getLines(sign);

											for (int j = 0; j < lines
													.size(); j++) {
												s.setLine(j,
														lines.get(j));
											}
											s.update();

											if (users.size() >= position) {
												String playerName = users
														.get(position - 1)
														.getPlayerName();
												checkSkulls(loc,
														playerName);
											}
										}

									}

								}, 10l + i);
							}

						}
					}

				} else {
					ServerData.getInstance().removeSign(sign);
				}
				i += 5;

			}
		}
	}
}
