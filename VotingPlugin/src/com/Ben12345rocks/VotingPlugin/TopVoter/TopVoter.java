package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.block.Skull;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class TopVoter {

	static ConfigFormat format = ConfigFormat.getInstance();

	static TopVoter instance = new TopVoter();

	static Main plugin = Main.plugin;

	public static TopVoter getInstance() {
		return instance;
	}

	private TopVoter() {
	}

	public TopVoter(Main plugin) {
		TopVoter.plugin = plugin;
	}

	public String[] topVoter(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		int pageSize = 1 + (users.size() / pagesize);

		String title = format.getCommandVoteTopTitle()
				.replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize);
		msg.add(Utils.getInstance().colorize(title));

		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				plugin.topVoter);

		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);
			Collections.sort(users, new Comparator<User>() {
				@Override
				public int compare(User p1, User p2) {
					int p1Total = p1.getTotalVotes();
					int p2Total = p2.getTotalVotes();

					if (p1Total < p2Total) {
						return 1;
					}
					if (p1Total > p2Total) {
						return -1;
					}

					return 0;
				}
			});
			for (int i = 0; i < users.size(); i++) {
				String line = format.getCommandVoteTopLine()
						.replace("%num%", "" + (i + 1))
						.replace("%player%", users.get(i).getPlayerName())
						.replace("%votes%", "" + users.get(i).getTotalVotes());
				msg.add(line);
			}
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	@SuppressWarnings("deprecation")
	public boolean hasMonthChanged() {
		int prevMonth = ServerData.getInstance().getPrevMonth();
		int month = new Date().getMonth();
		ServerData.getInstance().setPrevMonth(month);
		if (prevMonth != month) {
			return true;
		}
		return false;
	}

	@SuppressWarnings("deprecation")
	public void checkTopVoterAward() {
		if (hasMonthChanged()) {
			plugin.getLogger().info("Month changed!");
			TopVoters.getInstance().storeTopVoters(new Date().getYear() + 1900,
					new Date().getMonth() + 1, topVoterNoColor());
			if (!Config.getInstance().getTopVoterAwardsDisabled()) {
				Set<String> places = TopVoterAwards.getInstance()
						.getPossibleRewardPlaces();
				int i = 0;
				for (User user : topVotersSortedAll()) {
					i++;
					if (places.contains(Integer.toString(i))) {
						user.topVoterAward(i);
					}
				}
			}
			resetTopVoter();

		}
	}

	public ArrayList<User> topVotersSortedAll() {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		Collections.sort(users, new Comparator<User>() {
			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalVotes();
				int p2Total = p2.getTotalVotes();

				if (p1Total < p2Total) {
					return 1;
				}
				if (p1Total > p2Total) {
					return -1;
				}

				return 0;
			}
		});
		if (Config.getInstance().getDebugEnabled()) {
			for (User user : users) {
				plugin.getLogger().info(
						"Debug: " + user.getPlayerName() + ", "
								+ user.getTotalVotes());
			}
		}
		return users;
	}

	public ArrayList<User> topVotersSortedVoteSite(VoteSite voteSite) {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		Collections.sort(users, new Comparator<User>() {
			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalVotesSite(voteSite);
				int p2Total = p2.getTotalVotesSite(voteSite);

				if (p1Total < p2Total) {
					return 1;
				}
				if (p1Total > p2Total) {
					return -1;
				}

				return 0;
			}
		});
		if (Config.getInstance().getDebugEnabled()) {
			for (User user : users) {
				plugin.getLogger().info(
						"Debug: " + user.getPlayerName() + ", "
								+ user.getTotalVotesSite(voteSite));
			}
		}
		return users;
	}

	public String[] topVoterNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);
			Collections.sort(users, new Comparator<User>() {
				@Override
				public int compare(User p1, User p2) {
					int p1Total = p1.getTotalVotes();
					int p2Total = p2.getTotalVotes();

					if (p1Total < p2Total) {
						return 1;
					}
					if (p1Total > p2Total) {
						return -1;
					}

					return 0;
				}
			});
			for (int i = 0; i < users.size(); i++) {
				String line = "%num%: %player%, %votes%"
						.replace("%num%", "" + (i + 1))
						.replace("%player%", users.get(i).getPlayerName())
						.replace("%votes%", "" + users.get(i).getTotalVotes());
				msg.add(line);
			}
		}

		return Utils.getInstance().convertArray(msg);
	}

	public void resetTopVoter() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotal(voteSite, 0);
			}
		}
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
					if (data.equalsIgnoreCase("All")) {
						ArrayList<User> users = topVotersSortedAll();
						Bukkit.getScheduler().runTaskLater(plugin,
								new Runnable() {

									@Override
									public void run() {
										BlockState state = loc.getBlock()
												.getState();
										if (state instanceof Sign) {
											Sign s = (Sign) state;

											s.setLine(0, "TopVoter: " + data);
											s.setLine(1, "#" + position);
											if (users.size() >= position) {
												s.setLine(
														2,
														users.get(position - 1)
																.getPlayerName());
												s.setLine(
														3,
														""
																+ users.get(
																		position - 1)
																		.getTotalVotes()
																+ " Votes");

												checkSkulls(
														loc,
														users.get(position - 1)
																.getPlayerName());
											} else {
												s.setLine(2, "No Player");
											}
											s.update();

										}
									}

								}, 10l + i);
					} else {
						for (VoteSite voteSite : plugin.voteSites) {
							if (data.equalsIgnoreCase(voteSite.getSiteName())) {
								ArrayList<User> users = topVotersSortedVoteSite(voteSite);
								Bukkit.getScheduler().runTaskLater(plugin,
										new Runnable() {

											@Override
											public void run() {
												BlockState state = loc
														.getBlock().getState();
												if (state instanceof Sign) {
													Sign s = (Sign) state;

													s.setLine(0, "TopVoter: "
															+ data);
													s.setLine(1, "#" + position);
													if (users.size() >= position) {
														s.setLine(
																2,
																users.get(
																		position - 1)
																		.getPlayerName());
														s.setLine(
																3,
																""
																		+ users.get(
																				position - 1)
																				.getTotalVotesSite(
																						voteSite)
																		+ " Votes");
														checkSkulls(
																loc,
																users.get(
																		position - 1)
																		.getPlayerName());
													} else {
														s.setLine(2,
																"No Player");
													}
													s.update();

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
}
