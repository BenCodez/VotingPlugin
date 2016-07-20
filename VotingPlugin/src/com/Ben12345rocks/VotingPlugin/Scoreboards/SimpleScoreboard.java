// credit: https://bukkit.org/threads/simplescoreboard-make-pretty-scoreboards-with-ease.263041/

package com.Ben12345rocks.VotingPlugin.Scoreboards;

import java.util.AbstractMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.bukkit.scoreboard.DisplaySlot;
import org.bukkit.scoreboard.Objective;
import org.bukkit.scoreboard.Scoreboard;
import org.bukkit.scoreboard.Team;

import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

// TODO: Auto-generated Javadoc
/**
 * The Class SimpleScoreboard.
 */
public class SimpleScoreboard {

	/** The scoreboard. */
	private Scoreboard scoreboard;

	/** The title. */
	private String title;

	/** The scores. */
	private Map<String, Integer> scores;

	/** The teams. */
	private List<Team> teams;

	/**
	 * Instantiates a new simple scoreboard.
	 *
	 * @param title
	 *            the title
	 */
	public SimpleScoreboard(String title) {
		scoreboard = Bukkit.getScoreboardManager().getNewScoreboard();
		this.title = title;
		scores = Maps.newLinkedHashMap();
		teams = Lists.newArrayList();
	}

	/**
	 * Adds the.
	 *
	 * @param text
	 *            the text
	 */
	public void add(String text) {
		text = Utils.getInstance().colorize(text);
		add(text, null);
	}

	/**
	 * Adds the.
	 *
	 * @param text
	 *            the text
	 * @param score
	 *            the score
	 */
	public void add(String text, Integer score) {
		Preconditions.checkArgument(text.length() < 48,
				"text cannot be over 48 characters in length");
		text = Utils.getInstance().colorize(text);
		text = fixDuplicates(text);
		scores.put(text, score);
	}

	/**
	 * Blank line.
	 */
	public void blankLine() {
		add(" ");
	}

	/**
	 * Builds the.
	 */
	@SuppressWarnings("deprecation")
	public void build() {
		Objective obj = scoreboard
				.registerNewObjective(
						(title.length() > 16 ? title.substring(0, 15) : title),
						"dummy");
		obj.setDisplayName(title);
		obj.setDisplaySlot(DisplaySlot.SIDEBAR);

		int index = scores.size();

		for (Map.Entry<String, Integer> text : scores.entrySet()) {
			Map.Entry<Team, String> team = createTeam(text.getKey());
			Integer score = text.getValue() != null ? text.getValue() : index;
			OfflinePlayer player = Bukkit.getOfflinePlayer(team.getValue());
			if (team.getKey() != null) {
				team.getKey().addPlayer(player);
			}
			obj.getScore(player).setScore(score);
			index -= 1;
		}
	}

	/**
	 * Creates the team.
	 *
	 * @param text
	 *            the text
	 * @return the map. entry
	 */
	private Map.Entry<Team, String> createTeam(String text) {
		String result = "";
		if (text.length() <= 16) {
			return new AbstractMap.SimpleEntry<>(null, text);
		}
		Team team = scoreboard.registerNewTeam("text-"
				+ scoreboard.getTeams().size());
		Iterator<String> iterator = Splitter.fixedLength(16).split(text)
				.iterator();
		team.setPrefix(iterator.next());
		result = iterator.next();
		if (text.length() > 32) {
			team.setSuffix(iterator.next());
		}
		teams.add(team);
		return new AbstractMap.SimpleEntry<>(team, result);
	}

	/**
	 * Fix duplicates.
	 *
	 * @param text
	 *            the text
	 * @return the string
	 */
	private String fixDuplicates(String text) {
		while (scores.containsKey(text)) {
			text += "§r";
		}
		if (text.length() > 48) {
			text = text.substring(0, 47);
		}
		return text;
	}

	/**
	 * Gets the scoreboard.
	 *
	 * @return the scoreboard
	 */
	public Scoreboard getScoreboard() {
		return scoreboard;
	}

	/**
	 * Reset.
	 */
	public void reset() {
		title = null;
		scores.clear();
		for (Team t : teams) {
			t.unregister();
		}
		teams.clear();
	}

	/**
	 * Send.
	 *
	 * @param players
	 *            the players
	 */
	public void send(Player... players) {
		if (Config.getInstance().getSendScoreboards()) {
			for (Player p : players) {
				p.setScoreboard(scoreboard);
			}
		}

	}

}
