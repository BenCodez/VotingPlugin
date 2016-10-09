package com.Ben12345rocks.VotingPlugin.Converter;

import java.util.ArrayList;
import java.util.Map.Entry;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Configs.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.swifteh.GAL.GAL;
import com.swifteh.GAL.GALVote;
import com.swifteh.GAL.VoteType;

// TODO: Auto-generated Javadoc
/**
 * The Class GALConverter.
 */
public class GALConverter {

	/** The instance. */
	static GALConverter instance = new GALConverter();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of GALConverter.
	 *
	 * @return single instance of GALConverter
	 */
	public static GALConverter getInstance() {
		return instance;
	}

	/** The gal plug. */
	private GAL galPlug = GAL.p;

	/**
	 * Instantiates a new GAL converter.
	 */
	private GALConverter() {
	}

	/**
	 * Instantiates a new GAL converter.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public GALConverter(Main plugin) {
		GALConverter.plugin = plugin;
	}

	/**
	 * Convert.
	 */
	public void convert() {
		create();
		plugin.reload();
	}

	/**
	 * Creates the.
	 */
	public void create() {
		for (Entry<VoteType, GALVote> entry : galPlug.galVote.entries()) {
			GALVote vote = entry.getValue();
			ArrayList<String> commands = (ArrayList<String>) vote.commands;
			for (int i = 0; i < commands.size(); i++) {
				commands.set(i, commands.get(i).replaceFirst("/", ""));
			}
			if (entry.getKey().equals(VoteType.NORMAL)) {
				String service = vote.key;
				String rewardMessage = formatPlayer(vote.message);
				// String broadcast = vote.broadcast;

				if (!service.equalsIgnoreCase("default")) {

					ConfigVoteSites.getInstance().generateVoteSite(service);
					ArrayList<String> rewards = new ArrayList<String>();
					rewards.add(service);
					ConfigVoteSites.getInstance().setRewards(service, rewards);
					ConfigVoteSites.getInstance().setServiceSite(service,
							service);
					ConfigVoteSites.getInstance().setEnabled(service, true);
				} else {

					ArrayList<String> rewards = ConfigOtherRewards
							.getInstance().getAnySiteRewards();
					rewards.add(service);
					ConfigOtherRewards.getInstance().setAnySiteRewards(rewards);

				}
				ConfigRewards.getInstance().setMessagesReward(service,
						rewardMessage);
				ConfigRewards.getInstance().setCommandsConsole(service,
						commands);
			} else if (entry.getKey().equals(VoteType.CUMULATIVE)) {
				String key = vote.key;
				if (Utils.getInstance().isInt(key)) {
					String rewardMessage = formatPlayer(vote.message);
					String broadcast = formatPlayer(vote.broadcast);

					ArrayList<String> rewards = new ArrayList<String>();
					rewards.add("cumulative" + key);
					ConfigOtherRewards.getInstance().setCumulativeRewards(
							Integer.parseInt(key), rewards);
					commands.add("broadcast " + broadcast);
					ConfigRewards.getInstance().setMessagesReward(
							"cumulative" + key, rewardMessage);
					ConfigRewards.getInstance().setCommandsConsole(
							"cumulative" + key, commands);
				}
			} else if (entry.getKey().equals(VoteType.LUCKY)) {
				String key = vote.key;
				if (Utils.getInstance().isInt(key)) {
					String rewardMessage = formatPlayer(vote.message);
					String broadcast = formatPlayer(vote.broadcast);

					ArrayList<String> rewards = ConfigOtherRewards
							.getInstance().getAnySiteRewards();
					rewards.add("lucky" + key);
					ConfigOtherRewards.getInstance().setAnySiteRewards(rewards);
					commands.add("broadcast " + broadcast);
					ConfigRewards.getInstance().setMessagesReward(
							"lucky" + key, rewardMessage);
					ConfigRewards.getInstance().setChance("lucky" + key,
							Integer.parseInt(key));
					ConfigRewards.getInstance().setCommandsConsole(
							"lucky" + key, commands);
				}
			} else if (entry.getKey().equals(VoteType.PERMISSION)) {
				String key = vote.key;

				String rewardMessage = formatPlayer(vote.message);
				String broadcast = formatPlayer(vote.broadcast);

				ArrayList<String> rewards = ConfigOtherRewards.getInstance()
						.getAnySiteRewards();
				rewards.add("perm" + key);
				ConfigOtherRewards.getInstance().setAnySiteRewards(rewards);
				commands.add("broadcast " + broadcast);
				ConfigRewards.getInstance().setMessagesReward("perm" + key,
						rewardMessage);
				ConfigRewards.getInstance().setRequirePermission("perm" + key,
						true);
				ConfigRewards.getInstance().setCommandsConsole("perm" + key,
						commands);
			}
		}
	}

	/**
	 * Format player.
	 *
	 * @param format
	 *            the format
	 * @return the string
	 */
	public String formatPlayer(String format) {
		return format.replace("{username}", "%player%")
				.replace("{player}", "%player%").replace("{name}", "%player%");
	}

}
