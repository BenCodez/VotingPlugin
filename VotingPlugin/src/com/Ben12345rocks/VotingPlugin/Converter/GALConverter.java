package com.Ben12345rocks.VotingPlugin.Converter;

import java.util.ArrayList;
import java.util.Map.Entry;

import com.Ben12345rocks.AdvancedCore.Objects.Reward;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
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
					ConfigVoteSites.getInstance().setServiceSite(service, service);
					ConfigVoteSites.getInstance().setEnabled(service, true);
				} else {

					ArrayList<String> rewards = Config.getInstance().getAnySiteRewards();
					rewards.add(service);
					Config.getInstance().setAnySiteRewards(rewards);

				}
				Reward rewardFile = RewardHandler.getInstance().getReward(service);

				rewardFile.getConfig().setMessagesReward(rewardMessage);
				rewardFile.getConfig().setCommandsConsole(commands);
			} else if (entry.getKey().equals(VoteType.CUMULATIVE)) {
				String key = vote.key;
				if (StringUtils.getInstance().isInt(key)) {
					String rewardMessage = formatPlayer(vote.message);
					String broadcast = formatPlayer(vote.broadcast);

					ArrayList<String> rewards = new ArrayList<String>();
					rewards.add("cumulative" + key);
					Config.getInstance().setCumulativeRewards(Integer.parseInt(key), rewards);
					commands.add("broadcast " + broadcast);
					Reward rewardFile = RewardHandler.getInstance().getReward("cumulative" + key);

					rewardFile.getConfig().setMessagesReward(rewardMessage);
					rewardFile.getConfig().setCommandsConsole(commands);
				}
			} else if (entry.getKey().equals(VoteType.LUCKY)) {
				String key = vote.key;
				if (StringUtils.getInstance().isInt(key)) {
					String rewardMessage = formatPlayer(vote.message);
					String broadcast = formatPlayer(vote.broadcast);

					ArrayList<String> rewards = Config.getInstance().getAnySiteRewards();
					rewards.add("lucky" + key);
					Config.getInstance().setAnySiteRewards(rewards);
					commands.add("broadcast " + broadcast);
					Reward rewardFile = RewardHandler.getInstance().getReward("lucky" + key);

					rewardFile.getConfig().setMessagesReward(rewardMessage);
					rewardFile.getConfig().setChance(Integer.parseInt(key));
					rewardFile.getConfig().setCommandsConsole(commands);
				}
			} else if (entry.getKey().equals(VoteType.PERMISSION)) {
				String key = vote.key;

				String rewardMessage = formatPlayer(vote.message);
				String broadcast = formatPlayer(vote.broadcast);

				ArrayList<String> rewards = Config.getInstance().getAnySiteRewards();
				rewards.add("perm" + key);
				Config.getInstance().setAnySiteRewards(rewards);
				commands.add("broadcast " + broadcast);
				Reward rewardFile = RewardHandler.getInstance().getReward("perm" + key);
				rewardFile.getConfig().setMessagesReward(rewardMessage);
				rewardFile.getConfig().setRequirePermission(true);
				rewardFile.getConfig().setCommandsConsole(commands);
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
		return format.replace("{username}", "%player%").replace("{player}", "%player%").replace("{name}", "%player%");
	}

}
