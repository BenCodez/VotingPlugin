package com.Ben12345rocks.VotingPlugin.Converter;

import java.util.ArrayList;
import java.util.List;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.swifteh.GAL.GAL;
import com.swifteh.GAL.GALVote;

// TODO: Auto-generated Javadoc
/**
 * The Class Utils.
 */
public class GALConverter {

	/** The instance. */
	static GALConverter instance = new GALConverter();

	/** The plugin. */
	static Main plugin = Main.plugin;

	private GAL galPlug = GAL.p;

	/**
	 * Gets the single instance of Utils.
	 *
	 * @return single instance of Utils
	 */
	public static GALConverter getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new utils.
	 */
	private GALConverter() {
	}

	/**
	 * Instantiates a new utils.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public GALConverter(Main plugin) {
		GALConverter.plugin = plugin;
	}

	public void convert() {
		createSites();
	}

	public void createSites() {
		for (GALVote vote : galPlug.galVote.values()) {
			String service = vote.key;
			String rewardMessage = vote.message.replace("{username}",
					"%Player%");
			// String broadcast = vote.broadcast;
			List<String> commands = vote.commands;
			ConfigVoteSites.getInstance().generateVoteSite(service);
			ArrayList<String> rewards = new ArrayList<String>();
			rewards.add(service);
			ConfigVoteSites.getInstance().setRewards(service, rewards);
			ConfigRewards.getInstance().setMessagesReward(service,
					rewardMessage);
			ConfigRewards.getInstance().setCommandsConsole(service,
					(ArrayList<String>) commands);
		}
	}

	public void createCumulative() {

	}

}
