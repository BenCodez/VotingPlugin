package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;

import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class Config.
 */
public class Config extends YMLFile {

	/** The instance. */
	static Config instance = new Config();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static Config getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new config.
	 */
	public Config() {
		super(new File(plugin.getDataFolder(), "Config.yml"));
	}

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public boolean getVoteRemindingEnabled() {
		return getData().getBoolean("VoteReminding.Enabled");
	}

	public void setVoteRemindingEnabled(boolean value) {
		getData().set("VoteReminding.Enabled", value);
		saveData();
	}

	/**
	 * Gets the remind delay.
	 *
	 * @return the remind delay
	 */
	public int getVoteRemindingRemindDelay() {
		return getData().getInt("VoteReminding.RemindDelay", 30);
	}

	public void setVoteRemindingRemindDelay(int value) {
		getData().set("VoteReminding.RemindDelay", value);
		saveData();
	}

	/**
	 * Gets the remind on login.
	 *
	 * @return the remind on login
	 */
	public boolean getVoteRemindingRemindOnLogin() {
		return getData().getBoolean("VoteReminding.RemindOnLogin");
	}

	public void setVoteRemindingRemindOnLogin(boolean value) {
		getData().set("VoteReminding.RemindOnLogin", value);
		saveData();
	}

	/**
	 * Gets the remind only once.
	 *
	 * @return the remind only once
	 */
	public boolean getVoteRemindingRemindOnlyOnce() {
		return getData().getBoolean("VoteReminding.RemindOnlyOnce");
	}

	public void setVoteRemindingRemindOnlyOnce(boolean value) {
		getData().set("VoteReminding.RemindOnlyOnce", value);
		saveData();
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVoteRemindingRewards() {
		return (ArrayList<String>) getData().getList("VoteReminding.Rewards",
				new ArrayList<String>());
	}

	public void setVoteRemindingRewards(ArrayList<String> value) {
		getData().set("VoteReminding.Rewards", value);
		saveData();
	}

	/**
	 * Allow un joined.
	 *
	 * @return true, if successful
	 */
	public boolean allowUnJoined() {
		return getData().getBoolean("AllowUnjoined");
	}

	/**
	 * Gets the auto create vote sites.
	 *
	 * @return the auto create vote sites
	 */
	public boolean getAutoCreateVoteSites() {
		return getData().getBoolean("AutoCreateVoteSites");
	}

	/**
	 * Gets the broad cast votes enabled.
	 *
	 * @return the broad cast votes enabled
	 */
	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("BroadcastVote");
	}

	/**
	 * Gets the log votes to file.
	 *
	 * @return the log votes to file
	 */
	public boolean getLogVotesToFile() {
		return getData().getBoolean("LogVotesToFile");
	}

	/**
	 * Gets the send scoreboards.
	 *
	 * @return the send scoreboards
	 */
	public boolean getSendScoreboards() {
		return getData().getBoolean("SendScoreboards");
	}

	/**
	 * Sets the allow un joined.
	 *
	 * @param value
	 *            the new allow un joined
	 */
	public void setAllowUnJoined(boolean value) {
		getData().set("AllowUnjoined", value);
		saveData();
	}

	/**
	 * Sets the broadcast vote enabled.
	 *
	 * @param value
	 *            the new broadcast vote enabled
	 */
	public void setBroadcastVoteEnabled(boolean value) {
		getData().set("BroadcastVote", value);
		saveData();
	}

	/**
	 * Sets the debug enabled.
	 *
	 * @param value
	 *            the new debug enabled
	 */
	public void setDebugEnabled(boolean value) {
		getData().set("Debug", value);
		saveData();
	}

	/**
	 * Sets the debug info ingame.
	 *
	 * @param value
	 *            the new debug info ingame
	 */
	public void setDebugInfoIngame(boolean value) {
		getData().set("DebugInfoIngame", value);
		saveData();
	}

	/**
	 * Sets the top voter awards enabled.
	 *
	 * @param value
	 *            the new top voter awards enabled
	 */
	public void setTopVoterAwardsEnabled(boolean value) {
		getData().set("TopVoterAwards", value);
		saveData();
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("Config.yml", true);
	}

	public boolean getCommandsUseGUIToday() {
		return getData().getBoolean("Commands.UseGUI.Today", true);
	}

	public boolean getCommandsUseGUITotal() {
		return getData().getBoolean("Commands.UseGUI.Total", true);
	}

	public boolean getCommandsUseGUINext() {
		return getData().getBoolean("Commands.UseGUI.Next", true);
	}

	public boolean getCommandsUseGUITopVoter() {
		return getData().getBoolean("Commands.UseGUI.TopVoter", true);
	}

	public boolean getCommandsUseGUILast() {
		return getData().getBoolean("Commands.UseGUI.Last", true);
	}

	public boolean getCommandsUseGUIVote() {
		return getData().getBoolean("Commands.UseGUI.Vote", true);
	}

}
