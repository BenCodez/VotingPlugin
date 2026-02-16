package com.bencodez.votingplugin.data;

import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.signs.SignHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;

// TODO: Auto-generated Javadoc
/**
 * The Class ServerData.
 */
public class ServerData {

	private VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Constructs a new ServerData.
	 *
	 * @param plugin the main plugin instance
	 */
	public ServerData(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Adds an auto-cached placeholder.
	 *
	 * @param placeholder the placeholder to add
	 */
	public void addAutoCachedPlaceholder(String placeholder) {
		List<String> p = getAutoCachedPlaceholder();

		if (!ArrayUtils.containsIgnoreCase(p, placeholder)) {
			p.add(placeholder);
			setAutoCachedPlaceholder(p);
		}
	}

	/**
	 * Adds a service site to the list.
	 *
	 * @param site the service site to add
	 */
	public synchronized void addServiceSite(String site) {
		ArrayList<String> l = getServiceSites();
		if (!getServiceSites().contains(site)) {
			l.add(site);
		}
		setServiceSites(ArrayUtils.removeDuplicates(l));
	}

	/**
	 * Adds the sign.
	 *
	 * @param location the location
	 * @param data     the data
	 * @param position the position
	 */
	public void addSign(Location location, String data, int position) {

		int count = nextSignNumber();

		getData().set("Signs." + count + ".World", location.getWorld().getName());
		getData().set("Signs." + count + ".X", location.getBlockX());
		getData().set("Signs." + count + ".Y", location.getBlockY());
		getData().set("Signs." + count + ".Z", location.getBlockZ());
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		saveData();
		plugin.getSigns().getSigns().add(new SignHandler(plugin, "" + count, getSignLocation("" + count),
				getSignSkullLocation("" + count), getSignData("" + count), getSignPosition("" + count)));
	}

	/**
	 * Adds a timed vote to the cache.
	 *
	 * @param num the vote number
	 * @param vote the vote time queue entry
	 */
	public void addTimeVoted(int num, VoteTimeQueue vote) {
		getData().set("TimedVoteCache." + num + ".Name", vote.getName());
		getData().set("TimedVoteCache." + num + ".Service", vote.getService());
		getData().set("TimedVoteCache." + num + ".Time", vote.getTime());
		saveData();
	}

	/**
	 * Adds a vote shop purchase for the specified identifier.
	 *
	 * @param ident the vote shop identifier
	 */
	public void addVoteShopPurchase(String ident) {
		setVoteShopPurchases(ident, (getVoteShopPurchases(ident) + 1));
	}

	/**
	 * Clears the timed vote cache.
	 */
	public void clearTimedVoteCache() {
		getData().set("TimedVoteCache", null);
		saveData();
	}

	/**
	 * Gets the auto cached placeholders.
	 *
	 * @return the auto cached placeholder list
	 */
	public List<String> getAutoCachedPlaceholder() {
		return getData().getStringList("AutoCachePlaceholders");
	}

	/**
	 * Gets the current bungee vote party count.
	 *
	 * @return the bungee vote party current count
	 */
	public int getBungeeVotePartyCurrent() {
		return getData().getInt("BungeeVotePartyCurrent");
	}

	/**
	 * Gets the required votes for bungee vote party.
	 *
	 * @return the bungee vote party required votes
	 */
	public int getBungeeVotePartyRequired() {
		return getData().getInt("BungeeVotePartyRequired");
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public ConfigurationSection getData() {
		ConfigurationSection data = plugin.getServerDataFile().getData().getConfigurationSection("VotingPlugin");
		if (data == null) {
			plugin.getServerDataFile().getData().createSection("VotingPlugin");
			data = plugin.getServerDataFile().getData().getConfigurationSection("VotingPlugin");
		}
		return data;
	}

	/**
	 * Gets the list of disabled reminders.
	 *
	 * @return the disabled reminders
	 */
	public List<String> getDisabledReminders() {
		return getData().getStringList("DisabledReminders");
	}

	/**
	 * Gets the service sites list.
	 *
	 * @return the service sites
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getServiceSites() {
		return (ArrayList<String>) getData().getList("ServiceSites", new ArrayList<>());
	}

	/**
	 * Gets the sign data.
	 *
	 * @param sign the sign
	 * @return the sign data
	 */
	public String getSignData(String sign) {
		return getData().getString("Signs." + sign + ".Data");
	}

	/**
	 * Gets the sign location.
	 *
	 * @param sign the sign
	 * @return the sign location
	 */
	public Location getSignLocation(String sign) {
		return new Location(Bukkit.getWorld(getData().getString("Signs." + sign + ".World")),
				getData().getDouble("Signs." + sign + ".X"), getData().getDouble("Signs." + sign + ".Y"),
				getData().getDouble("Signs." + sign + ".Z"));
	}

	/**
	 * Gets the sign position.
	 *
	 * @param sign the sign
	 * @return the sign position
	 */
	public int getSignPosition(String sign) {
		return getData().getInt("Signs." + sign + ".Position");
	}

	/**
	 * Gets the signs.
	 *
	 * @return the signs
	 */
	public Set<String> getSigns() {
		try {
			return getData().getConfigurationSection("Signs").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<>();
		}
	}

	/**
	 * Gets the sign location.
	 *
	 * @param sign the sign
	 * @return the sign location
	 */
	public Location getSignSkullLocation(String sign) {
		if (getData().getString("Signs." + sign + ".Skull.World", "").isEmpty()) {
			return null;
		}
		return new Location(Bukkit.getWorld(getData().getString("Signs." + sign + ".Skull.World")),
				getData().getDouble("Signs." + sign + ".Skull.X"), getData().getDouble("Signs." + sign + ".Skull.Y"),
				getData().getDouble("Signs." + sign + ".Skull.Z"));
	}

	/**
	 * Gets the timed vote cache keys.
	 *
	 * @return the timed vote cache keys
	 */
	public Set<String> getTimedVoteCacheKeys() {
		if (getData().isConfigurationSection("TimedVoteCache")) {
			return getData().getConfigurationSection("TimedVoteCache").getKeys(false);
		}
		return new HashSet<>();
	}

	/**
	 * Gets the timed vote cache section for the given number.
	 *
	 * @param num the cache entry number
	 * @return the timed vote cache section
	 */
	public ConfigurationSection getTimedVoteCacheSection(String num) {
		return getData().getConfigurationSection("TimedVoteCache." + num);
	}

	/**
	 * Gets the extra required votes for vote party.
	 *
	 * @return the vote party extra required votes
	 */
	public int getVotePartyExtraRequired() {
		return getData().getInt("VotePartyExtraRequired", 0);
	}

	/**
	 * Gets the number of purchases for a vote shop item.
	 *
	 * @param ident the vote shop identifier
	 * @return the vote shop purchases
	 */
	public int getVoteShopPurchases(String ident) {
		return getData().getInt("VoteShopPurchases." + ident);
	}

	/**
	 * Checks if the last vote party was on the same day.
	 *
	 * @return true if same day
	 */
	public boolean isLastVotePartySameDay() {
		int num = getData().getInt("LastVoteParty", 0);
		if (num == plugin.getTimeChecker().getTime().getDayOfYear()) {
			return true;
		}
		return false;
	}

	/**
	 * Checks if the last vote party was in the same week.
	 *
	 * @return true if same week
	 */
	public boolean isLastVotePartySameWeek() {
		int num = getData().getInt("LastVotePartyWeek", -1);
		if (num == plugin.getTimeChecker().getTime().get(WeekFields.of(Locale.getDefault()).weekOfYear())
				&& num != -1) {
			return true;
		}
		return false;
	}

	/**
	 * Checks if the vote shop has been converted.
	 *
	 * @return true if converted
	 */
	public boolean isVoteShopConverted() {
		return getData().getBoolean("VoteShopConverted");
	}

	/**
	 * Next sign number.
	 *
	 * @return the int
	 */
	public int nextSignNumber() {
		Set<String> signs = getSigns();

		if (signs != null) {
			for (int i = 0; i < 100000; i++) {
				if (!signs.contains(Integer.toString(i))) {
					return i;
				}
			}
		}
		return 0;
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		plugin.getServerDataFile().reloadData();
	}

	/**
	 * Gets the Discord message ID for the top voter.
	 *
	 * @param top the top voter type
	 * @return the top voter message ID
	 */
	public long getTopVoterMessageId(TopVoter top) {
		return getData().getLong("DiscordSRV.TopVoterMessageId." + top.toString(), 0);
	}

	/**
	 * Sets the Discord message ID for the top voter.
	 *
	 * @param top the top voter type
	 * @param messageId the message ID
	 */
	public void setTopVoterMessageId(TopVoter top, long messageId) {
		getData().set("DiscordSRV.TopVoterMessageId." + top.toString(), messageId);
		saveData();
	}

	/**
	 * Removes the sign.
	 *
	 * @param sign the sign
	 */
	public void removeSign(String sign) {
		getData().set("Signs." + sign + ".World", null);
		getData().set("Signs." + sign + ".X", null);
		getData().set("Signs." + sign + ".Y", null);
		getData().set("Signs." + sign + ".Z", null);
		getData().set("Signs." + sign + ".Data", null);
		getData().set("Signs." + sign + ".Position", null);
		getData().set("Signs." + sign, null);
		getData().set("Signs." + sign + ".Skull.World", null);
		getData().set("Signs." + sign + ".Skull.X", null);
		getData().set("Signs." + sign + ".Skull.Y", null);
		getData().set("Signs." + sign + ".Skull.Z", null);
		saveData();
	}

	/**
	 * Save data.
	 */
	public synchronized void saveData() {
		plugin.getServerDataFile().saveData();
	}

	/**
	 * Saves the disabled reminders list.
	 *
	 * @param disabledReminders the list of UUIDs with disabled reminders
	 */
	public void saveDisabledReminders(ArrayList<UUID> disabledReminders) {
		ArrayList<String> uuids = new ArrayList<>();
		for (UUID uuid : disabledReminders) {
			uuids.add(uuid.toString());
		}
		getData().set("DisabledReminders", uuids);
		saveData();
	}

	/**
	 * Sets the auto cached placeholders.
	 *
	 * @param placeholders the list of placeholders
	 */
	public void setAutoCachedPlaceholder(List<String> placeholders) {
		getData().set("AutoCachePlaceholders", placeholders);
		saveData();
	}

	/**
	 * Sets the current bungee vote party count.
	 *
	 * @param current the current vote count
	 */
	public void setBungeeVotePartyCurrent(int current) {
		getData().set("BungeeVotePartyCurrent", current);
		saveData();
	}

	/**
	 * Sets the required votes for bungee vote party.
	 *
	 * @param required the required vote count
	 */
	public void setBungeeVotePartyRequired(int required) {
		getData().set("BungeeVotePartyRequired", required);
		saveData();
	}

	/**
	 * Sets the service sites list.
	 *
	 * @param list the service sites list
	 */
	public void setServiceSites(ArrayList<String> list) {
		getData().set("ServiceSites", list);
		saveData();
	}

	/**
	 * Sets the vote shop converted status.
	 *
	 * @param value the converted status
	 */
	public void setShopConverted(boolean value) {
		getData().set("VoteShopConverted", value);
		saveData();
	}

	/**
	 * Sets sign data.
	 *
	 * @param count the sign count
	 * @param location the sign location
	 * @param skullLocation the skull location
	 * @param data the sign data
	 * @param position the sign position
	 */
	public void setSign(String count, Location location, Location skullLocation, String data, int position) {

		getData().set("Signs." + count + ".World", location.getWorld().getName());
		getData().set("Signs." + count + ".X", (int) location.getX());
		getData().set("Signs." + count + ".Y", (int) location.getY());
		getData().set("Signs." + count + ".Z", (int) location.getZ());
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		if (skullLocation != null) {
			getData().set("Signs." + count + ".Skull.World", skullLocation.getWorld().getName());
			getData().set("Signs." + count + ".Skull.X", (int) skullLocation.getX());
			getData().set("Signs." + count + ".Skull.Y", (int) skullLocation.getY());
			getData().set("Signs." + count + ".Skull.Z", (int) skullLocation.getZ());
		}
		saveData();
	}

	/**
	 * Sets the skull location for a sign.
	 *
	 * @param count the sign count
	 * @param skullLocation the skull location
	 */
	public void setSkullLocation(String count, Location skullLocation) {
		if (skullLocation != null) {
			getData().set("Signs." + count + ".Skull.World", skullLocation.getWorld().getName());
			getData().set("Signs." + count + ".Skull.X", (int) skullLocation.getX());
			getData().set("Signs." + count + ".Skull.Y", (int) skullLocation.getY());
			getData().set("Signs." + count + ".Skull.Z", (int) skullLocation.getZ());
		}
	}

	/**
	 * Sets the version.
	 */
	public void setVersion() {
		getData().set("Version", Bukkit.getVersion());
		saveData();
	}

	/**
	 * Sets the extra required votes for vote party.
	 *
	 * @param value the extra required votes
	 */
	public void setVotePartyExtraRequired(int value) {
		getData().set("VotePartyExtraRequired", value);
		saveData();
	}

	/**
	 * Sets the number of purchases for a vote shop item.
	 *
	 * @param ident the vote shop identifier
	 * @param amount the purchase amount
	 */
	public void setVoteShopPurchases(String ident, int amount) {
		getData().set("VoteShopPurchases." + ident, amount);
		saveData();
	}

	/**
	 * Updates the last vote party timestamp to today.
	 */
	public void updateLastVoteParty() {
		getData().set("LastVoteParty", plugin.getTimeChecker().getTime().getDayOfYear());
		saveData();
	}

	/**
	 * Updates the last vote party week timestamp to this week.
	 */
	public void updateLastVotePartyWeek() {
		getData().set("LastVotePartyWeek",
				plugin.getTimeChecker().getTime().get(WeekFields.of(Locale.getDefault()).weekOfYear()));
		saveData();
	}

	/**
	 * Updates placeholders to lowercase format.
	 */
	public void updatePlaceholders() {
		boolean data = getData().getBoolean("AutoCacheUpdated", false);
		if (!data) {
			List<String> placeholders = getAutoCachedPlaceholder();
			for (int i = 0; i < placeholders.size(); i++) {
				placeholders.set(i, placeholders.get(i).toLowerCase());
			}
			setAutoCachedPlaceholder(placeholders);
			getData().set("AutoCacheUpdated", true);
			saveData();
		}
	}

	/**
	 * Update values.
	 */
	public void updateValues() {
		setVersion();
	}
}
