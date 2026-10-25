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

public class ServerData {

	/** Immutable live display values, separate from durable publication acknowledgement. */
	public record VotePartyDisplay(int total, int extraRequired) {
		public int required(int configured) { return extraRequired > 0 ? configured + extraRequired : configured; }
		public int needed(int configured) { return required(configured) - total; }
	}

	private volatile VotePartyDisplay votePartyDisplay = new VotePartyDisplay(0, 0);

	/** Snapshot-only: never acquires a YAML owner or waits for filesystem publication. */
	public VotePartyDisplay getVotePartyDisplay() { return votePartyDisplay; }

	/** Called during startup before placeholders are registered. */
	public void refreshVotePartyDisplay() { mutateData(this::captureVotePartyDisplay); }

	private void captureVotePartyDisplay() {
		var data = getFile().getData().getConfigurationSection("VotingPlugin");
		votePartyDisplay = data == null ? new VotePartyDisplay(0, 0)
				: new VotePartyDisplay(data.getInt("VoteParty.Total"), data.getInt("VotePartyExtraRequired"));
	}

	private VotingPluginMain plugin = VotingPluginMain.plugin;
	private volatile com.bencodez.advancedcore.data.ServerData generationFile;
	private final Object generationBinding = new Object();
	private volatile com.bencodez.votingplugin.util.ServerDataPublicationFence generationFence;

	private com.bencodez.advancedcore.data.ServerData getFile() {
		var current = generationFile;
		if (current != null) return current;
		synchronized (generationBinding) {
			if (generationFile == null) {
				var owner = getPublicationFence();
				// Reject an old lazy wrapper before exposing successor mutable state.
				try (var lease = owner == null ? null : owner.acquire()) {
					var file = plugin.getServerDataFile();
					if (file instanceof GenerationFencedServerDataFile guarded && !guarded.belongsTo(owner))
						throw new IllegalStateException("Server-data file belongs to another generation");
					generationFile = file;
				}
			}
			return generationFile;
		}
	}



	private record SignCoordinates(String world, int x, int y, int z) {
		static SignCoordinates capture(Location location) {
			return new SignCoordinates(location.getWorld().getName(), (int) location.getX(),
					(int) location.getY(), (int) location.getZ());
		}
		void store(ConfigurationSection data, String path) {
			data.set(path + ".World", world);
			data.set(path + ".X", x);
			data.set(path + ".Y", y);
			data.set(path + ".Z", z);
		}
	}

	/** Only YAML work belongs here; platform callbacks run outside the file owner. */
	private void mutateData(Runnable mutation) {
		var fence = getPublicationFence();
		try (var owner = fence == null ? null : fence.acquire()) {
			synchronized (publicationMonitor()) {
				mutation.run();
			}
		}
	}

	/** Admit the complete ordinary mutation before changing live YAML or its display. */
	private void mutateSavedData(Runnable mutation) {
		mutateData(() -> getFile().mutateOrdinaryData(ignored -> {
			try { mutation.run(); }
			catch (RuntimeException | Error failure) {
				try { captureVotePartyDisplay(); }
				catch (RuntimeException | Error observationFailure) {
					if (observationFailure != failure) failure.addSuppressed(observationFailure);
				}
				throw failure;
			}
			captureVotePartyDisplay();
		}));
	}

	/** Store a scalar and save under the same YAML ownership as final SDK snapshots. */
	public void setInt(String path, int value) {
		mutateSavedData(() -> {
			getData().set(path, value);
		});
	}

	/** Store a detached string list, preventing later caller mutation of live YAML. */
	public void setStringList(String path, List<String> values) {
		var snapshot = values == null ? null : new ArrayList<>(values);
		mutateSavedData(() -> {
			getData().set(path, snapshot);
		});
	}

	/** All mutations serialize with the actual SDK YAML owner. */
	private Object publicationMonitor() {
		var file = getFile();
		return java.util.Objects.requireNonNull(file.publicationMonitor(), "Server-data publication monitor");
	}

	private com.bencodez.votingplugin.util.ServerDataPublicationFence getPublicationFence() {
		var current = generationFence;
		if (current != null) return current;
		synchronized (generationBinding) {
			if (generationFence == null) generationFence = plugin.getServerDataPublicationFence();
			return generationFence;
		}
	}

	/**
	 * Constructs a new ServerData.
	 *
	 * @param plugin the main plugin instance
	 */
	public ServerData(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.generationFence = plugin.getServerDataPublicationFence();
	}

	/**
	 * Adds an auto-cached placeholder.
	 *
	 * @param placeholder the placeholder to add
	 */
	public void addAutoCachedPlaceholder(String placeholder) {
		mutateData(() -> {
			List<String> p = getAutoCachedPlaceholder();

			if (!ArrayUtils.containsIgnoreCase(p, placeholder)) {
				p.add(placeholder);
				setAutoCachedPlaceholder(p);
			}
		});
	}

	/**
	 * Adds a service site to the list.
	 *
	 * @param site the service site to add
	 */
	public void addServiceSite(String site) {
		mutateData(() -> {
			ArrayList<String> l = getServiceSites();
			if (!getServiceSites().contains(site)) {
				l.add(site);
			}
			setServiceSites(ArrayUtils.removeDuplicates(l));
		});
	}

	/**
	 * Adds the sign.
	 *
	 * @param location the location
	 * @param data     the data
	 * @param position the position
	 */
	public void addSign(Location location, String data, int position) {
		var coordinates = new SignCoordinates(location.getWorld().getName(), location.getBlockX(),
				location.getBlockY(), location.getBlockZ());
		int[] selected = new int[1];
		mutateSavedData(() -> {
			int count = nextSignNumber();
			selected[0] = count;
			coordinates.store(getData(), "Signs." + count);
			getData().set("Signs." + count + ".Data", data);
			getData().set("Signs." + count + ".Position", position);
		});
		String count = Integer.toString(selected[0]);
		plugin.getSigns().getSigns().add(new SignHandler(plugin, count, getSignLocation(count),
				getSignSkullLocation(count), getSignData(count), getSignPosition(count)));
	}

	/**
	 * Adds a timed vote to the cache.
	 *
	 * @param num the vote number
	 * @param vote the vote time queue entry
	 */
	public void addTimeVoted(int num, VoteTimeQueue vote) {
		mutateData(() -> {
			getData().set("TimedVoteCache." + num + ".Name", vote.getName());
			getData().set("TimedVoteCache." + num + ".Service", vote.getService());
			getData().set("TimedVoteCache." + num + ".Time", vote.getTime());
			saveTimedVoteCache();
		});
	}

	/**
	 * Adds a vote shop purchase for the specified identifier.
	 *
	 * @param ident the vote shop identifier
	 */
	public void addVoteShopPurchase(String ident) {
		mutateData(() -> {
			setVoteShopPurchases(ident, (getVoteShopPurchases(ident) + 1));
		});
	}

	/**
	 * Clears the timed vote cache.
	 */
	public void clearTimedVoteCache() {
		mutateData(() -> {
			getData().set("TimedVoteCache", null);
			saveTimedVoteCache();
		});
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
		var fence = getPublicationFence();
		try (var owner = fence == null ? null : fence.acquire()) {
			synchronized (publicationMonitor()) {
				ConfigurationSection data = getFile().getData().getConfigurationSection("VotingPlugin");
				if (data == null) data = getFile().getData().createSection("VotingPlugin");
				return data;
			}
		}
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
	public ArrayList<String> getServiceSites() {
		synchronized (publicationMonitor()) {
			return new ArrayList<>(getData().getStringList("GottenServiceSites"));
		}
	}

	/**
	 * Returns a detached service-site snapshot without creating the VotingPlugin
	 * server-data section. Intended for read-only diagnostics and inspections.
	 *
	 * @return persisted service-site observations
	 */
	public List<String> getServiceSitesReadOnly() {
		synchronized (publicationMonitor()) {
			ConfigurationSection root = getFile().getData().getConfigurationSection("VotingPlugin");
			return root == null ? List.of() : List.copyOf(root.getStringList("GottenServiceSites"));
		}
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
		mutateData(() -> {
			getFile().reloadData();
			captureVotePartyDisplay();
		});
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
		mutateSavedData(() -> {
			getData().set("DiscordSRV.TopVoterMessageId." + top.toString(), messageId);
		});
	}

	/**
	 * Uses the existing writer and confirms this ID in the saved file. Legacy void saves may
	 * swallow I/O failure; queued/custom writers may also remain unconfirmed at this point.
	 * False is an uncertain outcome, never permission to delete a possibly recorded message.
	 * Call only on the existing worker, under this facade's normal save ordering.
	 */
	public synchronized boolean setTopVoterMessageIdConfirmed(TopVoter top, long messageId) {
		setTopVoterMessageId(top, messageId);
		try {
			var saved = new org.bukkit.configuration.file.YamlConfiguration();
			saved.load(plugin.getServerDataFile().getdFile());
			return saved.getLong("VotingPlugin.DiscordSRV.TopVoterMessageId." + top.toString(), 0) == messageId;
		} catch (java.io.IOException | org.bukkit.configuration.InvalidConfigurationException | RuntimeException failure) {
			return false;
		}
	}

	/**
	 * Removes the sign.
	 *
	 * @param sign the sign
	 */
	public void removeSign(String sign) {
		mutateSavedData(() -> {
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
		});
	}

	/** Checkpoint the pending suffix without retiring its live queue entries. */
	public void replaceTimedVoteCache(java.util.Collection<VoteTimeQueue> pending) {
		mutateData(() -> {
			Object previous = getData().get("TimedVoteCache");
			try {
				setTimedVoteCache(pending);
				saveTimedVoteCache();
			} catch (RuntimeException | Error failure) {
				getData().set("TimedVoteCache", previous);
				throw failure;
			}
		});
	}

	/** Publish the recovery suffix and conservative in-flight record in one file save. */
	public void prepareTimedVoteDispatch(UUID id, VoteTimeQueue vote,
			java.util.Collection<VoteTimeQueue> pending) {
		mutateData(() -> {
			String path = "UncertainTimedVotes." + id;
			Object previousCache = getData().get("TimedVoteCache");
			Object previousRecord = getData().get(path);
			try {
				setTimedVoteCache(pending);
				getData().set(path, null);
				getData().createSection(path, timedVoteFields(vote));
				saveTimedVoteCache();
			} catch (RuntimeException | Error failure) {
				getData().set("TimedVoteCache", previousCache);
				getData().set(path, previousRecord);
				throw failure;
			}
		});
	}

	/** Restore an undispatched occurrence and its pending suffix for ordinary restart recovery. */
	public void abortTimedVoteDispatch(UUID id, java.util.Collection<VoteTimeQueue> pending) {
		mutateData(() -> {
			String path = "UncertainTimedVotes." + id;
			Object previousCache = getData().get("TimedVoteCache");
			Object previousRecord = getData().get(path);
			try {
				setTimedVoteCache(pending);
				getData().set(path, null);
				saveTimedVoteCache();
			} catch (RuntimeException | Error failure) {
				getData().set("TimedVoteCache", previousCache);
				getData().set(path, previousRecord);
				throw failure;
			}
		});
	}

	/** Clear only a confirmed dispatch record; failed writes retain its live representation. */
	public void completeTimedVoteDispatch(UUID id) {
		mutateData(() -> {
			String path = "UncertainTimedVotes." + id;
			Object previous = getData().get(path);
			try {
				getData().set(path, null);
				saveTimedVoteCache();
			} catch (RuntimeException | Error failure) {
				getData().set(path, previous);
				throw failure;
			}
		});
	}

	private void setTimedVoteCache(java.util.Collection<VoteTimeQueue> pending) {
		java.util.Map<String, Object> rows = new java.util.LinkedHashMap<>();
		int index = 0;
		for (VoteTimeQueue vote : pending) rows.put(Integer.toString(index++), timedVoteFields(vote));
		getData().set("TimedVoteCache", null);
		getData().createSection("TimedVoteCache", rows);
	}

	private static java.util.Map<String, Object> timedVoteFields(VoteTimeQueue vote) {
		java.util.Map<String, Object> fields = new java.util.LinkedHashMap<>();
		fields.put("Name", vote.getName());
		fields.put("Service", vote.getService());
		fields.put("Time", vote.getTime());
		return fields;
	}

	/** Retain an ambiguous event delivery without making it eligible for automatic replay. */
	public void addUncertainTimedVote(java.util.UUID id, VoteTimeQueue vote) {
		mutateData(() -> {
			String path = "UncertainTimedVotes." + id;
			getData().set(path + ".Name", vote.getName());
			getData().set(path + ".Service", vote.getService());
			getData().set(path + ".Time", vote.getTime());
			saveTimedVoteCache();
		});
	}

	/** Timed votes must observe write failures before their in-memory queue is retired. */
	private void saveTimedVoteCache() {
		var fence = getPublicationFence();
		try (var owner = fence == null ? null : fence.acquire()) {
			var file = getFile();
			if (file instanceof GenerationFencedServerDataFile guarded) guarded.saveDataChecked();
			else file.getData().save(file.getdFile());
		} catch (java.io.IOException failure) {
			throw new java.io.UncheckedIOException("Failed to save timed vote cache", failure);
		}
	}

	/** Saves server data through the existing general-purpose file helper. */
	public void saveData() {
		mutateData(() -> {
			// Publish the complete live display projection before entering disk I/O.
			// A failed save still does not acknowledge durable storage.
			captureVotePartyDisplay();
			getFile().saveData();
		});
	}

	/**
	 * Saves the disabled reminders list.
	 *
	 * @param disabledReminders the list of UUIDs with disabled reminders
	 */
	public void saveDisabledReminders(ArrayList<UUID> disabledReminders) {
		ArrayList<String> uuids = new ArrayList<>();
		for (UUID uuid : disabledReminders) uuids.add(uuid.toString());
		setStringList("DisabledReminders", uuids);
	}

	/**
	 * Sets the auto cached placeholders.
	 *
	 * @param placeholders the list of placeholders
	 */
	public void setAutoCachedPlaceholder(List<String> placeholders) {
		setStringList("AutoCachePlaceholders", placeholders);
	}

	/**
	 * Sets the current bungee vote party count.
	 *
	 * @param current the current vote count
	 */
	public void setBungeeVotePartyCurrent(int current) {
		setInt("BungeeVotePartyCurrent", current);
	}

	/**
	 * Sets the required votes for bungee vote party.
	 *
	 * @param required the required vote count
	 */
	public void setBungeeVotePartyRequired(int required) {
		setInt("BungeeVotePartyRequired", required);
	}

	/**
	 * Sets the service sites list.
	 *
	 * @param list the service sites list
	 */
	public void setServiceSites(ArrayList<String> list) {
		setStringList("GottenServiceSites", list);
	}

	/**
	 * Sets the vote shop converted status.
	 *
	 * @param value the converted status
	 */
	public void setShopConverted(boolean value) {
		mutateSavedData(() -> {
			getData().set("VoteShopConverted", value);
		});
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
		var coordinates = SignCoordinates.capture(location);
		var skull = skullLocation == null ? null : SignCoordinates.capture(skullLocation);
		mutateSavedData(() -> {
			coordinates.store(getData(), "Signs." + count);
			getData().set("Signs." + count + ".Data", data);
			getData().set("Signs." + count + ".Position", position);
			if (skull != null) skull.store(getData(), "Signs." + count + ".Skull");
		});
	}

	/**
	 * Sets the skull location for a sign.
	 *
	 * @param count the sign count
	 * @param skullLocation the skull location
	 */
	public void setSkullLocation(String count, Location skullLocation) {
		if (skullLocation == null) return;
		var coordinates = SignCoordinates.capture(skullLocation);
		mutateData(() -> coordinates.store(getData(), "Signs." + count + ".Skull"));
	}

	/**
	 * Sets the version.
	 */
	public void setVersion() {
		String version = Bukkit.getVersion();
		mutateSavedData(() -> {
			getData().set("Version", version);
		});
	}

	/**
	 * Sets the extra required votes for vote party.
	 *
	 * @param value the extra required votes
	 */
	public void setVotePartyExtraRequired(int value) {
		mutateSavedData(() -> {
			getData().set("VotePartyExtraRequired", value);
		});
	}

	/**
	 * Sets the number of purchases for a vote shop item.
	 *
	 * @param ident the vote shop identifier
	 * @param amount the purchase amount
	 */
	public void setVoteShopPurchases(String ident, int amount) {
		mutateSavedData(() -> {
			getData().set("VoteShopPurchases." + ident, amount);
		});
	}

	/**
	 * Updates the last vote party timestamp to today.
	 */
	public void updateLastVoteParty() {
		int day = plugin.getTimeChecker().getTime().getDayOfYear();
		setInt("LastVoteParty", day);
	}

	/**
	 * Updates the last vote party week timestamp to this week.
	 */
	public void updateLastVotePartyWeek() {
		int week = plugin.getTimeChecker().getTime().get(WeekFields.of(Locale.getDefault()).weekOfYear());
		setInt("LastVotePartyWeek", week);
	}

	/**
	 * Updates placeholders to lowercase format.
	 */
	public void updatePlaceholders() {
		mutateData(() -> {
			boolean data = getData().getBoolean("AutoCacheUpdated", false);
			if (!data) {
				mutateSavedData(() -> {
					List<String> placeholders = getAutoCachedPlaceholder();
					for (int i = 0; i < placeholders.size(); i++) {
						placeholders.set(i, placeholders.get(i).toLowerCase());
					}
					getData().set("AutoCachePlaceholders", placeholders);
					getData().set("AutoCacheUpdated", true);
				});
			}
		});
	}

	/**
	 * Update values.
	 */
	public void updateValues() {
		setVersion();
	}
}
