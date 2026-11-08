package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.UUID;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import com.bencodez.advancedcore.AdvancedCorePlugin.UserStorageOwner;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Owns VoteParty reward delivery and global command fan-out. */
public final class VotePartyRewardHandler {

	private final VotingPluginMain plugin;
	private final VotePartyState state;
	private final VoteParty voteParty;

	public VotePartyRewardHandler(VotingPluginMain plugin, VotePartyState state, VoteParty voteParty) {
		this.plugin = plugin;
		this.state = state;
		this.voteParty = voteParty;
	}

	public void giveReward(VotingPluginUser user, boolean useBungee) {
		if (plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired() > 0
				&& user.getVotePartyVotes() < plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired()) {
			return;
		}
		giveReward(user, user.isOnline(), useBungee);
	}

	public void giveReward(VotingPluginUser user, boolean online, boolean useBungee) {
		givePreparedReward(user, online, useBungee, !plugin.getServerData().isLastVotePartySameDay());
	}

	private void givePreparedReward(VotingPluginUser user, boolean online, boolean useBungee,
			boolean firstVotePartyToday) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVotePartyRewardsPath()).setOnline(online)
				.withPlaceHolder("VotesRequired", "" + plugin.getSpecialRewardsConfig().getVotePartyVotesRequired())
				.withPlaceHolder("FirstVotePartyToday", "" + firstVotePartyToday)
				.setServer(useBungee).send(user);
	}

	public void giveRewards(VotingPluginUser triggeringUser, boolean forceBungee) {
		if (prepareStoredRecipients(ids -> giveRewards(triggeringUser, forceBungee, ids.forDispatch()),
				this::logPreparationFailure)) return;
		giveRewards(triggeringUser, forceBungee, null);
	}

	/** True only for the shared-MySQL path that needs stored recipients. */
	boolean needsStoredPreparation() {
		return sharedDataManager() != null && plugin.getStorageType() == UserStorage.MYSQL
				&& plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers()
				&& !plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly();
	}

	/** Prepare a complete recipient list before any party effects or counter reset. */
	boolean prepareStoredRecipients(Consumer<StoredRecipients> success, Consumer<Throwable> failure) {
		if (!needsStoredPreparation()) return false;
		UserDataManager manager = sharedDataManager();
		AtomicBoolean completed = new AtomicBoolean();
		AtomicReference<UserStorageOwner> capturedOwner = new AtomicReference<>();
		Consumer<List<String>> ready = ids -> {
			if (!completed.compareAndSet(false, true)) return;
			StoredRecipients prepared = new StoredRecipients(manager, capturedOwner.get(), ids, false, 0);
			if (prepared.isCurrent()) success.accept(prepared);
			else failure.accept(new IllegalStateException("Shared VoteParty storage changed after enumeration"));
		};
		Consumer<Throwable> failed = error -> {
			if (completed.compareAndSet(false, true)) failure.accept(error);
		};
		Supplier<List<String>> query = () -> manager.withSharedNativeUserStorage(owner -> {
			if (owner == null || owner.storageType() != UserStorage.MYSQL || owner.mysql() == null) {
				throw new IllegalStateException("Shared VoteParty storage owner is unavailable");
			}
			// The legacy UUID-list API converts this query's null failure to an empty
			// list. Use the existing public nullable query under current-owner admission.
			ArrayList<Column> rows = owner.mysql().getRowsQuery();
			if (rows == null) throw new IllegalStateException("Stored VoteParty recipients could not be loaded");
			ArrayList<String> ids = new ArrayList<>();
			for (Column row : rows) {
				if (row == null || row.getValue() == null || !row.getValue().isString()) {
					throw new IllegalStateException("Stored VoteParty recipient identity is invalid");
				}
				ids.add(UUID.fromString(row.getValue().getString()).toString());
			}
			capturedOwner.set(owner);
			return List.copyOf(ids);
		});
		try {
			// Always queue preparation, including worker-originated votes. A vote
			// can hold its coordinator monitor; no native query belongs in that call.
			if (!manager.deferSharedStorageResultFromPlatform(query, ready, failed)) {
				failed.accept(new IllegalStateException("Shared VoteParty storage changed before admission"));
			}
		} catch (RuntimeException | Error error) {
			failed.accept(error);
		}
		return true;
	}

	/** The owner identity travels with the list through delivery admission and reset. */
	final class StoredRecipients {
		private final UserDataManager manager;
		private final UserStorageOwner owner;
		private final List<String> ids;
		private final boolean firstPartyToday;
		private final int userVotesRequired;

		private StoredRecipients(UserDataManager manager, UserStorageOwner owner, List<String> ids,
				boolean firstPartyToday, int userVotesRequired) {
			this.manager = manager;
			this.owner = owner;
			this.ids = ids;
			this.firstPartyToday = firstPartyToday;
			this.userVotesRequired = userVotesRequired;
		}

		StoredRecipients forDispatch() {
			return new StoredRecipients(manager, owner, ids, !plugin.getServerData().isLastVotePartySameDay(),
					plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired());
		}

		boolean isCurrent() {
			try {
				return owner != null && sharedDataManager() == manager && manager.sharedNativeUserStorageOwner() == owner;
			} catch (RuntimeException unavailable) { return false; }
		}

		<T> T withCurrentOwner(Supplier<T> work) {
			return manager.withSharedNativeUserStorage(current -> {
				if (current != owner) throw new IllegalStateException("Shared VoteParty storage owner changed");
				return work.get();
			});
		}
	}

	void logPreparationFailure(Throwable error) {
		plugin.getLogger().warning("VoteParty not started: stored recipients are unavailable: " + error);
	}

	void giveRewards(VotingPluginUser triggeringUser, boolean forceBungee, StoredRecipients storedRecipients) {
		MiscUtils.getInstance().broadcast(plugin.getSpecialRewardsConfig().getVotePartyBroadcast());
		dispatchGlobalCommands(voteParty.getRandomPlayerName());

		if (plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers()) {
			giveAllPlayers(triggeringUser, forceBungee, storedRecipients);
		} else {
			giveVotedPlayers(triggeringUser, forceBungee);
		}
	}

	public String randomOnlinePlayerName() {
		ArrayList<String> players = new ArrayList<>();
		for (Player player : Bukkit.getOnlinePlayers()) {
			players.add(player.getName());
		}
		if (players.isEmpty()) {
			return "No Player";
		}
		return players.get(ThreadLocalRandom.current().nextInt(players.size()));
	}

	private void dispatchGlobalCommands(String randomPlayer) {
		for (String command : plugin.getSpecialRewardsConfig().getVotePartyGlobalCommands()) {
			plugin.getBukkitScheduler().runTask(plugin,
					() -> Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
							PlaceholderUtils.replacePlaceHolder(command, "randomonlineplayer", randomPlayer)));
		}

		ArrayList<String> randomCommands = plugin.getSpecialRewardsConfig().getVotePartyGlobalRandomCommand();
		if (!randomCommands.isEmpty()) {
			plugin.getBukkitScheduler().runTask(plugin, () -> Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					PlaceholderUtils.replacePlaceHolder(
							randomCommands.get(ThreadLocalRandom.current().nextInt(randomCommands.size())),
							"randomonlineplayer", randomPlayer)));
		}
	}

	/** Dispatch to every eligible user, preserving each online player's scheduler owner. */
	private void giveAllPlayers(VotingPluginUser triggeringUser, boolean forceBungee) {
		if (prepareStoredRecipients(ids -> giveAllPlayers(triggeringUser, forceBungee, ids.forDispatch()),
				this::logPreparationFailure)) return;
		giveAllPlayers(triggeringUser, forceBungee, null);
	}

	private void giveAllPlayers(VotingPluginUser triggeringUser, boolean forceBungee, StoredRecipients prepared) {
		plugin.debug("Trying to give all players vote party");
		ArrayList<String> alreadyGiven = new ArrayList<>();
		for (Player player : Bukkit.getOnlinePlayers()) {
			VotingPluginUser user = resolveUser(triggeringUser, player.getUniqueId(), player);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				if (prepared == null) giveRewardWhenLoaded(user, forceBungee, player);
				else giveRewardWhenLoaded(user, forceBungee, player, prepared.firstPartyToday,
						prepared.userVotesRequired, prepared);
			}
			alreadyGiven.add(player.getUniqueId().toString());
		}

		UserDataManager manager = sharedDataManager();
		if (manager != null && plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly()) {
			// Every live player was considered above. No stored-user query is needed.
			return;
		}
		// The party timestamp is updated as soon as dispatch returns. Carry its
		// decisions into the deferred enumeration rather than reading the next state.
		boolean firstPartyToday = prepared == null ? !plugin.getServerData().isLastVotePartySameDay() : prepared.firstPartyToday;
		int userVotesRequired = prepared == null ? plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired() : prepared.userVotesRequired;
		Runnable recipients = () -> {
			for (String uuid : prepared != null ? prepared.ids : plugin.getVotingPluginUserManager().getAllUUIDs()) {
				if (alreadyGiven.contains(uuid)) {
					continue;
				}
				VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveRewardWhenLoaded(user, forceBungee, null, firstPartyToday, userVotesRequired, prepared);
				}
			}
		};
		Runnable storedRecipients = prepared == null ? recipients : () -> prepared.withCurrentOwner(() -> {
			recipients.run();
			return null;
		});
		// UUID enumeration itself enters storage. Admit it after the online reads
		// and before VoteParty queues its counter wipe on this same ordered worker.
		if (manager != null && manager.deferSharedStorageWork(storedRecipients)) return;
		storedRecipients.run();
	}

	/** Dispatch to users recorded for this party before its voter list is reset. */
	private void giveVotedPlayers(VotingPluginUser triggeringUser, boolean forceBungee) {
		plugin.debug("Trying to give all voted players vote party");
		plugin.debug(ArrayUtils.makeStringList(voteParty.getVotedUsers()));
		for (String uuid : voteParty.getVotedUsers()) {
			VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				// Capture a live player only on a platform-owned thread. The vote path
				// can also originate from a storage worker, where Bukkit lookups are unsafe.
				UserDataManager manager = sharedDataManager();
				Player owner = manager != null && manager.isPlatformOwnedThread()
						? Bukkit.getPlayer(user.getJavaUUID()) : null;
				giveRewardWhenLoaded(user, forceBungee, owner);
			}
		}
	}

	/** Resolve a recipient without forcing an uncached SQL read for shared storage. */
	private VotingPluginUser resolveUser(VotingPluginUser triggeringUser, UUID uuid, Player onlinePlayer) {
		VotingPluginUser user = triggeringUser != null && triggeringUser.getJavaUUID().equals(uuid)
				? triggeringUser
				: onlinePlayer == null
						? plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, true)
						: plugin.getVotingPluginUserManager().getVotingPluginUser(onlinePlayer);
		if (sharedDataManager() != null) {
			// Reward placeholders need the published shared cache, including for the triggering user.
			user.userDataFetechMode(UserDataFetchMode.DEFAULT);
		} else if (user != triggeringUser) {
			// Preserve direct reads for file/SQLite storage.
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		return user;
	}

	/** Returns the active shared SQL manager or null for nonshared storage. */
	private UserDataManager sharedDataManager() {
		UserDataManager manager = plugin.getUserManager().getDataManager();
		return manager != null && manager.hasSharedSqlBackend() ? manager : null;
	}

	/**
	 * Capture shared SQL reward eligibility on AdvancedCore's storage worker.
	 * This must finish before the VoteParty counter wipe is enqueued on that worker.
	 * A caller running on a platform thread must not check even a published cache
	 * directly because checking publication takes the cache's synchronized monitor.
	 */
	private void giveRewardWhenLoaded(VotingPluginUser user, boolean forceBungee, Player callbackOwner) {
		giveRewardWhenLoaded(user, forceBungee, callbackOwner,
				!plugin.getServerData().isLastVotePartySameDay(),
				plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired());
	}

	private void giveRewardWhenLoaded(VotingPluginUser user, boolean forceBungee, Player callbackOwner,
			boolean firstPartyToday, int userVotesRequired) {
		giveRewardWhenLoaded(user, forceBungee, callbackOwner, firstPartyToday, userVotesRequired, null);
	}

	private void giveRewardWhenLoaded(VotingPluginUser user, boolean forceBungee, Player callbackOwner,
			boolean firstPartyToday, int userVotesRequired, StoredRecipients prepared) {
		UserDataManager manager = sharedDataManager();
		if (manager == null) {
			voteParty.giveReward(user, forceBungee);
			return;
		}
		UUID uuid = user.getJavaUUID();

		// A mapped cache entry can still be an *unpublished* placeholder, so
		// cacheUserIfNeeded() alone would not make reward placeholders safe to read.
		Supplier<Integer> read = () -> {
			if (manager.getPublishedCache(uuid) == null) manager.cacheUser(uuid, null);
			if (manager.getPublishedCache(uuid) == null) return null;
			// Read eligibility on the storage worker, before VoteParty resets counters.
			return userVotesRequired > 0 ? user.getVotePartyVotes() : 0;
		};
		Supplier<Integer> populate = prepared == null ? read : () -> prepared.withCurrentOwner(read);
		Consumer<Integer> success = votes -> {
			if (prepared != null && !prepared.isCurrent()) return;
			if (votes == null) {
				plugin.getLogger().warning("VoteParty reward not sent: shared user data is unavailable for " + uuid);
				return;
			}
			if (votes < userVotesRequired) return;

			// Eligibility was captured on the storage worker before reset. Do not
			// re-read the cache on a platform thread or re-check reset counters.
			Runnable deliver = () -> {
				if (prepared == null || prepared.isCurrent())
					givePreparedReward(user, user.isOnline(), forceBungee, firstPartyToday);
			};
			if (callbackOwner != null) {
				deliver.run();
			} else {
				// A worker-originated vote has no captured Player. Its completion is
				// on the global platform scheduler; discover a live owner there.
				Player online = Bukkit.getPlayer(uuid);
				if (online == null) deliver.run();
				else plugin.getBukkitScheduler().runTask(plugin, deliver, online);
			}
		};
		Consumer<Throwable> failure = error -> plugin.getLogger()
				.warning("Failed to load shared user data for VoteParty reward " + uuid + ": " + error);

		try {
			if (manager.deferSharedStorageResult(populate, success, failure, callbackOwner)) return;

			// Called from a non-platform worker: storage reads are safe here, but reward
			// effects still need the Bukkit/Folia scheduler.
			Integer ready = populate.get();
			if (callbackOwner != null) {
				plugin.getBukkitScheduler().runTask(plugin, () -> success.accept(ready), callbackOwner);
			} else {
				plugin.getBukkitScheduler().runTask(plugin, () -> success.accept(ready));
			}
		} catch (RuntimeException | Error error) {
			failure.accept(error);
		}
	}
}
