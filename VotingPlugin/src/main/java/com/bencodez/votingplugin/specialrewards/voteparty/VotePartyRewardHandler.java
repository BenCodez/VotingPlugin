package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
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
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVotePartyRewardsPath()).setOnline(online)
				.withPlaceHolder("VotesRequired", "" + plugin.getSpecialRewardsConfig().getVotePartyVotesRequired())
				.withPlaceHolder("FirstVotePartyToday", "" + !plugin.getServerData().isLastVotePartySameDay())
				.setServer(useBungee).send(user);
	}

	public void giveRewards(VotingPluginUser triggeringUser, boolean forceBungee) {
		MiscUtils.getInstance().broadcast(plugin.getSpecialRewardsConfig().getVotePartyBroadcast());
		dispatchGlobalCommands(voteParty.getRandomPlayerName());

		if (plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers()) {
			giveAllPlayers(triggeringUser, forceBungee);
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

	private void giveAllPlayers(VotingPluginUser triggeringUser, boolean forceBungee) {
		plugin.debug("Trying to give all players vote party");
		ArrayList<String> alreadyGiven = new ArrayList<>();
		for (Player player : Bukkit.getOnlinePlayers()) {
			VotingPluginUser user = resolveUser(triggeringUser, player.getUniqueId(), player);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				giveRewardWhenLoaded(user, forceBungee, player);
			}
			alreadyGiven.add(player.getUniqueId().toString());
		}

		for (String uuid : plugin.getVotingPluginUserManager().getAllUUIDs()) {
			if (alreadyGiven.contains(uuid)) {
				continue;
			}
			VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				giveRewardWhenLoaded(user, forceBungee, null);
			}
		}
	}

	private void giveVotedPlayers(VotingPluginUser triggeringUser, boolean forceBungee) {
		plugin.debug("Trying to give all voted players vote party");
		plugin.debug(ArrayUtils.makeStringList(voteParty.getVotedUsers()));
		for (String uuid : voteParty.getVotedUsers()) {
			VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				giveRewardWhenLoaded(user, forceBungee, Bukkit.getPlayer(user.getJavaUUID()));
			}
		}
	}

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

	private UserDataManager sharedDataManager() {
		UserDataManager manager = plugin.getUserManager().getDataManager();
		return manager != null && manager.hasSharedSqlBackend() ? manager : null;
	}

	private void giveRewardWhenLoaded(VotingPluginUser user, boolean forceBungee, Player callbackOwner) {
		UserDataManager manager = sharedDataManager();
		if (manager == null) {
			voteParty.giveReward(user, forceBungee);
			return;
		}
		UUID uuid = user.getJavaUUID();
		if (manager.getPublishedCache(uuid) != null) {
			voteParty.giveReward(user, forceBungee);
			return;
		}

		// A cache entry may be an *unpublished* placeholder. cacheUserIfNeeded() only
		// checks map membership and would incorrectly treat that placeholder as ready.
		Supplier<Boolean> populate = () -> {
			if (manager.getPublishedCache(uuid) == null) manager.cacheUser(uuid, null);
			return manager.getPublishedCache(uuid) != null;
		};
		Consumer<Boolean> success = ready -> {
			if (!Boolean.TRUE.equals(ready) || manager.getPublishedCache(uuid) == null) {
				plugin.getLogger().warning("VoteParty reward not sent: shared user data is unavailable for " + uuid);
				return;
			}
			voteParty.giveReward(user, forceBungee);
		};
		Consumer<Throwable> failure = error -> plugin.getLogger()
				.warning("Failed to load shared user data for VoteParty reward " + uuid + ": " + error);

		try {
			if (manager.deferSharedStorageResult(populate, success, failure, callbackOwner)) return;

			// Called from a non-platform worker: storage reads are safe here, but reward
			// effects still need the Bukkit/Folia scheduler.
			Boolean ready = populate.get();
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
