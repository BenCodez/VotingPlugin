package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
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
				voteParty.giveReward(user, forceBungee);
			}
			alreadyGiven.add(player.getUniqueId().toString());
		}

		for (String uuid : plugin.getVotingPluginUserManager().getAllUUIDs()) {
			if (alreadyGiven.contains(uuid)) {
				continue;
			}
			VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				voteParty.giveReward(user, forceBungee);
			}
		}
	}

	private void giveVotedPlayers(VotingPluginUser triggeringUser, boolean forceBungee) {
		plugin.debug("Trying to give all voted players vote party");
		plugin.debug(ArrayUtils.makeStringList(voteParty.getVotedUsers()));
		for (String uuid : voteParty.getVotedUsers()) {
			VotingPluginUser user = resolveUser(triggeringUser, UUID.fromString(uuid), null);
			if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
				voteParty.giveReward(user, forceBungee);
			}
		}
	}

	private VotingPluginUser resolveUser(VotingPluginUser triggeringUser, UUID uuid, Player onlinePlayer) {
		if (triggeringUser != null && triggeringUser.getJavaUUID().equals(uuid)) {
			return triggeringUser;
		}
		VotingPluginUser user = onlinePlayer == null
				? plugin.getVotingPluginUserManager().getVotingPluginUser(uuid)
				: plugin.getVotingPluginUserManager().getVotingPluginUser(onlinePlayer);
		user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		return user;
	}
}
