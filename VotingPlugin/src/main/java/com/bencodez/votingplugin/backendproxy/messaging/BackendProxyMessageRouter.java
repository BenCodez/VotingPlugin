package com.bencodez.votingplugin.backendproxy.messaging;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * Registers and handles backend-side proxy message routes.
 */
public class BackendProxyMessageRouter {

	private final VotingPluginMain plugin;
	private final BackendPresenceManager presenceManager;
	private final BackendGlobalDataSync globalDataSync;
	private final BackendVotePartySync votePartySync;
	private final ProcessedVoteCache processedVoteCache;

	public BackendProxyMessageRouter(VotingPluginMain plugin, BackendPresenceManager presenceManager,
			BackendGlobalDataSync globalDataSync, BackendVotePartySync votePartySync,
			ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.presenceManager = presenceManager;
		this.globalDataSync = globalDataSync;
		this.votePartySync = votePartySync;
		this.processedVoteCache = processedVoteCache;
	}

	public void register(GlobalMessageHandler messages, BungeeMethod method) {
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVote(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_ONLINE) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVote(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_DELAY_REJECTED) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVoteDelayRejected(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_CONTROL_ENROLLMENT_RESULT) {
			@Override public void onReceive(JsonEnvelope msg) { plugin.handleBackendControlEnrollmentResult(msg); }
		});

		if (method.supportsBackendPresence()) {
			messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST) {
				@Override public void onReceive(JsonEnvelope msg) { presenceManager.handleResyncRequest(msg); }
			});
			messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_PRESENCE_SNAPSHOT_REQUEST) {
				@Override public void onReceive(JsonEnvelope msg) { presenceManager.handleSnapshotRequest(msg); }
			});
		}

		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_UPDATE) {
			@Override public void onReceive(JsonEnvelope msg) { handleVoteUpdate(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_BUNGEE_TIME_CHANGE) {
			@Override public void onReceive(JsonEnvelope msg) { globalDataSync.checkGlobalData(); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_BROADCAST) {
			@Override public void onReceive(JsonEnvelope msg) { handleVoteBroadcast(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_STATUS) {
			@Override public void onReceive(JsonEnvelope msg) {
				HashMap<String, Object> out = new HashMap<>();
				out.put(VotingPluginWire.K_SERVER, nvl(plugin.getOptions().getServer()));
				String requestId = nvl(msg.getFields().get(VotingPluginWire.K_REQUEST_ID));
				if (!requestId.isEmpty()) out.put(VotingPluginWire.K_REQUEST_ID, requestId);
				sendSubChannel(messages, VotingPluginWire.SUB_STATUS_OKAY, out);
			}
		});
		messages.addListener(new GlobalMessageListener("ServerName") {
			@Override public void onReceive(JsonEnvelope msg) {
				String server = nvl(msg.getFields().get("server"));
				if (!plugin.getOptions().getServer().equals(server)) {
					plugin.getLogger().warning("Server name doesn't match in BungeeSettings.yml, should be " + server);
				}
			}
		});
		messages.addListener(new GlobalMessageListener("VotePartyBungee") {
			@Override public void onReceive(JsonEnvelope msg) { votePartySync.runGlobalRewards(); }
		});
		messages.addListener(new GlobalMessageListener("VotePartyBroadcast") {
			@Override public void onReceive(JsonEnvelope msg) {
				votePartySync.broadcast(nvl(msg.getFields().get("broadcast")));
			}
		});
	}

	void handleVoteUpdate(JsonEnvelope msg) {
		VotingPluginWire.VoteUpdate update = VotingPluginWire.readVoteUpdate(msg);
		String playerUuid = update.uuid;
		if (playerUuid == null || playerUuid.isEmpty()) {
			return;
		}

		plugin.debug("pluginmessaging voteupdate received for " + playerUuid + ": " + update.votePartyCurrent + "/"
				+ update.votePartyRequired + " on " + update.service);
		votePartySync.update(update.votePartyCurrent, update.votePartyRequired);

		VotingPluginUser user;
		try {
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(playerUuid));
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in VoteUpdate: " + playerUuid);
			return;
		}
		user.cache();
		user.offVote();

		if (update.service != null && !update.service.isEmpty() && update.time > 0) {
			VoteSite voteSite = plugin.getVoteSiteManager().getVoteSite(update.service, true);
			if (voteSite == null) {
				plugin.getLogger().warning("Ignoring VoteUpdate last vote time for unknown service site: "
						+ update.service);
			} else {
				user.setTime(voteSite, update.time);
			}
		} else if (update.service != null && !update.service.isEmpty() && update.time <= 0
				&& plugin.getBungeeSettings().isBungeeDebug()) {
			plugin.debug("Invalid last vote time received from bungee: " + update.time);
		}
		plugin.setUpdate(true);
	}

	private void handleVoteBroadcast(JsonEnvelope msg) {
		Map<String, String> fields = msg.getFields();
		String uuidStr = nvl(fields.get(VotingPluginWire.K_UUID));
		String playerName = nvl(fields.get(VotingPluginWire.K_PLAYER));
		String service = nvl(fields.get(VotingPluginWire.K_SERVICE));
		if (uuidStr.isEmpty() || service.isEmpty()) {
			return;
		}

		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(uuidStr);
		} catch (Exception e) {
			plugin.getLogger().warning("Invalid UUID in VoteBroadcast: " + uuidStr);
			return;
		}

		String totalsRaw = nvl(fields.get(VotingPluginWire.K_TOTALS));
		VoteTotalsSnapshot totals = totalsRaw.isEmpty() ? null : VoteTotalsSnapshot.parseStorage(totalsRaw);
		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '" + service + "'");
			return;
		}
		if (!voteSite.isEnabled()) {
			plugin.debug("Votesite: " + voteSite.getKey() + " is not enabled (VoteBroadcast)");
			return;
		}

		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, playerName);
		user.cache();
		user.updateName(true);
		if (plugin.getBroadcastHandler() == null || user.isVanished()) {
			if (user.isVanished()) {
				plugin.debug("Not broadcasting vote for vanished user: " + user.getPlayerName());
			}
			return;
		}

		boolean online = fields.containsKey(VotingPluginWire.K_WAS_ONLINE)
				? Boolean.parseBoolean(fields.get(VotingPluginWire.K_WAS_ONLINE)) : user.isOnline();
		plugin.getBroadcastHandler().broadcastVote(user.getJavaUUID(), user.getPlayerName(),
				voteSite.getDisplayName(), online, totals);
	}

	private void handleWireVoteDelayRejected(JsonEnvelope msg) {
		if (!validSchema(msg) || !plugin.getOptions().isProcessRewards()) {
			return;
		}
		VotingPluginWire.VoteDelayRejected rejected = VotingPluginWire.readVoteDelayRejected(msg);
		if (rejected.uuid.isEmpty() || rejected.service.isEmpty()) {
			return;
		}
		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(rejected.uuid);
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in VoteDelayRejected: " + rejected.uuid);
			return;
		}
		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, rejected.service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '" + rejected.service + "'");
			return;
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, rejected.player);
		user.cache();
		user.updateName(true);
		voteSite.giveWaitUntilVoteDelayRewards(user, rejected.wasOnline && user.isOnline(), true);
	}

	private void handleWireVote(JsonEnvelope msg) {
		if (!validSchema(msg)) {
			return;
		}
		VotingPluginWire.Vote vote = VotingPluginWire.readVote(msg);
		if (vote.uuid == null || vote.uuid.isEmpty()) {
			return;
		}
		if (!ServiceSiteValidator.isValid(vote.service)) {
			plugin.getLogger().warning("Rejected proxy vote with invalid service site '"
					+ ServiceSiteValidator.sanitizeForLog(vote.service) + "'");
			return;
		}

		plugin.debug("wire vote received from " + vote.player + "/" + vote.uuid + " on " + vote.service);
		VoteTotalsSnapshot totals = VoteTotalsSnapshot.parseStorage(vote.totals == null ? "" : vote.totals);
		@SuppressWarnings("deprecation")
		UUID voteId = vote.voteId != null ? vote.voteId : totals.getVoteUUID();
		if (!processedVoteCache.reserve(voteId)) {
			plugin.debug("Ignoring duplicate wire vote " + voteId + " for " + vote.player + " on " + vote.service);
			return;
		}

		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(vote.uuid);
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in proxy vote: " + vote.uuid);
			return;
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, vote.player);
		votePartySync.replace(totals.getVotePartyCurrent(), totals.getVotePartyRequired());
		user.cache();
		user.bungeeVotePluginMessaging(vote.service, vote.time, totals, !vote.manageTotals,
				vote.wasOnline, vote.broadcast, vote.num);
		if (plugin.getBungeeSettings().isPerServerPoints()) {
			user.addPoints(plugin.getConfigFile().getPointsOnVote());
		}
		if (vote.service != null && !vote.service.isEmpty()) {
			plugin.getServerData().addServiceSite(vote.service);
		}
	}

	private boolean validSchema(JsonEnvelope msg) {
		if (msg.getSchema() == VotingPluginWire.SCHEMA_VERSION) {
			return true;
		}
		plugin.getLogger().warning("Incompatible version with bungee/proxy, please update all servers: "
				+ msg.getSchema() + " != " + VotingPluginWire.SCHEMA_VERSION);
		return false;
	}

	private void sendSubChannel(GlobalMessageHandler messages, String subChannel, HashMap<String, Object> fields) {
		JsonEnvelope.Builder builder = JsonEnvelope.builder(subChannel).schema(VotingPluginWire.SCHEMA_VERSION);
		for (Map.Entry<String, Object> entry : fields.entrySet()) {
			builder.put(entry.getKey(), entry.getValue());
		}
		messages.sendMessage(builder.build());
	}

	private static String nvl(String value) {
		return value == null ? "" : value;
	}
}
