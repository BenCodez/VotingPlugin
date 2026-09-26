package com.bencodez.votingplugin.backendproxy.messaging;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;

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
	public enum OrderedVoteOutcome { COMPLETE, RETRY, QUARANTINE }

	private final VotingPluginMain plugin;
	private final BackendPresenceManager presenceManager;
	private final BackendGlobalDataSync globalDataSync;
	private final BackendVotePartySync votePartySync;
	private final ProcessedVoteCache processedVoteCache;
	private GlobalMessageHandler messages;

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
		this.messages = messages;
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVote(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_ONLINE) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVote(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_DELAY_REJECTED) {
			@Override public void onReceive(JsonEnvelope msg) { handleWireVoteDelayRejected(msg); }
		});
		messages.addListener(new GlobalMessageListener(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE) {
			@Override public void onReceive(JsonEnvelope msg) { handleVoteDeliveryReceiptRelease(messages, msg); }
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
				out.put(VotingPluginWire.K_VOTE_DELIVERY_ACK_VERSION,
						VotingPluginWire.VOTE_DELIVERY_ACK_VERSION);
				String requestId = nvl(msg.getFields().get(VotingPluginWire.K_REQUEST_ID));
				if (!requestId.isEmpty()) out.put(VotingPluginWire.K_REQUEST_ID, requestId);
				sendSubChannel(messages, VotingPluginWire.SUB_STATUS_OKAY, out);
			}
		});
		messages.addListener(new GlobalMessageListener("ServerName") {
			@Override public void onReceive(JsonEnvelope msg) {
				String server = nvl(msg.getFields().get("server"));
				if (!plugin.getOptions().getServer().equals(server)) {
					plugin.getLogger().warning("Server name doesn't match in BungeeSettings.yml, should be "
							+ ServiceSiteValidator.sanitizeForLog(server));
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
		handleVoteUpdate(msg, () -> {
		});
	}

	/**
	 * Handles the three ordered vote messages without routing back through the
	 * transport-facing GlobalMessageHandler. Only transient lookup/cache failures
	 * are retried. An exception after possible effects requires quarantine.
	 */
	public void handleOrderedVote(JsonEnvelope msg, Consumer<OrderedVoteOutcome> completion) {
		if (completion == null) throw new IllegalArgumentException("Ordered vote completion is required");
		String subChannel = msg.getSubChannel();
		if (VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(subChannel)) {
			completion.accept(handleVoteDeliveryReceiptRelease(messages, msg));
			return;
		}
		if (VotingPluginWire.SUB_VOTE_UPDATE.equals(subChannel)) {
			handleVoteUpdateWithOutcome(msg, completion);
			return;
		}
		if (VotingPluginWire.SUB_VOTE.equals(subChannel) || VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)) {
			try {
				WireVoteResult result = handleWireVote(msg);
				if (VotingPluginWire.requestsVoteDeliveryAcknowledgement(msg)
						&& (result == null || !result.effectsComplete())) {
					completion.accept(OrderedVoteOutcome.QUARANTINE);
					return;
				}
				UUID completedVoteId = result == null ? null : result.voteId();
				if (VotingPluginWire.requestsVoteDeliveryAcknowledgement(msg)
						&& completedVoteId != null && !processedVoteCache.complete(completedVoteId)) {
					completion.accept(OrderedVoteOutcome.RETRY);
					return;
				}
			} catch (RuntimeException | Error failure) {
				completion.accept(OrderedVoteOutcome.QUARANTINE);
				throw failure;
			}
			completion.accept(OrderedVoteOutcome.COMPLETE);
			return;
		}
		completion.accept(OrderedVoteOutcome.QUARANTINE);
		throw new IllegalArgumentException("Unsupported ordered proxy vote message: " + subChannel);
	}

	/** Returns whether the envelope is a valid release targeted at this backend. */
	public boolean isValidReceiptRelease(JsonEnvelope msg) {
		return validReceiptReleaseVoteId(msg) != null;
	}

	/** Returns whether a valid release already has an acknowledgement-safe durable receipt. */
	public boolean hasDurableReceiptForRelease(JsonEnvelope msg) {
		UUID voteId = validReceiptReleaseVoteId(msg);
		return voteId != null && processedVoteCache.hasDurableReceipt(voteId);
	}

	/**
	 * Processes one ordered VoteUpdate. User identity and shared cache population
	 * are allowed to leave the platform thread, while offline reward/Bukkit work
	 * returns to the platform scheduler before the ordered lane is released.
	 */
	public void handleVoteUpdate(JsonEnvelope msg, Runnable completion) {
		if (completion == null) throw new IllegalArgumentException("VoteUpdate completion is required");
		handleVoteUpdateWithOutcome(msg, ignored -> completion.run());
	}

	private void handleVoteUpdateWithOutcome(JsonEnvelope msg, Consumer<OrderedVoteOutcome> completion) {
		if (completion == null) throw new IllegalArgumentException("VoteUpdate completion is required");
		AtomicBoolean completed = new AtomicBoolean();
		Consumer<OrderedVoteOutcome> complete = outcome -> {
			if (completed.compareAndSet(false, true)) completion.accept(outcome);
		};

		VotingPluginWire.VoteUpdate update;
		try {
			update = VotingPluginWire.readVoteUpdate(msg);
		} catch (RuntimeException | Error failure) {
			complete.accept(OrderedVoteOutcome.QUARANTINE);
			throw failure;
		}
		String playerUuid = update.uuid;
		if (playerUuid == null || playerUuid.isEmpty()) {
			complete.accept(OrderedVoteOutcome.COMPLETE);
			return;
		}

		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> beginVoteUpdateOnPlatform(update, complete));
		} catch (RuntimeException | Error failure) {
			complete.accept(OrderedVoteOutcome.RETRY);
			throw failure;
		}
	}

	private void beginVoteUpdateOnPlatform(VotingPluginWire.VoteUpdate update,
			Consumer<OrderedVoteOutcome> completion) {
		String playerUuid = update.uuid;
		UUID uuid;
		try {
			plugin.debug("pluginmessaging voteupdate received for "
					+ ServiceSiteValidator.sanitizeForLog(playerUuid) + ": " + update.votePartyCurrent + "/"
					+ update.votePartyRequired + " on " + ServiceSiteValidator.sanitizeForLog(update.service));
			votePartySync.update(update.votePartyCurrent, update.votePartyRequired);

			try {
				uuid = UUID.fromString(playerUuid);
			} catch (IllegalArgumentException invalidUuid) {
				plugin.getLogger().warning("Invalid UUID in VoteUpdate: "
						+ ServiceSiteValidator.sanitizeForLog(playerUuid));
				completion.accept(OrderedVoteOutcome.COMPLETE);
				return;
			}
		} catch (RuntimeException | Error failure) {
			completion.accept(OrderedVoteOutcome.QUARANTINE);
			throw failure;
		}
		try {
			plugin.getUserManager().getUserAsync(uuid,
					resolved -> cacheVoteUpdateUser(update, resolved, completion),
					failure -> {
						try {
							plugin.getLogger().warning("Unable to resolve UUID user in VoteUpdate: " + playerUuid);
							plugin.debug(failure);
						} finally {
							completion.accept(OrderedVoteOutcome.RETRY);
						}
					});
		} catch (RuntimeException | Error failure) {
			completion.accept(OrderedVoteOutcome.RETRY);
			throw failure;
		}
	}

	private void cacheVoteUpdateUser(VotingPluginWire.VoteUpdate update, AdvancedCoreUser resolved,
			Consumer<OrderedVoteOutcome> completion) {
		VotingPluginUser user;
		try {
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(resolved);
			UserDataManager dataManager = plugin.getUserManager().getDataManager();
			if (dataManager != null && dataManager.hasSharedSqlBackend()) {
				boolean deferred = dataManager.deferSharedStorageResultFromPlatform(() -> {
					user.cache();
					return Boolean.TRUE;
				}, ignored -> applyVoteUpdate(update, user, completion), failure -> {
					try {
						plugin.getLogger().warning("Unable to cache UUID user in VoteUpdate: " + update.uuid);
						plugin.debug(failure);
					} finally {
						completion.accept(OrderedVoteOutcome.RETRY);
					}
				});
				if (deferred) return;
			}
			user.cache();
			applyVoteUpdate(update, user, completion);
		} catch (RuntimeException | Error failure) {
			completion.accept(OrderedVoteOutcome.RETRY);
			throw failure;
		}
	}

	private void applyVoteUpdate(VotingPluginWire.VoteUpdate update, VotingPluginUser user,
			Consumer<OrderedVoteOutcome> completion) {
		try {
			user.offVote();

			if (update.service != null && !update.service.isEmpty() && update.time > 0) {
				VoteSite voteSite = plugin.getVoteSiteManager().getVoteSite(update.service, true);
				if (voteSite == null) {
					plugin.getLogger().warning("Ignoring VoteUpdate last vote time for unresolved or disabled service site: "
							+ ServiceSiteValidator.sanitizeForLog(update.service));
				} else {
					user.setTime(voteSite, update.time);
				}
			} else if (update.service != null && !update.service.isEmpty() && update.time <= 0
					&& plugin.getBungeeSettings().isBungeeDebug()) {
				plugin.debug("Invalid last vote time received from bungee: " + update.time);
			}
			plugin.setUpdate(true);
		} catch (RuntimeException | Error failure) {
			completion.accept(OrderedVoteOutcome.QUARANTINE);
			throw failure;
		}
		completion.accept(OrderedVoteOutcome.COMPLETE);
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
			plugin.getLogger().warning("Invalid UUID in VoteBroadcast: "
					+ ServiceSiteValidator.sanitizeForLog(uuidStr));
			return;
		}

		String totalsRaw = nvl(fields.get(VotingPluginWire.K_TOTALS));
		VoteTotalsSnapshot totals = totalsRaw.isEmpty() ? null : VoteTotalsSnapshot.parseStorage(totalsRaw);
		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '"
					+ ServiceSiteValidator.sanitizeForLog(service) + "'");
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
			plugin.getLogger().warning("Invalid UUID in VoteDelayRejected: "
					+ ServiceSiteValidator.sanitizeForLog(rejected.uuid));
			return;
		}
		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, rejected.service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '"
					+ ServiceSiteValidator.sanitizeForLog(rejected.service) + "'");
			return;
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, rejected.player);
		user.cache();
		user.updateName(true);
		voteSite.giveWaitUntilVoteDelayRewards(user, rejected.wasOnline && user.isOnline(), true);
	}

	private WireVoteResult handleWireVote(JsonEnvelope msg) {
		if (!validSchema(msg)) {
			return null;
		}
		VotingPluginWire.Vote vote = VotingPluginWire.readVote(msg);
		if (vote.uuid == null || vote.uuid.isEmpty()) {
			return null;
		}
		if (!ServiceSiteValidator.isValid(vote.service)) {
			plugin.getLogger().warning("Rejected proxy vote with invalid service site '"
					+ ServiceSiteValidator.sanitizeForLog(vote.service) + "'");
			return null;
		}

		plugin.debug("wire vote received from " + ServiceSiteValidator.sanitizeForLog(vote.player) + "/"
				+ ServiceSiteValidator.sanitizeForLog(vote.uuid) + " on "
				+ ServiceSiteValidator.sanitizeForLog(vote.service));
		VoteTotalsSnapshot totals = VoteTotalsSnapshot.parseStorage(vote.totals == null ? "" : vote.totals);
		@SuppressWarnings("deprecation")
		UUID voteId = vote.voteId != null ? vote.voteId : totals.getVoteUUID();
		if (!processedVoteCache.reserve(voteId)) {
			plugin.debug("Ignoring duplicate wire vote " + voteId + " for "
					+ ServiceSiteValidator.sanitizeForLog(vote.player) + " on "
					+ ServiceSiteValidator.sanitizeForLog(vote.service));
			return new WireVoteResult(voteId, processedVoteCache.hasCompletedEffects(voteId));
		}

		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(vote.uuid);
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in proxy vote: "
					+ ServiceSiteValidator.sanitizeForLog(vote.uuid));
			return new WireVoteResult(voteId, false);
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
		return new WireVoteResult(voteId, true);
	}

	private record WireVoteResult(UUID voteId, boolean effectsComplete) {
	}

	private boolean validSchema(JsonEnvelope msg) {
		if (msg.getSchema() == VotingPluginWire.SCHEMA_VERSION) {
			return true;
		}
		plugin.getLogger().warning("Incompatible version with bungee/proxy, please update all servers: "
				+ msg.getSchema() + " != " + VotingPluginWire.SCHEMA_VERSION);
		return false;
	}

	private OrderedVoteOutcome handleVoteDeliveryReceiptRelease(GlobalMessageHandler messages, JsonEnvelope msg) {
		if (messages == null) return OrderedVoteOutcome.QUARANTINE;
		UUID voteId = validReceiptReleaseVoteId(msg);
		if (voteId == null) return OrderedVoteOutcome.QUARANTINE;
		String subChannel = nvl(msg.getFields().get(VotingPluginWire.K_VOTE_DELIVERY_SUBCHANNEL));
		try {
			if (processedVoteCache.releaseCompletedReceipt(voteId)) {
				messages.sendMessage(VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(
						plugin.getOptions().getServer(), voteId, subChannel));
				return OrderedVoteOutcome.COMPLETE;
			}
			return OrderedVoteOutcome.RETRY;
		} catch (IllegalArgumentException invalidVoteId) {
			plugin.debug("Ignored vote receipt release with invalid vote ID");
			return OrderedVoteOutcome.QUARANTINE;
		}
	}

	private UUID validReceiptReleaseVoteId(JsonEnvelope msg) {
		if (msg == null || !VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE.equals(msg.getSubChannel())
				|| !VotingPluginWire.requestsVoteDeliveryAcknowledgement(msg)) return null;
		String server = nvl(msg.getFields().get(VotingPluginWire.K_SERVER));
		if (!plugin.getOptions().getServer().equalsIgnoreCase(server)) return null;
		String subChannel = nvl(msg.getFields().get(VotingPluginWire.K_VOTE_DELIVERY_SUBCHANNEL));
		if (!VotingPluginWire.SUB_VOTE.equals(subChannel)
				&& !VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)) return null;
		try {
			return UUID.fromString(nvl(msg.getFields().get(VotingPluginWire.K_VOTE_ID)));
		} catch (IllegalArgumentException invalidVoteId) {
			return null;
		}
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
