package com.bencodez.votingplugin.backendproxy.messaging;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageListener;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache.Reservation;
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
	private final AtomicBoolean voteReplayCacheSaturationLogged = new AtomicBoolean();
	private final Object pendingVoteUpdateLock = new Object();
	private final Set<PendingVoteUpdateHandoff> pendingVoteUpdateHandoffs = new HashSet<>();
	private boolean pendingVoteUpdateHandoffsClosing;
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
				out.put(VotingPluginWire.K_VOTE_DELAY_REJECTION_ACK_VERSION,
						VotingPluginWire.VOTE_DELAY_REJECTION_ACK_VERSION);
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
		if (VotingPluginWire.SUB_VOTE_DELAY_REJECTED.equals(subChannel)) {
			try {
				WireVoteResult result = handleWireVoteDelayRejected(msg);
				if (result != null && result.retryable()) {
					completion.accept(OrderedVoteOutcome.RETRY);
					return;
				}
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
		if (VotingPluginWire.SUB_VOTE.equals(subChannel) || VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)) {
			try {
				WireVoteResult result = handleWireVote(msg);
				if (result != null && result.retryable()) {
					completion.accept(OrderedVoteOutcome.RETRY);
					return;
				}
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

		// Track admission until execution starts; ordinary lag is not cancellation.
		PendingVoteUpdateHandoff handoff = trackVoteUpdateHandoff(complete);
		if (handoff.isClaimed()) return;
		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> {
				if (handoff.begin()) beginVoteUpdateOnPlatform(update, complete);
			});
		} catch (RuntimeException | Error failure) {
			handoff.cancel();
			throw failure;
		}
	}

	/** Cancel only unstarted VoteUpdates when the ordered lane retires. */
	public void cancelPendingVoteUpdateHandoffs() {
		Set<PendingVoteUpdateHandoff> waiting;
		synchronized (pendingVoteUpdateLock) {
			pendingVoteUpdateHandoffsClosing = true;
			waiting = new HashSet<>(pendingVoteUpdateHandoffs);
		}
		// Completion can acquire the ordered-lane lock; never hold ours here.
		for (PendingVoteUpdateHandoff handoff : waiting) handoff.cancel();
	}

	/** Resume admission when a staged replacement is rolled back. */
	public void resumeVoteUpdateHandoffs() {
		synchronized (pendingVoteUpdateLock) {
			pendingVoteUpdateHandoffsClosing = false;
		}
	}

	private PendingVoteUpdateHandoff trackVoteUpdateHandoff(Consumer<OrderedVoteOutcome> completion) {
		PendingVoteUpdateHandoff handoff = new PendingVoteUpdateHandoff(completion);
		boolean rejected;
		synchronized (pendingVoteUpdateLock) {
			rejected = pendingVoteUpdateHandoffsClosing;
			if (!rejected) pendingVoteUpdateHandoffs.add(handoff);
		}
		if (rejected) handoff.cancel();
		return handoff;
	}

	private final class PendingVoteUpdateHandoff {
		private final AtomicBoolean claimed = new AtomicBoolean();
		private final Consumer<OrderedVoteOutcome> completion;

		private PendingVoteUpdateHandoff(Consumer<OrderedVoteOutcome> completion) {
			this.completion = completion;
		}

		private boolean isClaimed() { return claimed.get(); }

		private boolean begin() {
			if (!claimed.compareAndSet(false, true)) return false;
			synchronized (pendingVoteUpdateLock) {
				pendingVoteUpdateHandoffs.remove(this);
			}
			return true;
		}

		private void cancel() {
			if (!claimed.compareAndSet(false, true)) return;
			synchronized (pendingVoteUpdateLock) {
				pendingVoteUpdateHandoffs.remove(this);
			}
			completion.accept(OrderedVoteOutcome.RETRY);
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
		try {
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(resolved);
			UserDataManager dataManager = plugin.getUserManager().getDataManager();
			// Site resolution may auto-create YAML and reload reward/site registries.
			// Capture it once on the platform scheduler before entering the SQL worker.
			VoteSite voteSite = resolveVoteUpdateSite(update);
			// Capture player state on its owning thread before entering storage.
			// A retired entity must retry; only a genuinely absent player may skip offline rewards.
			// Match offVote(): offline-mode identities are looked up by name,
			// not by their stored UUID. Permission reads still use the entity owner.
			org.bukkit.entity.Player player = plugin.getOptions().isOnlineMode()
					? user.getPlayer()
					: org.bukkit.Bukkit.getPlayer(user.getPlayerName());
			if (player == null) {
				deferVoteUpdate(update, user, dataManager, voteSite, false, false, completion);
			} else {
				// A cancelled callback retries only on explicit lifecycle retirement.
				PendingVoteUpdateHandoff handoff = trackVoteUpdateHandoff(completion);
				if (handoff.isClaimed()) return;
				com.bencodez.votingplugin.util.BukkitCompletionScheduler.run(plugin, player, () -> {
					if (!handoff.begin()) return;
					try {
						boolean online = player.isOnline();
						deferVoteUpdate(update, user, dataManager, voteSite, online,
								online && player.hasPermission("VotingPlugin.TopVoter.Ignore"), completion);
					} catch (RuntimeException | Error failure) {
						completion.accept(OrderedVoteOutcome.RETRY);
						throw failure;
					}
				}, handoff::cancel, handoff::cancel);
			}
		} catch (RuntimeException | Error failure) {
			completion.accept(OrderedVoteOutcome.RETRY);
			throw failure;
		}
	}

	/**
	 * The cache must be populated and LastVotes read/updated on the same storage
	 * worker. A platform callback between those steps permits another storage
	 * operation to retire the published cache and makes setTime() fail on Bukkit.
	 */
	private void deferSharedVoteUpdate(VotingPluginWire.VoteUpdate update, VotingPluginUser user,
			UserDataManager dataManager, VoteSite voteSite, boolean processOfflineVotes, boolean topVoterIgnore,
			Consumer<OrderedVoteOutcome> completion) {
		AtomicBoolean effectsMayHaveStarted = new AtomicBoolean();
		try {
			boolean deferred = dataManager.deferSharedStorageResultFromPlatform(() -> {
				user.cache();
				CompletionStage<Void> rewards = CompletableFuture.completedFuture(null);
				if (processOfflineVotes) {
					// Persistent reads and cache publication may fail before any
					// reward begins. The async user API signals only the first
					// potentially nontransactional reward/pending grant boundary.
					rewards = user.offVoteWithCapturedTopVoterIgnoreAsync(topVoterIgnore,
							() -> effectsMayHaveStarted.set(true));
					if (rewards == null) throw new IllegalStateException("Offline vote reward chain was null");
				}
				applyVoteUpdateTime(update, user, voteSite, () -> effectsMayHaveStarted.set(true));
				return rewards;
			}, rewards -> {
				if (rewards == null) {
					completion.accept(OrderedVoteOutcome.QUARANTINE);
					return;
				}
				// This stage may finish on an injection/region thread. Only the
				// ordered result and update flag return to the platform scheduler.
				rewards.whenComplete((ignored, rewardFailure) -> {
					if (rewardFailure != null) {
						plugin.getLogger().warning("Unable to finish offline rewards for UUID VoteUpdate: "
								+ ServiceSiteValidator.sanitizeForLog(update.uuid));
						plugin.debug(rewardFailure);
						// The user API marks any ambiguous pending/effect boundary.
						completion.accept(effectsMayHaveStarted.get()
								? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
						return;
					}
					try {
						dataManager.dispatchSharedStorageNotification(() -> {
							try {
								plugin.setUpdate(true);
								completion.accept(OrderedVoteOutcome.COMPLETE);
							} catch (RuntimeException | Error failure) {
								completion.accept(OrderedVoteOutcome.QUARANTINE);
								throw failure;
							}
						});
					} catch (RuntimeException | Error failure) {
						plugin.debug(failure);
						completion.accept(OrderedVoteOutcome.QUARANTINE);
					}
				});
			}, failure -> {
				try {
					plugin.getLogger().warning("Unable to apply UUID user VoteUpdate: "
							+ ServiceSiteValidator.sanitizeForLog(update.uuid));
					plugin.debug(failure);
				} finally {
					// Pre-effect read/cache failures retry; possible external
					// effects or a vote-time write require quarantine.
					completion.accept(effectsMayHaveStarted.get()
							? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
				}
			});
			if (!deferred) completion.accept(OrderedVoteOutcome.RETRY);
		} catch (RuntimeException | Error failure) {
			completion.accept(effectsMayHaveStarted.get()
					? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
			throw failure;
		}
	}

	/** May mutate VoteSites.yml when auto-create is enabled; never call on the SQL worker. */
	private VoteSite resolveVoteUpdateSite(VotingPluginWire.VoteUpdate update) {
		if (update.service != null && !update.service.isEmpty() && update.time > 0) {
			return plugin.getVoteSiteManager().getVoteSite(update.service, true);
		}
		return null;
	}

	private void applyVoteUpdateTime(VotingPluginWire.VoteUpdate update, VotingPluginUser user,
			VoteSite voteSite, Runnable beforeTimeWrite) {
		if (update.service != null && !update.service.isEmpty() && update.time > 0) {
			if (voteSite == null) {
				plugin.getLogger().warning("Ignoring VoteUpdate last vote time for unresolved or disabled service site: "
						+ ServiceSiteValidator.sanitizeForLog(update.service));
			} else {
				// Lookups and validation may fail without any effect to replay.
				beforeTimeWrite.run();
				user.setTime(voteSite, update.time);
			}
		} else if (update.service != null && !update.service.isEmpty() && update.time <= 0
				&& plugin.getBungeeSettings().isBungeeDebug()) {
			plugin.debug("Invalid last vote time received from bungee: " + update.time);
		}
	}

	private void deferVoteUpdate(VotingPluginWire.VoteUpdate update, VotingPluginUser user,
			UserDataManager dataManager, VoteSite site, boolean online, boolean topVoterIgnore,
			Consumer<OrderedVoteOutcome> completion) {
		if (dataManager.hasSharedSqlBackend()) {
			deferSharedVoteUpdate(update, user, dataManager, site, online, topVoterIgnore, completion);
			return;
		}
		AtomicBoolean effectsMayHaveStarted = new AtomicBoolean();
		try {
			dataManager.getTimer().execute(() -> {
				try {
					user.cache();
					CompletionStage<Void> replay = online
							? user.offVoteWithCapturedTopVoterIgnoreAsync(topVoterIgnore,
									() -> effectsMayHaveStarted.set(true))
							: CompletableFuture.completedFuture(null);
					if (replay == null) throw new IllegalStateException("Missing offline replay completion");
					replay.whenComplete((ignored, failure) -> {
						if (failure != null) {
							plugin.debug(failure);
							completion.accept(effectsMayHaveStarted.get()
									? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
							return;
						}
						try {
							// Even an empty replay can finish on an owner callback.
							dataManager.getTimer().execute(() -> {
								try {
									user.cache();
									applyVoteUpdateTime(update, user, site, () -> effectsMayHaveStarted.set(true));
									dataManager.dispatchSharedStorageNotification(() -> {
										try {
											plugin.setUpdate(true);
											completion.accept(OrderedVoteOutcome.COMPLETE);
										} catch (RuntimeException | Error notificationFailure) {
											plugin.debug(notificationFailure);
											completion.accept(OrderedVoteOutcome.QUARANTINE);
										}
									});
								} catch (RuntimeException | Error applyFailure) {
									plugin.debug(applyFailure);
									completion.accept(effectsMayHaveStarted.get()
											? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
								}
							});
						} catch (RuntimeException | Error rejected) {
							plugin.debug(rejected);
							completion.accept(effectsMayHaveStarted.get()
									? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
						}
					});
				} catch (RuntimeException | Error failure) {
					plugin.debug(failure);
					completion.accept(effectsMayHaveStarted.get()
							? OrderedVoteOutcome.QUARANTINE : OrderedVoteOutcome.RETRY);
				}
			});
		} catch (RuntimeException | Error rejected) {
			plugin.debug(rejected);
			completion.accept(OrderedVoteOutcome.RETRY);
		}
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
				voteSite.getDisplayNameForFormatting(), online, totals);
	}

	private WireVoteResult handleWireVoteDelayRejected(JsonEnvelope msg) {
		if (!validSchema(msg)) return null;
		VotingPluginWire.VoteDelayRejected rejected = VotingPluginWire.readVoteDelayRejected(msg);
		boolean reliable = VotingPluginWire.requestsVoteDeliveryAcknowledgement(msg);
		if (reliable && rejected.voteId == null) {
			plugin.getLogger().warning("Rejected VoteDelayRejected without a valid vote ID from a capable proxy");
			return null;
		}
		if (!plugin.getOptions().isProcessRewards()) {
			return new WireVoteResult(rejected.voteId, true);
		}
		if (rejected.uuid.isEmpty() || rejected.service.isEmpty()) {
			return new WireVoteResult(rejected.voteId, true);
		}
		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(rejected.uuid);
		} catch (IllegalArgumentException e) {
			plugin.getLogger().warning("Invalid UUID in VoteDelayRejected: "
					+ ServiceSiteValidator.sanitizeForLog(rejected.uuid));
			return new WireVoteResult(rejected.voteId, true);
		}
		VoteSite voteSite = plugin.getVoteSiteManager()
				.getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, rejected.service), true);
		if (voteSite == null) {
			plugin.getLogger().warning("No voting site with the service site: '"
					+ ServiceSiteValidator.sanitizeForLog(rejected.service) + "'");
			return new WireVoteResult(rejected.voteId, true);
		}
		if (!voteSite.isEnabled() || !voteSite.isWaitUntilVoteDelay()) {
			return new WireVoteResult(rejected.voteId, true);
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, rejected.player);
		user.cache();
		user.updateName(true);
		if (reliable && user.canVoteSite(voteSite)) {
			return new WireVoteResult(rejected.voteId, true);
		}
		if (rejected.voteId != null) {
			Reservation reservation = processedVoteCache.reserveWithOutcome(rejected.voteId);
			if (reservation == Reservation.SATURATED) {
				return new WireVoteResult(rejected.voteId, false, true);
			}
			if (reservation == Reservation.DUPLICATE) {
				return new WireVoteResult(rejected.voteId, processedVoteCache.hasCompletedEffects(rejected.voteId));
			}
		}
		voteSite.giveWaitUntilVoteDelayRewards(user, rejected.wasOnline && user.isOnline(), true);
		return new WireVoteResult(rejected.voteId, true);
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
		Reservation reservation = processedVoteCache.reserveWithOutcome(voteId);
		if (reservation == Reservation.SATURATED) {
			if (voteReplayCacheSaturationLogged.compareAndSet(false, true)) {
				plugin.getLogger().warning("Backend vote replay cache is full; retaining votes for retry");
			}
			return new WireVoteResult(voteId, false, true);
		}
		if (reservation == Reservation.DUPLICATE) {
			plugin.debug("Ignoring duplicate wire vote " + voteId + " for "
					+ ServiceSiteValidator.sanitizeForLog(vote.player) + " on "
					+ ServiceSiteValidator.sanitizeForLog(vote.service));
			return new WireVoteResult(voteId, processedVoteCache.hasCompletedEffects(voteId), false);
		}
		voteReplayCacheSaturationLogged.set(false);

		UUID javaUuid;
		try {
			javaUuid = UUID.fromString(vote.uuid);
		} catch (IllegalArgumentException e) {
			processedVoteCache.cancelReservation(voteId);
			plugin.getLogger().warning("Invalid UUID in proxy vote: "
					+ ServiceSiteValidator.sanitizeForLog(vote.uuid));
			return new WireVoteResult(voteId, false, false);
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(javaUuid, vote.player);
		votePartySync.replace(totals.getVotePartyCurrent(), totals.getVotePartyRequired());
		user.cache();
		boolean wasOnline = vote.wasOnlineKnown ? vote.wasOnline : user.isOnline();
		boolean queuedDelivery = vote.queuedDeliveryKnown ? vote.queuedDelivery : vote.delayValidated;
		// Even an immediate proxy send can arrive after a guide opened on this backend.
		// No cross-node ordering handshake exists, so transport freshness flags cannot
		// establish original occurrence order. This affects only guide confirmation.
		user.bungeeVotePluginMessaging(vote.service, vote.time, totals, !vote.manageTotals,
				wasOnline, vote.broadcast, vote.num, queuedDelivery, vote.delayValidationKnown,
				vote.queuedDeliveryKnown, voteId,
				VotingPluginWire.SUB_VOTE_ONLINE.equals(msg.getSubChannel()),
				true);
		if (plugin.getBungeeSettings().isPerServerPoints()) {
			user.addPoints(plugin.getConfigFile().getPointsOnVote());
		}
		if (vote.service != null && !vote.service.isEmpty()) {
			plugin.getServerData().addServiceSite(vote.service);
		}
		return new WireVoteResult(voteId, true, false);
	}

	private record WireVoteResult(UUID voteId, boolean effectsComplete, boolean retryable) {
		private WireVoteResult(UUID voteId, boolean effectsComplete) {
			this(voteId, effectsComplete, false);
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
				&& !VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)
				&& !VotingPluginWire.SUB_VOTE_DELAY_REJECTED.equals(subChannel)) return null;
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
