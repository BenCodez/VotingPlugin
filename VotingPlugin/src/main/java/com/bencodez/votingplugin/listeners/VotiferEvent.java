package com.bencodez.votingplugin.listeners;

import java.util.concurrent.RejectedExecutionException;
import java.util.UUID;
import java.util.LinkedHashMap;
import java.util.Map;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.vexsoftware.votifier.model.Vote;
import com.vexsoftware.votifier.model.VotifierEvent;

public class VotiferEvent implements Listener {

	private final VotingPluginMain plugin;
    private final java.util.function.BooleanSupplier ready;
    private volatile boolean accepting = true;
    private static final int MAX_CAPTURED = 256;
    private final Object captureLock = new Object();
    // Receipt ownership only. Entries leave at completed processing or confirmed
    // overflow transfer; STARTED work remains owned until then.
    private final Map<UUID, CapturedVote> captured = new LinkedHashMap<>();
    private static final class CapturedVote {
        final String username, site;
        final long occurredAt;
        final UUID id;
        boolean transferring;
        volatile boolean started, completed, finished, transferred;
        VotifierVoteOverflowQueue recoveryOwner;
        CapturedVote(String username, String site, long occurredAt, UUID id) {
            this.username = username; this.site = site; this.occurredAt = occurredAt; this.id = id;
        }
    }

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public VotiferEvent(VotingPluginMain plugin) { this(plugin, () -> true); }

    public VotiferEvent(VotingPluginMain plugin, java.util.function.BooleanSupplier ready) {
        this.plugin = plugin; this.ready = ready;
    }
    /** Retire admission and reconcile tasks before the vote executor is cancelled. */
    public void stop() {
        synchronized (captureLock) {
            accepting = false;
            for (CapturedVote vote : java.util.List.copyOf(captured.values())) transferLocked(vote);
        }
    }

    public int getPendingCaptureCount() {
        synchronized (captureLock) { return captured.size(); }
    }

    private void runCaptured(CapturedVote vote) {
        synchronized (captureLock) {
            if (!captured.containsKey(vote.id) || vote.transferring || vote.started) return;
            if (!accepting || !ready.getAsBoolean()) { transferLocked(vote); return; }
            vote.started = true;
        }
        boolean completed = false;
        try { completed = processCapturedVote(vote.site, vote.username, vote.occurredAt, vote.id)
                && !Thread.currentThread().isInterrupted(); }
        finally {
            VotifierVoteOverflowQueue acknowledge = null;
            synchronized (captureLock) {
                vote.completed = completed;
                vote.finished = true;
                if (completed) {
                    captured.remove(vote.id);
                    if (vote.transferred) acknowledge = vote.recoveryOwner;
                }
            }
            if (acknowledge != null) acknowledge.acknowledgeOccurrence(vote.id);
            else if (vote.transferred && vote.recoveryOwner != null) vote.recoveryOwner.resumeTransfers();
        }
    }

    private void transferLocked(CapturedVote vote) {
        if (vote.transferring) return;
        vote.transferring = true;
        VotifierVoteOverflowQueue overflow = plugin.getVotifierVoteOverflowQueue();
        vote.recoveryOwner = overflow;
        if (overflow == null) { transferred(vote, false); return; }
        overflow.enqueueAfterInitialization(vote.username, vote.site, vote.occurredAt, vote.id,
                () -> !vote.started || vote.finished && !vote.completed,
                accepted -> transferred(vote, accepted));
    }

    private void transferred(CapturedVote vote, boolean admitted) {
        boolean completed;
        synchronized (captureLock) {
            vote.transferred = admitted; completed = vote.completed;
            if (admitted || completed) captured.remove(vote.id);
        }
        if (admitted && completed) vote.recoveryOwner.acknowledgeOccurrence(vote.id);
        if (completed) return; // Current normal processing won; do not leave an ordinary duplicate retry.
        if (!admitted) plugin.getLogger().severe("Votifier vote queue unavailable or full; vote was not admitted for "
                + MinecraftUsernameValidator.sanitizeForLog(vote.username));
        else plugin.getLogger().warning("Vote pipeline unavailable or retiring; queued Votifier vote for retry");
    }

	/**
	 * Processes a validated vote. The overflow queue invokes this callback from
	 * its worker after the bounded vote executor has capacity again.
	 *
	 * @param voteSite the validated service site
	 * @param voteUsername the validated player name
	 */
	public void processVote(String voteSite, String voteUsername) { processVote(voteSite, voteUsername, 0L); }

    public void processVote(String voteSite, String voteUsername, long occurredAt) {
        processVote(voteSite, voteUsername, occurredAt, UUID.randomUUID());
    }

    public void processVote(String voteSite, String voteUsername, long occurredAt, UUID localOccurrenceId) {
        processCapturedVote(voteSite, voteUsername, occurredAt, localOccurrenceId);
    }

    private boolean processCapturedVote(String voteSite, String voteUsername, long occurredAt, UUID localOccurrenceId) {
        java.util.Objects.requireNonNull(localOccurrenceId, "localOccurrenceId");
		try {
			plugin.getServerData().addServiceSite(voteSite);
			if (plugin.getBungeeSettings().isUseBungeecoord() && !plugin.getBungeeSettings().isVotifierBypass()
					&& (plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.SOCKETS)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.HTTP)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.MQTT)
							|| plugin.getBackendProxyHandler().getMethod().equals(BungeeMethod.REDIS))) {
				plugin.getLogger().severe(
						"Ignoring vote from votifier since a proxy vote transport is enabled; receive votes on the proxy or enable VotifierBypass, then check: https://github.com/BenCodez/VotingPlugin/wiki/Bungeecord-Setups");
				return true;
			}

			String matchSite = "";
			if (plugin.getConfigFile().isAdvancedServiceSiteHandling()) {
				if (plugin.getServiceSiteHandler() != null) {
					matchSite = plugin.getServiceSiteHandler().matchReverse(voteSite);
				}
			}

			String voteSiteNameStr = plugin.getVoteSiteManager().getVoteSiteName(false, voteSite, matchSite);
			boolean createSite = !plugin.getVoteSiteManager().hasVoteSite(voteSiteNameStr)
					&& !plugin.getVoteSiteManager().hasConfiguredVoteSite(voteSiteNameStr);

			String serviceSite = voteSite;

			if (plugin.getConfigFile().isAutoCreateVoteSites() && createSite) {
				plugin.getLogger().warning("VoteSite with service site '" + voteSiteNameStr
						+ "' does not exist, attempting to generate...");
				if (plugin.getConfigVoteSites().tryAutoGenerateVoteSite(voteSiteNameStr)) {
					plugin.getLogger().info("Current known service sites: "
							+ ArrayUtils.makeStringList(plugin.getServerData().getServiceSites()));
				} else {
					plugin.getLogger().warning("Unable to generate VoteSite for service site '"
							+ ServiceSiteValidator.sanitizeForLog(voteSiteNameStr) + "'");
				}
			}

			if (plugin.getTimeChecker().isActiveProcessing()
					&& plugin.getConfigFile().isQueueVotesDuringTimeChange()) {
				plugin.debug("Adding vote to time queue " + voteUsername + "/" + voteSite);
				plugin.getTimeQueueHandler().addVote(voteUsername, voteSite, occurredAt, localOccurrenceId);
				return true;
			}

			String voteSiteName = plugin.getVoteSiteManager().getVoteSiteName(true, serviceSite, matchSite);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSiteManager().getVoteSite(voteSiteName, true), voteUsername, voteSite, true);
            voteEvent.setLocalOccurrenceId(localOccurrenceId);
            if (occurredAt > 0) voteEvent.setCanonicalOccurrenceTime(occurredAt);
			plugin.getServer().getPluginManager().callEvent(voteEvent);

			if (voteEvent.isCancelled()) {
				plugin.debug("Vote cancelled");
			}
            return true;
		} catch (Exception e) {
			plugin.getLogger().severe("Error occured during vote processing");
			e.printStackTrace();
            return false; // An exception does not prove ordinary accounting/rewards completed.
		}
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onVotiferEvent(VotifierEvent event) {
        if (!accepting) return;
        final long occurredAt = System.currentTimeMillis();
        final UUID localOccurrenceId = UUID.randomUUID();

		Vote vote = event.getVote();
		String str = vote.getServiceName();
		if (str == null || str.isEmpty()) {
			str = "Empty";
		}
		final String voteSite = str;
		final String IP = vote.getAddress();
		final String voteUsername = vote.getUsername();
		if (IP.equals("VotingPlugin")) {
			return;
		}
		if (!ServiceSiteValidator.isValid(voteSite)) {
			plugin.getLogger().warning("Rejected vote with invalid service site '"
					+ ServiceSiteValidator.sanitizeForLog(voteSite) + "'");
			return;
		}

		if (!MinecraftUsernameValidator.isValid(voteUsername, plugin.getOptions().getBedrockPlayerPrefix())) {
			plugin.getLogger().warning("Rejected vote with invalid Minecraft username '"
					+ MinecraftUsernameValidator.sanitizeForLog(voteUsername) + "' from service '"
					+ ServiceSiteValidator.sanitizeForLog(voteSite) + "'");
			return;
		}

		plugin.getLogger()
				.info("Received a vote from service site '" + voteSite + "' by player '" + voteUsername + "'!");

		plugin.debug("PlayerUsername: " + voteUsername);
		plugin.debug("VoteSite: " + voteSite);
		plugin.debug("IP: " + IP);

        CapturedVote capturedVote = new CapturedVote(voteUsername, voteSite, occurredAt, localOccurrenceId);
        synchronized (captureLock) {
            if (!accepting) return;
            if (captured.size() >= MAX_CAPTURED) {
                plugin.getLogger().severe("Votifier capture capacity exhausted; vote was not admitted for "
                        + MinecraftUsernameValidator.sanitizeForLog(voteUsername));
                return;
            }
            captured.put(localOccurrenceId, capturedVote);
        }
        try {
            plugin.getVoteTimer().submit(() -> runCaptured(capturedVote));
        } catch (RejectedExecutionException rejected) {
            synchronized (captureLock) { transferLocked(capturedVote); }
        }
    }
}
