package com.bencodez.votingplugin.signs;

import java.util.ArrayList;

import org.bukkit.Location;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

public class Signs {
	private VotingPluginMain plugin;

	@Getter
	@Setter
	private ArrayList<SignHandler> signs;

	/**
	 * Constructs a new Signs manager.
	 *
	 * @param plugin the main plugin instance
	 */
	public Signs(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.signs = new ArrayList<>();
	}

	/**
	 * Gets the sign from location.
	 *
	 * @param loc the loc
	 * @return the sign from location
	 */
	public String getSignFromLocation(Location loc) {
		for (String sign : plugin.getServerData().getSigns()) {
			if (plugin.getServerData().getSignLocation(sign).equals(loc)) {
				return sign;
			}
		}

		return null;
	}

	/**
	 * Load signs.
	 */
	public void loadSigns() {
		setSigns(new ArrayList<>());
		for (String sign : plugin.getServerData().getSigns()) {
			// plugin.getLogger().info("Loading sign " + sign);
			getSigns().add(new SignHandler(plugin, sign, plugin.getServerData().getSignLocation(sign),
					plugin.getServerData().getSignSkullLocation(sign), plugin.getServerData().getSignData(sign),
					plugin.getServerData().getSignPosition(sign)));
		}
	}

	/**
	 * Store signs.
	 */
	public void storeSigns() {
		for (SignHandler sign : getSigns()) {
			sign.storeSign();
		}
	}

    private final Object shutdownLock = new Object();
    private ShutdownSigns shutdownSigns;
    private java.util.concurrent.CompletableFuture<Void> shutdownAttempt;

    private record ShutdownSigns(com.bencodez.votingplugin.data.ServerData data,
            java.util.List<com.bencodez.votingplugin.data.ServerData.SignSnapshot> snapshot,
            com.bencodez.votingplugin.util.ServerDataPublicationFence.Lease lease) { }

    /** No YAML lock or disk wait on the lifecycle caller; failures retain the original payload. */
    public java.util.concurrent.CompletionStage<Void> storeSignsForShutdown() {
        synchronized (shutdownLock) {
            if (shutdownAttempt != null && !shutdownAttempt.isCompletedExceptionally())
                return shutdownAttempt.minimalCompletionStage();
            if (shutdownSigns == null) {
                var snapshot = new java.util.ArrayList<com.bencodez.votingplugin.data.ServerData.SignSnapshot>();
                for (var sign : java.util.List.copyOf(getSigns())) snapshot.add(sign.captureShutdownSnapshot());
                if (snapshot.isEmpty()) {
                    shutdownAttempt = java.util.concurrent.CompletableFuture.completedFuture(null);
                    return shutdownAttempt.minimalCompletionStage();
                }
                var data = java.util.Objects.requireNonNull(plugin.getServerData());
                var fence = java.util.Objects.requireNonNull(plugin.getServerDataPublicationFence());
                shutdownSigns = new ShutdownSigns(data, java.util.List.copyOf(snapshot), fence.acquire());
            }
            var publication = shutdownSigns;
            var attempt = new java.util.concurrent.CompletableFuture<Void>();
            shutdownAttempt = attempt;
            try {
                startShutdownWorker(() -> {
                    try {
                        java.util.Objects.requireNonNull(publication.data().publishShutdownSigns(publication.snapshot()))
                                .whenComplete((ignored, failure) -> finishShutdownSigns(publication, attempt, failure));
                    } catch (Throwable failure) { finishShutdownSigns(publication, attempt, failure); }
                });
            } catch (RuntimeException | Error failure) {
                finishShutdownSigns(publication, attempt, failure);
                throw failure;
            }
            return attempt.minimalCompletionStage();
        }
    }

    private void finishShutdownSigns(ShutdownSigns publication,
            java.util.concurrent.CompletableFuture<Void> attempt, Throwable failure) {
        if (failure == null) {
            try { publication.lease().close(); attempt.complete(null); }
            catch (Throwable releaseFailure) { attempt.completeExceptionally(releaseFailure); }
        } else {
            // Neither rejected admission nor failed I/O proves durability or writer drain.
            // Keep the same detached payload and lease for an explicit repeated-unload attempt.
            try { plugin.getLogger().warning("Sign shutdown persistence failed; its payload remains owned. Recovery is not confirmed."); }
            catch (RuntimeException | Error ignored) { }
            attempt.completeExceptionally(failure);
        }
    }

    /** Admission seam for deterministic rejection/cancellation tests. */
    protected void startShutdownWorker(Runnable task) {
        var worker = new Thread(task, "VotingPlugin-sign-shutdown");
        worker.setDaemon(true);
        worker.start();
    }

	/**
	 * Update signs.
	 */
	public void updateSigns() {
		int time = 0;
		for (int i = getSigns().size() - 1; i >= 0; i--) {
			if (!getSigns().get(i).isValid()) {
				plugin.debug(
						"Sign " + i + " invalid, removing from data: " + getSigns().get(i).getLocation().toString());
				getSigns().get(i).removeSign();
				getSigns().remove(i);
			} else {
				getSigns().get(i).updateLines();
				getSigns().get(i).updateSign(time);
				time += 1;
			}
		}

		plugin.debug(getSigns().size() + " Signs updated");
	}

}
