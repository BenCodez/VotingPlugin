package com.bencodez.votingplugin.backendproxy.global;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import lombok.Getter;

/**
 * Owns backend global-data polling and proxy-driven time-change processing.
 */
public class BackendGlobalDataSync {

	private final VotingPluginMain plugin;
	private final Consumer<JsonEnvelope> sender;
	private final AtomicBoolean forceUpdateInProgress = new AtomicBoolean(false);
	private final Set<TimeType> timeChangesInProgress = ConcurrentHashMap.newKeySet();

	@Getter
	private GlobalDataHandler globalDataHandler;
	@Getter
	private ScheduledExecutorService timer;

	public BackendGlobalDataSync(VotingPluginMain plugin, Consumer<JsonEnvelope> sender) {
		this.plugin = plugin;
		this.sender = sender;
	}

	public void checkGlobalData() {
		if (globalDataHandler == null) {
			return;
		}
		HashMap<String, DataValue> data = globalDataHandler.getExact(plugin.getBungeeSettings().getServer());

		if (data.containsKey("ForceUpdate") && checkGlobalDataTimeValue(data.get("ForceUpdate"))
				&& forceUpdateInProgress.compareAndSet(false, true)) {
			String serverName = plugin.getBungeeSettings().getServer();
			try {
				if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
					plugin.getMysql().clearCacheBasic();
				}
				plugin.getBukkitScheduler().executeOrScheduleSync(plugin, () -> {
					try {
						plugin.getUserManager().getDataManager().clearCache();
						plugin.setUpdate(true);
						plugin.update();
					} catch (RuntimeException failure) {
						forceUpdateInProgress.set(false);
						plugin.debug(failure);
						return;
					}
					try {
						plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
								() -> clearForceUpdateFlag(serverName));
					} catch (RuntimeException failure) {
						forceUpdateInProgress.set(false);
						plugin.debug(failure);
					}
				});
			} catch (RuntimeException failure) {
				forceUpdateInProgress.set(false);
				plugin.debug(failure);
			}
		}

		checkGlobalDataTime(TimeType.MONTH, data);
		checkGlobalDataTime(TimeType.WEEK, data);
		checkGlobalDataTime(TimeType.DAY, data);
	}

	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		if (!data.containsKey(type.toString()) || !checkGlobalDataTimeValue(data.get(type.toString()))) {
			return false;
		}

		long lastUpdated = Long.valueOf(data.get("LastUpdated").getString()).longValue();
		plugin.debug("LastUpdated: " + lastUpdated);
		if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastUpdated > 1000 * 60 * 60 * 2) {
			plugin.getLogger().warning("Ignoring bungee time change since it was more than 2 hours ago");
			globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), type.toString(), false);
			return false;
		}
		if (!timeChangesInProgress.add(type)) {
			return false;
		}

		String serverName = plugin.getBungeeSettings().getServer();
		globalDataHandler.setBoolean(serverName, "Processing", true);
		plugin.debug("Detected time change from bungee: " + type.toString());
		try {
			plugin.getBukkitScheduler().executeOrScheduleSync(plugin, () -> {
				try {
					plugin.getTimeChecker().forceChanged(type, false, true, true);
				} catch (RuntimeException failure) {
					timeChangesInProgress.remove(type);
					plugin.debug(failure);
					return;
				}
				try {
					plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
							() -> finishTimeChange(type, serverName));
				} catch (RuntimeException failure) {
					timeChangesInProgress.remove(type);
					plugin.debug(failure);
				}
			});
		} catch (RuntimeException failure) {
			timeChangesInProgress.remove(type);
			plugin.debug(failure);
			return false;
		}
		return true;
	}

	private void finishTimeChange(TimeType type, String serverName) {
		boolean completed = false;
		try {
			globalDataHandler.setBoolean(serverName, type.toString(), false);
			JsonEnvelope.Builder builder = JsonEnvelope.builder("TimeChangeFinished")
					.schema(VotingPluginWire.SCHEMA_VERSION);
			builder.put("server", serverName);
			sender.accept(builder.build());
			completed = true;
		} finally {
			timeChangesInProgress.remove(type);
			if (completed && timeChangesInProgress.isEmpty()) {
				HashMap<String, DataValue> dataToSet = new HashMap<>();
				dataToSet.put("FinishedProcessing", new DataValueBoolean(true));
				dataToSet.put("Processing", new DataValueBoolean(false));
				globalDataHandler.setData(serverName, dataToSet);
			}
		}
	}

	public boolean checkGlobalDataTimeValue(DataValue data) {
		if (data.isBoolean()) {
			return data.getBoolean();
		}
		return Boolean.valueOf(data.getString());
	}

	private void clearForceUpdateFlag(String serverName) {
		try {
			globalDataHandler.setBoolean(serverName, "ForceUpdate", false);
		} finally {
			forceUpdateInProgress.set(false);
		}
	}

	public void load() {
		if (!plugin.getBungeeSettings().isGloblalDataEnabled()) {
			return;
		}

		shutdownTimer();
		timer = Executors.newScheduledThreadPool(1);
		timer.scheduleWithFixedDelay(this::checkGlobalData, 60, 10, TimeUnit.SECONDS);
		timer.scheduleWithFixedDelay(() -> {
			if (globalDataHandler != null) {
				globalDataHandler.setString(plugin.getBungeeSettings().getServer(), "LastOnline",
						"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli());
			}
		}, 1, 60, TimeUnit.MINUTES);

		closeGlobalMysql();

		if (plugin.getBungeeSettings().isGloblalDataUseMainMySQL()
				&& plugin.getStorageType().equals(UserStorage.MYSQL)) {
			globalDataHandler = new GlobalDataHandler(new GlobalMySQL("VotingPlugin_GlobalData", plugin.getMysql().getMysql()) {
				@Override public void debugEx(Exception e) { plugin.debug(e); }
				@Override public void debugLog(String text) { plugin.debug(text); }
				@Override public void info(String text) { plugin.getLogger().info(text); }
				@Override public void logSevere(String text) { plugin.getLogger().severe(text); }
				@Override public void warning(String text) { plugin.getLogger().warning(text); }
			});
		} else {
			globalDataHandler = new GlobalDataHandler(new GlobalMySQL("VotingPlugin_GlobalData",
					new MysqlConfigSpigot(plugin.getBungeeSettings().getData().getConfigurationSection("GlobalData"))) {
				@Override public void debugEx(Exception e) { plugin.debug(e); }
				@Override public void debugLog(String text) { plugin.debug(text); }
				@Override public void info(String text) { plugin.getLogger().info(text); }
				@Override public void logSevere(String text) { plugin.getLogger().severe(text); }
				@Override public void warning(String text) { plugin.getLogger().warning(text); }
			});
		}

		for (Map.Entry<String, String> column : Map.of(
				"IgnoreTime", "VARCHAR(5)",
				"MONTH", "VARCHAR(5)",
				"WEEK", "VARCHAR(5)",
				"DAY", "VARCHAR(5)",
				"FinishedProcessing", "VARCHAR(5)",
				"Processing", "VARCHAR(5)",
				"LastUpdated", "MEDIUMTEXT",
				"ForceUpdate", "VARCHAR(5)").entrySet()) {
			globalDataHandler.getGlobalMysql().alterColumnType(column.getKey(), column.getValue());
		}
		plugin.getTimeChecker().setProcessingEnabled(false);
	}

	public void close() {
		shutdownTimer();
		closeGlobalMysql();
	}

	private void closeGlobalMysql() {
		if (globalDataHandler != null) {
			globalDataHandler.getGlobalMysql().close();
		}
	}

	private void shutdownTimer() {
		if (timer == null) {
			return;
		}
		timer.shutdown();
		try {
			timer.awaitTermination(5, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		timer.shutdownNow();
		timer = null;
	}
}
