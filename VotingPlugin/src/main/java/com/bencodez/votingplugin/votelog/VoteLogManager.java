package com.bencodez.votingplugin.votelog;

import java.util.concurrent.TimeUnit;

import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.votingplugin.VotingPluginMain;

/** Owns vote-log table setup and maintenance scheduling. */
public final class VoteLogManager {

	private final VotingPluginMain plugin;
	private VoteLogMysqlTable voteLogMysqlTable;

	public VoteLogManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void load() {
		if (!plugin.getConfigFile().isVoteLoggingEnabled()) {
			plugin.debug("Vote logging MySQL disabled");
			return;
		}

		MysqlConfigSpigot config = new MysqlConfigSpigot(plugin.getConfigFile().getVoteLoggingSection());
		if (plugin.getConfigFile().isVoteLoggingUseMainMySQL()) {
			voteLogMysqlTable = createMainMysqlTable(config);
		} else {
			voteLogMysqlTable = createDedicatedMysqlTable(config);
		}

		plugin.getTimer().scheduleAtFixedRate(
				() -> voteLogMysqlTable.purgeOlderThanDays(plugin.getConfigFile().getVoteLoggingPurgeDays(), 100),
				60, 60 * 60, TimeUnit.SECONDS);
		plugin.debug("Vote logging MySQL enabled");
	}

	public VoteLogMysqlTable getVoteLogMysqlTable() {
		return voteLogMysqlTable;
	}

	private VoteLogMysqlTable createMainMysqlTable(MysqlConfigSpigot config) {
		return new VoteLogMysqlTable("votingplugin_votelog", plugin.getMysql().getMysql(), config,
				plugin.getOptions().getDebug().isDebug()) {
			@Override
			public void logSevere(String string) {
				plugin.getLogger().severe(string);
			}

			@Override
			public void logInfo(String string) {
				plugin.getLogger().info(string);
			}

			@Override
			public String getServerName() {
				return serverName();
			}

			@Override
			public void debug(Throwable throwable) {
				debugThrowable(throwable);
			}
		};
	}

	private VoteLogMysqlTable createDedicatedMysqlTable(MysqlConfigSpigot config) {
		return new VoteLogMysqlTable("votingplugin_votelog", config, plugin.getOptions().getDebug().isDebug()) {
			@Override
			public void logSevere(String string) {
				plugin.getLogger().severe(string);
			}

			@Override
			public void logInfo(String string) {
				plugin.getLogger().info(string);
			}

			@Override
			public String getServerName() {
				return serverName();
			}

			@Override
			public void debug(Throwable throwable) {
				debugThrowable(throwable);
			}
		};
	}

	private String serverName() {
		return plugin.getBungeeSettings().isUseBungeecoord() ? plugin.getBungeeSettings().getServer() : "";
	}

	private void debugThrowable(Throwable throwable) {
		if (plugin.getOptions().getDebug().isDebug()) {
			plugin.debug(throwable);
		}
	}
}
