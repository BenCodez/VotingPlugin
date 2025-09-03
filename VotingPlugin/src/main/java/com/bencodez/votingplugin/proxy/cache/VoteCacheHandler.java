package com.bencodez.votingplugin.proxy.cache;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.votingplugin.proxy.BungeeMessageData;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

import lombok.Getter;
import lombok.Setter;

public abstract class VoteCacheHandler {

	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	@Getter
	@Setter
	// uuid based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	@Getter
	@Setter
	// server based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new ConcurrentHashMap<>();

	public void resetMilestoneCountInVotes() {
		// Iterate through cached online votes
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				String updatedText = updateMilestoneCount(vote.getText());
				vote.setText(updatedText);
			}
		}

		// Iterate through cached votes
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				String updatedText = updateMilestoneCount(vote.getText());
				vote.setText(updatedText);
			}
		}
	}

	private String updateMilestoneCount(String text) {
		BungeeMessageData data = new BungeeMessageData(text);
		data.setMilestoneCount(0); // Reset milestone count to 0
		return data.toString();
	}

	public void saveVoteCache() {
		if (useMySQL) {
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getCachedVotes().entrySet()) {
				String server = entry.getKey();
				for (OfflineBungeeVote voteData : entry.getValue()) {
					voteCacheTable.insertVote(voteData.getUuid(), voteData.getPlayerName(), voteData.getService(),
							voteData.getTime(), voteData.isRealVote(), voteData.getText(), server);

				}
			}
			for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getCachedOnlineVotes().entrySet()) {
				for (OfflineBungeeVote voteData : entry.getValue()) {
					onlineVoteCacheTable.insertOnlineVote(voteData.getUuid(), voteData.getPlayerName(),
							voteData.getService(), voteData.getTime(), voteData.isRealVote(), voteData.getText());
				}
			}

			if (!getTimeChangeQueue().isEmpty()) {
				for (VoteTimeQueue vote : getTimeChangeQueue()) {
					timedVoteCacheTable.insertTimedVote(vote.getName(), vote.getService(), vote.getTime());
				}
			}
		} else {
			if (!getCachedVotes().isEmpty()) {
				for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getCachedVotes().entrySet()) {
					String server = entry.getKey();
					int num = 0;
					for (OfflineBungeeVote voteData : entry.getValue()) {
						jsonStorage.addVote(server, num, voteData);
						num++;
					}
				}
			}

			if (!getCachedOnlineVotes().isEmpty()) {
				for (Entry<String, ArrayList<OfflineBungeeVote>> entry : getCachedOnlineVotes().entrySet()) {
					String name = entry.getKey();
					int num = 0;
					for (OfflineBungeeVote voteData : entry.getValue()) {
						jsonStorage.addVoteOnline(name, num, voteData);
						num++;
					}
				}
			}

			if (!getTimeChangeQueue().isEmpty()) {
				int num = 0;
				for (VoteTimeQueue vote : getTimeChangeQueue()) {
					jsonStorage.addTimedVote(num, vote);
					num++;
				}
			}
			jsonStorage.save();
		}
	}

	public void addVoteToCache(String server, OfflineBungeeVote vote) {
		cachedVotes.putIfAbsent(server, new ArrayList<>());
		cachedVotes.get(server).add(vote);
	}

	public void addOnlineVoteToCache(String player, OfflineBungeeVote vote) {
		cachedOnlineVotes.putIfAbsent(player, new ArrayList<>());
		cachedOnlineVotes.get(player).add(vote);
	}

	public void addTimeVoteToCache(VoteTimeQueue vote) {
		timeChangeQueue.add(vote);
	}

	public void load() {
		if (useMySQL) {
			// Load votes from MySQL
			voteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getPlayerName(), voteRow.getUuid(),
						voteRow.getService(), voteRow.getTime(), voteRow.isRealVote(), voteRow.getText());
				String server = voteRow.getServer();
				cachedVotes.putIfAbsent(server, new ArrayList<>());
				cachedVotes.get(server).add(vote);
			});

			voteCacheTable.clearTable();

			// Load online votes from MySQL
			onlineVoteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getPlayerName(), voteRow.getUuid(),
						voteRow.getService(), voteRow.getTime(), voteRow.isRealvote(), voteRow.getText());
				String player = vote.getUuid();
				cachedOnlineVotes.putIfAbsent(player, new ArrayList<>());
				cachedOnlineVotes.get(player).add(vote);
			});

			onlineVoteCacheTable.clearTable();

			// Load timed votes from MySQL
			ArrayList<VoteTimeQueue> timedVotes = new ArrayList<>();
			timedVoteCacheTable.getAllVotes().forEach(timedVoteRow -> {
				VoteTimeQueue voteTimeQueue = new VoteTimeQueue(timedVoteRow.getPlayerName(), timedVoteRow.getService(),
						timedVoteRow.getTime());
				timedVotes.add(voteTimeQueue);
			});
			timeChangeQueue.addAll(timedVotes);

			timedVoteCacheTable.clearTable();

		} else {
			try {
				for (String key : jsonStorage.getTimedVoteCache()) {
					DataNode data = jsonStorage.getTimedVoteCache(key);

					if (data != null && data.isObject()) {
						String name = data.has("Name") ? data.get("Name").asString() : "";
						String service = data.has("Service") ? data.get("Service").asString() : "";
						long time = data.has("Time") ? data.get("Time").asLong() : 0L;

						getTimeChangeQueue().add(new VoteTimeQueue(name, service, time));
					}
				}

			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String server : jsonStorage.getServers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : jsonStorage.getServerVotes(server)) {
						DataNode data = jsonStorage.getServerVotes(server, num);

						if (data != null && data.isObject()) {

							String name = data.has("Name") ? data.get("Name").asString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
							String service = data.has("Service") ? data.get("Service").asString() : "";
							long time = data.has("Time") ? data.get("Time").asLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").asBoolean();
							String text = data.has("Text") ? data.get("Text").asString() : "";

							votes.add(new OfflineBungeeVote(name, uuid, service, time, real, text));
						}
					}
					getCachedVotes().put(server, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : jsonStorage.getPlayers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : jsonStorage.getOnlineVotes(player)) {
						DataNode data = jsonStorage.getOnlineVotes(player, num);

						if (data != null && data.isObject()) {

							String name = data.has("Name") ? data.get("Name").asString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
							String service = data.has("Service") ? data.get("Service").asString() : "";
							long time = data.has("Time") ? data.get("Time").asLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").asBoolean();
							String text = data.has("Text") ? data.get("Text").asString() : "";

							votes.add(new OfflineBungeeVote(name, uuid, service, time, real, text));
						}
					}
					getCachedOnlineVotes().put(player, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			jsonStorage.clearData();
		}
	}

	private final boolean useMySQL;
	private ProxyVoteCacheTable voteCacheTable;
	private ProxyTimedVoteCacheTable timedVoteCacheTable;
	private ProxyOnlineVoteCacheTable onlineVoteCacheTable;

	private IVoteCache jsonStorage;

	public abstract void logInfo1(String msg);

	public abstract void logSevere1(String msg);

	public abstract void debug1(Exception e);

	public VoteCacheHandler(MysqlConfig mysqlConfig, boolean useMySQL, boolean useExistingConnection, MySQL mysql,
			boolean debug, IVoteCache jsonStorage) {
		this.useMySQL = useMySQL;

		if (useMySQL) {
			if (useExistingConnection) {
				voteCacheTable = new ProxyVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};

				timedVoteCacheTable = new ProxyTimedVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};

				onlineVoteCacheTable = new ProxyOnlineVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};
			} else {
				voteCacheTable = new ProxyVoteCacheTable(mysqlConfig, debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};

				timedVoteCacheTable = new ProxyTimedVoteCacheTable(mysqlConfig, debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};

				onlineVoteCacheTable = new ProxyOnlineVoteCacheTable(mysqlConfig, debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};
			}
		} else {
			this.jsonStorage = jsonStorage;
		}
	}

}
