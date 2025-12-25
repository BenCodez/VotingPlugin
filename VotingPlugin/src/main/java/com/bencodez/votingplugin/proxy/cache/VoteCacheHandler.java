package com.bencodez.votingplugin.proxy.cache;

import java.time.LocalDateTime;
import java.time.ZoneId;
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

public abstract class VoteCacheHandler {

	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	// uuid based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	// server based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new ConcurrentHashMap<>();

	public boolean hasVotes(String server) {
		return cachedVotes.containsKey(server);
	}

	public ArrayList<OfflineBungeeVote> getVotes(String server) {
		return cachedVotes.getOrDefault(server, new ArrayList<>());
	}

	/**
	 * Get total cached votes for a UUID across: - UUID-based online vote cache -
	 * Server-based vote cache
	 *
	 * @param uuid player UUID (string form)
	 * @return total cached votes across all proxy caches
	 */
	public int getProxyCachedTotal(String uuid) {
		if (uuid == null || uuid.isEmpty()) {
			return 0;
		}

		String u = uuid.toLowerCase();
		int total = 0;

		// 1) UUID-based cache (fast lookup)
		ArrayList<OfflineBungeeVote> onlineVotes = cachedOnlineVotes.get(u);
		if (onlineVotes != null) {
			total += onlineVotes.size();
		}

		// 2) Server-based caches (scan)
		for (ArrayList<OfflineBungeeVote> serverVotes : cachedVotes.values()) {
			for (OfflineBungeeVote vote : serverVotes) {
				if (vote != null && vote.getUuid() != null && vote.getUuid().equalsIgnoreCase(u)) {
					total++;
				}
			}
		}

		return total;
	}

	public void addServerVote(String server, OfflineBungeeVote vote) {
		cachedVotes.putIfAbsent(server, new ArrayList<>());
		cachedVotes.get(server).add(vote);
		if (useMySQL) {
			voteCacheTable.insertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText(), server);
		} else {
			jsonStorage.addVote(server, cachedVotes.get(server).size() - 1, vote);
			jsonStorage.save();
		}
	}

	public void removeVote(String server, String uuid) {
		if (cachedVotes.containsKey(server)) {
			ArrayList<OfflineBungeeVote> votes = cachedVotes.get(server);
			votes.removeIf(vote -> vote.getUuid().equals(uuid));
			if (useMySQL) {
				voteCacheTable.removeVotesByServerAndUUID(server, uuid);
			} else {
				jsonStorage.removeServerVote(server, uuid);
				jsonStorage.save();
			}
		}
	}

	public void removeVotes(String server) {
		cachedVotes.remove(server);
		if (useMySQL) {
			voteCacheTable.removeVotesByServer(server);
		} else {
			jsonStorage.removeServerVotes(server);
			jsonStorage.save();
		}
	}

	public boolean hasOnlineVotes(String uuid) {
		return cachedOnlineVotes.containsKey(uuid);
	}

	public ArrayList<OfflineBungeeVote> getOnlineVotes(String uuid) {
		return cachedOnlineVotes.getOrDefault(uuid, new ArrayList<>());
	}

	public void addOnlineVote(String uuid, OfflineBungeeVote vote) {
		cachedOnlineVotes.putIfAbsent(uuid, new ArrayList<>());
		cachedOnlineVotes.get(uuid).add(vote);
		if (useMySQL) {
			onlineVoteCacheTable.insertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText());
		} else {
			jsonStorage.addVoteOnline(uuid, cachedOnlineVotes.get(uuid).size() - 1, vote);
			jsonStorage.save();
		}
	}

	public void removeOnlineVotes(String uuid) {
		cachedOnlineVotes.remove(uuid);
		if (useMySQL) {
			onlineVoteCacheTable.removeVotesByUuid(uuid);
		} else {
			jsonStorage.removeOnlineVotes(uuid);
			jsonStorage.save();
		}
	}

	public void checkVoteCacheTime(int voteCacheTime) {
		long cTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();

		// Collect expired online votes
		ArrayList<OfflineBungeeVote> expiredOnlineVotes = new ArrayList<>();
		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				if (vote.getTime() + (voteCacheTime * 24 * 60 * 60 * 1000) < cTime) {
					debug1("Removing vote from cache: " + vote.toString());
					expiredOnlineVotes.add(vote);
				}
			}
		}
		removeOnlineVotes(expiredOnlineVotes);

		// Collect expired server votes
		ArrayList<OfflineBungeeVote> expiredServerVotes = new ArrayList<>();
		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				if (vote.getTime() + (voteCacheTime * 24 * 60 * 60 * 1000) < cTime) {
					debug1("Removing vote from cache: " + vote.toString());
					expiredServerVotes.add(vote);
				}
			}
		}

		for (String server : cachedVotes.keySet()) {
			removeServerVotes(server, expiredServerVotes);
		}
	}

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

			if (!getTimeChangeQueue().isEmpty()) {
				for (VoteTimeQueue vote : getTimeChangeQueue()) {
					timedVoteCacheTable.insertTimedVote(vote.getName(), vote.getService(), vote.getTime());
				}
			}
		} else {

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

	public void addTimeVoteToCache(VoteTimeQueue vote) {
		timeChangeQueue.add(vote);
	}

	public void load() {
		if (useMySQL) {
			// Load votes from MySQL
			voteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getVoteId(), voteRow.getPlayerName(),
						voteRow.getUuid(), voteRow.getService(), voteRow.getTime(), voteRow.isRealVote(),
						voteRow.getText());
				String server = voteRow.getServer();
				cachedVotes.putIfAbsent(server, new ArrayList<>());
				cachedVotes.get(server).add(vote);
			});

			// Load online votes from MySQL
			onlineVoteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getVoteId(), voteRow.getPlayerName(),
						voteRow.getUuid(), voteRow.getService(), voteRow.getTime(), voteRow.isRealVote(),
						voteRow.getText());
				String player = vote.getUuid();
				cachedOnlineVotes.putIfAbsent(player, new ArrayList<>());
				cachedOnlineVotes.get(player).add(vote);
			});

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
							String voteId = data.has("VoteId") ? data.get("VoteId").asString() : "";

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text));
						}
					}
					cachedVotes.put(server, votes);
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
							String voteId = data.has("VoteId") ? data.get("VoteId").asString() : "";

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text));
						}
					}
					cachedOnlineVotes.put(player, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

		}

		// log vote cache load summary

		debug1("Loaded " + cachedVotes.size() + " server vote caches.");
		int totalServerVotes = cachedVotes.values().stream().mapToInt(ArrayList::size).sum();
		debug1("Loaded " + totalServerVotes + " total server votes.");

		debug1("Loaded " + cachedOnlineVotes.size() + " online vote caches.");
		int totalOnlineVotes = cachedOnlineVotes.values().stream().mapToInt(ArrayList::size).sum();
		debug1("Loaded " + totalOnlineVotes + " total online votes.");

		debug1("Loaded " + timeChangeQueue.size() + " timed votes.");

	}

	private final boolean useMySQL;
	private ProxyVoteCacheTable voteCacheTable;
	private ProxyTimedVoteCacheTable timedVoteCacheTable;
	private ProxyOnlineVoteCacheTable onlineVoteCacheTable;

	private IVoteCache jsonStorage;

	public abstract void logInfo1(String msg);

	public abstract void logSevere1(String msg);

	public abstract void debug1(Exception e);

	public abstract void debug1(Throwable e);

	public abstract void debug1(String msg);

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
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
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
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
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
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
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
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}
				};

				timedVoteCacheTable = new ProxyTimedVoteCacheTable(voteCacheTable.getMysql(),
						mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}
				};

				onlineVoteCacheTable = new ProxyOnlineVoteCacheTable(voteCacheTable.getMysql(),
						mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}
				};
			}
		} else {
			this.jsonStorage = jsonStorage;
		}
	}

	public String[] getCachedVotesServers() {
		return cachedVotes.keySet().toArray(new String[0]);
	}

	public void removeServerVotes(String server, ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : removed) {
			for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
				if (entry.getKey().equals(server)) {
					entry.getValue().removeIf(v -> v.getUuid().equals(vote.getUuid())
							&& v.getService().equals(vote.getService()) && v.getTime() == vote.getTime());
				}
			}
			if (useMySQL) {
				voteCacheTable.removeVote(vote, server);
			} else {
				jsonStorage.removeVote(server, vote);
				jsonStorage.save();
			}
		}
	}

	public void removeOnlineVotes(ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : removed) {
			for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
				entry.getValue().removeIf(v -> v.getUuid().equals(vote.getUuid())
						&& v.getService().equals(vote.getService()) && v.getTime() == vote.getTime());
			}
			if (useMySQL) {
				onlineVoteCacheTable.removeVote(vote);
			} else {
				jsonStorage.removeOnlineVote(vote);
				jsonStorage.save();
			}
		}
	}

}
