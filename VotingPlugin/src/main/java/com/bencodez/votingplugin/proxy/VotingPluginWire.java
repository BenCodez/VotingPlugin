// File: com/bencodez/votingplugin/proxy/VotingPluginWire.java
package com.bencodez.votingplugin.proxy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Canonical wire format for proxy - backend messages.
 *
 * Rules: - Only JsonEnvelope is sent over the wire (no legacy String[] /
 * delimiters). - All fields are named keys (no numeric indices). - Versioning
 * uses the JsonEnvelope schema as the source of truth. - A "bungeeVersion"
 * field is also included for legacy compatibility checks. - Values are written
 * as native types where possible, but readers tolerate Strings.
 *
 * Note: Some keys are public because other classes (e.g. MultiProxyHandler)
 * reference them directly.
 */
public final class VotingPluginWire {
	private static final int MAX_PRESENCE_SNAPSHOT_JSON_LENGTH = 65536;

	private VotingPluginWire() {
	}

	public static final int SCHEMA_VERSION = 1;

	// =========================
	// Subchannels (canonical)
	// =========================
	public static final String SUB_VOTE = "Vote";
	public static final String SUB_VOTE_ONLINE = "VoteOnline";
	public static final String SUB_VOTE_UPDATE = "VoteUpdate";
	public static final String SUB_VOTE_DELAY_REJECTED = "VoteDelayRejected";
	public static final String SUB_VOTE_BROADCAST = "VoteBroadcast";
	public static final String SUB_BUNGEE_TIME_CHANGE = "BungeeTimeChange";

	public static final String SUB_STATUS = "Status";
	public static final String SUB_STATUS_OKAY = "statusokay";
	public static final String SUB_SERVER_NAME = "ServerName";

	public static final String SUB_VOTE_PARTY = "VotePartyBungee";

	public static final String SUB_LOGIN = "login";
	public static final String SUB_LOGOUT = "logout";
	public static final String SUB_BACKEND_STARTED = "BackendStarted";
	public static final String SUB_BACKEND_STOPPED = "BackendStopped";
	public static final String SUB_BACKEND_HEARTBEAT = "BackendHeartbeat";
	public static final String SUB_PRESENCE_SNAPSHOT_REQUEST = "PresenceSnapshotRequest";
	public static final String SUB_PRESENCE_SNAPSHOT = "PresenceSnapshot";
	public static final String SUB_VOTEUPDATE_RELAY = "voteupdate";

	// =========================
	// Multi-proxy subchannels (REQUIRED by MultiProxyHandler)
	// =========================
	public static final String SUB_CLEAR_VOTE = "ClearVote";
	public static final String SUB_CLEAR_VOTE_PRIMARY = "ClearVotePrimary";

	// =========================
	// Field keys (public where referenced externally)
	// =========================
	public static final String K_VERSION = "version";

	public static final String K_PLAYER = "player";
	public static final String K_UUID = "uuid";
	public static final String K_SERVER = "server";
	public static final String K_SERVICE = "service";
	public static final String K_TIME = "time";
	public static final String K_CONNECTION_ID = "connectionId";
	public static final String K_REQUEST_ID = "requestId";
	public static final String K_PLAYERS = "players";
	public static final String K_CHUNK_INDEX = "chunkIndex";
	public static final String K_CHUNK_COUNT = "chunkCount";

	// Vote/VoteOnline extras
	public static final String K_WAS_ONLINE = "wasOnline";
	public static final String K_REAL_VOTE = "realVote";
	public static final String K_TOTALS = "totals";
	public static final String K_VOTE_ID = "voteId";
	public static final String K_SET_TOTALS = "setTotals";
	public static final String K_MANAGE_TOTALS = "manageTotals";
	public static final String K_BUNGEE_BROADCAST = "bungeeBroadcast";
	public static final String K_NUM = "num";
	public static final String K_NUMBER_OF_VOTES = "numberOfVotes";

	// VoteUpdate extras
	public static final String K_PLAYER_UUID = "playerUuid";
	public static final String K_VOTE_PARTY_CURRENT = "votePartyCurrent";
	public static final String K_VOTE_PARTY_REQUIRED = "votePartyRequired";
	public static final String K_LAST_VOTE_TIME = "lastVoteTime";

	// =========================
	// Builders
	// =========================

	public static JsonEnvelope bungeeTimeChange() {
		return base(SUB_BUNGEE_TIME_CHANGE).build();
	}

	public static JsonEnvelope vote(String player, String uuid, String service, long time, boolean wasOnline,
			boolean realVote, String totals, UUID voteId, boolean manageTotals, boolean bungeeBroadcast, int num,
			int numberOfVotes) {

		return base(SUB_VOTE).put(K_PLAYER, safe(player)).put(K_UUID, safe(uuid)).put(K_SERVICE, safe(service))
				.put(K_TIME, time).put(K_WAS_ONLINE, wasOnline).put(K_REAL_VOTE, realVote).put(K_TOTALS, safe(totals))
				.put(K_VOTE_ID, voteId == null ? "" : voteId.toString())
				.put(K_SET_TOTALS, true) // backend historically defaulted to true
				.put(K_MANAGE_TOTALS, manageTotals).put(K_BUNGEE_BROADCAST, bungeeBroadcast).put(K_NUM, num)
				.put(K_NUMBER_OF_VOTES, numberOfVotes).build();
	}

	public static JsonEnvelope voteOnline(String player, String uuid, String service, long time, boolean wasOnline,
			boolean realVote, String totals, UUID voteId, boolean manageTotals, boolean bungeeBroadcast, int num,
			int numberOfVotes) {

		return base(SUB_VOTE_ONLINE).put(K_PLAYER, safe(player)).put(K_UUID, safe(uuid)).put(K_SERVICE, safe(service))
				.put(K_TIME, time).put(K_WAS_ONLINE, wasOnline).put(K_REAL_VOTE, realVote).put(K_TOTALS, safe(totals))
				.put(K_VOTE_ID, voteId == null ? "" : voteId.toString())
				.put(K_SET_TOTALS, true).put(K_MANAGE_TOTALS, manageTotals).put(K_BUNGEE_BROADCAST, bungeeBroadcast)
				.put(K_NUM, num).put(K_NUMBER_OF_VOTES, numberOfVotes).build();
	}

	public static JsonEnvelope voteDelayRejected(String player, String uuid, String service, boolean wasOnline) {
		return base(SUB_VOTE_DELAY_REJECTED).put(K_PLAYER, safe(player)).put(K_UUID, safe(uuid))
				.put(K_SERVICE, safe(service)).put(K_WAS_ONLINE, wasOnline).build();
	}

	public static JsonEnvelope voteBroadcast(String uuid, String player, String service, long time, String totals,
			boolean wasOnline) {
		return base(SUB_VOTE_BROADCAST).put(K_UUID, safe(uuid)).put(K_PLAYER, safe(player))
				.put(K_SERVICE, safe(service)).put(K_TIME, time).put(K_TOTALS, safe(totals))
				.put(K_WAS_ONLINE, wasOnline).build();
	}

	public static JsonEnvelope voteUpdate(String playerUuid, int votePartyCurrent, int votePartyRequired,
			String service, long lastVoteTime, String totals) {

		final String u = safe(playerUuid);
		return base(SUB_VOTE_UPDATE).put(K_PLAYER_UUID, u).put(K_PLAYER, u) // legacy fallback key used by older
																			// handlers
				.put(K_VOTE_PARTY_CURRENT, votePartyCurrent).put(K_VOTE_PARTY_REQUIRED, votePartyRequired)
				.put(K_SERVICE, safe(service)).put(K_LAST_VOTE_TIME, lastVoteTime).put(K_TOTALS, safe(totals)).build();
	}

	public static JsonEnvelope votePartyBungee() {
		return base(SUB_VOTE_PARTY).build();
	}

	public static JsonEnvelope status(String server) {
		return base(SUB_STATUS).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope statusOkay(String server) {
		return base(SUB_STATUS_OKAY).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope serverName(String server) {
		return base(SUB_SERVER_NAME).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope login(String player, String uuid, String server) {
		return login(player, uuid, server, null);
	}

	public static JsonEnvelope login(String player, String uuid, String server, UUID connectionId) {
		return base(SUB_LOGIN).put(K_PLAYER, safe(player)).put(K_UUID, safe(uuid)).put(K_SERVER, safe(server))
				.put(K_CONNECTION_ID, connectionId == null ? "" : connectionId.toString()).build();
	}

	public static JsonEnvelope logout(String player, String uuid, String server, UUID connectionId) {
		return base(SUB_LOGOUT).put(K_PLAYER, safe(player)).put(K_UUID, safe(uuid)).put(K_SERVER, safe(server))
				.put(K_CONNECTION_ID, connectionId == null ? "" : connectionId.toString()).build();
	}

	public static JsonEnvelope backendStarted(String server) {
		return base(SUB_BACKEND_STARTED).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope backendStopped(String server) {
		return base(SUB_BACKEND_STOPPED).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope backendHeartbeat(String server) {
		return base(SUB_BACKEND_HEARTBEAT).put(K_SERVER, safe(server)).build();
	}

	public static JsonEnvelope presenceSnapshotRequest(String server, UUID requestId) {
		return base(SUB_PRESENCE_SNAPSHOT_REQUEST).put(K_SERVER, safe(server))
				.put(K_REQUEST_ID, requestId == null ? "" : requestId.toString()).build();
	}

	public static JsonEnvelope presenceSnapshot(String server, UUID requestId, Collection<PresencePlayer> players) {
		return presenceSnapshot(server, requestId, 0, 1, players);
	}

	public static JsonEnvelope presenceSnapshot(String server, UUID requestId, int chunkIndex, int chunkCount,
			Collection<PresencePlayer> players) {
		JsonArray jsonPlayers = new JsonArray();
		if (players != null) {
			for (PresencePlayer player : players) {
				if (player == null) {
					continue;
				}
				JsonObject object = new JsonObject();
				object.addProperty(K_PLAYER, safe(player.player));
				object.addProperty(K_UUID, safe(player.uuid));
				object.addProperty(K_CONNECTION_ID, safe(player.connectionId));
				jsonPlayers.add(object);
			}
		}
		return base(SUB_PRESENCE_SNAPSHOT).put(K_SERVER, safe(server))
				.put(K_REQUEST_ID, requestId == null ? "" : requestId.toString()).put(K_CHUNK_INDEX, chunkIndex)
				.put(K_CHUNK_COUNT, chunkCount).put(K_PLAYERS, jsonPlayers.toString()).build();
	}

	// =========================
	// Multi-proxy builders (REQUIRED by MultiProxyHandler)
	// =========================

	/**
	 * Sent by non-primary servers to ask the primary to clear vote state. Primary
	 * responds by broadcasting ClearVotePrimary.
	 */
	public static JsonEnvelope clearVote(String uuid, String player, String server) {
		return base(SUB_CLEAR_VOTE).put(K_UUID, safe(uuid)).put(K_PLAYER, safe(player)).put(K_SERVER, safe(server))
				.build();
	}

	/**
	 * Broadcast by the primary server so all proxies clear vote state.
	 */
	public static JsonEnvelope clearVotePrimary(String uuid, String player, String server) {
		return base(SUB_CLEAR_VOTE_PRIMARY).put(K_UUID, safe(uuid)).put(K_PLAYER, safe(player))
				.put(K_SERVER, safe(server)).build();
	}

	// =========================
	// Readers (decode)
	// =========================

	public static final class Vote {
		public final String subChannel;
		public final String player;
		public final String uuid;
		public final String service;
		public final long time;

		public final boolean wasOnline;
		public final boolean realVote;
		public final String totals;
		public final UUID voteId;

		public final boolean setTotals;
		public final boolean manageTotals;
		public final boolean broadcast; // historically bungeeBroadcast
		public final int num;
		public final int numberOfVotes;

		private Vote(String subChannel, String player, String uuid, String service, long time, boolean wasOnline,
				boolean realVote, String totals, UUID voteId, boolean setTotals, boolean manageTotals, boolean broadcast, int num,
				int numberOfVotes) {
			this.subChannel = subChannel;
			this.player = player;
			this.uuid = uuid;
			this.service = service;
			this.time = time;
			this.wasOnline = wasOnline;
			this.realVote = realVote;
			this.totals = totals;
			this.voteId = voteId;
			this.setTotals = setTotals;
			this.manageTotals = manageTotals;
			this.broadcast = broadcast;
			this.num = num;
			this.numberOfVotes = numberOfVotes;
		}
	}

	public static Vote readVote(JsonEnvelope env) {
		Map<String, String> f = env.getFields();

		final String sub = safe(env.getSubChannel());
		final String player = safe(f.get(K_PLAYER));
		final String uuid = safe(f.get(K_UUID));
		final String service = safe(f.get(K_SERVICE));
		final long time = readLong(f, K_TIME, 0L);

		final boolean wasOnline = readBool(f, K_WAS_ONLINE, false);
		final boolean realVote = readBool(f, K_REAL_VOTE, false);
		final String totals = safe(f.get(K_TOTALS));
		final UUID voteId = readUuid(f, K_VOTE_ID);

		final boolean setTotals = readBool(f, K_SET_TOTALS, true);
		final boolean manageTotals = readBool(f, K_MANAGE_TOTALS, false);

		final boolean broadcast = readBool(f, K_BUNGEE_BROADCAST, false);

		final int num = readInt(f, K_NUM, 1);
		final int numberOfVotes = readInt(f, K_NUMBER_OF_VOTES, 1);

		return new Vote(sub, player, uuid, service, time, wasOnline, realVote, totals, voteId, setTotals, manageTotals,
				broadcast, num, numberOfVotes);
	}

	public static final class VoteDelayRejected {
		public final String player;
		public final String uuid;
		public final String service;
		public final boolean wasOnline;

		private VoteDelayRejected(String player, String uuid, String service, boolean wasOnline) {
			this.player = player;
			this.uuid = uuid;
			this.service = service;
			this.wasOnline = wasOnline;
		}
	}

	public static VoteDelayRejected readVoteDelayRejected(JsonEnvelope env) {
		Map<String, String> f = env.getFields();
		return new VoteDelayRejected(safe(f.get(K_PLAYER)), safe(f.get(K_UUID)), safe(f.get(K_SERVICE)),
				readBool(f, K_WAS_ONLINE, false));
	}

	public static final class VoteUpdate {
		public final String uuid;
		public final int votePartyCurrent;
		public final int votePartyRequired;
		public final String service;
		public final long time; // lastVoteTime

		private VoteUpdate(String uuid, int votePartyCurrent, int votePartyRequired, String service, long time) {
			this.uuid = uuid;
			this.votePartyCurrent = votePartyCurrent;
			this.votePartyRequired = votePartyRequired;
			this.service = service;
			this.time = time;
		}
	}

	public static VoteUpdate readVoteUpdate(JsonEnvelope env) {
		Map<String, String> f = env.getFields();

		String uuid = safe(f.get(K_PLAYER_UUID));
		if (uuid.isEmpty()) {
			uuid = safe(f.get(K_PLAYER));
		}

		int cur = readInt(f, K_VOTE_PARTY_CURRENT, 0);
		int req = readInt(f, K_VOTE_PARTY_REQUIRED, 0);
		String service = safe(f.get(K_SERVICE));

		long t = readLong(f, K_LAST_VOTE_TIME, 0L);
		if (t == 0L) {
			t = readLong(f, K_TIME, 0L);
		}

		return new VoteUpdate(uuid, cur, req, service, t);
	}

	public static final class PresencePlayer {
		public final String player;
		public final String uuid;
		public final String connectionId;

		public PresencePlayer(String player, String uuid, String connectionId) {
			this.player = safe(player);
			this.uuid = safe(uuid);
			this.connectionId = safe(connectionId);
		}
	}

	public static final class PlayerPresenceEvent {
		public final String player;
		public final String uuid;
		public final String server;
		public final UUID connectionId;

		private PlayerPresenceEvent(String player, String uuid, String server, UUID connectionId) {
			this.player = player;
			this.uuid = uuid;
			this.server = server;
			this.connectionId = connectionId;
		}
	}

	public static PlayerPresenceEvent readPlayerPresenceEvent(JsonEnvelope env) {
		Map<String, String> fields = env.getFields();
		return new PlayerPresenceEvent(safe(fields.get(K_PLAYER)), safe(fields.get(K_UUID)),
				safe(fields.get(K_SERVER)), readUuid(fields, K_CONNECTION_ID));
	}

	public static final class PresenceSnapshotRequest {
		public final String server;
		public final UUID requestId;

		private PresenceSnapshotRequest(String server, UUID requestId) {
			this.server = server;
			this.requestId = requestId;
		}
	}

	public static PresenceSnapshotRequest readPresenceSnapshotRequest(JsonEnvelope env) {
		Map<String, String> fields = env.getFields();
		return new PresenceSnapshotRequest(safe(fields.get(K_SERVER)), readUuid(fields, K_REQUEST_ID));
	}

	public static final class PresenceSnapshot {
		public final String server;
		public final UUID requestId;
		public final int chunkIndex;
		public final int chunkCount;
		public final List<PresencePlayer> players;
		public final boolean valid;

		private PresenceSnapshot(String server, UUID requestId, int chunkIndex, int chunkCount,
				List<PresencePlayer> players, boolean valid) {
			this.server = server;
			this.requestId = requestId;
			this.chunkIndex = chunkIndex;
			this.chunkCount = chunkCount;
			this.players = Collections.unmodifiableList(players);
			this.valid = valid;
		}
	}

	public static PresenceSnapshot readPresenceSnapshot(JsonEnvelope env) {
		Map<String, String> fields = env.getFields();
		String server = safe(fields.get(K_SERVER));
		UUID requestId = readUuid(fields, K_REQUEST_ID);
		int chunkIndex = readInt(fields, K_CHUNK_INDEX, 0);
		int chunkCount = readInt(fields, K_CHUNK_COUNT, 1);
		String encodedPlayers = fields.get(K_PLAYERS);
		List<PresencePlayer> players = new ArrayList<>();
		if (encodedPlayers == null || encodedPlayers.length() > MAX_PRESENCE_SNAPSHOT_JSON_LENGTH) {
			return new PresenceSnapshot(server, requestId, chunkIndex, chunkCount, players, false);
		}

		try {
			JsonElement root = JsonParser.parseString(encodedPlayers);
			if (!root.isJsonArray()) {
				return new PresenceSnapshot(server, requestId, chunkIndex, chunkCount, players, false);
			}
			for (JsonElement element : root.getAsJsonArray()) {
				if (!element.isJsonObject()) {
					return new PresenceSnapshot(server, requestId, chunkIndex, chunkCount, new ArrayList<>(), false);
				}
				JsonObject object = element.getAsJsonObject();
				players.add(new PresencePlayer(readJsonString(object, K_PLAYER), readJsonString(object, K_UUID),
						readJsonString(object, K_CONNECTION_ID)));
			}
			boolean valid = requestId != null && !server.isEmpty() && chunkIndex >= 0 && chunkCount > 0
					&& chunkIndex < chunkCount;
			return new PresenceSnapshot(server, requestId, chunkIndex, chunkCount, players, valid);
		} catch (RuntimeException e) {
			return new PresenceSnapshot(server, requestId, chunkIndex, chunkCount, new ArrayList<>(), false);
		}
	}

	// =========================
	// Internals
	// =========================

	private static JsonEnvelope.Builder base(String subChannel) {
		int ver = SCHEMA_VERSION;
		return JsonEnvelope.builder(subChannel).schema(ver);
	}

	private static String safe(String s) {
		return s == null ? "" : s;
	}

	private static boolean readBool(Map<String, String> f, String key, boolean def) {
		String v = f.get(key);
		if (v == null) {
			return def;
		}
		return Boolean.parseBoolean(v);
	}

	private static int readInt(Map<String, String> f, String key, int def) {
		String v = f.get(key);
		if (v == null) {
			return def;
		}
		try {
			return Integer.parseInt(v);
		} catch (Exception ignored) {
			return def;
		}
	}

	private static UUID readUuid(Map<String, String> f, String key) {
		String value = f.get(key);
		if (value == null || value.isEmpty()) {
			return null;
		}
		try {
			return UUID.fromString(value);
		} catch (IllegalArgumentException ignored) {
			return null;
		}
	}

	private static String readJsonString(JsonObject object, String key) {
		JsonElement value = object.get(key);
		if (value == null || value.isJsonNull() || !value.isJsonPrimitive()) {
			return "";
		}
		return safe(value.getAsString());
	}

	private static long readLong(Map<String, String> f, String key, long def) {
		String v = f.get(key);
		if (v == null) {
			return def;
		}
		try {
			return Long.parseLong(v);
		} catch (Exception ignored) {
			return def;
		}
	}
}
