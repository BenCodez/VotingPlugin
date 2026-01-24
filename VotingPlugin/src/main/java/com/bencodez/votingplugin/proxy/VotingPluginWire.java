// File: com/bencodez/votingplugin/proxy/VotingPluginWire.java
package com.bencodez.votingplugin.proxy;

import java.util.Map;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/**
 * Canonical wire format for proxy <-> backend messages.
 *
 * Rules:
 * - Only JsonEnvelope is sent over the wire (no legacy String[] / delimiters).
 * - All fields are named keys (no numeric indices).
 * - Versioning uses the JsonEnvelope schema as the source of truth.
 * - A "bungeeVersion" field is also included for legacy compatibility checks.
 * - Values are written as native types where possible, but readers tolerate Strings.
 *
 * Note: Some keys are public because other classes (e.g. MultiProxyHandler) reference them directly.
 */
public final class VotingPluginWire {

	private VotingPluginWire() {
	}

	// =========================
	// Subchannels (canonical)
	// =========================
	public static final String SUB_VOTE = "Vote";
	public static final String SUB_VOTE_ONLINE = "VoteOnline";
	public static final String SUB_VOTE_UPDATE = "VoteUpdate";
	public static final String SUB_VOTE_BROADCAST = "VoteBroadcast";
	public static final String SUB_BUNGEE_TIME_CHANGE = "BungeeTimeChange";

	public static final String SUB_STATUS = "Status";
	public static final String SUB_STATUS_OKAY = "statusokay";
	public static final String SUB_SERVER_NAME = "ServerName";

	public static final String SUB_VOTE_PARTY = "VotePartyBungee";

	public static final String SUB_LOGIN = "login";
	public static final String SUB_VOTEUPDATE_RELAY = "voteupdate";

	// =========================
	// Multi-proxy subchannels (REQUIRED by MultiProxyHandler)
	// =========================
	public static final String SUB_CLEAR_VOTE = "ClearVote";
	public static final String SUB_CLEAR_VOTE_PRIMARY = "ClearVotePrimary";

	// =========================
	// Field keys (public where referenced externally)
	// =========================
	public static final String K_BUNGEE_VERSION = "bungeeVersion";

	public static final String K_PLAYER = "player";
	public static final String K_UUID = "uuid";
	public static final String K_SERVER = "server";
	public static final String K_SERVICE = "service";
	public static final String K_TIME = "time";

	// Vote/VoteOnline extras
	public static final String K_WAS_ONLINE = "wasOnline";
	public static final String K_REAL_VOTE = "realVote";
	public static final String K_TOTALS = "totals";
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
			boolean realVote, String totals, boolean manageTotals, boolean bungeeBroadcast, int num,
			int numberOfVotes) {

		return base(SUB_VOTE)
				.put(K_PLAYER, safe(player))
				.put(K_UUID, safe(uuid))
				.put(K_SERVICE, safe(service))
				.put(K_TIME, time)
				.put(K_WAS_ONLINE, wasOnline)
				.put(K_REAL_VOTE, realVote)
				.put(K_TOTALS, safe(totals))
				.put(K_SET_TOTALS, true) // backend historically defaulted to true
				.put(K_MANAGE_TOTALS, manageTotals)
				.put(K_BUNGEE_BROADCAST, bungeeBroadcast)
				.put(K_NUM, num)
				.put(K_NUMBER_OF_VOTES, numberOfVotes)
				.build();
	}

	public static JsonEnvelope voteOnline(String player, String uuid, String service, long time, boolean wasOnline,
			boolean realVote, String totals, boolean manageTotals, boolean bungeeBroadcast, int num,
			int numberOfVotes) {

		return base(SUB_VOTE_ONLINE)
				.put(K_PLAYER, safe(player))
				.put(K_UUID, safe(uuid))
				.put(K_SERVICE, safe(service))
				.put(K_TIME, time)
				.put(K_WAS_ONLINE, wasOnline)
				.put(K_REAL_VOTE, realVote)
				.put(K_TOTALS, safe(totals))
				.put(K_SET_TOTALS, true)
				.put(K_MANAGE_TOTALS, manageTotals)
				.put(K_BUNGEE_BROADCAST, bungeeBroadcast)
				.put(K_NUM, num)
				.put(K_NUMBER_OF_VOTES, numberOfVotes)
				.build();
	}

	public static JsonEnvelope voteBroadcast(String uuid, String player, String service) {
		return base(SUB_VOTE_BROADCAST)
				.put(K_UUID, safe(uuid))
				.put(K_PLAYER, safe(player))
				.put(K_SERVICE, safe(service))
				.build();
	}

	public static JsonEnvelope voteUpdate(String playerUuid, int votePartyCurrent, int votePartyRequired,
			String service, long lastVoteTime, String totals) {

		final String u = safe(playerUuid);
		return base(SUB_VOTE_UPDATE)
				.put(K_PLAYER_UUID, u)
				.put(K_PLAYER, u) // legacy fallback key used by older handlers
				.put(K_VOTE_PARTY_CURRENT, votePartyCurrent)
				.put(K_VOTE_PARTY_REQUIRED, votePartyRequired)
				.put(K_SERVICE, safe(service))
				.put(K_LAST_VOTE_TIME, lastVoteTime)
				.put(K_TOTALS, safe(totals))
				.build();
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
		return base(SUB_LOGIN)
				.put(K_PLAYER, safe(player))
				.put(K_UUID, safe(uuid))
				.put(K_SERVER, safe(server))
				.build();
	}

	// =========================
	// Multi-proxy builders (REQUIRED by MultiProxyHandler)
	// =========================

	/**
	 * Sent by non-primary servers to ask the primary to clear vote state.
	 * Primary responds by broadcasting ClearVotePrimary.
	 */
	public static JsonEnvelope clearVote(String uuid, String player, String server) {
		return base(SUB_CLEAR_VOTE)
				.put(K_UUID, safe(uuid))
				.put(K_PLAYER, safe(player))
				.put(K_SERVER, safe(server))
				.build();
	}

	/**
	 * Broadcast by the primary server so all proxies clear vote state.
	 */
	public static JsonEnvelope clearVotePrimary(String uuid, String player, String server) {
		return base(SUB_CLEAR_VOTE_PRIMARY)
				.put(K_UUID, safe(uuid))
				.put(K_PLAYER, safe(player))
				.put(K_SERVER, safe(server))
				.build();
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

		public final boolean setTotals;
		public final boolean manageTotals;
		public final boolean broadcast; // historically bungeeBroadcast
		public final int num;
		public final int numberOfVotes;

		private Vote(String subChannel, String player, String uuid, String service, long time, boolean wasOnline,
				boolean realVote, String totals, boolean setTotals, boolean manageTotals, boolean broadcast, int num,
				int numberOfVotes) {
			this.subChannel = subChannel;
			this.player = player;
			this.uuid = uuid;
			this.service = service;
			this.time = time;
			this.wasOnline = wasOnline;
			this.realVote = realVote;
			this.totals = totals;
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

		final boolean setTotals = readBool(f, K_SET_TOTALS, true);
		final boolean manageTotals = readBool(f, K_MANAGE_TOTALS, false);

		final boolean broadcast = readBool(f, K_BUNGEE_BROADCAST, false);

		final int num = readInt(f, K_NUM, 1);
		final int numberOfVotes = readInt(f, K_NUMBER_OF_VOTES, 1);

		return new Vote(sub, player, uuid, service, time, wasOnline, realVote, totals, setTotals, manageTotals, broadcast,
				num, numberOfVotes);
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

	// =========================
	// Internals
	// =========================

	private static JsonEnvelope.Builder base(String subChannel) {
		int ver = BungeeVersion.getPluginMessageVersion();
		return JsonEnvelope.builder(subChannel)
				.schema(ver)
				.put(K_BUNGEE_VERSION, ver);
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
