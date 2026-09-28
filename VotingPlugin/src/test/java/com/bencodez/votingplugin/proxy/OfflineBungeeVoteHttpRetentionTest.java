package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Test;

class OfflineBungeeVoteHttpRetentionTest {
	private static OfflineBungeeVote vote(UUID voteId) {
		return new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true, "totals");
	}

	private static String rewardJournalId(UUID voteId, String server) {
		return UUID.nameUUIDFromBytes((voteId + ":reward:" + server)
				.getBytes(StandardCharsets.UTF_8)).toString();
	}

	@Test
	void deterministicRewardJournalTargetDoesNotRetainHttp() {
		UUID voteId = UUID.randomUUID();
		OfflineBungeeVote vote = vote(voteId);
		vote.setHttpDeliveryId("Server1", rewardJournalId(voteId, "server1"));

		assertTrue(vote.hasPendingHttpDeliveryIds());
		assertFalse(vote.hasPendingHttpTransportDeliveryIds());
	}

	@Test
	void rewardJournalOwnerMarkersDoNotRetainHttp() {
		UUID voteId = UUID.randomUUID();
		OfflineBungeeVote vote = vote(voteId);
		vote.setHttpDeliveryId("__vp_reward_target__:Server1", rewardJournalId(voteId, "server1"));

		assertFalse(vote.hasPendingHttpTransportDeliveryIds());
	}

	@Test
	void recoveredHttpDeliveryIdStillRetainsHttp() {
		OfflineBungeeVote vote = vote(UUID.randomUUID());
		vote.setHttpDeliveryId("Server1", UUID.randomUUID().toString());

		assertTrue(vote.hasPendingHttpTransportDeliveryIds());
	}

	@Test
	void standaloneHttpBroadcastIdStillRetainsHttp() {
		OfflineBungeeVote vote = vote(UUID.randomUUID());
		vote.setHttpBroadcastDeliveryId("Server1", UUID.randomUUID().toString());

		assertTrue(vote.hasPendingHttpTransportDeliveryIds());
	}

	@Test
	void legacyRowWithoutVoteIdKeepsConservativeHttpRetention() {
		OfflineBungeeVote vote = new OfflineBungeeVote((UUID) null, "Player", "player-uuid",
				"Service", 100L, true, "totals", false, false, Collections.emptySet(),
				Collections.emptySet(), false);
		vote.setHttpDeliveryId("Server1", UUID.randomUUID().toString());

		assertTrue(vote.hasPendingHttpTransportDeliveryIds());
	}

	@Test
	void persistedAffectedCacheRemainsDropInCompatible() {
		UUID voteId = UUID.randomUUID();
		OfflineBungeeVote original = vote(voteId);
		original.setHttpDeliveryId("Server1", rewardJournalId(voteId, "server1"));

		OfflineBungeeVote restored = new OfflineBungeeVote(voteId, original.getPlayerName(), original.getUuid(),
				original.getService(), original.getTime(), original.isRealVote(), original.getText(), false, false,
				Collections.emptySet(), Collections.emptySet(), false,
				OfflineBungeeVote.decodeHttpDeliveryIds(original.encodeHttpDeliveryIds()));

		assertTrue(restored.hasPendingHttpDeliveryIds());
		assertFalse(restored.hasPendingHttpTransportDeliveryIds());
	}
}
