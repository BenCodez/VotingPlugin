package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;

/**
 * Unit tests for versioned MySQL storage of VoteTotalsSnapshot.
 *
 * Assumes VoteTotalsSnapshot has:
 *  - static VoteTotalsSnapshot parseStorage(String)
 *  - String toStorageString()  (writes CURRENT_VERSION, expected v2)
 *
 * Legacy v1 (no version prefix, milestone present, ignored):
 *   allTime//month//week//day//points//milestoneCount//partyCur//partyReq//dateMonth//uuid
 *
 * v2 (versioned, no milestone):
 *   v2//allTime//month//week//day//points//partyCur//partyReq//dateMonth//uuid
 */
public class VoteTotalsSnapshotStorageTest {

	@Test
	public void constructor_toStorageString_roundTripsThroughParseStorage_v2() {
		UUID voteId = UUID.randomUUID();

		VoteTotalsSnapshot data = new VoteTotalsSnapshot(
				100,   // allTime
				10,    // month
				7,     // week
				1,     // day
				55,    // points
				3,     // votePartyCurrent
				50,    // votePartyRequired
				9,     // dateMonthTotal
				voteId // voteUUID
		);

		String stored = data.toStorageString();

		assertNotNull(stored);
		assertTrue(stored.startsWith("v2//"), "Expected versioned storage string starting with v2//");

		VoteTotalsSnapshot parsed = VoteTotalsSnapshot.parseStorage(stored);

		assertEquals(100, parsed.getAllTimeTotal());
		assertEquals(10, parsed.getMonthTotal());
		assertEquals(7, parsed.getWeeklyTotal());
		assertEquals(1, parsed.getDailyTotal());
		assertEquals(55, parsed.getPoints());
		assertEquals(3, parsed.getVotePartyCurrent());
		assertEquals(50, parsed.getVotePartyRequired());
		assertEquals(9, parsed.getDateMonthTotal());
		assertEquals(voteId, parsed.getVoteUUID());
	}

	@Test
	public void parseStorage_readsLegacyV1StringWithMilestoneCount_ignoresMilestone() {
		UUID voteId = UUID.randomUUID();

		// v1 format:
		// allTime//month//week//day//points//milestoneCount//partyCur//partyReq//dateMonth//uuid
		String legacy = "100//10//7//1//55//999//3//50//9//" + voteId;

		VoteTotalsSnapshot parsed = VoteTotalsSnapshot.parseStorage(legacy);

		assertEquals(100, parsed.getAllTimeTotal());
		assertEquals(10, parsed.getMonthTotal());
		assertEquals(7, parsed.getWeeklyTotal());
		assertEquals(1, parsed.getDailyTotal());
		assertEquals(55, parsed.getPoints());

		// milestoneCount must not shift indices
		assertEquals(3, parsed.getVotePartyCurrent());
		assertEquals(50, parsed.getVotePartyRequired());
		assertEquals(9, parsed.getDateMonthTotal());
		assertEquals(voteId, parsed.getVoteUUID());
	}

	@Test
	public void v1_to_v2_migration_parseLegacyThenStoreAsV2_preservesValues() {
		UUID voteId = UUID.randomUUID();

		// Legacy v1 stored in DB (no version prefix, includes milestoneCount)
		String legacy = "123//45//6//7//89//1000//11//22//33//" + voteId;

		VoteTotalsSnapshot parsed = VoteTotalsSnapshot.parseStorage(legacy);

		// "Migrate" by writing back canonical storage (v2)
		String migrated = parsed.toStorageString();

		assertNotNull(migrated);
		assertTrue(migrated.startsWith("v2//"), "Migration should write v2 format");

		// Ensure migrated string re-parses identically
		VoteTotalsSnapshot reparsed = VoteTotalsSnapshot.parseStorage(migrated);

		assertEquals(123, reparsed.getAllTimeTotal());
		assertEquals(45, reparsed.getMonthTotal());
		assertEquals(6, reparsed.getWeeklyTotal());
		assertEquals(7, reparsed.getDailyTotal());
		assertEquals(89, reparsed.getPoints());
		assertEquals(11, reparsed.getVotePartyCurrent());
		assertEquals(22, reparsed.getVotePartyRequired());
		assertEquals(33, reparsed.getDateMonthTotal());
		assertEquals(voteId, reparsed.getVoteUUID());
	}

	@Test
	public void parseStorage_handlesEmptyUuidInV2_uuidBecomesNull() {
		// Versioned v2 with empty UUID token
		String v2NoUuid = "v2//100//10//7//1//55//3//50//9//";

		VoteTotalsSnapshot parsed = VoteTotalsSnapshot.parseStorage(v2NoUuid);

		assertEquals(100, parsed.getAllTimeTotal());
		assertEquals(10, parsed.getMonthTotal());
		assertEquals(7, parsed.getWeeklyTotal());
		assertEquals(1, parsed.getDailyTotal());
		assertEquals(55, parsed.getPoints());
		assertEquals(3, parsed.getVotePartyCurrent());
		assertEquals(50, parsed.getVotePartyRequired());
		assertEquals(9, parsed.getDateMonthTotal());
		assertNull(parsed.getVoteUUID());
	}

	@Test
	public void toStorageString_withNullUuid_stillParses() {
		VoteTotalsSnapshot data = new VoteTotalsSnapshot(
				1, 2, 3, 4, 5,
				6, 7,
				8,
				null
		);

		String stored = data.toStorageString();
		assertTrue(stored.startsWith("v2//"));

		VoteTotalsSnapshot parsed = VoteTotalsSnapshot.parseStorage(stored);

		assertEquals(1, parsed.getAllTimeTotal());
		assertEquals(2, parsed.getMonthTotal());
		assertEquals(3, parsed.getWeeklyTotal());
		assertEquals(4, parsed.getDailyTotal());
		assertEquals(5, parsed.getPoints());
		assertEquals(6, parsed.getVotePartyCurrent());
		assertEquals(7, parsed.getVotePartyRequired());
		assertEquals(8, parsed.getDateMonthTotal());
		assertNull(parsed.getVoteUUID());
	}
}
