package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.RangeSpec;

class RangeSpecTest {

	@Test
	void parseRangeWithStep() {
		RangeSpec r = RangeSpec.tryParse("10..50 step 10");
		assertNotNull(r);
		assertEquals(10, r.start);
		assertEquals(50, r.end);
		assertEquals(10, r.step);

		List<Long> vals = r.expand();
		assertEquals(Arrays.asList(10L, 20L, 30L, 40L, 50L), vals);
	}

	@Test
	void parseRangeDefaultStep1() {
		RangeSpec r = RangeSpec.tryParse("3..6");
		assertNotNull(r);
		assertEquals(Arrays.asList(3L, 4L, 5L, 6L), r.expand());
	}

	@Test
	void parseRangeReversedSwaps() {
		RangeSpec r = RangeSpec.tryParse("20..10 step 5");
		assertNotNull(r);
		assertEquals(Arrays.asList(10L, 15L, 20L), r.expand());
	}

	@Test
	void invalidRangeReturnsNull() {
		assertNull(RangeSpec.tryParse("nope"));
		assertNull(RangeSpec.tryParse("10..x step 2"));
		assertNull(RangeSpec.tryParse("10..20 step 0"));
		assertNull(RangeSpec.tryParse("10....20"));
	}
}
