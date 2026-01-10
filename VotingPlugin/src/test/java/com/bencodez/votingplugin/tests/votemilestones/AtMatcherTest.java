package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;

import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.AtMatcher;

class AtMatcherTest {

	@Test
	void atZeroMeansFirstVote() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", 0);

		AtMatcher m = AtMatcher.fromConfig(c, "At");
		assertNotNull(m);

		assertTrue(m.matches(1));   // first vote
		assertFalse(m.matches(0));
		assertFalse(m.matches(2));
	}

	@Test
	void atSingleNumber() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", 50);

		AtMatcher m = AtMatcher.fromConfig(c, "At");
		assertNotNull(m);

		assertTrue(m.matches(50));
		assertFalse(m.matches(49));
		assertFalse(m.matches(51));
	}

	@Test
	void atListNumbers() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", Arrays.asList(20, 50, 100));

		AtMatcher m = AtMatcher.fromConfig(c, "At");
		assertNotNull(m);

		assertTrue(m.matches(20));
		assertTrue(m.matches(50));
		assertTrue(m.matches(100));
		assertFalse(m.matches(99));
	}

	@Test
	void atRangesAndSingles() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", Arrays.asList("10..30 step 10", 25, "100..102"));

		AtMatcher m = AtMatcher.fromConfig(c, "At");
		assertNotNull(m);

		assertTrue(m.matches(10));
		assertTrue(m.matches(20));
		assertTrue(m.matches(30));
		assertTrue(m.matches(25));
		assertTrue(m.matches(100));
		assertTrue(m.matches(101));
		assertTrue(m.matches(102));

		assertFalse(m.matches(15));
		assertFalse(m.matches(99));
	}

	@Test
	void invalidEntriesAreIgnored() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", Arrays.asList("nope", -5, "10..x step 2", ""));

		AtMatcher m = AtMatcher.fromConfig(c, "At");
		assertNull(m);
	}
}
