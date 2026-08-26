package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.time.YearMonth;

import org.junit.jupiter.api.Test;

public class TopVoterLoaderTest {

	@Test
	public void parsesStoredMonthColumns() {
		assertEquals(YearMonth.of(2025, 1), TopVoterLoader.parseMonthColumn("MonthTotal-JANUARY-2025"));
		assertEquals(YearMonth.of(2026, 12), TopVoterLoader.parseMonthColumn("MonthTotal-DECEMBER-2026"));
	}

	@Test
	public void rejectsMalformedMonthColumns() {
		assertNull(TopVoterLoader.parseMonthColumn("MonthTotal-BAD-2025"));
		assertNull(TopVoterLoader.parseMonthColumn("MonthTotal-JANUARY-nope"));
		assertNull(TopVoterLoader.parseMonthColumn("Other-JANUARY-2025"));
		assertNull(TopVoterLoader.parseMonthColumn(null));
	}
}
