package com.bencodez.votingplugin.tests.topvoter;

import com.bencodez.votingplugin.topvoter.TopVoterLoader;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.time.YearMonth;

import org.junit.jupiter.api.Test;

public class TopVoterLoaderTest {

	@Test
	public void parsesStoredMonthColumns() {
		assertEquals(YearMonth.of(2025, 1), TopVoterLoaderAccess.parseMonthColumn("MonthTotal-JANUARY-2025"));
		assertEquals(YearMonth.of(2026, 12), TopVoterLoaderAccess.parseMonthColumn("MonthTotal-DECEMBER-2026"));
	}

	@Test
	public void rejectsMalformedMonthColumns() {
		assertNull(TopVoterLoaderAccess.parseMonthColumn("MonthTotal-BAD-2025"));
		assertNull(TopVoterLoaderAccess.parseMonthColumn("MonthTotal-JANUARY-nope"));
		assertNull(TopVoterLoaderAccess.parseMonthColumn("Other-JANUARY-2025"));
		assertNull(TopVoterLoaderAccess.parseMonthColumn(null));
	}
}
