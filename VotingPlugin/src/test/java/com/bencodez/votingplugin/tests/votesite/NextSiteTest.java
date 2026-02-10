package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.votesites.NextSite;
import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * Unit tests for {@link NextSite}.
 */
public class NextSiteTest {

	@Test
	public void testGettersReturnConstructorValues() {
		VoteSite site = mock(VoteSite.class);

		NextSite next = new NextSite(site, 123L);

		assertEquals(site, next.getSite());
		assertEquals(123L, next.getSecondsUntilAvailable());
	}
}
