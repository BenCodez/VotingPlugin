package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import org.junit.jupiter.api.Test;

class HologramVoteModelTest {
    @Test void urlsAreStrictlyHttpAndHttps() {
        assertEquals("https://example.test/vote", HologramVoteModel.votingUrl(" https://example.test/vote ").orElseThrow());
        for (String value : new String[] {null, "", "javascript:alert(1)", "ftp://example.test", "https://user@example.test/", "https:///missing-host"})
            assertTrue(HologramVoteModel.votingUrl(value).isEmpty(), value);
    }

    @Test void paginationClampsAndPreservesOrder() {
        List<HologramVoteModel.Site> sites = List.of(
                new HologramVoteModel.Site("a", "A", "https://a.test", true, 0),
                new HologramVoteModel.Site("b", "B", "https://b.test", false, 0),
                new HologramVoteModel.Site("c", "C", "https://c.test", false, 61));
        assertEquals(2, HologramVoteModel.pages(3, 2));
        assertEquals(List.of(sites.get(2)), HologramVoteModel.page(sites, 99, 2));
        assertEquals("\u00a7c\u25cf B \u00a77- Cooldown active", sites.get(1).label());
        assertTrue(sites.get(2).label().contains("1m Remaining"));
        assertThrows(IllegalArgumentException.class, () -> HologramVoteModel.pages(1, 0));
    }
}
