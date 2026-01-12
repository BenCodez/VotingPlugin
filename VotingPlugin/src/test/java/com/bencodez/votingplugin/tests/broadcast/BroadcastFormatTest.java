package com.bencodez.votingplugin.tests.broadcast;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.broadcast.BroadcastFormat;

public class BroadcastFormatTest {

	@Test
	public void render_single_uses_broadcastMsg_and_replaces_placeholders() {
		BroadcastFormat fmt = new BroadcastFormat("Thanks %player% for voting on %site% (%sites_count%): %sites%",
				"HEADER %player% %sites_count%", " - %site%");

		List<String> out = fmt.render("Ben", Arrays.asList("PlanetMinecraft"), "vote");

		assertEquals(1, out.size());
		assertTrue(out.get(0).contains("Thanks Ben"));
		assertTrue(out.get(0).contains("PlanetMinecraft"));
		assertTrue(out.get(0).contains("(1)"));
		assertTrue(out.get(0).contains("PlanetMinecraft")); // %sites% csv
	}

	@Test
	public void render_multi_uses_header_and_listLine_per_item() {
		BroadcastFormat fmt = new BroadcastFormat("SINGLE %player% %site%", "HEADER %player% (%sites_count%) %sites%",
				"LINE %site%");

		List<String> out = fmt.render("Ben", Arrays.asList("SiteA", "SiteB"), "vote");

		assertEquals(1 + 2, out.size());
		assertEquals("HEADER Ben (2) SiteA, SiteB", out.get(0));
		assertEquals("LINE SiteA", out.get(1));
		assertEquals("LINE SiteB", out.get(2));
	}

	@Test
	public void render_supports_reason_and_context_placeholders() {
		BroadcastFormat fmt = new BroadcastFormat("%player% %site% %reason% %extra%", "H %reason% %extra%",
				"L %site% %extra%");

		Map<String, String> ctx = new HashMap<>();
		ctx.put("extra", "CTX");

		List<String> out = fmt.render("Ben", Arrays.asList("SiteA", "SiteB"), "cooldown", ctx);

		assertEquals(3, out.size());
		assertEquals("H cooldown CTX", out.get(0));
		assertEquals("L SiteA CTX", out.get(1));
		assertEquals("L SiteB CTX", out.get(2));
	}

	@Test
	public void render_null_items_are_safely_handled() {
		BroadcastFormat fmt = new BroadcastFormat("S %player% %site%", "H %sites_count% %sites%", "L %site%");

		List<String> out = fmt.render("Ben", Arrays.asList(null, "SiteB"), "vote");

		assertEquals(3, out.size());
		assertEquals("H 2 SiteB", out.get(0));
		assertEquals("L ", out.get(1));
		assertEquals("L SiteB", out.get(2));
	}

}
