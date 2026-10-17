package com.bencodez.votingplugin.tests.session;

import static org.junit.jupiter.api.Assertions.*;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

class GuidedSessionWireProvenanceTest {
	@Test
	public void observationProvenancePreservesAllRewardFieldsAndEnvelopeIdentity() {
		JsonEnvelope live = VotingPluginWire.voteOnline("Player", UUID.randomUUID().toString(), "Service",
				100L, true, true, "totals", UUID.randomUUID(), true, false, 1, 1, true);
		for (String origin : new String[] { "Primary", "" }) {
			JsonEnvelope forwarded = VotingPluginWire.unconfirmedSessionDelivery(live, origin);
			assertEquals(live.getSchema(), forwarded.getSchema());
			assertEquals(live.getSubChannel(), forwarded.getSubChannel());
			java.util.Map<String, String> fields = new java.util.LinkedHashMap<>(forwarded.getFields());
			assertEquals(origin, fields.remove(VotingPluginWire.K_MULTI_PROXY_ORIGIN));
			fields.put(VotingPluginWire.K_SESSION_DELIVERY_FRESH, "false");
			assertEquals(live.getFields(), fields);
		}
		assertFalse(live.getFields().containsKey(VotingPluginWire.K_MULTI_PROXY_ORIGIN));
	}

}
