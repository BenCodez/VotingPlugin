
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.votingplugin.proxy.ProxyMysqlUserTable;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;

public class VotingPluginProxyTest {

	@InjectMocks
	private VotingPluginProxyTestImpl votingPluginProxy;

	@Mock
	private ProxyMysqlUserTable proxyMySQL;

	@Mock
	private GlobalDataHandlerProxy globalDataHandler;

	@Mock
	private MultiProxyHandler multiProxyHandler;

	@BeforeEach
	void setUp() {
		MockitoAnnotations.openMocks(this);
		votingPluginProxy.setProxyMySQL(proxyMySQL);
		votingPluginProxy.setGlobalDataHandler(globalDataHandler);
		votingPluginProxy.setMultiProxyHandler(multiProxyHandler);

	}

	@Test
	void testAddVoteParty() {
		// Initial votePartyVotes should be 0
		assertEquals(0, votingPluginProxy.getVotePartyVotes());

		// Spy on the votingPluginProxy object
		VotingPluginProxy spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).checkVoteParty();

		// Add one vote party
		spyProxy.addCurrentVotePartyVotes(1);

		// Verify that votePartyVotes increased by 1
		assertEquals(1, spyProxy.getVotePartyVotes());
	}

	@Test
	void testAddCurrentVotePartyVotes() {
		// Initial votePartyVotes should be 0
		assertEquals(0, votingPluginProxy.getVotePartyVotes());

		// Add 3 votes
		votingPluginProxy.addCurrentVotePartyVotes(3);
		assertEquals(3, votingPluginProxy.getVotePartyVotes());

		// Add 2 more votes
		votingPluginProxy.addCurrentVotePartyVotes(2);
		assertEquals(5, votingPluginProxy.getVotePartyVotes());
	}

	@Test
	void invalidVoteStopsBeforeAnyPersistentRewardCacheOrForwardingState() {
		for (String username : new String[] { "MchtNameOver16xxx", "../MchtTraversal", "Mcht/Slash", "Mcht\\Slash",
				"Mcht Space", "Mcht\tTab", "Mcht\u00E9Unicode" }) {
			VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);

			spyProxy.vote(username, "MCHT", true, true, 0, null, null);

			verify(spyProxy, never()).getUUID(Mockito.anyString());
			verify(spyProxy, never()).addVoteParty();
			verify(spyProxy, never()).getVoteCacheHandler();
			verify(spyProxy, never()).getGlobalMessageProxyHandler();
			verify(proxyMySQL, never()).containsKeyQuery(Mockito.anyString());
			verify(multiProxyHandler, never()).sendMultiProxyEnvelope(Mockito.any());
			assertEquals(0, spyProxy.getVotePartyVotes(), username);
			assertTrue(spyProxy.getWarnings().stream().anyMatch(warning -> warning.contains("Rejected vote")), username);
		}
	}

	@Test
	void immediatePluginMessageReportsActualDeliveryResult() {
		votingPluginProxy.setPluginMessageDeliveryResult(false);
		assertFalse(votingPluginProxy.sendPluginMessageImmediately("Server1",
				VotingPluginWire.voteBroadcast("uuid", "Player", "Service", 100L, "", false)));

		votingPluginProxy.setPluginMessageDeliveryResult(true);
		assertTrue(votingPluginProxy.sendPluginMessageImmediately("Server1",
				VotingPluginWire.voteBroadcast("uuid", "Player", "Service", 100L, "", false)));
	}

	@Test
	void rejectedVoteStopsBeforeGlobalDataQueueOrOfflineBroadcast() {
		votingPluginProxy.setPlayerOnline(false);
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getGlobalDataEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
		Mockito.when(proxyMySQL.containsKeyQuery(Mockito.anyString())).thenReturn(true);
		Mockito.when(proxyMySQL.getExactQuery(Mockito.any())).thenReturn(new java.util.ArrayList<>());

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(false).when(spyProxy).checkVoteDelay(Mockito.anyString(), Mockito.anyString(), Mockito.any());

		spyProxy.vote("Player", "Service", true, true, 0, null, null);

		verify(spyProxy).checkVoteDelay(Mockito.anyString(), Mockito.eq("Service"), Mockito.any());
		verify(globalDataHandler, never()).isTimeChangedHappened();
		verify(spyProxy, never()).sendPluginMessageData(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
				Mockito.anyBoolean());
	}
}
