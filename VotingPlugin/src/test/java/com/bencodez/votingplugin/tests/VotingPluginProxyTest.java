
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import java.io.FileNotFoundException;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.advancedcore.bungeeapi.mysql.ProxyMySQL;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.global.multiproxy.MultiProxyHandler;

public class VotingPluginProxyTest {

	@InjectMocks
	private VotingPluginProxyTestImpl votingPluginProxy;

	@Mock
	private ProxyMySQL proxyMySQL;

	@Mock
	private VotingPluginProxyConfig config;

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

		// Mocking config methods
		when(config.getVotePartyEnabled()).thenReturn(true);
		when(config.getVotePartyVotesRequired()).thenReturn(5);
		when(config.getVotePartyIncreaseVotesRequired()).thenReturn(5);
		when(config.getBungeeManageTotals()).thenReturn(true);
		when(config.getPluginMessageEncryption()).thenReturn(false);
		when(config.getDebug()).thenReturn(false);
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
	void testFetchUUID_Success() throws Exception {
		String playerName = "TestPlayer";
		String apiResponse = "{\"id\":\"aed922d6-5f64-43ce-8b98-efa8e4e288d3\",\"name\":\"TestPlayer\"}";

		// Mock the URL connection and response
		HttpURLConnectionMock.setupMockHttpURLConnection(playerName, 200, apiResponse);

		UUID uuid = votingPluginProxy.fetchUUID(playerName);

		assertNotNull(uuid);
		assertEquals("aed922d6-5f64-43ce-8b98-efa8e4e288d3", uuid.toString());
	}

	@Test
	void testFetchUUID_NotFound() throws Exception {
		String playerName = "NonExistentPlayer";
		String apiResponse = "{}";

		// Mock the URL connection and response
		HttpURLConnectionMock.setupMockHttpURLConnection(playerName, 400, apiResponse);

		UUID uuid = null;
		try {
			uuid = votingPluginProxy.fetchUUID(playerName);
		} catch (FileNotFoundException e) {
			// Expected exception for non-existent player
		}

		assertNull(uuid);
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
}
