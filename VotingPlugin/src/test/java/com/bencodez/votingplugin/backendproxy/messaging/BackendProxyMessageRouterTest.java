package com.bencodez.votingplugin.backendproxy.messaging;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class BackendProxyMessageRouterTest {

	private static final UUID PLAYER_UUID = UUID.fromString("e5baec32-9b2c-4fc8-9aed-0e0285e3c33d");
	private static final long LAST_VOTE_TIME = 1_788_201_600_000L;

	private VotingPluginMain plugin;
	private VoteSiteManager voteSiteManager;
	private VotingPluginUser user;
	private Logger logger;
	private BackendProxyMessageRouter router;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class);
		voteSiteManager = mock(VoteSiteManager.class);
		UserManager userManager = mock(UserManager.class);
		user = mock(VotingPluginUser.class);
		logger = mock(Logger.class);

		when(plugin.getVoteSiteManager()).thenReturn(voteSiteManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(plugin.getLogger()).thenReturn(logger);
		when(userManager.getVotingPluginUser(PLAYER_UUID)).thenReturn(user);

		router = new BackendProxyMessageRouter(plugin, mock(BackendPresenceManager.class),
				mock(BackendGlobalDataSync.class), mock(BackendVotePartySync.class),
				mock(ProcessedVoteCache.class));
	}

	@Test
	void ignoresLastVoteTimeForUnknownServiceSite() {
		router.handleVoteUpdate(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"unknown.example", LAST_VOTE_TIME, ""));

		verify(user, never()).setTime(any(), anyLong());
		verify(logger).warning("Ignoring VoteUpdate last vote time for unknown service site: unknown.example");
		verify(plugin).setUpdate(true);
	}

	@Test
	void appliesLastVoteTimeForKnownServiceSite() {
		VoteSite voteSite = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(voteSite);

		router.handleVoteUpdate(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""));

		verify(user).setTime(voteSite, LAST_VOTE_TIME);
		verify(logger, never()).warning(any(String.class));
		verify(plugin).setUpdate(true);
	}
}
