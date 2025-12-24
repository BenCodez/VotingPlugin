
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Server;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteparty.VoteParty;

public class VotePartyTest {

	private VotingPluginMain plugin;
	private VoteParty voteParty;
	private VotingPluginUser user;
	private MockedStatic<Bukkit> mockedBukkit;

	@BeforeEach
	public void setUp() {
		plugin = Mockito.mock(VotingPluginMain.class);
		voteParty = Mockito.spy(new VoteParty(plugin)); // Use spy instead of mock
		user = Mockito.mock(VotingPluginUser.class);

		// Mock the return values for getServerData, getSpecialRewardsConfig, and
		// getPlaceholders
		ServerData serverData = Mockito.mock(ServerData.class);
		SpecialRewardsConfig specialRewardsConfig = Mockito.mock(SpecialRewardsConfig.class);
		ConfigurationSection configSection = Mockito.mock(ConfigurationSection.class);
		Config configFile = Mockito.mock(Config.class);
		PlaceHolders placeHolders = Mockito.mock(PlaceHolders.class);
		UserManager userManager = Mockito.mock(UserManager.class);

		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getSpecialRewardsConfig()).thenReturn(specialRewardsConfig);
		when(plugin.getConfigFile()).thenReturn(configFile);
		when(plugin.getPlaceholders()).thenReturn(placeHolders);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(serverData.getData()).thenReturn(configSection);

		// Deregister existing static mock and create a new one
		if (mockedBukkit != null) {
			mockedBukkit.close();
		}
		mockedBukkit = Mockito.mockStatic(Bukkit.class);
		Server mockServer = Mockito.mock(Server.class);
		when(Bukkit.getServer()).thenReturn(mockServer);
		PluginManager mockPluginManager = Mockito.mock(PluginManager.class);
		when(mockServer.getPluginManager()).thenReturn(mockPluginManager);
		when(Bukkit.getPluginManager()).thenReturn(mockPluginManager);
		when(mockServer.getOnlinePlayers()).thenReturn(Collections.emptyList());
	}

	@AfterEach
	public void tearDown() {
		if (mockedBukkit != null) {
			mockedBukkit.close();
		}
	}

	@Test
	public void addTotal_IncrementsTotalVotes() {
		when(plugin.getServerData().getData().getInt("VoteParty.Total")).thenReturn(5);
		voteParty.addTotal(user);
		verify(plugin.getServerData().getData()).set("VoteParty.Total", 6);
	}

	@Test
	public void check_EnoughVotes_TriggersVoteParty() {
	    when(plugin.getServerData().getData().getInt("VoteParty.Total")).thenReturn(10);
	    when(plugin.getSpecialRewardsConfig().getVotePartyVotesRequired()).thenReturn(10);

	    voteParty.check(user, false);

	    verify(voteParty, times(1)).giveRewards(any(), eq(false));
	    verify(Bukkit.getPluginManager()).callEvent(any());
	}

	@Test
	public void check_NotEnoughVotes_DoesNotTriggerVoteParty() {
	    when(plugin.getServerData().getData().getInt("VoteParty.Total")).thenReturn(5);
	    when(plugin.getSpecialRewardsConfig().getVotePartyVotesRequired()).thenReturn(10);

	    voteParty.check(user, false);

	    verify(voteParty, never()).giveRewards(any(), eq(false));
	}

	@Test
	public void commandVoteParty_VotePartyEnabled_SendsMessage() {
		CommandSender sender = Mockito.mock(CommandSender.class);
		when(plugin.getSpecialRewardsConfig().isVotePartyEnabled()).thenReturn(true);
		when(plugin.getConfigFile().getFormatCommandsVoteParty()).thenReturn(new ArrayList<>());

		voteParty.commandVoteParty(sender);

		verify(sender).sendMessage(any(String[].class));
	}

	@Test
	public void commandVoteParty_VotePartyDisabled_SendsErrorMessage() {
		CommandSender sender = Mockito.mock(CommandSender.class);
		when(plugin.getSpecialRewardsConfig().isVotePartyEnabled()).thenReturn(false);

		voteParty.commandVoteParty(sender);

		verify(sender).sendMessage(ChatColor.translateAlternateColorCodes('&', "&cVoteParty not enabled"));
	}

	@Test
	public void getNeededVotes_ReturnsCorrectValue() {
		when(plugin.getServerData().getData().getInt("VoteParty.Total")).thenReturn(5);
		when(plugin.getSpecialRewardsConfig().getVotePartyVotesRequired()).thenReturn(10);

		int neededVotes = voteParty.getNeededVotes();

		assertEquals(5, neededVotes);
	}

	@Test
	public void getRandomPlayerName_ReturnsPlayerName() {
		Player player = Mockito.mock(Player.class);
		when(player.getName()).thenReturn("player1");
		when(Bukkit.getOnlinePlayers()).thenReturn((Collection) Collections.singletonList(player));

		String playerName = voteParty.getRandomPlayerName();

		assertEquals("player1", playerName);
	}

	@Test
	public void getRandomPlayerName_NoPlayersOnline_ReturnsNoPlayer() {
		when(Bukkit.getOnlinePlayers()).thenReturn(Collections.emptyList());

		String playerName = voteParty.getRandomPlayerName();

		assertEquals("No Player", playerName);
	}
}
