package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.UUID;
import java.util.logging.Logger;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

class CoolDownRewardDiagnosticsTest {
    @Test void synchronousFailureIsCorrelatedRedactedAndPropagated() {
        Fixture f = new Fixture(true);
        IllegalStateException failure = new IllegalStateException("private command and placeholder");
        doThrow(failure).when(f.handler).giveReward(eq(f.user), eq(f.data), eq("VoteCoolDownEndedReward"), any(RewardOptions.class));
        try {
            assertSame(failure, assertThrows(IllegalStateException.class,
                () -> f.check.onCoolDownEnd(new PlayerVoteCoolDownEndEvent(f.user))));
            ArgumentCaptor<String> attempted = ArgumentCaptor.forClass(String.class);
            ArgumentCaptor<String> warning = ArgumentCaptor.forClass(String.class);
            verify(f.logger).info(attempted.capture());
            verify(f.logger).warning(warning.capture());
            var match = java.util.regex.Pattern.compile("occurrence=([0-9a-f-]+)").matcher(attempted.getValue());
            assertTrue(match.find());
            assertTrue(warning.getValue().contains("occurrence=" + match.group(1)));
            assertTrue(warning.getValue().contains("player=" + f.uuid));
            assertTrue(warning.getValue().contains("site=all"));
            assertTrue(warning.getValue().contains("outcome=unknown/partial"));
            assertFalse(warning.getValue().contains("private command"));
        } finally { f.check.shutdown(); }
    }

    @Test void absentRewardKeepsLegacyDispatchWithoutDiagnosticNoise() {
        Fixture f = new Fixture(false);
        try {
            f.check.onCoolDownEnd(new PlayerVoteCoolDownEndEvent(f.user));
            verify(f.handler).giveReward(eq(f.user), eq(f.data), eq("VoteCoolDownEndedReward"), any(RewardOptions.class));
            verifyNoInteractions(f.logger);
        } finally { f.check.shutdown(); }
    }

    @Test void loggerFailureDoesNotSuppressRewardDispatch() {
        Fixture f = new Fixture(true);
        doThrow(new IllegalStateException("logger unavailable")).when(f.logger).info(anyString());
        try {
            f.check.onCoolDownEnd(new PlayerVoteCoolDownEndEvent(f.user));
            verify(f.handler).giveReward(eq(f.user), eq(f.data), eq("VoteCoolDownEndedReward"), any(RewardOptions.class));
        } finally { f.check.shutdown(); }
    }

    @Test void nestedSiteSectionProducesAdmissionWithoutChangingDispatch() {
        Fixture f = new Fixture(false);
        var site = mock(com.bencodez.votingplugin.votesites.VoteSite.class);
        var section = f.data.createSection("site");
        section.set("CoolDownEndRewards.Command", "say hello");
        when(site.getSiteData()).thenReturn(section);
        when(site.getKey()).thenReturn("example");
        when(site.getDisplayName()).thenReturn("Example");
        when(site.getVoteURL(false)).thenReturn("https://example.test");
        try {
            f.check.onCoolDownEnd(new com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent(f.user, site));
            verify(f.logger).info(contains("status=attempted"));
            verify(f.handler).giveReward(eq(f.user), eq(section), eq("CoolDownEndRewards"), any(RewardOptions.class));
        } finally { f.check.shutdown(); }
    }

    private static final class Fixture {
        final UUID uuid = UUID.randomUUID();
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final SpecialRewardsConfig special = mock(SpecialRewardsConfig.class);
        final RewardHandler handler = mock(RewardHandler.class);
        final Logger logger = mock(Logger.class);
        final YamlConfiguration data = new YamlConfiguration();
        final CoolDownCheck check;
        Fixture(boolean configured) {
            when(plugin.getSpecialRewardsConfig()).thenReturn(special);
            when(special.getData()).thenReturn(data);
            when(plugin.getRewardHandler()).thenReturn(handler);
            when(plugin.getLogger()).thenReturn(logger);
            when(user.getUUID()).thenReturn(uuid.toString());
            when(handler.hasRewards(data, "VoteCoolDownEndedReward")).thenReturn(configured);
            check = new CoolDownCheck(plugin);
        }
    }
}
