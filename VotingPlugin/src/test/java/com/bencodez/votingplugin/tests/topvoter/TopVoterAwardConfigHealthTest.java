package com.bencodez.votingplugin.tests.topvoter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.config.VotingPluginConfigHealth;

class TopVoterAwardConfigHealthTest {

	private final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
	private final Logger logger = mock(Logger.class);
	private final YamlConfiguration configData = new YamlConfiguration();
	private final YamlConfiguration rewardData = new YamlConfiguration();
	private Config config;
	private SpecialRewardsConfig rewards;
	@TempDir
	File dataFolder;

	@BeforeEach
	void setUp() {
		when(plugin.getDataFolder()).thenReturn(dataFolder);
		config = new Config(plugin);
		when(plugin.getConfigFile()).thenReturn(config);
		rewards = new SpecialRewardsConfig(plugin);
		when(plugin.getSpecialRewardsConfig()).thenReturn(rewards);
		when(plugin.getLogger()).thenReturn(logger);
	}

	@Test
	void warnsOnceForMonthlyInlineAwards() {
		configData.set("LoadTopVoter.Monthly", false);
		rewardData.set("MonthlyAwards.1.Rewards.Messages.Player", "You won!");
		rewardData.set("MonthlyAwards.2.Rewards.Messages.Player", "You placed second!");

		checkSingleWarning("Monthly");
	}

	@Test
	void warnsForWeeklyAwardList() {
		rewardData.set("EnableWeeklyAwards", true);
		rewardData.set("WeeklyAwards.1-2.Rewards", List.of("example-reward"));

		checkSingleWarning("Weekly");
	}

	@Test
	void warnsForDailyNamedAward() {
		rewardData.set("EnableDailyRewards", true);
		rewardData.set("DailyAwards.1.Rewards", "example-reward");

		checkSingleWarning("Daily");
	}

	@Test
	void warnsForUnquotedNumericRewardName() {
		rewardData.set("EnableDailyRewards", true);
		rewardData.set("DailyAwards.1.Rewards", 123);

		checkSingleWarning("Daily");
	}

	@Test
	void ignoresPackagedDefaults() throws Exception {
		check(loadDefaults("Config.yml"), loadDefaults("SpecialRewards.yml"));

		verifyNoInteractions(logger);
	}

	@Test
	void ignoresLoadedLeaderboardsAndAbsentOrEmptyRewards() {
		configData.set("LoadTopVoter.Monthly", true);
		rewardData.set("EnableMonthlyAwards", true);
		rewardData.set("MonthlyAwards.1.Rewards.Messages.Player", "You won!");
		rewardData.set("EnableWeeklyAwards", true);
		rewardData.createSection("WeeklyAwards.1.Rewards");
		rewardData.set("WeeklyAwards.2.Rewards", List.of());
		rewardData.set("WeeklyAwards.3.Rewards", "");
		rewardData.createSection("WeeklyAwards.4");
		rewardData.set("WeeklyAwards.5.Rewards", List.of(""));
		rewardData.set("EnableDailyRewards", true);

		check(configData, rewardData);

		verifyNoInteractions(logger);
	}

	@Test
	void ignoresMonthlyAwardsUsingDatedTotals() {
		configData.set("LoadTopVoter.Monthly", false);
		configData.set("UseMonthDateTotalsAsPrimaryTotal", true);
		rewardData.set("MonthlyAwards.1.Rewards", "example-reward");

		check(configData, rewardData);

		verifyNoInteractions(logger);
	}

	@Test
	void ignoresRewardsWithoutAnAwardableRank() {
		rewardData.set("EnableWeeklyAwards", true);
		rewardData.set("WeeklyAwards.2-2.Rewards", "example-reward");
		rewardData.set("WeeklyAwards.0.Rewards", "example-reward");
		rewardData.set("WeeklyAwards.invalid.Rewards", "example-reward");

		check(configData, rewardData);

		verifyNoInteractions(logger);
	}

	@Test
	void ignoresRewardsWhenLaterEntriesCoverEveryRank() {
		rewardData.set("EnableWeeklyAwards", true);
		rewardData.set("WeeklyAwards.1-4.Rewards", "example-reward");
		rewardData.set("WeeklyAwards.1.Rewards", List.of());
		rewardData.set("WeeklyAwards.2-4.Rewards", List.of(""));

		check(configData, rewardData);

		verifyNoInteractions(logger);
	}

	@Test
	void warnsWhenLaterEntriesLeaveOneRankWithAReward() {
		rewardData.set("EnableWeeklyAwards", true);
		rewardData.set("WeeklyAwards.1-2147483647.Rewards", "example-reward");
		rewardData.set("WeeklyAwards.2-2147483647.Rewards", List.of());

		checkSingleWarning("Weekly");
	}

	private void check(YamlConfiguration configYaml, YamlConfiguration rewardsYaml) {
		config.setData(configYaml);
		config.loadValues();
		rewards.setData(rewardsYaml);
		rewards.loadValues();
		VotingPluginConfigHealth health = new VotingPluginConfigHealth(plugin);
		health.check();
		assertFalse(health.hasYmlError());
	}

	private void checkSingleWarning(String period) {
		check(configData, rewardData);
		verify(logger).warning(contains(period + " top-voter awards are configured. Set LoadTopVoter." + period));
		verifyNoMoreInteractions(logger);
	}

	private static YamlConfiguration loadDefaults(String name) throws Exception {
		try (var stream = TopVoterAwardConfigHealthTest.class.getClassLoader().getResourceAsStream(name);
				var reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
			var defaults = new YamlConfiguration();
			defaults.load(reader);
			return defaults;
		}
	}
}
