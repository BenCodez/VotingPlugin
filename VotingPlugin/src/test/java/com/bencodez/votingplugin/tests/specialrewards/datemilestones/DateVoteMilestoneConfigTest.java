package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import com.bencodez.votingplugin.specialrewards.datemilestones.*;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.stream.Stream;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

class DateVoteMilestoneConfigTest {
    @Test void documentedCompleteOwnerExampleLoadsThroughTheProductionParser() throws Exception {
        var path = java.nio.file.Path.of(System.getProperty("basedir")).getParent().resolve("docs/date-vote-milestones.md");
        String guide = java.nio.file.Files.readString(path);
        String yaml = guide.split("```yaml\\n", 2)[1].split("```", 2)[0];
        var config = new YamlConfiguration(); config.loadFromString(yaml);
        var event = DateVoteMilestones.parse("october_2026", config.getConfigurationSection("DateVoteMilestones.october_2026"));
        assertTrue(event.enabled()); assertEquals(List.of(10, 25), event.thresholds());
        assertEquals("", event.accountingServer());
        assertEquals("give %player% diamond 1", config.getStringList("DateVoteMilestones.october_2026.Milestones.TwentyFiveVotes.Rewards.Commands").getFirst());
    }
    private YamlConfiguration complete(String milestones, String newline) throws Exception {
        String yaml = """
                VoteMilestones:
                  unrelated:
                    At: 75
                    Rewards: {Commands: ['say literal At: 3']}
                DateVoteMilestones:
                  festival:
                    Enabled: true
                    DisplayName: 'Fête 🎉 — At: 99'
                    Start: '2026-10-01T00:00:00'
                    End: '2026-11-01T00:00:00'
                    Timezone: UTC
                    Milestones:
                """ + milestones.indent(6);
        var config = new YamlConfiguration(); config.loadFromString(yaml.replace("\n", newline));
        return config;
    }
    @TestFactory Stream<DynamicTest> fullYamlAtRepresentationsRetainTheSameAccountingContract() {
        return Stream.of("1", "'1'", "[0, 1]", "['0..1 step 1']", "['1..0']").flatMap(at ->
                Stream.of("\n", "\r\n").map(newline -> DynamicTest.dynamicTest(at + " line=" + newline.length(), () -> {
                    var named = complete("first:\n  At: " + at + "\n  Rewards: {Messages: {Player: 'At: 99'}}\n", newline);
                    var legacy = complete("'1':\n  Rewards: {Messages: {Player: 'At: 99'}}\n", newline);
                    var a = DateVoteMilestones.parse("festival", named.getConfigurationSection("DateVoteMilestones.festival"));
                    var b = DateVoteMilestones.parse("festival", legacy.getConfigurationSection("DateVoteMilestones.festival"));
                    assertEquals(List.of(1), a.thresholds()); assertEquals(b.fingerprint(), a.fingerprint());
                    assertEquals(a, DateVoteMilestones.parse("festival", named.getConfigurationSection("DateVoteMilestones.festival")));
                    assertEquals(75, named.getInt("VoteMilestones.unrelated.At"));
                    assertEquals("At: 99", named.getString("DateVoteMilestones.festival.Milestones.first.Rewards.Messages.Player"));
                })));
    }
    @Test void rangesEveryAndAtUseABoundedUnion() throws Exception {
        var config = complete("bonus:\n  At: [0, '10..30 step 10', 128]\n  Every: 128\n  Rewards: {}\n", "\n");
        var event = DateVoteMilestones.parse("festival", config.getConfigurationSection("DateVoteMilestones.festival"));
        assertEquals(36, event.thresholds().size()); assertTrue(event.thresholds().containsAll(List.of(1, 10, 20, 30, 128, 4096)));
    }
    @TestFactory Stream<DynamicTest> invalidAtCannotCoerceOrTruncateIntoAnAward() {
        return Stream.of("true", "1.5", "[]", "[1, nope]", "[1, null]", "[[1]]", "{At: 1}",
                "4097", "99999999999999999999", "'1..4096'", "'1..2 step 0'", "'1..2 step 1 trailing'",
                "'1..2..'", "'١'", "'1\\n2'").map(at -> DynamicTest.dynamicTest(at, () -> {
                    var config = complete("bad:\n  At: " + at + "\n  Rewards: {}\n", "\n");
                    assertThrows(IllegalArgumentException.class, () -> DateVoteMilestones.parse("festival",
                            config.getConfigurationSection("DateVoteMilestones.festival")));
                }));
    }
    @TestFactory Stream<DynamicTest> ambiguousOrUnsupportedEntriesFailClosed() {
        return Stream.of("first:\n  Every: 1\n  Rewards: {}\n", "first:\n  Enabled: 'false'\n  At: 1\n  Rewards: {}\n",
                "first:\n  At: 1\n  Total: AllTime\n  Rewards: {}\n", "first:\n  At: 1\n  Rewards: {}\nsecond:\n  At: 1\n  Rewards: {}\n",
                "'01':\n  Rewards: {}\n", "first:\n  At: 1\n  Rewards: 'Commands: []'\n")
                .map(yaml -> DynamicTest.dynamicTest(yaml.lines().findFirst().orElseThrow(), () -> {
                    var config = complete(yaml, "\n");
                    assertThrows(IllegalArgumentException.class, () -> DateVoteMilestones.parse("festival",
                            config.getConfigurationSection("DateVoteMilestones.festival")));
                }));
    }
}
