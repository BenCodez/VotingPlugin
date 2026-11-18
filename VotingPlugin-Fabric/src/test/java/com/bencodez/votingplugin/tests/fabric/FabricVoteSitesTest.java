package com.bencodez.votingplugin.tests.fabric;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class FabricVoteSitesTest {
    private static final String TYPE = "com.bencodez.votingplugin.fabric.FabricVoteSites";
    @TempDir Path configRoot;

    @Test
    void absentAndEmptyConfigurationDoNotCreateRulesOrFiles() throws Throwable {
        assertTrue(resolve(load(), "example.test").isEmpty());
        assertFalse(Files.exists(configRoot.resolve("votingplugin")));
        for (String content : List.of("", "# No sites\n", "{}", "VoteSites: {}")) {
            write(content);
            assertTrue(resolve(load(), "example.test").isEmpty());
        }
    }

    @Test
    void matchesSupportedAliasesAndUsesPriorityThenFileOrder() throws Throwable {
        write("""
                VoteSites:
                  Low: {Enabled: true, ServiceSite: example.test, Priority: 1}
                  First: {Enabled: true, ServiceSite: example.test, Priority: 9, Name: Display Title}
                  Tied: {Enabled: true, ServiceSite: example.test, Priority: 9}
                  Disabled: {Enabled: false, ServiceSite: disabled.test, Priority: 100}
                """);
        Object sites = load();
        assertEquals("First", key(resolve(sites, "EXAMPLE.TEST").orElseThrow()));
        assertEquals("First", key(resolve(sites, "display title").orElseThrow()));
        assertEquals("Tied", key(resolve(sites, "tIeD").orElseThrow()));
        assertEquals(true, call(sites, sites.getClass(), "ambiguousAliases", new Class<?>[0]));
        assertTrue(resolve(sites, "disabled.test").isEmpty());
        assertTrue(resolve(sites, "unknown.test").isEmpty());
        assertTrue(resolve(sites, "exa\nmple.test").isEmpty());
        assertTrue(resolve(sites, "x".repeat(2049)).isEmpty());
        assertTrue(resolve(sites, "  ").isEmpty());
        assertTrue(resolve(sites, null).isEmpty());

        write("VoteSites: {Only: {Enabled: true, ServiceSite: only.test}}");
        assertTrue(resolve(sites, "only.test").isEmpty());
        assertTrue(resolve(load(), "only.test").isPresent());
    }

    @Test
    void rejectsWholeInvalidDocuments() throws IOException {
        for (String content : List.of(
                "VoteSites: {First: {}, FIRST: {}}",
                "VoteSites: {First: {}, First: {}}",
                "VoteSites: {First: {Priority: 1, Priority: 2}}",
                "VoteSites: {First: {Enabled: true}}",
                "VoteSites: {First: {Enabled: true, ServiceSite: 'a[b'}}",
                "VoteSites: {First: {VoteDelay: -1}}",
                "VoteSites: {First: {VoteDelay: 100000000000000000000000h}}",
                "VoteSites: {First: {VoteDelayDailyHour: 24}}",
                "VoteSites: {First: {Enabled: 'false'}}",
                "VoteSites: {First: {Enabled: true, ServiceSite: first.test}, Bad: {Priority: text}}",
                "VoteSites: {First: {Unknown: true}}",
                "VoteSites: {First: {Name: &name test, ServiceSite: *name}}",
                "VoteSites: {First: &site {Enabled: false}, Second: *site}",
                "VoteSites: {First: {!!str '<<': {Enabled: false}}}",
                "VoteSites: {First: {Name: !!java.lang.String ignored}}",
                "VoteSites: {}\n---\nVoteSites: {}",
                "VoteSites: []",
                " ".repeat(65_537))) {
            write(content);
            assertThrows(IOException.class, this::load, content.substring(0, Math.min(100, content.length())));
        }
    }

    @Test
    void rejectsNonUtf8AndSymlinkInputsWithoutWritingThem() throws IOException {
        Path file = write("");
        Files.write(file, new byte[] {(byte) 0xc3, 0x28});
        assertThrows(IOException.class, this::load);
        Files.delete(file);
        Path target = Files.writeString(configRoot.resolve("target.yml"), "VoteSites: {}");
        Files.createSymbolicLink(file, target);
        assertThrows(IOException.class, this::load);
        assertEquals("VoteSites: {}", Files.readString(target));
    }

    @Test
    void intervalGateUsesStrictBoundaryAndLegacyZeroDelay() throws Throwable {
        write("""
                VoteSites:
                  Site: {Enabled: true, ServiceSite: site.test, WaitUntilVoteDelay: true,
                    VoteDelay: 0.5, VoteDelayMin: 15}
                  Zero: {Enabled: true, ServiceSite: zero.test, WaitUntilVoteDelay: true}
                  Ungated: {Enabled: true, ServiceSite: ungated.test}
                  Text: {Enabled: true, ServiceSite: text.test, WaitUntilVoteDelay: true, VoteDelay: 30m 15m,
                    VoteDelayMin: 99}
                """);
        Object rules = load();
        Object site = resolve(rules, "site.test").orElseThrow();
        Instant last = Instant.parse("2026-01-01T12:00:00Z");
        Instant equal = last.plusSeconds(45 * 60);
        assertTrue(allows(site, 0L, last, ZoneOffset.UTC));
        assertFalse(allows(site, last.toEpochMilli(), equal.minusMillis(1), ZoneOffset.UTC));
        assertFalse(allows(site, last.toEpochMilli(), equal, ZoneOffset.UTC));
        assertTrue(allows(site, last.toEpochMilli(), equal.plusMillis(1), ZoneOffset.UTC));
        assertFalse(allows(resolve(rules, "text.test").orElseThrow(), last.toEpochMilli(), equal, ZoneOffset.UTC));
        assertTrue(allows(resolve(rules, "text.test").orElseThrow(), last.toEpochMilli(), equal.plusMillis(1), ZoneOffset.UTC));
        Object zero = resolve(rules, "zero.test").orElseThrow();
        assertTrue(allows(zero, 0L, equal, ZoneOffset.UTC));
        assertFalse(allows(zero, last.toEpochMilli(), equal, ZoneOffset.UTC));
        assertTrue(allows(resolve(rules, "ungated.test").orElseThrow(), last.toEpochMilli(), last, ZoneOffset.UTC));
    }

    @Test
    void dailyRolloverUsesExplicitZoneAndWallClockAcrossDst() throws Throwable {
        write("""
                VoteSites:
                  Daily: {Enabled: true, ServiceSite: daily.test, WaitUntilVoteDelay: true,
                    VoteDelayDaily: true, VoteDelayDailyHour: 2}
                  Interval: {Enabled: true, ServiceSite: interval.test, WaitUntilVoteDelay: true, VoteDelay: 1h}
                """);
        Object rules = load();
        Object daily = resolve(rules, "daily.test").orElseThrow();
        ZoneId zone = ZoneId.of("America/New_York");
        long beforeSpringGap = Instant.parse("2026-03-08T06:30:00Z").toEpochMilli();
        Instant justBefore = Instant.parse("2026-03-08T06:59:59.999Z");
        Instant after = Instant.parse("2026-03-08T07:00:00Z");
        assertFalse(allows(daily, beforeSpringGap, justBefore, zone));
        assertTrue(allows(daily, beforeSpringGap, after, zone));
        assertFalse(allows(daily, beforeSpringGap, after, ZoneOffset.UTC));
        assertTrue(allows(resolve(rules, "interval.test").orElseThrow(), beforeSpringGap, after, zone));

        long priorDay = Instant.parse("2026-03-08T07:10:00Z").toEpochMilli();
        Instant exactReset = Instant.parse("2026-03-09T06:00:00Z");
        assertFalse(allows(daily, priorDay, exactReset, zone));
        assertTrue(allows(daily, priorDay, exactReset.plusMillis(1), zone));

        long beforeFallOverlap = Instant.parse("2026-11-01T04:30:00Z").toEpochMilli();
        assertFalse(allows(daily, beforeFallOverlap, Instant.parse("2026-11-01T06:55:00Z"), zone));
        assertFalse(allows(daily, beforeFallOverlap, Instant.parse("2026-11-01T07:00:00Z"), zone));
        assertTrue(allows(daily, beforeFallOverlap, Instant.parse("2026-11-01T07:00:00.001Z"), zone));
    }

    private Path write(String content) throws IOException {
        return Files.writeString(Files.createDirectories(configRoot.resolve("votingplugin")).resolve("VoteSites.yml"),
                content);
    }

    private Object load() throws Throwable {
        return call(null, Class.forName(TYPE), "load", new Class<?>[] {Path.class}, configRoot);
    }

    private static Optional<?> resolve(Object sites, String identifier) throws Throwable {
        return (Optional<?>) call(sites, sites.getClass(), "resolve", new Class<?>[] {String.class}, identifier);
    }

    private static Object key(Object site) throws Throwable {
        return call(site, site.getClass(), "key", new Class<?>[0]);
    }

    private static boolean allows(Object site, long previous, Instant now, ZoneId zone) throws Throwable {
        return (boolean) call(site, site.getClass(), "canVote", new Class<?>[] {long.class, Clock.class, ZoneId.class},
                previous, Clock.fixed(now, ZoneOffset.UTC), zone);
    }

    private static Object call(Object receiver, Class<?> type, String method, Class<?>[] parameters,
            Object... arguments) throws Throwable {
        var target = type.getDeclaredMethod(method, parameters);
        target.setAccessible(true);
        try {
            return target.invoke(receiver, arguments);
        } catch (InvocationTargetException failure) {
            throw failure.getCause();
        }
    }
}
