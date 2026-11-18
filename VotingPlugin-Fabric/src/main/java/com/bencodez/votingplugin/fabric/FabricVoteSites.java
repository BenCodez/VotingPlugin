package com.bencodez.votingplugin.fabric;

import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.util.ServiceSiteValidator;

import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.error.YAMLException;
import org.yaml.snakeyaml.nodes.MappingNode;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.ScalarNode;
import org.yaml.snakeyaml.nodes.Tag;

/** Bounded parser and side-effect-free lookup for Fabric's optional site rules. */
record FabricVoteSites(List<FabricVoteSite> sites, boolean ambiguousAliases) {
    static final FabricVoteSites EMPTY = new FabricVoteSites(List.of(), false);
    private static final int MAX_BYTES = 64 * 1024;
    private static final int MAX_SITES = 256;
    private static final Set<String> FIELDS = Set.of("Enabled", "ServiceSite", "Name", "Priority",
            "WaitUntilVoteDelay", "VoteDelay", "VoteDelayMin", "VoteDelayDaily", "VoteDelayDailyHour");
    private static final Pattern DURATION_PART = Pattern.compile("([0-9]+)\\s*([a-z]+)");
    private static final Pattern INTEGER = Pattern.compile("[+-]?(?:0|[1-9][0-9]*)");
    private static final Pattern DECIMAL = Pattern.compile("[0-9]+(?:\\.[0-9]+)?");

    FabricVoteSites {
        sites = List.copyOf(sites);
    }

    static FabricVoteSites load(Path configRoot) throws IOException {
        Path directory = configRoot.resolve("votingplugin");
        if (!Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS)) {
            if (Files.notExists(directory, LinkOption.NOFOLLOW_LINKS)) return EMPTY;
            throw invalid("expected a regular configuration directory");
        }
        Path file = directory.resolve("VoteSites.yml");
        if (!Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) {
            if (Files.notExists(file, LinkOption.NOFOLLOW_LINKS)) return EMPTY;
            throw invalid("expected a regular configuration file");
        }
        byte[] bytes;
        try (var input = Files.newInputStream(file, LinkOption.NOFOLLOW_LINKS)) {
            bytes = input.readNBytes(MAX_BYTES + 1);
        } catch (NoSuchFileException removed) {
            return EMPTY;
        }
        if (bytes.length > MAX_BYTES) throw invalid("configuration exceeds 64 KiB");
        String text = StandardCharsets.UTF_8.newDecoder().decode(ByteBuffer.wrap(bytes)).toString();

        var options = new LoaderOptions();
        options.setCodePointLimit(MAX_BYTES);
        options.setNestingDepthLimit(4);
        options.setMaxAliasesForCollections(0);
        Node root;
        try {
            root = new Yaml(options).compose(new StringReader(text));
        } catch (YAMLException | IllegalArgumentException malformed) {
            throw invalid("invalid YAML document");
        }
        if (root == null) return EMPTY;
        Map<String, Node> document = mapping(root);
        if (document.isEmpty()) return EMPTY;
        if (!document.keySet().equals(Set.of("VoteSites"))) throw invalid("expected a VoteSites mapping");
        Map<String, Node> entries = mapping(document.get("VoteSites"));
        if (entries.size() > MAX_SITES) throw invalid("too many vote sites");
        Set<String> keys = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        Set<String> aliases = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        List<FabricVoteSite> sites = new ArrayList<>();
        boolean ambiguous = false;
        for (var entry : entries.entrySet()) {
            String key = entry.getKey();
            if (!ServiceSiteValidator.isValid(key) || !keys.add(key)) {
                throw invalid("invalid or duplicate site key");
            }
            FabricVoteSite site = site(key, mapping(entry.getValue()));
            sites.add(site);
            if (site.enabled()) {
                var siteAliases = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
                siteAliases.addAll(List.of(site.key(), site.name(), site.service()));
                for (String alias : siteAliases) {
                    if (!aliases.add(alias)) ambiguous = true;
                }
            }
        }
        sites.sort(Comparator.comparingInt(FabricVoteSite::priority).reversed());
        return new FabricVoteSites(sites, ambiguous);
    }

    Optional<FabricVoteSite> resolve(String identifier) {
        if (!ServiceSiteValidator.isValid(identifier)) return Optional.empty();
        return sites.stream().filter(site -> site.matches(identifier)).findFirst();
    }

    private static FabricVoteSite site(String key, Map<String, Node> fields) throws IOException {
        if (!FIELDS.containsAll(fields.keySet())) throw invalid("unsupported vote site setting");
        boolean enabled = bool(fields, "Enabled");
        String service = scalar(fields, "ServiceSite", Tag.STR, "");
        if ((enabled || !service.isEmpty()) && !ServiceSiteValidator.isValid(service)) {
            throw invalid("missing or invalid ServiceSite");
        }
        String name = scalar(fields, "Name", Tag.STR, key);
        if (name.isEmpty()) name = key;
        if (!ServiceSiteValidator.isValid(name)) throw invalid("invalid site Name");
        int hour = integer(fields, "VoteDelayDailyHour");
        if (hour < 0 || hour > 23) throw invalid("VoteDelayDailyHour must be 0–23");
        return new FabricVoteSite(key, service, name, enabled, integer(fields, "Priority"),
                bool(fields, "WaitUntilVoteDelay"), delay(fields), bool(fields, "VoteDelayDaily"), hour);
    }

    private static Map<String, Node> mapping(Node node) throws IOException {
        if (!(node instanceof MappingNode map) || !Tag.MAP.equals(node.getTag()) || node.getAnchor() != null) {
            throw invalid("expected an unaliased mapping");
        }
        Map<String, Node> result = new LinkedHashMap<>();
        for (var pair : map.getValue()) {
            String key = scalar(pair.getKeyNode(), Tag.STR);
            if (result.putIfAbsent(key, pair.getValueNode()) != null) throw invalid("duplicate mapping key");
        }
        return result;
    }

    private static String scalar(Node node, Tag tag) throws IOException {
        if (!(node instanceof ScalarNode value) || !tag.equals(node.getTag()) || node.getAnchor() != null) {
            throw invalid("invalid or aliased YAML value");
        }
        return value.getValue();
    }

    private static String scalar(Map<String, Node> fields, String key, Tag tag, String fallback) throws IOException {
        return fields.containsKey(key) ? scalar(fields.get(key), tag) : fallback;
    }

    private static boolean bool(Map<String, Node> fields, String key) throws IOException {
        return switch (scalar(fields, key, Tag.BOOL, "false").toLowerCase(Locale.ROOT)) {
            case "true", "yes", "on" -> true;
            case "false", "no", "off" -> false;
            default -> throw invalid("invalid boolean setting");
        };
    }

    private static int integer(Map<String, Node> fields, String key) throws IOException {
        String value = scalar(fields, key, Tag.INT, "0");
        if (!INTEGER.matcher(value).matches()) throw invalid("expected a decimal integer setting");
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException invalidInteger) {
            throw invalid("invalid integer setting");
        }
    }

    private static long delay(Map<String, Node> fields) throws IOException {
        Node hours = fields.get("VoteDelay");
        Node minutes = fields.get("VoteDelayMin");
        try {
            // Existing formatted VoteDelay values take precedence over numeric VoteDelayMin.
            if (hours != null && Tag.STR.equals(hours.getTag())) {
                long formatted = formattedDuration(scalar(hours, Tag.STR));
                if (minutes != null) decimalMillis(minutes, 60_000L);
                return formatted;
            }
            return Math.addExact(decimalMillis(hours, 3_600_000L), decimalMillis(minutes, 60_000L));
        } catch (ArithmeticException | NumberFormatException invalidDuration) {
            throw invalid("vote delay exceeds the supported range");
        }
    }

    private static long decimalMillis(Node node, long unit) throws IOException {
        if (node == null) return 0L;
        Tag tag = Tag.INT.equals(node.getTag()) ? Tag.INT : Tag.FLOAT;
        String value = scalar(node, tag);
        Pattern syntax = Tag.INT.equals(tag) ? INTEGER : DECIMAL;
        if (value.length() > 32 || value.startsWith("-") || !syntax.matcher(value).matches()) {
            throw invalid("expected a nonnegative decimal vote delay");
        }
        return new BigDecimal(value).multiply(BigDecimal.valueOf(unit)).setScale(0, RoundingMode.DOWN).longValueExact();
    }

    private static long formattedDuration(String value) throws IOException {
        String text = value.strip().toLowerCase(Locale.ROOT);
        if (text.length() > 128) throw invalid("invalid formatted vote delay");
        if (text.matches("[0-9]+")) return Math.multiplyExact(Long.parseLong(text), 3_600_000L);
        var matcher = DURATION_PART.matcher(text);
        long result = 0L;
        int end = 0;
        while (matcher.find()) {
            if (!text.substring(end, matcher.start()).isBlank()) throw invalid("invalid formatted vote delay");
            long unit = switch (matcher.group(2)) {
                case "ms", "millisecond", "milliseconds" -> 1L;
                case "s", "sec", "second", "seconds" -> 1_000L;
                case "m", "min", "minute", "minutes" -> 60_000L;
                case "h", "hr", "hour", "hours" -> 3_600_000L;
                case "d", "day", "days" -> 86_400_000L;
                case "w", "week", "weeks" -> 604_800_000L;
                case "mo", "month", "months" -> 2_592_000_000L;
                default -> throw invalid("unsupported vote delay unit");
            };
            result = Math.addExact(result, Math.multiplyExact(Long.parseLong(matcher.group(1)), unit));
            end = matcher.end();
        }
        if (end != text.length() || end == 0) throw invalid("invalid formatted vote delay");
        return result;
    }

    private static IOException invalid(String reason) {
        return new InvalidSitesException(reason);
    }

    static final class InvalidSitesException extends IOException {
        private InvalidSitesException(String reason) {
            super(reason);
        }
    }
}
