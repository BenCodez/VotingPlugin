package com.bencodez.votingplugin.presets;

import lombok.Getter;
import lombok.Setter;
import java.util.Map;
import java.util.List;

/**
 * Represents a vote site preset as defined in a meta JSON file.
 *
 * The meta JSON file defines metadata about the preset such as display
 * information, matching criteria, placeholder definitions and optional YAML
 * fragments.  This class provides a simple POJO for deserialization via
 * Gson.  Only the {@code placeholders} field is strictly necessary when
 * applying the preset programmatically; the other fields are included
 * for completeness.
 */
@Getter
@Setter
public class VoteSitePreset {

    /**
     * The schema version of the preset definition.  This may be used to
     * handle future format changes.
     */
    private int schemaVersion;

    /**
     * The unique identifier of the preset (e.g. {@code votesite:generic}).
     */
    private String id;

    /**
     * Human‑friendly display information for the preset.
     */
    private DisplayInfo display;

    /**
     * Matching information that can be used to automatically select a
     * preset based on domain or keywords.  Optional when applying
     * programmatically.
     */
    private MatchInfo match;

    /**
     * Definitions of placeholders used by this preset.  Each entry in
     * the map corresponds to a placeholder name and defines its type,
     * label and default value.
     */
    private Map<String, PlaceholderDef> placeholders;

    /**
     * A list of YAML fragments referenced by this preset.  When not using
     * YAML, this field is unused but retained for completeness.
     */
    private List<Fragment> fragments;

    /**
     * Whether the preset has been verified by the project maintainers.
     */
    private boolean verified;
}