package com.bencodez.votingplugin.presets;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a fragment defined in a preset meta JSON file.  A
 * fragment contains the path to a YAML file and a path indicating
 * where the fragment should be merged into when applying the preset.
 * In the context of using only values from JSON (without YAML
 * fragments), this class is unused but included for completeness.
 */
@Getter
@Setter
public class Fragment {

    /**
     * The relative path to the fragment file.  This file typically
     * contains a YAML snippet defining a portion of the configuration.
     */
    private String path;

    /**
     * The dot‑separated path within the configuration to merge the
     * fragment into.  For example, {@code VoteSites} indicates the
     * fragment should be merged into the {@code VoteSites} section of
     * {@code VoteSites.yml}.
     */
    private String mergeInto;
}