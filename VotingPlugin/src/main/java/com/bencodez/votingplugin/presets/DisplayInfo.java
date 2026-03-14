package com.bencodez.votingplugin.presets;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents display information for a preset.  Display information
 * includes a name and a description used to describe the preset to
 * users.  This class is used when parsing meta JSON files.
 */
@Getter
@Setter
public class DisplayInfo {

    /**
     * The human‑readable name of the preset.  This name will appear in
     * preset selection UIs.
     */
    private String name;

    /**
     * A brief description of the preset.  This description helps users
     * understand what the preset is intended for.
     */
    private String description;
}