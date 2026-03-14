package com.bencodez.votingplugin.presets;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents matching information for a preset.  Match information
 * identifies which domains or keywords should match this preset.  This
 * class is used for deserializing preset meta JSON files but is
 * otherwise optional when programmatically applying the preset.
 */
@Getter
@Setter
public class MatchInfo {

    /**
     * Domains associated with the preset.  These are used by the preset
     * generator to automatically select a preset based on a vote site
     * hostname.
     */
    private java.util.List<String> domains;

    /**
     * Keywords associated with the preset.  These can be used to search
     * presets in user interfaces.
     */
    private java.util.List<String> keywords;
}