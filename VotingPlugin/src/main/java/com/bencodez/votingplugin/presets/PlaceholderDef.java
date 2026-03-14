package com.bencodez.votingplugin.presets;

import lombok.Getter;
import lombok.Setter;
import com.google.gson.annotations.SerializedName;

/**
 * Represents a placeholder definition loaded from a preset meta JSON file.
 *
 * The VotingPlugin presets define a set of placeholders that may be
 * substituted when creating vote site entries.  Each placeholder has a
 * type (for example, {@code string}), a human‑readable label, and a
 * default value.  This class is a simple container for these fields so
 * they can be deserialized via Gson and accessed via Lombok‑generated
 * getters and setters.
 */
@Getter
@Setter
public class PlaceholderDef {
    /**
     * The data type of this placeholder (e.g. {@code string}).  The type
     * information is primarily for user interfaces and may be ignored when
     * programmatically applying the preset.
     */
    private String type;

    /**
     * A human‑readable label describing the placeholder.  This is used by
     * external tools (such as the web preset generator) to prompt for
     * values.
     */
    private String label;

    /**
     * The default value for the placeholder. If the user does not supply a
     * value when applying the preset, this value will be used instead.
     * <p>
     * The JSON meta files use the key {@code "default"} for this value. Because
     * {@code default} is a reserved keyword in Java, we cannot name this
     * field {@code default}. Instead, we annotate this field with
     * {@link SerializedName @SerializedName("default")} so that Gson
     * will map the {@code "default"} property in the JSON to this field.
     */
    @SerializedName("default")
    private String defaultValue;
}