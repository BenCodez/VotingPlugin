package com.bencodez.votingplugin.experimental;

/** Stable configuration and command identities; styles never silently substitute for each other. */
public enum ExperimentalGUIType {
    HOLOGRAM("Hologram", "testhologram"),
    ANIMATED_INVENTORY("AnimatedInventory", "testinventorygui"),
    NPC("NPC", "testnpcgui"),
    STREAK_TRACK("StreakTrack", "teststreakgui"),
    RADIAL("Radial", "testradialgui"),
    NATIVE_DIALOG("NativeDialog", "testdialoggui"),
    REWARD_SHOWCASE("RewardShowcase", "testshowcasegui"),
    VOTING_TERMINAL("VotingTerminal", "testterminalgui");

    private final String configurationKey;
    private final String command;

    ExperimentalGUIType(String configurationKey, String command) {
        this.configurationKey = configurationKey;
        this.command = command;
    }

    public String configurationKey() { return configurationKey; }
    public String command() { return command; }
}
