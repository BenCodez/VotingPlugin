package com.bencodez.votingplugin.neoforge;

import java.util.List;

/** Immutable ordered reward work selected for one retained NeoForge vote. */
public record NeoForgeRewardPlan(Status status, List<Action> actions, boolean requiresOnline, String detail) {
    public NeoForgeRewardPlan {
        actions = List.copyOf(actions);
    }

    public enum Status { READY, WAITING_FOR_PLAYER, BLOCKED_UNSUPPORTED }
    public enum ActionType { PLAYER_MESSAGE, CONSOLE_COMMAND }
    public record Action(ActionType type, String value) { }

    public boolean ready() { return status == Status.READY; }
}
