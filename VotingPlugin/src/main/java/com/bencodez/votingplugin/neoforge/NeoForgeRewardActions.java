package com.bencodez.votingplugin.neoforge;

import java.util.concurrent.CompletionStage;

/** Platform action boundary for the initial NeoForge reward subset. */
interface NeoForgeRewardActions {
    CompletionStage<Void> execute(NeoForgeDeferredVote vote, NeoForgeRewardPlan plan);
}
