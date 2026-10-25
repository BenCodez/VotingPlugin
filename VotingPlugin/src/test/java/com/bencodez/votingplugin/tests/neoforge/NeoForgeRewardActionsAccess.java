package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.neoforge.*;
import java.util.concurrent.CompletionStage;

/** Test-only forwarding access to the non-public production NeoForgeRewardActions. */
@SuppressWarnings({"unchecked", "rawtypes"})
@FunctionalInterface
public interface NeoForgeRewardActionsAccess {
    CompletionStage<Void> execute(NeoForgeDeferredVote vote, NeoForgeRewardPlan plan);
}
