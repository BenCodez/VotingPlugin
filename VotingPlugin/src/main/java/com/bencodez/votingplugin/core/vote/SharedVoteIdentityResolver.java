package com.bencodez.votingplugin.core.vote;

import java.util.concurrent.CompletionStage;

/** Adapter to the existing AdvancedCore/user identity services. */
@FunctionalInterface
public interface SharedVoteIdentityResolver {
    CompletionStage<SharedVoteIdentity> resolve(SharedVoteInput input);
}
