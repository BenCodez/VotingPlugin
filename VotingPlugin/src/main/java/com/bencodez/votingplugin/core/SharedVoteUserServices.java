package com.bencodez.votingplugin.core;

import java.util.UUID;
import java.util.concurrent.CompletionStage;

/** Port for the existing user/cache/storage owner. */
public interface SharedVoteUserServices {
    CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity, SharedVoteMutation mutation);

    CompletionStage<SharedVoteUserSnapshot> load(UUID uuid);
}
