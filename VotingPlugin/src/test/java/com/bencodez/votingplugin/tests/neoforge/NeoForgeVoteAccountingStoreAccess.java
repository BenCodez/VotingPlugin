package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeVoteAccountingStore.*;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Typed test-only access to package-private NeoForgeVoteAccountingStore seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeVoteAccountingStoreAccess {
    public static final String PLAYER_NAME = (String) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeVoteAccountingStore", "PLAYER_NAME");
    private NeoForgeVoteAccountingStoreAccess() { }


}
