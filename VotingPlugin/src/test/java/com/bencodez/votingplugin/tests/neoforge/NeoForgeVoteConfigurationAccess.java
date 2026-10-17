package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeVoteConfiguration.*;
import java.util.Optional;
import org.spongepowered.configurate.ConfigurationNode;

/** Typed test-only access to package-private NeoForgeVoteConfiguration seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeVoteConfigurationAccess {
    private NeoForgeVoteConfigurationAccess() { }

    public static Optional<NeoForgeVoteSite> configuredSite(NeoForgeVoteConfiguration target, String key) { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeVoteConfiguration", target, "configuredSite", new String[]{"String"}, new Object[]{key}); return rawResult == null ? null : ((java.util.Optional<?>) rawResult).map(value -> (NeoForgeVoteSite) value); }

}
