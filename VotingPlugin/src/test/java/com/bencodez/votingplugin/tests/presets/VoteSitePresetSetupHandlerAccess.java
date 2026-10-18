package com.bencodez.votingplugin.tests.presets;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.presets.*;
import com.bencodez.votingplugin.presets.VoteSitePresetSetupHandler.*;
import com.bencodez.votingplugin.VotingPluginMain;

/** Typed test-only access to package-private VoteSitePresetSetupHandler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteSitePresetSetupHandlerAccess {
    private VoteSitePresetSetupHandlerAccess() { }
    public static VoteSitePresetSetupHandler create(VotingPluginMain plugin) { return (VoteSitePresetSetupHandler) ReflectiveAccess.construct("com.bencodez.votingplugin.presets.VoteSitePresetSetupHandler", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static VoteSitePresetSetupHandler create(VotingPluginMain plugin, GitHubVoteSitePresetLoader loader) { return (VoteSitePresetSetupHandler) ReflectiveAccess.construct("com.bencodez.votingplugin.presets.VoteSitePresetSetupHandler", new String[]{"VotingPluginMain", "GitHubVoteSitePresetLoader"}, new Object[]{plugin, loader}); }
}
