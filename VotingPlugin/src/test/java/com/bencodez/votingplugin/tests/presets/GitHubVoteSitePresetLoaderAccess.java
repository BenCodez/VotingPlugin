package com.bencodez.votingplugin.tests.presets;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.presets.*;
import com.bencodez.votingplugin.presets.GitHubVoteSitePresetLoader.*;
import java.io.IOException;
import java.util.List;

/** Typed test-only access to package-private GitHubVoteSitePresetLoader seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class GitHubVoteSitePresetLoaderAccess {
    public static final int MAX_PRESET_COUNT = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.presets.GitHubVoteSitePresetLoader", "MAX_PRESET_COUNT");
    private GitHubVoteSitePresetLoaderAccess() { }
    public static List<String> parsePresetPaths(byte[] body) throws IOException { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.presets.GitHubVoteSitePresetLoader", null, "parsePresetPaths", new String[]{"byte[]"}, new Object[]{body}); }
    public static boolean isPresetPathAllowed(String path) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.presets.GitHubVoteSitePresetLoader", null, "isPresetPathAllowed", new String[]{"String"}, new Object[]{path}); }
}
