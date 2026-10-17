package com.bencodez.votingplugin.tests.topvoter;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.topvoter.*;
import com.bencodez.votingplugin.topvoter.TopVoterLoader.*;
import java.time.YearMonth;

/** Typed test-only access to package-private TopVoterLoader seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class TopVoterLoaderAccess {
    private TopVoterLoaderAccess() { }
    public static YearMonth parseMonthColumn(String column) { return (YearMonth) ReflectiveAccess.call("com.bencodez.votingplugin.topvoter.TopVoterLoader", null, "parseMonthColumn", new String[]{"String"}, new Object[]{column}); }
}
