package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import java.nio.file.Path;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteLedger;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

/** Preserves the native ledger and its bounded-capacity constructor checks. */
public final class DateVoteLedgerAccess {
    private DateVoteLedgerAccess() { }
    public static DateVoteLedger create(Path directory, int capacity) {
        return (DateVoteLedger) ReflectiveAccess.construct(
                "com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteLedger",
                new String[]{"Path", "int"}, new Object[]{directory, capacity});
    }
}
