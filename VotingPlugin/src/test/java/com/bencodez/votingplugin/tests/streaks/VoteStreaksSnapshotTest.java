package com.bencodez.votingplugin.tests.streaks;

import com.bencodez.votingplugin.streaks.VoteStreaksSnapshot;

import static org.junit.jupiter.api.Assertions.*;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;

class VoteStreaksSnapshotTest {
    static List<VoteStreaksSnapshot.Period> periods() {
        return List.of(new VoteStreaksSnapshot.Period(VoteStreakType.DAILY, 3, 7),
                new VoteStreaksSnapshot.Period(VoteStreakType.WEEKLY, 1, 2),
                new VoteStreaksSnapshot.Period(VoteStreakType.MONTHLY, 0, 1));
    }
    static VoteStreaksSnapshot snapshot(int count) {
        var rows = new ArrayList<VoteStreaksSnapshot.Milestone>();
        for (int i = 1; i <= count; i++) rows.add(new VoteStreaksSnapshot.Milestone("milestone-" + i,
                VoteStreakType.DAILY, 3, 7, i, 1, false));
        return new VoteStreaksSnapshot(periods(), rows, false);
    }
    @Test void pagesClampEmptyNegativeAndExcessiveNavigation() {
        assertEquals(1, snapshot(0).pages()); assertTrue(snapshot(0).page(Integer.MAX_VALUE).isEmpty());
        var data = snapshot(12); assertEquals(3, data.pages());
        assertEquals("milestone-1", data.page(-1).getFirst().id());
        assertEquals(List.of("milestone-11", "milestone-12"), data.page(Integer.MAX_VALUE).stream().map(VoteStreaksSnapshot.Milestone::id).toList());
    }
    @Test void collectionsAreDetachedAndCannotBeChanged() {
        var rows = new ArrayList<>(snapshot(1).milestones());
        var data = new VoteStreaksSnapshot(periods(), rows, false); rows.clear();
        assertEquals(1, data.milestones().size()); assertThrows(UnsupportedOperationException.class, () -> data.page(0).clear());
        assertThrows(IllegalArgumentException.class, () -> new VoteStreaksSnapshot(List.of(), List.of(), false));
    }
    @Test void recurrenceUsesExistingIntervalMultiplesWithoutIntegerOverflow() {
        var row = new VoteStreaksSnapshot.Milestone("recurring", VoteStreakType.MONTHLY, Integer.MAX_VALUE,
                Integer.MAX_VALUE, Integer.MAX_VALUE, 1, true);
        assertEquals(4294967294L, row.nextThreshold());
        assertEquals(6, new VoteStreaksSnapshot.Milestone("repeat", VoteStreakType.WEEKLY, 3, 3, 3, 1, true).nextThreshold());
        assertEquals("weeks", VoteStreaksSnapshot.unit(VoteStreakType.WEEKLY));
    }
}
