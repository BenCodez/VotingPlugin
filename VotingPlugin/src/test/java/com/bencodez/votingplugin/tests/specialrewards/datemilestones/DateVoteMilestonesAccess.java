package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import java.util.function.BiConsumer;
import java.util.function.BooleanSupplier;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

/** Exposes only the existing path and progress scheduling test seams. */
public final class DateVoteMilestonesAccess {
    private DateVoteMilestonesAccess() { }
    public static String path(DateVoteEvent event, int threshold) {
        return (String) ReflectiveAccess.call(
                "com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones", null,
                "path", new String[]{"DateVoteEvent", "int"}, new Object[]{event, threshold});
    }
    public static void progress(DateVoteMilestones target, Player player,
            BiConsumer<DateVoteProgressSnapshot, BooleanSupplier> display) {
        ReflectiveAccess.call("com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones", target,
                "progress", new String[]{"Player", "BiConsumer"}, new Object[]{player, display});
    }
}
