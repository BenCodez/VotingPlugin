package com.bencodez.votingplugin.tests.discord;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doAnswer;

import java.util.LinkedHashMap;
import java.util.function.Consumer;
import java.util.function.LongFunction;

import com.bencodez.votingplugin.discord.DiscordHandler;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

import github.scarsz.discordsrv.dependencies.jda.api.entities.TextChannel;

/** Test-only access to the native publication and channel seams. */
final class DiscordHandlerAccess {
    private static final String OWNER = "com.bencodez.votingplugin.discord.DiscordHandler";

    private DiscordHandlerAccess() { }

    static void publish(DiscordHandler target, TopVoter period,
            LinkedHashMap<TopVoterPlayer, Integer> rows) {
        ReflectiveAccess.call(OWNER, target, "publishTopVoterMessage",
                new String[]{"TopVoter", "LinkedHashMap"}, new Object[]{period, rows});
    }

    static void recordPublications(DiscordHandler target,
            Consumer<LinkedHashMap<TopVoterPlayer, Integer>> observer) {
        var stub = doAnswer(call -> {
            observer.accept(call.getArgument(1));
            return null;
        }).when(target);
        ReflectiveAccess.call(OWNER, stub, "publishTopVoterMessage",
                new String[]{"TopVoter", "LinkedHashMap", "long"},
                new Object[]{any(TopVoter.class), any(LinkedHashMap.class), anyLong()});
    }

    static void channel(DiscordHandler target, LongFunction<TextChannel> lookup) {
        var stub = doAnswer(call -> lookup.apply(call.getArgument(0))).when(target);
        ReflectiveAccess.call(OWNER, stub, "leaderboardChannel",
                new String[]{"long"}, new Object[]{anyLong()});
    }
}
