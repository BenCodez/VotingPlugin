package com.bencodez.votingplugin.core.vote;

/** Durable state returned after the vote mutation has been applied. */
public record SharedVoteUserSnapshot(int allTimeTotal, int monthTotal, int weeklyTotal,
        int dailyTotal, int points) {
}
