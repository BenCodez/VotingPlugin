package com.bencodez.votingplugin.streaks;

import java.util.List;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;

/** Read-only observations of existing state; never evidence that a reward was delivered. */
public record VoteStreaksSnapshot(List<Period> periods, List<Milestone> milestones, boolean truncated) {
    public static final int MAX_MILESTONES = 512;
    public static final int PAGE_SIZE = 5;
    public VoteStreaksSnapshot {
        periods = List.copyOf(periods);
        milestones = List.copyOf(milestones);
        if (periods.size() != 3 || milestones.size() > MAX_MILESTONES) throw new IllegalArgumentException("Invalid streak snapshot size");
    }
    public record Period(VoteStreakType type, int current, int best) {
        public Period { current = Math.max(0, current); best = Math.max(current, best); }
    }
    public record Milestone(String id, VoteStreakType type, int current, int best,
            int threshold, int votesRequired, boolean recurring) {
        public Milestone {
            current = Math.max(0, current); best = Math.max(current, best);
            threshold = Math.max(1, threshold); votesRequired = Math.max(1, votesRequired);
        }
        public long nextThreshold() {
            return recurring ? ((long) current / threshold + 1L) * threshold : threshold;
        }
    }
    public int pages() { return Math.max(1, (milestones.size() + PAGE_SIZE - 1) / PAGE_SIZE); }
    public int clampPage(int page) { return Math.max(0, Math.min(page, pages() - 1)); }
    public List<Milestone> page(int page) {
        int start = clampPage(page) * PAGE_SIZE;
        return milestones.subList(start, Math.min(start + PAGE_SIZE, milestones.size()));
    }
    public static String unit(VoteStreakType type) {
        return switch (type) { case DAILY -> "days"; case WEEKLY -> "weeks"; case MONTHLY -> "months"; };
    }
}
