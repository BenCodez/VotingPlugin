package com.bencodez.votingplugin.core.datemilestones;

import java.util.List;
import java.util.OptionalInt;
import java.util.Set;

/** Immutable worker result; viewing this snapshot never mutates accounting or rewards. */
public record DateVoteProgressSnapshot(long capturedAt, List<Event> events) {
    public DateVoteProgressSnapshot { events = List.copyOf(events); }
    public enum Availability { OWNED, REMOTE, UNAVAILABLE }
    public record Milestone(int threshold, String id, boolean enabled, List<String> rewardTypes) {
        public Milestone { rewardTypes = List.copyOf(rewardTypes); }
    }
    public record Event(DateVoteEvent definition, Availability availability, int votes,
            Set<Integer> submitted, Set<Integer> deferred, Set<Integer> reserved, List<Milestone> milestones) {
        public Event {
            submitted = Set.copyOf(submitted); deferred = Set.copyOf(deferred); reserved = Set.copyOf(reserved);
            milestones = List.copyOf(milestones);
        }
        public String status(long now) {
            return !definition.enabled() ? "disabled" : now < definition.start() ? "upcoming"
                    : now >= definition.end() ? "ended" : "active";
        }
        public OptionalInt nextThreshold() {
            return availability == Availability.OWNED ? milestones.stream().filter(m -> m.enabled() && m.threshold() > votes)
                    .mapToInt(Milestone::threshold).min() : OptionalInt.empty();
        }
        public String awardState(int threshold) {
            if (availability != Availability.OWNED) return "progress unavailable";
            if (reserved.contains(threshold)) return "pending review";
            if (submitted.contains(threshold)) return "submitted";
            if (deferred.contains(threshold)) return "deferred";
            return votes >= threshold ? "reached" : "not reached";
        }
    }
}
