# Check milestone eligibility after editing totals

Run `/av votemilestone check <player>` to evaluate the player's
current stored vote totals now. Permission:
`VotingPlugin.Commands.AdminVote.VoteMilestone.Check` (or `VotingPlugin.Admin`).
Console and offline player names are supported. The normal `VoteMilestone`/`VM`
aliases, case-insensitive invocation and `(player)` argument resolution/tab
completion match the neighboring milestone commands. `/av vm check <player>` is
the short form. The player-name argument is required.

This checks legacy `MileStones` and new `VoteMilestones` using all-time, daily,
weekly or monthly vote totals. The existing evaluator applies Enabled, At/Every,
group selection and configured Limit rules. Checks and normal milestone evaluation
share the same per-user limit fence, including force/reset operations, so overlapping
evaluations cannot both consume an unused configured limit. An exact `At: 100` checks 100, not
101 or all earlier thresholds. Legacy `ResetMilestonesMonthly` remains effective.
"Check" means evaluate eligibility; it does not bypass limits, reset award state,
force every milestone, or confirm that an external reward command succeeded.
Existing unlimited (`Limit: NONE`) milestones have no award-history fence and can
repeat when evaluated again at a matching total, just as on the normal evaluation
path. Do not use repeated checks as a delivery retry.

The command does not submit a vote, change totals/points/streaks/parties, or run
site, Cumulative, FirstVote, FirstVoteToday, AllSites/AlmostAllSites or date-event
rewards. POINTS and ALLSITES_TODAY milestone totals are excluded. Existing
AddTotal, SetTotal and GUI edits keep their mutation-only behavior.

The check runs on the existing storage worker, after work already admitted there.
The framework dispatches manual total commands separately and asynchronously;
typing an edit immediately followed by a check does not establish completion
order. Wait for the edit to finish before running the check. This command does
not wait for an unrelated still-pending async edit, flush all data, or promise
that queued storage changes have been durably written to disk.

Proxy mode is refused: this command cannot reconstruct the authoritative proxy
vote-total snapshot from backend-local state. Run it only on standalone servers.
A separate ordering improvement for manual total commands would need to preserve
their existing validation and mutation semantics; it is outside this command's
scope.
