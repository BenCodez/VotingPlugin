# Check milestone eligibility after editing totals

Run `/av User <exact-player-name-or-UUID> CheckMilestones` to evaluate the player's
current stored vote totals now. Permission:
`VotingPlugin.Commands.AdminVote.CheckMilestones` (or `VotingPlugin.Admin`).
Console and offline players are supported. Names are exact lookup inputs; the
command does not autocomplete a partial player name or accept selectors.

This checks legacy `MileStones` and new `VoteMilestones` using all-time, daily,
weekly or monthly vote totals. The existing evaluator applies Enabled, At/Every,
group selection and configured Limit rules. An exact `At: 100` checks 100, not
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
