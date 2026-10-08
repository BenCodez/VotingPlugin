# DateVoteMilestones

Owners define named voting events in `SpecialRewards.yml`. There are no built-in holidays, recurring rules, additional vote totals, or alternate reward engines. Existing lifetime/monthly totals and normal rewards continue unchanged. The default empty section enables nothing.

Complete opt-in example:

```yaml
DateVoteMilestones:
  october_2026:
    Enabled: true
    DisplayName: 'October voting event'
    Start: '2026-10-01T00:00:00'
    End: '2026-11-01T00:00:00'
    Timezone: 'UTC'
    # Empty on a standalone server. On a proxy network, set exactly ONE backend
    # BungeeSettings.yml Server name here, identically on every backend.
    AccountingServer: ''
    # Configured VoteSites.yml keys, NOT Votifier service names. Empty means all.
    VoteSites: []
    Milestones:
      '10':
        Rewards:
          Messages:
            Player: '&aYou reached 10 votes in the October voting event!'
      '25':
        Rewards:
          Messages:
            Player: '&aYou reached 25 votes in the October voting event!'
          Commands:
            - 'give %player% diamond 1'
```

Reload after configuration. `/vote dateevents` displays the player's event counts, thresholds, active/upcoming/ended status and submitted or pending-review awards. Permission: `VotingPlugin.Commands.Vote.DateEvents` or existing `VotingPlugin.Player`. Progress inspection runs on a storage worker and responds on the player's owner scheduler. It never submits rewards. No new PlaceholderAPI placeholder is added; the progress command is the supported display.

## Membership and ownership

Start is inclusive; end is exclusive. Values are local ISO date-times interpreted with the explicit IANA timezone, such as `UTC` or `America/New_York`. Nonexistent or ambiguous daylight-saving boundaries are rejected; choose an unambiguous boundary. Invalid events are disabled with an explanation without disabling valid sibling events. Stable event IDs permit 1–64 letters, digits, `_` and `-`. Up to 64 configured definitions and 64 positive thresholds per event are supported; thresholds are bounded to 4096.

Membership uses the accepted pipeline's authoritative epoch-millisecond occurrence time and occurrence UUID. Standalone ingress assigns its timestamp during accepted processing; an external site's untrusted timestamp is not used. Current identified proxy deliveries retain the original proxy occurrence time and UUID, including delayed offline delivery. A vote accepted during a window can count even when delivered after its end. Votes before start or at/after end do not count. Fake/test votes are always excluded. A matching occurrence counts once per player/event; overlapping events each count it once. Enabled-site validation remains part of the normal accepted-vote pipeline. An optional filter selects configured site keys.

For a proxy network, **one backend owns each event ledger**. Set `AccountingServer` to that backend's exact configured server name. Use identified **all-server** delivery so that accounting backend receives every occurrence. Other backends do not increment the event, and their progress command directs players to the owner. Targeted delivery, legacy proxy deliveries missing original IDs/times, and local backend ingress on a proxy-configured server are intentionally excluded. They cannot reliably provide both complete event progress and network deduplication. This does not change their existing normal vote processing. A proxy event without an accounting owner is disabled. Moving the owner requires preserving the owner-local ledger and a new event ID; this feature does not migrate or replicate ledgers automatically.

## Persistence, configuration changes and rewards

Progress is stored under the accounting backend's plugin data directory, `date-vote-milestones/`, without changing existing SQL schemas or user fields. Back up that directory with the normal plugin data. Each record retains up to 4096 original occurrence UUIDs and reserved/submitted milestone state. The store has a 100,000 player/event-record capacity and a 1024 historical-definition capacity; reaching a limit rejects additional accounting rather than deleting existing state. Retention is explicit: finished events and their files are not automatically removed or recycled. Do not delete ledgers and reuse their IDs, which would re-enable grants.

After the first qualifying vote, the event ID seals its start, end, timezone, thresholds, site filters and accounting owner. Changing those fields requires a **new ID**; unsafe edits are rejected while existing progress is preserved. Display names and enabled state can change without resetting counts. Changing reward contents affects only future submissions, never repeats a submitted/reserved threshold. Offline start/end needs no scheduled reset: each original occurrence is tested against its configured window.

Before passing milestone rewards to AdvancedCore's existing handler, the vote count and award reservation are forced and atomically published together. After the handler returns, the award is recorded as `SUBMITTED`. Submission uses existing offline reward handling and proxy routing, with `DateVoteEvent` and `DateVoteThreshold` reward placeholders. It never calls the ordinary vote pipeline a second time.

Arbitrary commands, inventory changes and ledger publication are not one transaction. A crash or an uncertain reward admission can leave an award `RESERVED`; it is shown as **pending review** and is deliberately not automatically resubmitted, avoiding duplicate external payouts. `SUBMITTED` means accepted by the existing handler, not proof that every external command succeeded. An owner must inspect ambiguous cases and decide on manual recovery. Normal restarts, duplicates, reloads and display-name edits do not reissue rewards. Corrupt, oversized or unsafe state fails closed rather than resetting progress. No universal exactly-once payout guarantee is claimed.

Persisted player files use a SHA-256 event-ID filename so case-distinct event IDs remain distinct on Windows and other case-insensitive filesystems. Accounting contracts use length-prefixed fields; incomplete reached-award records fail closed for administrator review. Progress resolves the same stored player identity as vote accounting on a worker, including offline UUID normalization. Proxy test flags are preserved into the existing vote event; legacy Java overloads retain their real-vote default. Standalone occurrence membership uses the actual current instant, including daylight-saving rollback.

Disabling an event stops new accounting but retains its existing reward handles for offline/delayed awards. Keep disabled event definitions and their reward sections until previously queued rewards have been delivered; deleting definitions is not a supported cancellation mechanism for submitted rewards. Internal reward references use the case-sensitive event-ID hash, preserving distinct rewards even though AdvancedCore's ordinary registry lookup is case-insensitive. Owner-facing reward configuration and edits remain under the named event's `Milestones.<threshold>.Rewards` section; internal aliases are never saved into `SpecialRewards.yml`. An existing empty definitions file is treated as corruption, not a fresh install.

If definition metadata is missing or omits an event with surviving player history, accounting and progress fail closed for that event until the historical seal is repaired. A new player cannot reseal the old ID under edited accounting fields. Other intact seals and genuinely new event IDs remain usable.

`ProcessRewards: false` still records qualifying date-event votes but stores reached thresholds as `DEFERRED`, without invoking the reward handler. Once processing is enabled, the next qualifying accepted vote notification for that player/event reserves and submits those known unattempted thresholds once; an identified replay reaching this observer does not increment the counter. Reload/restart preserves deferred state. There is no background sweep: an ended event with no further qualifying delivery keeps its deferred thresholds. Each threshold is reserved separately immediately before invoking the reward handler. If an earlier submission or its acknowledgement fails, later unattempted thresholds stay `DEFERRED` for a subsequent qualifying notification, including after restart. Turning processing off before the next threshold leaves it deferred. An uncertain attempted submission remains `RESERVED` for conservative owner reconciliation and is never automatically retried.

Runtime reward aliases register from canonical milestone reward sections independently of date/timezone/accounting validation. An accounting-only typo disables new progress while existing queued/offline awards can still resolve their unchanged reward section after reload or restart. Keep the event ID and reward sections until queued awards finish; deleting those sections is not a supported cancellation mechanism.

Proxy transports open only after the existing milestone, streak, vote-party, special-reward, top-voter, shop and placeholder handlers have initialized and reward registration has finished. Date definitions and reward aliases are published before that ingress opens.

If canonical proxy occurrence accounting cannot be durably confirmed, the accepted pipeline finishes its normal processing but marks the dispatch incomplete. The ordered proxy lane quarantines and retains the original envelope without acknowledging release or automatically replaying normal totals/rewards. Repair accounting storage and reconcile that original event/site/player/occurrence before manually releasing the quarantined delivery. Confirmed progress with a later reward-submission failure retains the existing DEFERRED/RESERVED award guarantees. Local ingress has no equivalent durable proxy envelope and logs unconfirmed progress for owner reconciliation.
