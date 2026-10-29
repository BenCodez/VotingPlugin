# DateVoteMilestones

Owners define named voting events in `SpecialRewards.yml`. There are no built-in holidays, recurring rules, additional vote totals, or alternate reward engines. Existing lifetime/monthly totals and normal rewards continue unchanged. The shipped example event is explicitly `Enabled: false` and enables no date rewards. Configure its dates and rewards before opting in.

Complete opt-in example:

```yaml
DateVoteMilestones:
  october_2026:
    Enabled: true
    CountFakeVotes: true # Defaults to true when absent; false excludes test votes.
    DisplayName: 'October voting event'
    Start: '2026-10-01T00:00:00'
    End: '2026-11-01T00:00:00'
    Timezone: 'UTC'
    # Standalone: empty/omitted means local accounting. In proxy mode it disables
    # new date accounting; set one exact backend Server name on every backend.
    AccountingServer: ''
    # Configured VoteSites.yml keys, NOT Votifier service names. Empty means all.
    VoteSites: []
    Milestones:
      TenVotes:
        Enabled: true
        At: 10
        Rewards:
          Messages:
            Player: '&aYou reached 10 votes in the October voting event!'
      TwentyFiveVotes:
        Enabled: true
        At: 25
        Rewards:
          Messages:
            Player: '&aYou reached 25 votes in the October voting event!'
          Commands:
            - 'give %player% diamond 1'
```

Reload after configuration. `/vote dateevents` opens the event list. Select an event for milestone details; use the page arrows and back button to navigate. The menu shows the player's event counts, next enabled threshold, active/upcoming/ended/disabled status, date window and timezone. Milestone details show submitted, deferred or pending-review state and configured reward types without running or displaying raw reward commands. Permission: `VotingPlugin.Commands.Vote.DateEvents` or existing `VotingPlugin.Player`. Progress inspection runs on a storage worker and opens the menu on the player's owner scheduler. It is a snapshot: run the command again after another vote for updated counts. Reload invalidates earlier menu navigation; reopen the command after changing event configuration. Disabled valid events remain visible, remote-owner events identify their accounting backend, and unavailable progress is shown as unavailable rather than zero. It never submits rewards. Administrative inspection and PlaceholderAPI snapshots are also available below.

## Administration and placeholders

The independent event ledger is stored in the plugin data folder under
`date-vote-milestones/`: `definitions.properties` seals accounting contracts and
`<event-file-id>-<player-uuid>.properties` stores occurrence UUIDs, count adjustments
and reward fences. The file ID is the SHA-256 of the case-sensitive event ID. Ordinary player totals
and SQL schemas are unchanged. Preserve these files together across backups/restarts;
do not delete or edit them to reset reward limits.

Use these commands on the event's accounting backend (exact event IDs are case sensitive):

```text
/av DateVoteMilestone Status <player-or-uuid> <event>
/av DateVoteMilestone Preview <player-or-uuid> <event>
/av DateVoteMilestone SetVotes <player-or-uuid> <event> <0-4096>
/av DateVoteMilestone Force <player-or-uuid> <event> <milestone-id-or-threshold>
/av DateVoteMilestone ForceNoLimits <player-or-uuid> <event> <milestone-id-or-threshold>
```

`DVM` aliases `DateVoteMilestone`. Permissions are `VotingPlugin.Admin` or the
individual `VotingPlugin.Commands.AdminVote.DateVoteMilestone.<action>` permission,
using the action names above. Status and Preview show the same read-only count and
per-threshold fence report. Storage and identity resolution run on the ordered worker;
messages follow confirmed completion on the sender's scheduler. Reload or shutdown can
cancel queued work. At most 64 admin requests may be outstanding.

SetVotes retains occurrence UUIDs and all prior reserved/submitted/deferred awards.
It changes only the event count, never normal totals, and does not submit rewards.
A later qualifying receipt can release a deferred threshold once its count is reached.
A named milestone selects every threshold expanded from its At/Every definition.
Force skips the count/window requirement, but requires an enabled event/reward and
local accounting ownership. ForceNoLimits explicitly permits repeating a submitted
award. Neither command clears an uncertain RESERVED fence. Submissions are fenced
one threshold at a time, so an error can leave earlier thresholds submitted and the
current one pending review; inspect Status before retrying. Counts remain unchanged.

PlaceholderAPI forms (replace `event` with the exact configured ID):

```text
%votingplugin_DateVoteMilestones_votes_event%
%votingplugin_DateVoteMilestones_next_event%
%votingplugin_DateVoteMilestones_status_event%
%votingplugin_DateVoteMilestones_award_10_event%
```

Underscores and case in event IDs are preserved. `votes` is the event count; `next`
is the next enabled threshold or `0` when none remains. `status` is active, upcoming,
ended or disabled. `award` reports submitted, deferred, pending_review or not_reached.
These calls never perform ledger I/O on the calling server thread. First/stale reads
return `loading` and queue a refresh; `remote`, `unavailable` and `invalid` are explicit
non-numeric states. Do not interpret them as a zero count. The cache retains at most
256 player snapshots for five seconds, with at most 64 pending refreshes. Confirmed
local votes/admin changes and reload invalidate snapshots; failed scheduling briefly
returns unavailable. There is no proxy query or background polling service. Placeholders
are informational snapshots, not a basis for granting rewards. Online requests use the
already published login storage UUID; absent identity snapshots report unavailable.
Offline requests use the supplied OfflinePlayer UUID without resolving a player name.

## Milestone format and compatibility

Date milestones use named reward entries, like `VoteMilestones`, with `Enabled` (defaults to true), `At` and `Every`. The total is always this event's accepted occurrence count. `At` supports an integer, a list, or ranges such as `'10..50 step 10'`; `At: 0` means the first vote, matching the existing milestone convention. `Every: 128` expands to multiples through the event's maximum of 4096 counted occurrences. `At` and `Every` together form a union. Each event supports at most 64 unique thresholds; an expansion exceeding that bound is rejected, so use explicit milestones or a larger interval. A threshold may belong to only one reward entry. `Total`, `Group` and `Limit` are not supported here: each threshold has one persistent award, and the explicit event window supplies its accounting period.

Existing numeric sections such as `Milestones.'10'.Rewards` remain supported without conversion. You may rename them to named entries with `At: 10`, or regroup the same thresholds into one `At` list, keeping the event ID and accounting fields. The ledger seal and queued reward aliases remain based on the threshold, so this does not reset progress or repeat reserved/submitted rewards. Rewards shared by one entry apply at each of its thresholds; queued aliases resolve the currently configured reward section. Do not overlap thresholds between entries. Changing the expanded threshold set after accounting begins still requires a new event ID.

A milestone's `Enabled: false` prevents new reward submissions while keeping its threshold, progress and queued reward handle. Reached, unattempted awards stay deferred. Re-enabling permits submission on the next qualifying accepted notification, including an identified replay, without recounting that occurrence. Reserved/submitted awards are never retried automatically. Keep disabled reward definitions until queued rewards finish. Invalid milestone types, malformed ranges and ambiguous reward definitions disable their event; valid sibling events continue.

## Membership and ownership

Start is inclusive; end is exclusive. Values are local ISO date-times interpreted with the explicit IANA timezone, such as `UTC` or `America/New_York`. Nonexistent or ambiguous daylight-saving boundaries are rejected; choose an unambiguous boundary. Invalid events are disabled with an explanation without disabling valid sibling events. Stable event IDs permit 1–64 letters, digits, `_` and `-`. Up to 64 configured definitions and 64 positive thresholds per event are supported; thresholds are bounded to 4096.

Membership uses the accepted pipeline's authoritative epoch-millisecond occurrence time and occurrence UUID. Standalone ingress captures its timestamp and a fresh local occurrence UUID once at receipt, before executor admission; an external site's untrusted timestamp is not used. Overflow retries and local time-change cache recovery retain that UUID, so a crash after ledger publication but before queue acknowledgement cannot recount the receipt or unlock another date milestone. Distinct receipts receive distinct UUIDs even when their player, service and timestamp match. Current identified proxy deliveries carry a separate `canonicalOccurrenceTime` and original UUID, including delayed offline delivery. BungeeCord and Velocity capture that original occurrence time when admitting the receipt, before scheduler handoff or identity resolution; durable snapshots preserve it even when normal processing has not started. This timestamp remains separate from the receiving proxy's normal processing and cooldown time. Ordinary event processing time, LastVotes, cooldowns, streaks, and post-vote `voteTime` retain their existing receiving-proxy clock semantics. The native vote and post-vote events expose the separate occurrence timestamp. `PlayerVoteEvent.localOccurrenceId` carries only local receipt identity; it does not populate `proxyVoteId` or imply proxy provenance. The existing post-vote `voteUUID` carries the effective occurrence UUID. A vote accepted during a window can count even when delivered after its end. Votes before start or at/after end do not count. Fake/test votes count by default for date milestones. Set that event's `CountFakeVotes: false` to exclude them. This option does not change ordinary vote totals, VoteParty or other reward policies. It still requires the event window, site filter, accounting owner and original occurrence identity; it never makes targeted or unidentified proxy delivery eligible. Changing this option preserves the existing event seal, counts and deduplication history. A matching occurrence counts once per player/event; overlapping events each count it once. Enabled-site validation remains part of the normal accepted-vote pipeline. An optional filter selects configured site keys.

For a proxy network, **one backend owns each event ledger**. Set `AccountingServer` to that backend's exact configured server name. Use identified **all-server** delivery so that accounting backend receives every occurrence. Other backends do not increment the event, and their progress command directs players to the owner. Targeted delivery, legacy proxy deliveries missing original IDs/times, and local backend ingress on a proxy-configured server are intentionally excluded. They cannot reliably provide both complete event progress and network deduplication. This does not change their existing normal vote processing. A proxy event without an accounting owner is disabled. Moving the owner requires preserving the owner-local ledger and a new event ID; this feature does not migrate or replicate ledgers automatically.

## AccountingServer configuration

`AccountingServer` selects the backend that maintains the event ledger. Its value is the exact, case-sensitive `Server` from that backend's `BungeeSettings.yml`.

| Installation | AccountingServer | New date accounting |
| --- | --- | --- |
| Standalone (`UseBungeecord: false`) | Empty or omitted | Local accepted votes can count and reach configured milestones. |
| Proxy backend (`UseBungeecord: true`) | Empty, omitted or blank | The event is rejected during reload; no new event progress or milestone submission occurs. Other valid events continue. |
| Proxy backend | This backend's exact server name | Identified all-server deliveries can count here. |
| Proxy backend | Another backend's name | This backend does not increment that event; progress points to the accounting owner. |

For a proxy network, reuse the complete example above on the participating backends and replace its owner line with the selected backend, for example:

```yaml
    AccountingServer: 'event-owner'
```

Set `BungeeSettings.yml -> Server: event-owner` on that selected backend and give every other backend its own distinct server name. Configure identified all-server vote delivery so the owner receives each occurrence. A missing owner or a name whose case differs from the owner's server name prevents accounting there. Changing the owner after the event is sealed requires a new event ID and preserving the original ledger for reconciliation.

Configure the same event ID, window, timezone, accounting owner, site filters and expanded milestone thresholds on backends using the event. Each backend that may receive queued or shared rewards needs a corresponding `Milestones.<milestone ID>.Rewards` section with matching `At`/`Every` thresholds, or the supported legacy numeric reward sections. Use one entry per threshold; do not add numeric sections alongside named entries for the same counts. Keep reward contents consistent with the intended delivery, including any deliberate server-specific differences. The accounting owner stores progress locally; neither owner selection nor shared vote totals copy or compare another backend's event definitions or reward handles.

An event omitted or invalid on the accounting owner cannot accumulate new progress. An event omitted on another backend has no local progress entry or reward handle. Missing threshold sections can prevent a queued reward from resolving, and different sections can resolve the same reward identity to different configured effects. Test the intended receiving backends before enabling the event and retain disabled definitions and reward sections while queued rewards remain.

Reward submission uses the existing AdvancedCore reward handler and the normal vote-delivery server context. An empty `AccountingServer` does not enable global date rewards in proxy mode. Existing queued reward handles are registered separately from accounting validation, so an invalid owner setting can stop new accounting while an earlier award remains resolvable.

## Persistence, configuration changes and rewards

Progress is stored under the accounting backend's plugin data directory, `date-vote-milestones/`, without changing existing SQL schemas or user fields. Back up that directory with the normal plugin data. Each record retains up to 4096 original occurrence UUIDs and reserved/submitted milestone state. The store has a 100,000 player/event-record capacity and a 1024 historical-definition capacity; reaching a limit rejects additional accounting rather than deleting existing state. Retention is explicit: finished events and their files are not automatically removed or recycled. Do not delete ledgers and reuse their IDs, which would re-enable grants.

Ledger transactions run on persistence workers. Instances using this transaction protocol, including instances in different plugin classloaders, serialize on the canonical accounting directory before opening its lock file. Each transaction also attempts a non-waiting cross-process file lock while it reads and publishes state; an unavailable lock fails closed, preserving existing progress. This prevents participating delayed and replacement instances from overwriting each other's state. New player/event records check the current directory contents under the same fence, so replacing an instance cannot bypass the 100,000-record capacity; existing records remain updatable at capacity.

After the first qualifying vote, the event ID seals its start, end, timezone, thresholds, site filters and accounting owner. Changing those fields requires a **new ID**; unsafe edits are rejected while existing progress is preserved. Display names and enabled state can change without resetting counts. Changing reward contents affects only future submissions, never repeats a submitted/reserved threshold. Offline start/end needs no scheduled reset: each original occurrence is tested against its configured window.

Before passing milestone rewards to AdvancedCore's existing handler, the vote count and award reservation are forced and atomically published together. After the handler returns, the award is recorded as `SUBMITTED`. Submission uses existing offline reward handling and proxy routing, with `DateVoteEvent` and `DateVoteThreshold` reward placeholders. It never calls the ordinary vote pipeline a second time.

Arbitrary commands, inventory changes and ledger publication are not one transaction. A crash or an uncertain reward admission can leave an award `RESERVED`; it is shown as **pending review** and is deliberately not automatically resubmitted, avoiding duplicate external payouts. `SUBMITTED` means accepted by the existing handler, not proof that every external command succeeded. An owner must inspect ambiguous cases and decide on manual recovery. Normal restarts, duplicates, reloads and display-name edits do not reissue rewards. Corrupt, oversized or unsafe state fails closed rather than resetting progress. No universal exactly-once payout guarantee is claimed.

Persisted player files use a SHA-256 event-ID filename so case-distinct event IDs remain distinct on Windows and other case-insensitive filesystems. Accounting contracts use length-prefixed fields; incomplete reached-award records fail closed for administrator review. Progress resolves the same stored player identity as vote accounting on a worker, including offline UUID normalization. Proxy test flags are preserved into the existing vote event; legacy Java overloads retain their real-vote default. Standalone occurrence membership uses the actual current instant, including daylight-saving rollback.

Disabling an event stops new accounting but retains its existing reward handles for offline/delayed awards. Keep disabled event definitions and their reward sections until previously queued rewards have been delivered; deleting definitions is not a supported cancellation mechanism for submitted rewards. Internal reward references use the case-sensitive event-ID hash, preserving distinct rewards even though AdvancedCore's ordinary registry lookup is case-insensitive. Owner-facing reward configuration and edits remain under the named event's `Milestones.<milestone ID>.Rewards` section (or the supported legacy numeric section); internal aliases are never saved into `SpecialRewards.yml`. An existing empty definitions file is treated as corruption, not a fresh install.

If definition metadata is missing or omits an event with surviving player history, accounting and progress fail closed for that event until the historical seal is repaired. A new player cannot reseal the old ID under edited accounting fields. Other intact seals and genuinely new event IDs remain usable.

`ProcessRewards: false` still records qualifying date-event votes but stores reached thresholds as `DEFERRED`, without invoking the reward handler. Once processing is enabled, the next qualifying accepted vote notification for that player/event reserves and submits those known unattempted thresholds once; an identified replay reaching this observer does not increment the counter. Reload/restart preserves deferred state. There is no background sweep: an ended event with no further qualifying delivery keeps its deferred thresholds. Each threshold is reserved separately immediately before invoking the reward handler. If an earlier submission or its acknowledgement fails, later unattempted thresholds stay `DEFERRED` for a subsequent qualifying notification, including after restart. Turning processing off before the next threshold leaves it deferred. An uncertain attempted submission remains `RESERVED` for conservative owner reconciliation and is never automatically retried.

Runtime reward aliases register from canonical milestone reward sections independently of date/timezone/accounting validation. An accounting-only typo disables new progress while existing queued/offline awards can still resolve their unchanged reward section after reload or restart. Keep the event ID and reward sections until queued awards finish; deleting those sections is not a supported cancellation mechanism.

Proxy transports open only after the existing milestone, streak, vote-party, special-reward, top-voter, shop and placeholder handlers have initialized and reward registration has finished. Date definitions and reward aliases are published before that ingress opens.

If canonical proxy occurrence accounting cannot be durably confirmed, the accepted pipeline finishes its normal processing but marks the dispatch incomplete. The router finishes its remaining normal per-server points mutation and service-site observation before surfacing the date-accounting failure. The ordered proxy lane quarantines and retains the original envelope without acknowledging release or automatically replaying normal totals/rewards. Repair accounting storage and reconcile that original event/site/player/occurrence before manually releasing the quarantined delivery. Receipt release retires the completed receipt and sends its acknowledgement; it does not re-enter the normal vote pipeline or replay totals/rewards. Releasing a receipt is not itself a retry of missing date accounting. Confirmed progress with a later reward-submission failure retains the existing DEFERRED/RESERVED award guarantees. Local ingress has no equivalent durable proxy envelope and logs unconfirmed progress for owner reconciliation.

Canonical date accounting requires an explicit transport vote ID; a legacy totals snapshot's correlation UUID is insufficient. `PlayerVoteEvent.proxyVoteId` and `PlayerPostVoteEvent.proxyVoteId` preserve the original explicit wire ID (or remain null when absent). Legacy totals correlation remains the effective identity for normal milestones, streaks, post-vote `voteUUID`, and backend replay fencing. Reliable multi-proxy delivery retains the original wire timestamp in the separate occurrence field through receiving-proxy forwarding, so delivery/retry time does not move a vote into a different event window. Older reliable senders with an explicit origin can supply their original wire time to an updated receiving proxy. Origin-less forwarding and backend envelopes without occurrence provenance remain excluded from canonical date accounting while normal rewards and cooldowns continue. An older intermediate proxy cannot recover occurrence metadata it did not retain; event accounting is unavailable for that route until its receiving proxy is updated. This requires no synchronized upgrade for ordinary voting.

Receiving-proxy retries and offline/time-change caches retain occurrence metadata separately from their normal time, including restart recovery. Existing JSON and SQL opaque totals payloads carry a bounded tagged trailing token; established totals and legacy UUID parsers ignore it. Totals updates preserve it. Legacy cache rows without the token retain normal voting effects but cannot prove date-event occurrence. Malformed, oversized, or conflicting metadata also excludes only date membership and never replaces ordinary processing time. Sender discovery and acknowledgement outboxes preserve the token across retries and restart. No SQL schema or dependency upgrade is required.

Local Votifier capture is registered at the start of post-load, before date definitions and normal reward handlers initialize. Captured events use the existing bounded vote executor (256 admissions) and durable Votifier overflow (256 entries). A bounded 256-entry ingress registry holds validated receipts; known queue capacity is reserved before executor submission, while unknown initial capacity remains provisional. The registry releases an admitted capture only when normal processing completes or the queue confirms ownership transfer. STARTED callbacks remain owned while blocked; retirement transfers them as well as callbacks that have not started before vote-executor cancellation, preserving the receipt UUID/time. Each transferred STARTED entry remains fenced from overflow execution until the original callback finishes. Successful, non-interrupted completion acknowledges only that exact transferred occurrence, including completion before or after transfer admission; exceptional/interrupted processing retains the recovery record. Failed transfers retain bounded capture ownership and emit an explicit admission failure. The overflow is published as a memory owner before listener registration; its independent worker loads recovered rows before accepting transfers, so cancelling the vote executor cannot cancel queue setup or overwrite recovered receipts. Queue-file loading and ordinary persistence stay on workers. The overflow opens only after date definitions and the accepted-vote consumer are ready, alongside the proxy-ingress readiness boundary. A full normal overflow lane rejects ordinary transfers, but reserved captures fit the bounded combined retirement snapshot. A full combined restored snapshot, an over-capacity persisted file or unreadable storage cannot admit another normal capture; recovered votes are not evicted or replaced with missing-state guesses. Initialization and queued transfers remain bounded; they are not additional reward or accounting engines. Repeated readiness calls do not register duplicate consumers, and retired callbacks retain queued entries for restart. The captured receipt timestamp is separate from normal reward/cooldown processing time; local time-change persistence carries it in an optional `CanonicalOccurrenceTime` key and retains the UUID under `LocalOccurrenceId`, while preserving the established `Time` field. Legacy overflow rows receive distinct UUIDs and are rewritten before admission; legacy timed-cache rows derive a stable migration UUID from their persisted row identity and save it before replay is scheduled. Explicit malformed IDs are withheld rather than replaced with fresh occurrences. Queue acknowledgement and ordinary vote rewards retain their existing semantics; stable date identity is not a transaction or exactly-once guarantee for external reward commands. `/vote dateevents` resolves stored identities and reads progress on the existing ordered user-data worker before returning messages to the player owner. `VoteSites` must be a YAML list containing strings; absent or empty lists mean all sites, while malformed filters disable only their event and retain queued reward handles.

Recovered time-change replay is constructed and registered only after date definitions, reward handles and the accepted-vote consumer are ready. This also gates the constructor’s initial replay timer; starting local Votifier or proxy ingress cannot race a missing replay consumer.

Extension events dispatched from a Bukkit/Paper/Folia gameplay thread are admitted to the existing ordered storage worker (at most 64 pending). The asynchronous event flag is not treated as a thread check. Deferred proxy accounting remains unconfirmed/quarantined until reconciliation; submission alone never acknowledges durable accounting. Reload, disable, or overload can withhold such extension occurrences. Native VotingPlugin asynchronous ingress retains synchronous completion reporting.

When the normal capture registry is full, validated receipts use the independent overflow admission lane directly; they do not need another normal executor/capture slot. If both bounded owners cannot admit a new receipt, rejection is explicit. A rejected transfer of an already captured receipt clears its in-flight state and retries with the same UUID/time through one coalesced 250 ms wakeup on the existing overflow worker. That wakeup targets only captures which already requested transfer, preserving normal callback execution and the STARTED release fence; listener stop submits one retirement batch that uses the captures' reserved snapshot capacity. No additional executor or unbounded waiter list is created, and retry wakeups stop when the overflow retires.

Native queue retirement drains its accepted worker transfers and writes the final snapshot after loading, with a one-second lifecycle wait. A blocked worker or failed write is reported as incomplete publication, not a successful durable admission; sudden process death before asynchronous publication can still lose an unpublished receipt. This does not claim universal exactly-once normal rewards or uninterrupted receipt while bounded owners are full.

A callback that outlives the bounded shutdown and completes only after the overflow seals its final snapshot remains an uncertain recovery case. The retired queue reports reconciliation is required and retains the occurrence rather than overwriting a successor queue. Partial ordinary side effects followed by an exception are also uncertain: recovery keeps the original UUID/time but can repeat ordinary effects under the existing at-least-once overflow contract. These boundaries do not weaken date-ledger identity, add a reward/accounting engine, or extend shutdown waits.

Overflow replay removes a receipt only after the production listener reports successful, non-interrupted processing. A known failure or callback exception retains its original occurrence UUID/time and retries through one coalesced 250 ms wakeup on the existing overflow worker. Individual failed entries cannot retry immediately in the same drain; other eligible entries can continue. Retired generations retain their receipts without running the listener, and callbacks finishing after the final snapshot cannot overwrite successor state. Existing public void callbacks retain normal-return completion semantics. A failure before event dispatch causes no normal vote effects; failure after a prefix of ordinary effects remains the documented at-least-once uncertainty above.

Native event dispatch alone is not completion: Bukkit can skip disabled listeners or catch consumer exceptions while returning normally. Explicit accepted-consumer completion keeps transferred receipts until the pipeline actually returns; a deliberate cancellation before consumer execution remains a completed no-op. A callback resuming during graceful shutdown therefore cannot discard an unprocessed receipt.

Overflow writers hold an exclusive file lease until their final worker publication finishes. A replacement instance waits asynchronously on the existing bounded queue worker before loading that file; it cannot publish an empty or stale snapshot while another writer owns it. The small owner sidecar is retained across shutdown and is not queue data. A blocked predecessor can delay replacement admission, which remains bounded and reports unsuccessful transfers for retry. This coordinates participating plugin instances, including different class loaders; it cannot control older binaries or other software that writes without the lease. Failed final publication retains that lease and the existing worker for retry; timeout still leaves recovery unconfirmed until publication succeeds.

## Testing the event menu

Run the focused regression suite from the repository root:

```shell
mvn -B -f VotingPlugin/pom.xml -Dtest=DateVoteEventsMenuTest,DateVoteMenuOpenBoundaryTest,DateVoteProgressSchedulingTest,VoteOccurrenceTotalsReplayTest,DateVoteMilestonesTest,DateVoteMilestoneConfigTest,DateVoteEventTest,DateVoteLedgerTest,BackendDateVoteAccountingRegressionTest,BukkitCompletionSchedulerTest,CorePlatformIsolationTest test
```

Then run the required clean package and packaged-artifact checks. Automated coverage uses complete synthetic YAML, the real ledger and reward registry, queued storage/entity callbacks, and an inventory boundary fixture. It verifies event and milestone pagination through 64 entries, list/detail/back navigation, start-inclusive/end-exclusive status, deferred/submitted/pending-review awards, literal display text, permission changes, reload fencing, offline/retired players, rejected storage admission, remote ownership, disabled events, empty configuration and corrupt storage. Navigation and inspection must not submit rewards, change accounting files or perform storage work on the player thread.

For an authorized gameplay test on a test server:

1. Leave the bundled example disabled. Create a separate synthetic event with a valid window, explicit timezone and test rewards, then enable it.
2. Open `/vote dateevents` with either supported permission. Inspect the event window, count, next enabled threshold and milestone preview. Navigate detail pages and back. The menu displays reward types, never executes previews.
3. Send a qualifying real test receipt through the normal test setup, reopen the menu and verify progress and award state. Duplicate the same identified receipt: it must not increment again or repeat a submitted award.
4. Verify disabled, upcoming and ended definitions, and a configuration with no events. On a proxy backend, verify a remote-owned event shows its exact owner without a fabricated local count; use matching definitions and identified all-server delivery for the accounting owner.
5. Revoke permission or reload while a menu is open. Old navigation must not reopen a stale or unauthorized view. Check fresh navigation after restoring permission and reopening the command. Repeat player scheduling checks on the supported server platforms.

A passing local suite is not evidence of a live Paper/Folia/proxy gameplay run; record those results separately when performed.

### Votifier capture retirement and capacity

Native intake keeps the ordinary overflow lane at 256 entries and reserves separate final-snapshot capacity for up to
256 normal captured receipts. The queue is the capacity owner: restored rows and current capture reservations together
are capped at 512. A full restored combined snapshot rejects new normal capture admission until processing frees room;
repeated shutdown/restart cannot grow the retained file indefinitely. Unknown initial disk capacity is provisional:
initialization and bounded deferred admission run on the existing queue worker before normal executor submission.

Shutdown stops intake, submits one coalesced capture-retirement batch, and then publishes the final queue snapshot on that
same worker. The existing YAML row format, receipt timestamp and occurrence ID are preserved, including captures whose
normal executor callback was cancelled. A started original callback retains execution ownership until its completion
status is known. Confirmed completion before the final seal removes its queue occurrence; completion after the seal
reports uncertainty and cannot overwrite a successor's file.

The close caller waits at most one second per call. Failed final publication retains the worker and file owner for retry,
and a replacement waits to acquire ownership; a timeout means recovery is unconfirmed. This does not guarantee completion
if the process exits while storage remains unavailable. Unreadable or unowned initial storage is never replaced with a
missing-state guess. Normal overflow admission remains distinct from a confirmed durable checkpoint.

A replacement that retires captures before acquiring its predecessor's queue owner keeps one bounded retirement batch
on the existing worker. It retries ownership and authoritative loading before merging and publishing those receipts;
close does not discard an uninitialized batch. Unknown or corrupt storage is retained for repair. If an already observed
queue file disappears, its absence cannot authorize an empty replacement. Retirement admissions across batches remain
capped at 256; normal and recovered row limits remain unchanged. Provisional receipt admission is not confirmed ownership
until recovery and capacity checks complete. A queue with no retirement receipts can close without publishing unknown
initial state. These retries do not extend the caller's shutdown wait or promise recovery if the process exits before
ownership/storage becomes available.
