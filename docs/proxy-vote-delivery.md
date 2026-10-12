# Proxy vote delivery

Reward-bearing `Vote` and `VoteOnline` messages use an additive, versioned
acknowledgement protocol when both the proxy and backend support it. The backend
advertises `voteDeliveryAckVersion` through presence or status replies. Older
backends ignore the extra fields and retain the existing transport behavior.
Plugin messaging probes each available backend every minute because that
transport does not use backend presence heartbeats.

Before a capable route reports acceptance to the existing vote pipeline, the
proxy writes the exact target server and envelope to
`ProxyVoteDeliveryOutbox.dat`. It retries that entry every ten seconds and after
capability discovery. The backend acknowledges only after its ordered vote lane
reports completion. A spilled message is acknowledged after removal from the
durable backend overflow queue. Before acknowledgement, the backend journals the
completed vote ID without time expiry so a delayed or lost acknowledgement
followed by backend restart does not repeat normal completed processing. The
bounded receipt journal fails closed at its capacity instead of evicting an ID
that may still have a proxy outbox entry. It reserves completion headroom larger
than the bounded ordered lane and separate space for release tombstones. A
release for an already-durable receipt may run independently of a
capacity-blocked vote because that receipt proves the vote effects completed;
unknown releases remain ordered. The proxy then durably transitions the
matching server, vote ID, and subchannel entry into a receipt-release phase. The
backend durably converts its completed receipt into a 24-hour post-release
tombstone and acknowledges that release;
only then does the proxy remove the outbox entry. Lost release messages and
acknowledgements remain retryable across either process restarting, so completed
receipts can be reclaimed without turning the journal bound into a lifetime cap.
If a backend generation stops advertising acknowledgements, the proxy drains
already-journaled entries once through the existing legacy send path and moves
each accepted entry into receipt release. This keeps rolling downgrades from
stranding accepted votes while retaining at-least-once behavior in case the previous capable backend
journaled completion before its acknowledgement was lost. Receipt-release
entries remain until a capable backend confirms retirement.

This is an **at least once delivery guarantee**. Proxy shutdown, restart, a lost
send, or a lost acknowledgement leaves the outbox entry available for retry.
After proxy startup, votes for a backend whose acknowledgement capability has
not been negotiated are journaled and wait for that negotiation instead of
being reported accepted through the legacy path.
The backend vote ID cache and durable completion journal suppress ordinary and
restart-spanning duplicate retries. The bounded post-release tombstone also
fences transport retries that were already in flight when completion was
acknowledged. A backend process crash during reward side
effects, before the completion record is durable, can still lead to a repeated
attempt because reward execution and the receipt cannot be committed atomically.
Stronger exactly once reward execution would require a separate reward API and
storage design.

HTTP retains its existing request recovery and also uses the completion outbox
after capability discovery. The generic outbox covers plugin messaging, Redis,
MQTT, MySQL, sockets, and HTTP. No reward data model or Bukkit vote ordering
changes.

## Velocity configuration reload failures

Normal Velocity soft/full reload validates `bungeeconfig.yml` before changing
channels, cancelling tasks, or replacing the proxy runtime. If the file is
missing, unreadable, or malformed, reload logs a failure and keeps the previous
active configuration and runtime. It does not rewrite the invalid file or
substitute default transport settings. Repair the file and reload again.
This preservation contract is distinct from initial startup default handling.

## Plugin-message pacing and cached batches

Plugin-message handoffs share one dispatcher for each backend, including control
messages, cache replay, capability probes and reliable delivery retries. The
minimum interval is 20 milliseconds after an accepted handoff; a failed carrier
attempt backs off for one second. Overdue scheduler tasks do not catch up by
sending a burst. This paces proxy API handoffs, not measured network arrival.
HTTP, Redis, MQTT, MySQL and socket transports keep their existing send paths.

The dispatcher has bounded queues and a 32-vote in-flight window per backend.
Receipt releases use separate headroom so completed receipts can progress while
that vote window is full. Reliable entries retain their original IDs and durable
outbox ownership until the existing acknowledgement protocol completes. Cache
entries remain owned by the cache when no carrier, send budget or durable outbox
capacity is available. Ready controls and durable replay alternate; a future
control does not block a ready control behind it.

Ordinary controls retain their existing best-effort semantics. Queue admission
is not backend acknowledgement. Effect-bearing control batches, such as vote-party announcements, are admitted
atomically; per-target vote updates retain their own durable admission identities. Full runtime replacement
and a change away from plugin messaging fail closed while admitted effect-bearing
controls are pending; retry the reload after they drain. Effect-free server-name
and status metadata do not block replacement when a backend has no carrier or
has been removed. Normal shutdown remains bounded and warns if effect-bearing
best-effort controls are still pending. It does not make these controls
restart-durable. An admitted control that becomes unencodable after settings
change is reported and retired without a handoff; durable votes remain queued.

Cached replay writes and verifies original batch number, batch size and SQL
source-row identity in the existing JSON journal before delivery. When waiting
for players to be online, only currently eligible player/backend rows receive
new batch positions. Existing durable positions are retained. Partial replay
and restarts preserve those positions instead of producing a new position-one
vote. Legacy SQL recovery associates an emergency JSON entry only with its
positive source-row ID and matching original vote fields; matching-looking
independent votes remain separate. Existing cache files remain readable and no
SQL schema migration is required. SQL cache snapshots must be read completely
before recovery replaces live cache maps or merges emergency journals. A failed
or partial read retains prior ownership instead of treating missing rows as
confirmed absence.

BungeeCord and Velocity JSON cache loads reject malformed, duplicate-field and
trailing-token documents. A failed reload keeps the last loaded in-memory state
and prevents writes until valid data can be loaded; it does not overwrite the
unreadable file with defaults. Repair or restore the cache file before retrying
recovery. Saves publish atomically and durable vote transitions verify the
journal before exposing staged state.

Frame validation uses the actual `writeUTF` encoding and encryption overhead,
including supplementary Unicode, before a platform handoff. Oversized durable
votes are retained for correction rather than discarded. Tests use controlled
clocks, schedulers and synthetic payloads; they do not establish live network
rate-limit or production delivery measurements.

Known effect-free metadata probes expire after 30 seconds of readiness when
no carrier accepts them. Admission rejection emits a bounded warning and does
not abort periodic probes for other backends. Unknown effect-bearing controls
are not expired by that policy.

Plugin-message `VoteUpdate` targets are now retained in the existing durable
outbox instead of occupying control slots while a backend is unavailable. They
retain the original vote ID and use a separate 1,024-entry ready window plus
bounded deferred ownership, preserving the reward-vote entry allowance. Failed handoffs retain ownership across restart;
accepted one-way handoffs use the existing conservative legacy attempt fence.
`VoteUpdate` has no completion acknowledgement protocol, including on backends
that acknowledge reward votes. Transport acceptance does not prove backend
execution. An ambiguous interrupted one-way attempt is not automatically
replayed, to avoid repeating external reward work. Pending plugin-message updates
remain retained when another transport is configured and resume when plugin
messaging is used again. Other transports keep their existing update behavior.

Live extra-target plugin-message broadcasts use the existing durable broadcast
journal. Targets are recorded as forwarded only after actual paced handoff;
unavailable targets stay pending without blocking other backends or later vote
work. Accepted reward target markers survive graceful restart in existing queue
metadata and keep a single-server reward on its original backend. They do not
represent a backend completion receipt or a pending HTTP transmission. No SQL
schema or owner configuration migration is needed.

Restart recovery retains the original reward routing mode and all-server target
plan. Routing edits and backend removal cannot turn an accepted single-server
reward into a new reward on another backend. Previously saved acceptance-only
markers are treated conservatively as a completed occurrence; existing pending
target-specific journals still own any remaining all-server work.

Single-server replay updates the canonical voter-keyed cache completion before
retiring the timed source. Cached delivery reconciles saved single-server
acceptance before sending, including after the player changes backend. A failed
completion write retains recovery ownership for retry.

### Deferred plugin-message updates

The update ready window and durable ownership are separate. An unavailable carrier
or a full ready window can leave a `VoteUpdate` deferred in the same outbox journal.
Normal vote logging, reward processing and proxy forwarding can finish once every
update target is durably retained; they do not wait for that backend to reconnect.
No updates are coalesced by player or discarded: each occurrence keeps its original
vote ID, site, timestamp, totals and backend target.

Replay moves deferred updates into the ready window only when a carrier and pacing
budget are available. Updates for each backend keep their journal order. A backend
without a carrier releases its ready slots while retaining its deferred owners;
other backends can continue. The existing one-way pre-send fence and definite-rejection
handling still govern restart recovery. Deferred admission is not backend execution
or a completion receipt.

The journal retains at most 1,024 ready updates, 4,096 deferred updates and 4,096
ordinary reliable deliveries, within the existing 16 MiB file limit and terminal-record
reserve. Exhausting durable capacity or failing publication leaves the original vote
owned for retry; it is not reported accepted, dropped or silently switched to another
transport. Durable capacity remains a finite backpressure boundary, distinct from an
unavailable backend occupying ready slots.

Deferred/ready transitions are journaled. Existing journals remain readable. Older
proxy JARs that do not recognize these new records must not be used while deferred
work remains: their strict reader rejects the journal rather than ignoring pending
votes. Drain pending deliveries with the updated JAR before downgrading; never delete
the journal to bypass a read error.

### Paired-cache recovery faults

A confirmed SQL deletion immediately removes its owner from live replay even if
paired JSON cleanup cannot publish. Recovery bytes remain protected on disk, and
a failed SQL deletion does not discard their completion metadata.

A complete checked SQL snapshot can prove that a positive-source emergency JSON twin
has lost its original SQL row. Such a confirmed orphan is excluded from replay even
when its verified JSON cleanup fails, preventing expired votes or completed broadcasts
from becoming new work. An incomplete SQL read or an identity mismatch alone does not
prove deletion and must not collapse independent cache records.

Failed batch staging leaves new positions unpublished. Already prepared, clean batches
with stable IDs can continue into the independently durable outbox for a negotiated
reliable plugin-message backend. Dirty entries, new batches, legacy delivery and
unsettled standalone broadcasts stay blocked. Quarantined JSON writes and paired
cleanup remain fail-closed until valid reload recovery; durable batch identity is not
proof that a reward completed.

Reliable cache acceptance remains bound to the outbox's original backend while a
cache save is blocked. Receipt release and retirement wait for matching surviving
cache and timed-source acceptance to be durable, including timed votes with no
voter-cache entry. After restart, cache and timed-queue reconciliation check that
existing outbox ownership before selecting a different backend; a player reconnecting
elsewhere must not create another reward delivery. A failed source write retains the
receipt-release phase for retry instead of retiring its last durable target record.
The same transfer requirement applies when retiring a legacy one-way send fence;
recovery does not resend a possibly accepted legacy reward while its source save fails.
Per-backend update admission is also retained in an unfinished timed source or graceful
shutdown snapshot before its outbox owner retires. A later vote-processing failure
must not cause already handed-off updates to replay after restart.
This does not change the reward receipt protocol or claim universal exactly-once
execution of external commands. Original plugin-message routing and acceptance
markers remain in shutdown snapshots if the transport changes while a vote is
unfinished; changing transport must not erase already accepted effects. Resuming
that vote through another transport does not resend its plugin-message updates.
Those updates retain their original outbox ownership and resume when plugin
messaging is available again; already handed-off updates stay completed. The
original single-server or all-server reward plan remains fixed on a live retry.
Cached reward reconciliation also honors an existing backend owner regardless of
the current transport, before offering the occurrence on a different backend.

A positive-source JSON entry whose SQL row still exists with a different identity
is retained on disk for recovery but excluded from ordinary replay. It is not
silently deleted, merged with that different row, or treated as a new vote. Restore
consistent source data before retrying recovery.
