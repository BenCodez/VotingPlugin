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
is not backend acknowledgement. Effect-bearing update batches are admitted
atomically, and live retries remember admitted updates. Full runtime replacement
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
retain the original vote ID and use a separate 1,024-entry allowance, preserving
the reward-vote entry allowance. Failed handoffs retain ownership across restart;
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
