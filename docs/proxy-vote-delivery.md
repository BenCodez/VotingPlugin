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
