# Proxy vote delivery

Reward-bearing `Vote` and `VoteOnline` messages use an additive, versioned
acknowledgement protocol when both the proxy and backend support it. The backend
advertises `voteDeliveryAckVersion` through presence or status replies. Older
backends ignore the extra fields and retain the existing transport behavior.

Before a capable route reports acceptance to the existing vote pipeline, the
proxy writes the exact target server and envelope to
`ProxyVoteDeliveryOutbox.dat`. It retries that entry every ten seconds and after
capability discovery. The backend acknowledges only after its ordered vote lane
reports completion. A spilled message is acknowledged after removal from the
durable backend overflow queue. Before acknowledgement, the backend journals the
completed vote ID for seven days so a lost acknowledgement followed by backend
restart does not repeat normal completed processing. The proxy then durably
removes the matching server, vote ID, and subchannel entry from its outbox.

This is an **at least once delivery guarantee**. Proxy shutdown, restart, a lost
send, or a lost acknowledgement leaves the outbox entry available for retry.
The backend vote ID cache and durable completion journal suppress ordinary and
restart-spanning duplicate retries. A backend process crash during reward side
effects, before the completion record is durable, can still lead to a repeated
attempt because reward execution and the receipt cannot be committed atomically.
Stronger exactly once reward execution would require a separate reward API and
storage design.

HTTP keeps its existing request recovery and acknowledgement path. The generic
outbox covers plugin messaging, Redis, MQTT, MySQL, and sockets after capability
discovery. No reward data model or Bukkit vote ordering changes.
