# Guided voting session

The opt-in `/vote session` chat guide helps players work through sites that are enabled, visible, permitted and eligible under VotingPlugin's existing cooldown rules. It does not replace `/vote`, `/vote gui`, inventory menus or rewards.

```yaml
GuidedVotingSession:
  Enabled: false
  TimeoutMinutes: 30
```

Enable it in `Config.yml`, reload, and grant `VotingPlugin.Commands.Vote.Session` or the established `VotingPlugin.Player` permission. The timeout is bounded to 1–120 minutes. At most 2,048 local sessions and 100 visible sites per session are retained.

## Player controls

- `/vote session`: start or reopen the current session.
- `/vote session next`, `previous`: select another site.
- `/vote session skip`: move on without confirming the site; a later real vote can still confirm it.
- `/vote session check`: refresh current eligibility and observed progress.
- `/vote session finish`: finish viewing without granting credit or any additional reward.
- `/vote session restart`: begin a fresh guide with the currently eligible sites.

Chat buttons run those same commands. The selected site's valid HTTP/HTTPS link (including the documented `[Text="...",url="..."]` wrapper) uses Minecraft's normal URL action and confirmation. Existing ForceLinks normalization and stored player-name placeholders are applied before HTTP/HTTPS validation; working site configuration does not need to be rewritten. Invalid/missing links are explained. Link display/clicks do not prove a browser opened or a vote occurred.

Eligible sites start as `AWAITING`; sites already cooling down at the initial sample stay outside the guide unless a positively identified new accepted receipt restores that visible candidate. Matching real accepted notifications mark a site `RECEIVED` using the asynchronously resolved storage UUID, configured site key and canonical occurrence ID. Fresh local ingress uses backend-local strictly increasing process sequence, including receipt in the same wall-clock millisecond as session creation. Historical local deliveries with a supplied original timestamp retain the original backend-time check. A bounded early receipt is provisional until its storage UUID matches. Duplicate IDs cannot confirm two sites or increase progress. Fake, cancelled and unrelated notifications never confirm a site. Hidden/disabled/removed/no-longer-permitted sites are redacted and have no link; received progress is retained internally. `SKIPPED` is navigation state, not credit.

## Observable timing and lifecycle

Original proxy occurrence timestamps are never compared with the backend clock. On a proxy-enabled backend, a new guide sends an additive version-1 fence request over the configured transport. A supporting originating proxy acknowledges its current ingress watermark. Only a canonical occurrence accepted after that source fence can confirm a site, and only after the ordinary backend credited-vote pipeline emits its notification. Status checks do not advance the fence. A link being displayed or clicked is not confirmation.

New Votifier ingress uses an opaque UUIDv8 canonical ID with a random 60-bit source incarnation and a monotonic 62-bit sequence. The ID is assigned before asynchronous handoff and retained unchanged by existing journals, caches, retries and multi-proxy forwarding; no cache/database schema migration is required. The source belongs to the proxy plugin classloader across runtime replacement. Restarting/reloading the plugin classloader creates a different incarnation, so old recovery IDs cannot become new occurrences. Existing external/legacy occurrence IDs remain valid for ordinary accounting but cannot establish this new ordering proof.

A genuinely new occurrence may arrive late, queued, or out of order and still confirm once credited. Pre-fence cached or in-flight votes do not confirm even if processed later. Fake/test classification is guide-only and never adds a reward path. A bounded provisional receipt can wait for its asynchronous source reply and exact resolved storage UUID. A session retains at most 32 source fences and 400 pending source receipts; first replies for each source are fixed and duplicate replies cannot move the boundary. Handshake replies expire after 30 seconds and must match the player, random session nonce and backend identity.

Multi-proxy negotiation requests a separate fence from configured origins; each original ID must match a successfully acknowledged source. Unsupported or unavailable peers safely leave that source unconfirmed while ordinary votes and rewards continue. The guide reports pending/unavailable confirmation rather than inventing freshness from arrival time. Votes accepted before a source acknowledges the guide are outside its confirmation window. After a proxy restart or failed negotiation, `/vote session restart` obtains a fresh fence; reopening the old session does not silently reset its boundary. Upgrading only a backend or only a proxy preserves normal voting; this optional cross-node confirmation requires supporting peers.

Closing chat or disconnecting does not erase progress or force an interface open on arrival. Reopen/check to see it. Sessions are local, reset on reload/plugin stop/server restart, and expire from their original start; actual vote totals, cooldowns and rewards remain authoritative in existing storage. A player moving to another backend starts that backend's local guide. There is no cross-server session synchronization.

Eligibility storage reads run on VotingPlugin's existing worker. Each request reads/parses one last-vote snapshot and reuses the existing cooldown decision for every site. Player output and permission snapshots run on the player's supported owner scheduler; a retired entity fallback never accesses the player. Request/lifecycle/deadline fences discard stale responses after replacement, reload or timeout. Expiration releases the owner admission even if its old read is stalled; that outstanding read retains its global slot until completion. Only one storage check per player and 64 total outstanding checks are admitted; repeated requests receive a busy message rather than filling the shared executor. Site identities are resolved again before sampling and checked again before rendering to handle configuration reloads. There are no entity spawns, inventory interception, vote submissions, completion rewards or new stored vote data.

Reliable backend outbox records retain freshness=false, including pre-upgrade stored envelopes. Sender admission, negotiation, rejected sends and restart retries never establish cross-node guide ordering. Normal vote processing is unchanged.

Local Votifier ingress captures backend observation order before executor admission. In-process overflow preserves that order, so a vote queued before a guide opens cannot appear as a fresh guide receipt. Recovered overflow and direct legacy processing callbacks have unknown original order and remain unconfirmed by the guide; normal vote totals, cooldowns and rewards still process normally. The order is process-local and is never reused from a persisted queue after restart.

Offline-mode backends resolve the established cached network storage UUID on the player owner before reading cooldowns asynchronously. The guide itself remains owned by the Bukkit player UUID; name/UUID resolution does not grant rewards or confirm proxy vote occurrences.
