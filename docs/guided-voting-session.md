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

Original proxy occurrence timestamps are never compared with the backend clock. All cross-node proxy deliveries remain unconfirmed in the guide, including direct/immediate sends, plugin messaging, HTTP, multi-proxy forwarding, timed replay and reward-cache delivery. A vote accepted on the proxy can be in flight before the backend-local guide opens. Neither receiver ingress nor a sender's positive freshness hint proves original occurrence order; there is no cross-node session handshake. The backend therefore also ignores positive hints from earlier proxy versions. Canonical IDs, original timestamps, queue classification, delay checks, totals and normal vote rewards remain unchanged.

Local Votifier ingress uses the backend's ordered observation sequence to confirm new receipts; this reports backend acceptance, not browser use. Network installations can still navigate sites and check authoritative cooldown/eligibility changes, but the guide cannot label a proxy-delivered site `RECEIVED`. This conservative limitation avoids falsely confirming older votes. Additive guide metadata is safely ignored by older backends; upgrade the backend for the conservative confirmation rule.

Closing chat or disconnecting does not erase progress or force an interface open on arrival. Reopen/check to see it. Sessions are local, reset on reload/plugin stop/server restart, and expire from their original start; actual vote totals, cooldowns and rewards remain authoritative in existing storage. A player moving to another backend starts that backend's local guide. There is no cross-server session synchronization.

Eligibility storage reads run on VotingPlugin's existing worker. Each request reads/parses one last-vote snapshot and reuses the existing cooldown decision for every site. Player output and permission snapshots run on the player's supported owner scheduler; a retired entity fallback never accesses the player. Request/lifecycle/deadline fences discard stale responses after replacement, reload or timeout. Expiration releases the owner admission even if its old read is stalled; that outstanding read retains its global slot until completion. Only one storage check per player and 64 total outstanding checks are admitted; repeated requests receive a busy message rather than filling the shared executor. Site identities are resolved again before sampling and checked again before rendering to handle configuration reloads. There are no entity spawns, inventory interception, vote submissions, completion rewards or new stored vote data.

Reliable backend outbox records retain freshness=false, including pre-upgrade stored envelopes. Sender admission, negotiation, rejected sends and restart retries never establish cross-node guide ordering. Normal vote processing is unchanged.
