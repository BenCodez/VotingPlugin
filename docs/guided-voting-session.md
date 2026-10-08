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

Eligible sites start as `AWAITING`; sites already cooling down at the initial sample stay outside the guide unless a positively identified new accepted receipt restores that visible candidate. Matching real accepted notifications mark a site `RECEIVED` using the asynchronously resolved storage UUID, configured site key and canonical occurrence ID. Fresh local ingress uses backend-local strictly increasing process sequence, including receipt in the same wall-clock millisecond as session creation. Historical local deliveries with a supplied original timestamp retain the original backend-time check. Explicitly live identified proxy deliveries use the backend-local strictly increasing ingress sequence, so differences between proxy/backend clocks do not reject a new vote. A fresh live proxy receipt can restore its visible candidate after an initial cooldown sample. A bounded early receipt is provisional until its storage UUID matches. Duplicate IDs cannot confirm two sites or increase progress. Fake, cancelled and unrelated notifications never confirm a site. Hidden/disabled/removed/no-longer-permitted sites are redacted and have no link; received progress is retained internally. `SKIPPED` is navigation state, not credit.

## Observable timing and lifecycle

Original proxy occurrence timestamps are never compared with the backend clock. Reliable multi-proxy forwarding, timed replay and reward-cache delivery do **not** confirm a fresh guide, even when normal VotingPlugin rewards them. This includes prompt forwarding and a vote queued after opening the guide: receiver ingress proves delivery here, not freshness at the original proxy. Guide-only provenance uses additive `sessionDeliveryFresh` metadata and the existing origin field (an empty origin means unknown original ingress) without changing queue classification, vote-delay enforcement, totals or rewards. Stable occurrence IDs and supplied original timestamps are retained. Queued or legacy proxy deliveries whose original age is unknown also remain unconfirmed.

The forwarding guarantee requires the updated receiving proxy and backend. Older proxies omit the positive freshness metadata, so their deliveries remain unconfirmed while normal rewards work. The additive fields are ignored safely by older backends. Normal voting remains compatible in either deployment order, but mixed versions cannot establish guide freshness. A proxy ingress observed before opening cannot confirm a later session. Local Votifier ingress assigns its authoritative timestamp during accepted processing; that reports backend acceptance, not the time an external link was clicked.

Closing chat or disconnecting does not erase progress or force an interface open on arrival. Reopen/check to see it. Sessions are local, reset on reload/plugin stop/server restart, and expire from their original start; actual vote totals, cooldowns and rewards remain authoritative in existing storage. A player moving to another backend starts that backend's local guide. There is no cross-server session synchronization.

Eligibility storage reads run on VotingPlugin's existing worker. Each request reads/parses one last-vote snapshot and reuses the existing cooldown decision for every site. Player output and permission snapshots run on the player's supported owner scheduler; a retired entity fallback never accesses the player. Request/lifecycle/deadline fences discard stale responses after replacement, reload or timeout. Expiration releases the owner admission even if its old read is stalled; that outstanding read retains its global slot until completion. Only one storage check per player and 64 total outstanding checks are admitted; repeated requests receive a busy message rather than filling the shared executor. Site identities are resolved again before sampling and checked again before rendering to handle configuration reloads. There are no entity spawns, inventory interception, vote submissions, completion rewards or new stored vote data.
