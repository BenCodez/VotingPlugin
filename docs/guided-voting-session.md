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

Eligible sites start as `AWAITING`; a site already cooling down at the initial sample stays outside the guide. A newly recorded post-start timestamp distinguishes a vote arriving during that sample from a pre-existing cooldown. A matching real `PlayerPostVoteEvent` with an identified occurrence newer than the session start marks a site `RECEIVED`. Matching is by the asynchronously resolved storage UUID and configured site key, including offline-mode UUID normalization. A bounded provisional receipt can arrive during that resolution, but is admitted only after its storage UUID matches. One canonical occurrence cannot confirm two selected sites. Repeated notifications cannot increase a site's progress; out-of-order older notifications cannot regress it. Unknown players/sites, fake votes, cancelled post notifications and timestamps at/before the session start do not confirm sites. A site that is removed, hidden, disabled, loses its viewing permission or becomes ineligible is `UNAVAILABLE`. Previously observed progress is retained internally; hidden or no-longer-permitted entries are redacted and never expose a link. A cooldown caused by a newly credited session vote retains its `RECEIVED` status. `SKIPPED` is navigation state, not credit.

## Observable timing and lifecycle

The guide uses the accepted pipeline's `PlayerPostVoteEvent.voteTime`, in epoch milliseconds. Current proxy delivery preserves the original proxy occurrence time/identity, so an older cached network vote does not confirm a new guide. Local Votifier ingress currently assigns its authoritative timestamp during accepted processing, not from the remote voting site's untrusted timestamp. The guide reports that local acceptance; it does not prove when a user clicked on an external site. Existing legacy deliveries lacking reliable original occurrence metadata retain their existing processing semantics.

Closing chat or disconnecting does not erase progress or force an interface open on arrival. Reopen/check to see it. Sessions are local, reset on reload/plugin stop/server restart, and expire from their original start; actual vote totals, cooldowns and rewards remain authoritative in existing storage. A player moving to another backend starts that backend's local guide. There is no cross-server session synchronization.

Eligibility storage reads run on VotingPlugin's existing worker. Player output and permission snapshots run on the player's supported owner scheduler; a retired entity fallback never accesses the player. Request/lifecycle fences discard stale responses after replacement or reload. Only one storage check per player and 64 total outstanding checks are admitted; repeated requests receive a busy message rather than filling the shared executor. Site identities are resolved again before sampling and checked again before rendering to handle configuration reloads. There are no entity spawns, inventory interception, vote submissions, completion rewards or new stored vote data.
