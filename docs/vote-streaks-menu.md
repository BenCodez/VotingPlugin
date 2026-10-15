# Vote streak progression menu

`/vote streaks` opens a separate read-only inventory menu showing your existing
vote streaks. The singular `/vote streak`, normal `/vote`, voting-site menus,
and configured reward delivery keep their existing behavior. This feature is
independent of the experimental GUI test commands.

Grant `VotingPlugin.Commands.Vote.Streaks` to players who should use it. This
permission also passes the `/vote` root command gate and grants no other voting
subcommands. The existing `VotingPlugin.Player` command bundle is accepted as
well; its normal root-command access still applies. Console and other-player
inspection are not part of this command.

The three summary icons show the current and best stored daily, weekly and
monthly streaks. The progression row shows enabled `VoteStreaks` definitions
from **SpecialRewards.yml**, including progress-group milestones. Each entry
shows its own current/best progress, threshold, required votes per period and
recurrence. Units are days, weeks or months, not a mixture of vote totals.

A threshold being reached is progress information, not proof that a reward was
delivered. There are no claim buttons. Opening, clicking, navigating and
refreshing never advance a streak, write progression or execute rewards.
The normal voting pipeline remains responsible for reward processing.

## Configuration and navigation

No configuration migration or companion-plugin update is required. Existing
streak definitions are used unchanged; no streak is enabled automatically.
An empty configuration still shows the three stored summary values and a clear
message that no enabled milestones exist.

The new optional title in **GUI.yml** defaults when absent:

```yaml
CHEST:
  VoteStreaks:
    Name: '&6Vote streaks'
```

Merge `VoteStreaks` into the existing `CHEST` section; do not duplicate it.
Run `/av reload` after changing it. The title accepts legacy colors. Other
configured menus and their titles are unaffected.

Use Previous/Next for five milestones per page, Refresh to read current values,
and Close or Escape to leave. Milestones follow the existing configuration
order. At most 512 enabled milestones are shown, with an explicit truncation
notice; this display limit does not affect reward eligibility or processing.
Legacy summary values and configured progress groups are separate existing
counters and can differ. This menu observes stored values without simulating a
vote or changing missed-period accounting.

## Scheduling and lifecycle

Player UUID and name are captured on the player owner, then resolved together
on the existing persistence worker so configured offline-mode storage identities
remain authoritative. User loading and state reads stay on that worker; inventory
creation and interaction run on the player owner. The menu uses an exact custom
inventory holder, never a title match or a production GUI registration. Click,
drag, shift, hotbar and drop actions cannot move preview items. Unrelated
inventories and their events are left alone.

A newer request, another menu opening, close, disconnect, world change, death,
reload or shutdown invalidates pending callbacks. A delayed read cannot force
a closed menu back open or overwrite a newer menu. Reload blocks admission
while configuration changes and closes only this menu. Reads and menus are
bounded (16 pending data loads and 128 sessions); overload reports a useful
retry message. A data read taking over 30 seconds is discarded when it returns.
There are no animation loops, new worker pools or new persistent files.

This is a native Bukkit inventory interface, without display entities, dialogs,
client mods or a resource pack. It uses the existing entity completion scheduler
for Paper/Folia ownership. Normal shutdown closes views directly only when the
public platform ownership check permits it (including Folia's stopped-region
shutdown owner); it otherwise uses the owner scheduler. External live plugin
unload/reload tools that disable owner scheduling are unsupported: use the
plugin's own reload or a normal server restart. Mocked scheduling and inventory tests are not a live
Spigot/Paper/Folia or visual-client verification claim.

## Testing

On a disposable server with existing streak definitions, grant the permission,
run `/vote streaks`, and check all three summaries and the configured milestones
against the stored streak values. Test multiple pages, Refresh, Escape, two
players, opening another menu during loading, and reload with a menu open.
Try shift-clicks, number keys and dragging: no preview items should move.
Confirm normal `/vote`, `/vote streak` and unrelated inventories still work.
Opening the menu repeatedly must not grant rewards or change any counters.
