# Experimental holographic vote menu

This opt-in prototype adds `/av testhologram`. It does not replace `/vote`, execute rewards, record votes, or require
VotingPlugin Control or an external hologram plugin. The command requires `VotingPlugin.Admin` or
`VotingPlugin.Commands.AdminVote.TestHologram` (op by default), and must be executed by a player.

## Try it

On Spigot/Paper 1.19.4+ or a supported Folia server, set this in `Config.yml`, then run `/av reload`:

```yaml
Experimental:
  HologramVoteGUI:
    Enabled: true
    Distance: 2.5
    TimeoutSeconds: 60
    SitesPerPage: 5
```

Look horizontally into an open area and execute `/av testhologram`. A stationary menu faces your opening direction.
Aim your crosshair at an entry and **right-click** it. VotingPlugin sends a clickable chat link containing that site's
configured HTTP/HTTPS VoteURL. Click the chat link and use Minecraft's normal URL confirmation. The server cannot
force your browser to open. Missing/invalid URLs produce a message instead of a link.

The menu includes enabled, non-hidden sites that you have permission to view, in VotingPlugin's existing order.
It displays a snapshot of your cooldowns from VotingPlugin's existing user APIs. Reopen to refresh the snapshot;
clicking an entry during cooldown only offers its URL and never bypasses vote processing. Previous/Next navigate
pages; Close removes the menu. Empty lists still have a Close button.

Distance accepts 1.5–3 blocks, timeout 1–60 seconds, and page size 1–5. Invalid values refuse to open. Defaults are
2.5/60/5 and the feature is **disabled**. There are at most 64 simultaneous menus and 64 outstanding data reads;
at most 200 visible enabled sites are included. Only the owner receives the entities, and another player's clicks
are rejected even if an entity ID is guessed.

## Lifecycle and compatibility

Reopening replaces your previous menu. Menus close on quit, world change, moving more than eight blocks from the
anchor, expiration, VotingPlugin reload, and plugin disable. Monitoring runs once per second. Entities are non-persistent
and never saved into chunks. Folia player work uses the entity scheduler; stationary entity spawning/removal uses the
anchor region. An opening that crosses an unowned region is refused. During final Folia server shutdown, region ticks may
already have stopped; non-persistent entities are discarded with world teardown rather than saved as orphaned menus.

Data reads run on the existing persistence worker, not the world thread. Late reads and queued callbacks are fenced
against replacement, close, expiration, and shutdown. Native display classes are isolated behind an API probe so older
servers can load VotingPlugin and receive an unsupported-feature message when testing this command.

## Interaction limits and assessment

The inspiration is [Hologram GUI (Mouse Menus)](https://youtu.be/ehlYco0E73w). This implementation uses native
TextDisplay/Interaction entities and ordinary Minecraft entity clicks. It does **not** implement a free mouse cursor,
true cursor hover, or continuous mouse tracking. Interaction hitboxes are axis-aligned and aiming matters; looking
steeply up/down or opening inside blocks can reduce usability. Unicode symbol appearance depends on the client's font.

Keep this experimental until visual gameplay, accessibility, different viewing angles, resource packs, and supported
server versions have broader coverage. The existing inventory GUI remains the normal player interface.

## Experimental style isolation

The experimental manager has its own player/session IDs, inventory holder and entity registry.
It never identifies an inventory by its title. Shift-click, hotbar swaps, drags and other transfers
are blocked only while the exact experimental inventory is open; unrelated menus and entities
are ignored. Opening another experimental style retires the previous session and invalidates
its pending snapshots and selections. A production inventory stays open until an administrator
explicitly requests an experimental inventory screen or clicks the experimental NPC.

`Experimental.VoteGUIs.Enabled` is false by default. Each style has an independent `Enabled`
setting under that section. The earlier explicit `Experimental.HologramVoteGUI.Enabled: true`
continues to enable only the hologram; `Experimental.VoteGUIs.Hologram.Enabled: false` overrides
that legacy opt-in. Other styles require the new global switch. Invalid legacy hologram-specific
settings do not prevent an independently enabled inventory style from opening.

The animated inventory uses configured visible voting sites, five sites per page by default,
read-only cooldown snapshots and optional available-item glints where the native API supports
them. Refresh and navigation never grant votes or rewards. The streak track reads the existing
streak definitions, stored counters and award state; displayed rewards are previews with no
claim action. Arbitrary reward conditions and external actions are not predicted or executed.

The NPC style uses a temporary native villager rather than a fake player or Citizens NPC.
Clicking it opens the dedicated experimental site inventory. It is stationary, nonpersistent,
invulnerable and nontrading; only its owner can activate it. Servers must expose the safe spawn,
AI and persistence APIs. Where private visibility APIs are absent, other players may see the
villager but cannot use it. No player skin customization is provided.

Snapshots run on the existing user-data worker, while inventory/player work uses the player
scheduler and stationary NPC work uses its fixed region. Entity counts and status use tracked
identities, never world scans. `/av testgui cleanup` retires temporary experimental sessions
and retries bounded pending NPC cleanup; it does not touch gameplay entities. A region scheduler
rejection retains the cleanup record and its capacity slot rather than admitting unbounded
replacement entities. Nonpersistent entities are discarded when their world unloads.

## Radial and native dialog prototypes

`/av testradialgui` places up to five configured site icons in a stationary circle, with previous,
next and close targets. Aim the crosshair and right-click an entry. `Radial.Radius` accepts
0.9–1.8 blocks (default 1.25). This is native entity targeting, not free mouse hover or tracking.
The renderer refuses an unloaded or unowned footprint instead of loading chunks or accessing
another Folia region. A full radial page has at most 23 entities: five icon/text/hitbox groups,
one central text/hitbox and three text/hitbox controls. Exact session generations fence stale
clicks and replacement cleanup; failed cleanup retains its bounded capacity slot for retry.

`/av testdialoggui` uses native dialog buttons. Voting-site buttons contain validated HTTP/HTTPS
URL actions handled by the client. Previous, next and close buttons use private experimental
callbacks, with owner and session checks and player-scheduler dispatch. The prototype never
uses the production dialog service's callback registrations or global cleanup. Invalid URLs
are reported instead of becoming executable commands. Closing a dialog or clicking a link
is not proof of a received vote, and never grants rewards.

Minecraft added dialogs in 1.21.6; Paper's developer API starts at 1.21.7
([official dialog documentation](https://docs.papermc.io/paper/dev/dialogs/)). Recent Spigot also
exposes native dialog APIs ([Player API](https://hub.spigotmc.org/javadocs/spigot/org/bukkit/entity/Player.html)).
Support is determined by the exact host classes and methods, not the version string. Unsupported
servers receive an explicit error; the command never falls back to a different GUI. Paper/Folia
and graphical client layout still require runtime acceptance testing; mocked API tests do not
establish gameplay verification.


## Reward showcase and temporary physical terminal

`/av testshowcasegui` creates a stationary floating reward preview with its configured item
(or a safe chest fallback), stored streak progress, award-recorded status and total votes.
Previous/next controls browse existing enabled streak definitions; the voting-sites control
cycles site pages. Preview interaction never claims, grants or repeats a reward. Arbitrary
reward conditions and external reward commands are not evaluated. The preview model turns
15 degrees once per second on its owning region; site icons remain stationary. A full page
has at most 26 entities (six item/text/hitbox groups and four text/hitbox controls).

`/av testterminalgui` or `/av testterminalgui create` creates a temporary private floating
board fixed to the opening location. It changes no blocks. Aim and right-click a listed style
to open that style, subject to its current switch, capability and permission. Disabled or
unsupported selections report their reason and leave the terminal intact. Opening a supported
style replaces the terminal session and removes its board. Its unique ID is the session UUID.
`/av testterminalgui list`, `inspect` and `remove` address only the invoking administrator's
current temporary terminal, never another player's terminal or another GUI type. Inspection
reports the stored world/coordinates and lifecycle without loading chunks or scanning worlds.
The board has 18 text/interaction entities and expires like other temporary sessions.

Persistent terminals are not implemented in this prototype. `AllowPersistentTerminals: false`
remains the default; changing that reserved option does not make temporary test terminals
persistent. There is no persistence/create-persistent command, world reconstruction or block
replacement. A durable public station needs a separate reviewed lifecycle before enabling it.

### Native dialog cleanup limitation

The supported Paper/Spigot APIs can show a dialog and unconditionally clear the player's
current dialog, but cannot inspect its identity. A deferred clear could dismiss a production
dialog opened afterward. Experimental retirement therefore unregisters only its own callbacks
and releases its session; it never calls an unconditional native clear. Close buttons and
Escape dismiss the native screen through the client. On timeout/reload/disable, an old native
screen may remain visible until dismissed or replaced, while its custom callbacks are inert.
Existing validated URL buttons remain ordinary client links, never reward actions. This is a
platform limitation, not a claim of full automatic screen dismissal. No packet/NMS interception
or global production-dialog hook is used.

### Command and capability matrix

| Style | Dedicated command | Required capability |
| --- | --- | --- |
| Hologram | `/av testhologram` | Native TextDisplay/Interaction APIs, 1.19.4+ |
| Animated inventory | `/av testinventorygui` | Plugin-supported Bukkit inventory/item APIs; Paper/Spigot only |
| Vanilla NPC | `/av testnpcgui` | Safe native Villager spawn, AI and nonpersistence APIs; Paper/Spigot only |
| Streak track | `/av teststreakgui` | Plugin-supported Bukkit inventory/item APIs; Paper/Spigot only |
| Radial | `/av testradialgui` | Native display/interaction/transformation APIs, 1.19.4+ |
| Native dialog | `/av testdialoggui` | Paper 1.21.7+ or compatible Spigot dialog API/client |
| Reward showcase | `/av testshowcasegui` | Native display/interaction/transformation APIs, 1.19.4+ |
| Voting terminal | `/av testterminalgui` | Native display/interaction/transformation APIs, 1.19.4+ |

Inventory styles provide the broadest compatibility. Display styles use crosshair targeting,
not free cursor movement. Capability checks are authoritative; no test command silently redirects
to a different renderer. Folia paths use player/entity/region ownership and refuse footprints
crossing an unowned region. Automated Paper/Folia client-protocol checks are recorded below. Visual layouts and manual
gameplay have not been verified; protocol checks do not establish visual usability. The existing Java 21
build requirement and production voting interfaces/accounting remain unchanged.

Use `/av testgui list` to see each independent enabled/supported state, `status` for bounded
tracked session/entity counts, `close` for your current experiment, and `cleanup` to retire all
temporary experiments. The global `Experimental.VoteGUIs.Enabled: false` default remains off.
Enable it explicitly before testing; keep individual style switches false to exclude them.

### Folia teleport lifecycle

Folia's current asynchronous teleport implementation does not publish the ordinary
`PlayerTeleportEvent` on all teleport paths. Experimental sessions therefore also
check the player's position on its entity scheduler once per second on Folia. Moving
from the opening position (including a short teleport) closes the menu; looking around
does not. This deliberately stricter Folia rule applies to all eight experimental
styles, including the legacy hologram test, and never closes an unrelated inventory.
Spigot/Paper retain teleport-event and distance-based cleanup. No packet interception
or internal server fields are used.

### Inventory cleanup and Folia limitation

Paper/Spigot cleanup closes the exact experimental inventory inline when already on the
server thread, including plugin disable. A different production inventory remains untouched.
Worker callbacks continue to use player scheduling.

Folia currently rejects or discards player-scheduler work after a plugin is disabled. A fixed
region scheduler cannot safely follow a player who teleported to a different region. To prevent
preview items becoming transferable after listeners are removed, AnimatedInventory, StreakTrack
and NPC (whose selection opens an inventory) are explicitly unavailable on Folia. Their commands
report this reason before creating a session or touching an inventory. The five native styles
remain independent and available when their individual capabilities exist. This restriction can
be removed when a supported lifecycle API guarantees owner-scheduled inventory retirement.

### Executed server/client checks

The candidate implementation was exercised on real Paper 26.2 build 133 and Folia 26.2 build 7
servers with Java 25, two automated client connections, seven reward-free configured voting
sites, and ViaVersion/ViaBackwards for protocol 26.1. No graphical client, screenshot or manual
gameplay claim is made. Native dialog callback automation needed a test-only length-prefixed
nullable-NBT protocol shim; that does not prove unmodified 26.1 client dialog compatibility.

Paper passed 56 bounded client-protocol checks covering all eight styles: creation, correct
site URL, relevant pagination, invalid URLs, private entity visibility, intruder rejection,
replacement, close, tracked resources, production `/vote gui` after the experiments, and active
display reload cleanup. Folia passed 43 checks for its five supported styles and explicit
resource-free rejection of the three unavailable inventory styles.

A separate run with a three-second timeout and then sixty-second timeout passed 17 checks on
Paper and 14 on Folia: supported styles expired without tracked resources, and a one-block
teleport retired them well before expiration. Both platforms stopped cleanly and had no
class-loading, event-delivery or thread-ownership failure markers in these runs. The test JAR
SHA-256 was `f7d8538b37bd5f7ff89313446ead165ad191b511ee00736552f07bab31eecfac`.

These checks observe tracked registries and client-visible entities, not an exhaustive world
scan. Earned streak/reward progression, graphical spacing/animation/accessibility, live death
and disconnect cleanup, failure during partial spawning, old-server gameplay, and Folia
region-boundary stress remain manual/integration verification limits. Deterministic tests
cover ownership, cancellation, stale callbacks, unrelated inventory/entity events and cleanup
failures, but do not substitute for those gameplay scenarios.
