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
