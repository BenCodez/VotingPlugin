# Vanilla voting NPCs

Voting NPCs are public, named vanilla villagers that open the existing voting-site
selection. They do not receive votes, grant rewards, change totals, or replace
the normal `/vote` menus. No Citizens, fake-player internals, resource pack or
client mod is required.

## Enable and create

The feature is disabled by default. Add this to **Config.yml**, or change the
bundled default, then run `/av reload`:

```yaml
VotingNPCs:
  Enabled: true
```

Stand where the villager should appear and use:

```text
/av npc create voting &aVote here
/av npc list
/av npc inspect voting
/av npc remove voting
```

IDs contain 1–40 lowercase letters, digits, underscores or hyphens and start with
a letter or digit. Names are optional, default to `&aVote`, and allow up to 128
characters without control characters. `&` legacy colors are supported; names
are not interpreted as commands, PlaceholderAPI, JavaScript or rich-text actions.
Creation is player-only. List, inspect and remove also work from console.
Administration requires `VotingPlugin.Commands.AdminVote.NPC` (operator default)
or `VotingPlugin.Admin`. Both also permit entering the `/av` command root; the
NPC permission does not grant other administrative subcommands. Normal vote-URL command permissions still apply when
a player interacts; an NPC never grants that permission.

Right-click with the main hand to open the existing configured vote-URL GUI.
Already-cancelled interactions do not open it. Off-hand and repeated interaction
packets are suppressed. Trading, damage, combustion, targeting, vehicles and
teleportation and transformations (including lightning) are blocked only for the
exact tracked NPC entities. Ordinary
villagers, other plugins' NPCs and unrelated inventories are unaffected.

## Persistence and lifecycle

Definitions are saved asynchronously in **VotingNPCs.json** in the plugin data
directory. A success message confirms the definition write, not that a live
entity has already spawned. Inspect reports pending creation or cleanup as well
as created villagers. At most 64 definitions and 64 runtime reservations exist;
storage work also has a bounded queue. File ownership remains exclusive while
accepted writes drain during shutdown; shutdown does not wait for file I/O.

Villagers themselves are **nonpersistent**. Definitions recreate them when their
world and surrounding chunks are available; the plugin never loads distant
chunks or scans worlds to find NPCs. There are no animation/tick loops. Chunk and
world unloads retire tracked entities; loading later reconstructs them. Changing
`Enabled` to false and reloading removes live NPCs while retaining definitions.
Reloading does not create a second entity for the same configured ID, including
when an earlier entity's cleanup is still pending.

Operator-configured directory aliases are supported; the definitions and lock
files themselves must not be symbolic links. Duplicate IDs, capacity limits and
a busy worker report fixed actionable messages without requiring a reload.

An invalid/unreadable definition file is unavailable, never an empty successful
load. Failed or uncertain writes stop further edits until a checked reload;
do not blindly repeat a command whose result was not confirmed. Preserve the
file, correct the problem and reload. Atomic file replacement and the existing
platform directory-sync policy are used. Abrupt shutdown has the filesystem's
normal durability limits; no universal exactly-once side effects are claimed.

## Compatibility and limitations

Spigot/Paper need the ordinary villager APIs, nonpersistent entities and the
`World.spawn(Location, Class, Consumer)` initialization API. Capability checks
reject unsupported implementations without changing normal voting. This does
not raise VotingPlugin's global Java or Minecraft requirement. It is a vanilla
villager, not a custom player skin or a Citizens-style scripted NPC.

Folia creation uses the anchor region and verifies ownership of the villager's
bounds; cleanup follows the entity scheduler. A position crossing unavailable
or independently owned region bounds may remain unavailable. Choose a position
well inside a loaded chunk and retry through reload. No unsafe global-thread
entity removal is used. During final server shutdown, scheduler rejection can
prevent immediate removal; the nonpersistent entity is discarded at world
unload. External hot-disable/reload tools are not a supported substitute for a
server restart, particularly on Folia. Definition files remain intact.

## Testing

On a disposable server, enable the feature and create an NPC. Check main-hand
selection, ordinary vote permissions, cancellation by protections, off-hand
input and an unrelated villager. Inspect/list must agree with the saved ID.
Create/remove/recreate an ID while cleanup is delayed, reload both enabled and
disabled, unload/reload its chunk/world, then restart: one NPC should appear for
each enabled definition. Confirm removal affects only the selected NPC.

Automated tests use real file storage, controlled Bukkit events, and explicit
region/entity scheduler queues. They are not visual Minecraft testing or proof
of an actual Paper/Folia deployment. Record native platform and client results
separately before asserting those scenarios were exercised.
