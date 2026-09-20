---
name: proxy-vote-change
description: Trace and verify changes to proxy/global vote routing.
triggers: [proxy routing, global vote, BungeeCord, Velocity]
edges:
  - target: context/proxy-voting.md
    condition: when mapping proxy and backend ownership
  - target: context/vote-rewards.md
    condition: when a route changes reward delivery
grounds_to: []
last_updated: 2026-09-20
mex:
  id: mx_01M305TTY6NXWZP72T00XY9ESP
  type: pattern
  status: promoted
  revision: 1
  title: proxy-vote-change
---

# Proxy vote change

Trace the selected transport, proxy entry point, backend router, and Bukkit listener. Route selection happens upstream; `event.isForceBungee()` affects Bungee-aware reward and downstream processing in the listener. Check queued vote timestamps, server selection, and offline queues before treating a route as equivalent. See `context/proxy-voting.md` for the distinct online and origin facts; root `AGENTS.md` owns test/build workflow.

Source: `VotingPlugin/src/main/java/com/bencodez/votingplugin/proxy/`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/backendproxy/`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/listeners/PlayerVoteListener.java`.
