---
name: architecture
description: VotingPlugin's runtime boundaries and vote flow.
triggers: [architecture, vote flow, module boundaries]
edges:
  - target: context/proxy-voting.md
    condition: when a vote crosses proxy and backend
  - target: context/vote-rewards.md
    condition: when vote state or reward behavior changes
grounds_to: []
last_updated: 2026-09-20
mex:
  id: mx_01M305TTX1QHVTAB2KR3S1VPH5
  type: architecture
  status: promoted
  revision: 1
  title: architecture
---

# Architecture

## Vote flow

Votifier and proxy ingress reach Bukkit's `PlayerVoteListener`, which validates the vote, resolves the player and service site, handles timing and proxy metadata, updates the `VotingPluginUser`, delivers or queues rewards, updates totals/points/streaks, and emits downstream effects. Proxy nodes route votes to backend servers, where player rewards normally execute. The optional Control connector is a management adapter and does not process votes.

<!-- mex:entity
id: mx_01M305TTVDWCN3GH24C15GADN2
type: component
status: promoted
revision: 1
-->
## Cross-component boundaries

- The proxy package routes network votes; backendproxy handles backend transport and coordination. A routing change may also alter the listener's online/offline reward decision.
- Vote-site resolution and user persistence meet in the listener. Some site lookup paths may create configuration; observational code must use non-creating resolution.
- The vote log records selected events rather than every network hop or reward outcome. It cannot prove complete delivery.

## Boundaries

No native Fabric, Forge, or NeoForge loader is in this repository's current Maven package. Source: `README.md`, root `AGENTS.md`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/`.
