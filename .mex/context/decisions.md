---
name: decisions
description: Durable reasons behind important VotingPlugin boundaries.
triggers: [decision, why, tradeoff, compatibility]
edges:
  - target: context/architecture.md
    condition: when the decision changes ownership
  - target: context/proxy-voting.md
    condition: when a decision affects network votes
  - target: context/vote-rewards.md
    condition: when a decision affects rewards
grounds_to: []
last_updated: 2026-09-20
---

# Decisions and constraints

## Control is optional

Control is a separate management plane. Connectors initiate outbound calls and are isolated from vote receipt, rewards, joins, and shutdown. This permits independent deployment and failure. Source: `README.md`, root `AGENTS.md`, `docs/control-connector.md`.

## Proxy and backend have different jobs

Proxy code routes network votes and selected global messaging; backend servers normally own player rewards and state. Network configuration requires consistent backend settings and unique server names. Source: `VotingPlugin/src/main/resources/Config.yml` (proxy setup guidance), `VotingPlugin/src/main/resources/bungeeconfig.yml`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/proxy/`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/backendproxy/`.

## Keep established reward behavior during portability work

Maintainer direction (2026-09-20): preserve existing function behavior while preparing for possible Fabric/NeoForge support; explain and ask before a necessary behavior change. Reward YAML changes should affect subsequent delivery through the existing configuration path. This is a compatibility preference, not a claim that native loaders or a new shared vote path exist in this checkout. Verify the current path in `VotingPlugin/src/main/java/com/bencodez/votingplugin/listeners/PlayerVoteListener.java`.
