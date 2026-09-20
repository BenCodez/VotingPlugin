---
name: router
description: Task routes for VotingPlugin architecture, proxy voting, rewards, and development constraints.
edges:
  - target: context/architecture.md
    condition: when tracing component boundaries
  - target: context/proxy-voting.md
    condition: when changing proxy or global voting
  - target: context/vote-rewards.md
    condition: when changing vote mutation or rewards
  - target: context/setup.md
    condition: when building or configuring the environment
  - target: patterns/INDEX.md
    condition: when a related task pattern could prevent a recurring regression
last_updated: 2026-09-20
---

# VotingPlugin memory routes

Use the authority hierarchy in `.mex/AGENTS.md`. This memory adds architecture, rationale, and known pitfalls; verify relevant claims against current source, tests, and formal contracts.

## Current state

- Working: Bukkit/Paper vote handling, BungeeCord/Velocity proxy routing, configurable vote sites/rewards, optional outbound Control integration. Sources: `VotingPlugin/src/main/java/com/bencodez/votingplugin/`, `README.md`.
- Graph limitation: MEX 0.8.2 indexes no Java symbols. Use Wiki search and direct Java source inspection.

## Route only when relevant

| Task | Context |
| --- | --- |
| Architecture or package ownership | `context/architecture.md` |
| Proxy/global vote handling | `context/proxy-voting.md`, then [proxy vote pattern](patterns/proxy-vote-change.md) |
| Vote mutation, totals, or rewards | `context/vote-rewards.md`, then [vote behavior pattern](patterns/vote-behavior-change.md) |
| Libraries and supported platforms | `context/stack.md` |
| Build/environment | `context/setup.md` |
| Why an unusual boundary exists | `context/decisions.md` |
| Unusual implementation convention | `context/conventions.md` |

Retrieve memory only when it may materially help; inspect current Java and tests before relying on it. Flag and correct stale entries through MEX. Do not force a query or memory write for trivial edits.
