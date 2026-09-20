---
name: conventions
description: Established VotingPlugin engineering and verification rules.
triggers: [conventions, code review, implementation, test]
edges:
  - target: context/architecture.md
    condition: when finding the owner of a change
  - target: context/vote-rewards.md
    condition: when a change affects vote behavior
grounds_to: []
last_updated: 2026-09-20
mex:
  id: mx_01M305TTXGZN471W11VFPJQKQH
  type: convention
  status: promoted
  revision: 2
  title: conventions
  relations:
    - type: related_to
      target: mx_01M305TTX1QHVTAB2KR3S1VPH5
      note: when finding the owner of a change
---

# Conventions that need context

- Vote-site lookup has both observational and potentially creating paths. Read-only Control inspection must use the observational path; see root `AGENTS.md` and the vote-site manager before changing a call.
- Control is an optional outbound management adapter. Its failure must not become a dependency of vote receipt or delivery; see `docs/control-connector.md`.

Build, testing, review, and security rules live in root `AGENTS.md` and `docs/control-agent-contract.md`; do not maintain a second copy here.
