---
name: setup
description: Local build and MEX environment for this VotingPlugin checkout.
triggers: [setup, build, Maven, test, environment]
edges:
  - target: context/stack.md
    condition: when a dependency or runtime version matters
  - target: context/conventions.md
    condition: when preparing validation
grounds_to: []
last_updated: 2026-09-20
mex:
  id: mx_01M305TTXSX79T4GDEXNMFVB3E
  type: guide
  status: promoted
  revision: 2
  title: setup
  relations:
    - type: related_to
      target: mx_01M305TTXGZN471W11VFPJQKQH
      note: when preparing validation
---

# Setup boundary

Root `AGENTS.md`, `VotingPlugin/pom.xml`, and `.github/workflows/maven.yml` own build instructions. MEX is local development tooling, not a plugin runtime dependency. This MEX release has no Java structural graph; inspect Java source directly. Rebuild the Wiki index after changing reviewed memory.
