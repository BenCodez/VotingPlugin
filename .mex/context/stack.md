---
name: stack
description: Runtime, build, and dependency facts for VotingPlugin.
triggers: [Java, Maven, Bukkit, Paper, Velocity, BungeeCord, dependency]
edges:
  - target: context/setup.md
    condition: when installing or building
  - target: context/architecture.md
    condition: when a dependency affects runtime ownership
grounds_to: []
last_updated: 2026-09-20
---

# Stack

- Java 21 is the Maven compile target. Backend runtime is Bukkit/Paper; proxy runtime has BungeeCord and Velocity implementations. Check current POM and descriptors for version support.
- AdvancedCore supplies user and reward infrastructure. The listener consumes Votifier API events, which may be supplied by a compatible Votifier implementation. These are cross-component dependencies when changing vote behavior.
- No native Fabric/Forge/NeoForge loader is present in this checkout.

Sources: `VotingPlugin/pom.xml`, `VotingPlugin/src/main/resources/Config.yml`, `VotingPlugin/src/main/resources/bungeeconfig.yml`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/backendproxy/transport/`.
