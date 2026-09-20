---
name: proxy-voting
description: Proxy/global vote routing and state distinctions.
triggers: [proxy, global vote, BungeeCord, Velocity, wasOnline, routing]
edges:
  - target: context/architecture.md
    condition: when tracing ownership between nodes
  - target: context/vote-rewards.md
    condition: when proxy metadata affects reward delivery
grounds_to: []
last_updated: 2026-09-20
---

# Proxy and global voting

Proxy nodes use BungeeCord or Velocity entry points under the proxy package. Backend transport and message routing live in the backendproxy package; configuration chooses plugin messaging, Redis, socket, MySQL, or MQTT where supported. Backend servers normally execute player rewards. Shared MySQL data and unique backend `ServerName` settings matter for network setups.

In the accepted Bukkit vote path, `event.isBungee()` (origin), `event.isForceBungee()` (Bungee-aware reward and downstream processing), `user.isOnline()` (current state), and `event.isWasOnline()` (historical state) drive different branches. `forceBungee` does not select the earlier proxy route. Do not collapse these into one flag. The listener also handles queued proxy duplicate timing, `WaitUntilVoteDelay`, vote party, broadcast, totals, and post-vote events; changing routing can affect each.

Sources: `VotingPlugin/src/main/resources/Config.yml` (proxy guidance), `VotingPlugin/src/main/resources/bungeeconfig.yml`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/listeners/PlayerVoteListener.java`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/proxy/`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/backendproxy/`.
