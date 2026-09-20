---
name: vote-rewards
description: Vote mutation, totals, reward, and offline-delivery compatibility.
triggers: [reward, totals, points, offline vote, vote streak, VoteParty]
edges:
  - target: context/architecture.md
    condition: when locating the vote or reward owner
  - target: context/proxy-voting.md
    condition: when proxy or online state changes behavior
grounds_to: []
last_updated: 2026-09-20
---

# Vote state and rewards

`PlayerVoteListener` is the accepted Bukkit vote path in this checkout. It resolves identity/site, applies vote-delay checks, caches the user, invokes vote party and broadcasts, then updates the last-vote time (normalizing a zero event timestamp). It delivers via `user.playerVote(...)` or queues an offline vote before updating totals, points, streaks, milestones, cooldowns, and `PlayerPostVoteEvent`.

Counting is conditional: `CountFakeVotes` or a real vote, plus `event.isAddTotals()`. `Config.AddTotals` and `AddTotalsOffline` govern totals, while configured points are awarded in the enclosing condition even when `Config.AddTotals` is false. Preserve those distinctions. Offline and proxy reward paths use different online facts. Reward definitions are in per-site `VotingPlugin/src/main/resources/VoteSites.yml`, `VotingPlugin/src/main/resources/SpecialRewards.yml`, and optional reward files; reward YAML edits are intended to affect live behavior.

Do not infer that shared processing work in another branch is present here. Verify actual persistence and reward paths before claiming exactly-once behavior.

Sources: `VotingPlugin/src/main/java/com/bencodez/votingplugin/listeners/PlayerVoteListener.java`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/user/VotingPluginUser.java`, `VotingPlugin/src/main/resources/Config.yml`.
