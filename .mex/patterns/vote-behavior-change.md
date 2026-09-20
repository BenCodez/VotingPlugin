---
name: vote-behavior-change
description: Preserve production vote and reward behavior during changes.
triggers: [vote listener, totals, points, offline reward, vote site, reward]
edges:
  - target: context/vote-rewards.md
    condition: when mapping vote mutation and delivery
  - target: context/proxy-voting.md
    condition: when proxy state affects counting or rewards
grounds_to: []
last_updated: 2026-09-20
mex:
  id: mx_01M305TTYFYNY85XFDEVFS6VXM
  type: pattern
  status: promoted
  revision: 1
  title: vote-behavior-change
---

# Vote behavior change

Before moving the accepted-vote path, map side effects in order: vote party and broadcast, last-vote timestamp, reward delivery or offline queue, then totals/points and later events. A zero event timestamp is normalized for the stored last-vote time. Vote party has its own fake-vote setting. See `context/vote-rewards.md` for counting conditions; inspect sibling behavior in `VotingPluginUser`, vote sites, special rewards, and proxy routing. Root `AGENTS.md` owns test/build workflow.

Source: `VotingPlugin/src/main/java/com/bencodez/votingplugin/listeners/PlayerVoteListener.java`, `VotingPlugin/src/main/java/com/bencodez/votingplugin/user/VotingPluginUser.java`, root `AGENTS.md`.
