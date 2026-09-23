---
name: agents
description: Project anchor for VotingPlugin; use ROUTER.md for task-specific MEX context.
last_updated: 2026-09-20
---

# VotingPlugin

VotingPlugin processes player votes, totals, and rewards on Bukkit/Paper servers and routes network votes through BungeeCord/Velocity.

## Authority and use

Use current code and tests for current behavior; root `AGENTS.md` and formal repository contracts for development rules and intended contracts; reviewed MEX project knowledge for rationale and known pitfalls; historical Relays and older records for background only. If memory conflicts with higher-authority evidence, follow that evidence, flag the stale entry, and correct or propose a correction through the MEX workflow.

For nontrivial work where architecture, prior decisions, or failure history matter, retrieve relevant MEX context, then inspect the current Java source and tests. Skip mechanical MEX queries for trivial edits. The root `AGENTS.md` owns build and review workflow.

## Retrieval boundary

MEX 0.8.2 Code Graph does not index Java. A fresh graph with zero Java sources is not Java architecture evidence. Use Wiki for context and `rg`, source reads, and tests for Java implementation truth; do not claim a Java Code Graph finding.

## User-data concurrency invariant

Treat AdvancedCore/VotingPlugin user-data, cache, and storage work as worker-only unless a current API is explicitly documented as a nonblocking snapshot. Bukkit/Paper primary-thread code may capture platform state, but cache population, SQL access, flush/dump/clear/remove operations, and shared-runtime admission belong on the existing storage/persistence worker.

Preserve lock ordering when touching legacy `UserDataCache`: acquire shared-runtime/per-user admission before entering the cache monitor. Never hold `synchronized (UserDataCache)` while invoking an operation that can acquire shared-runtime admission. This rule is a durable project constraint; verify it against current AdvancedCore/VotingPlugin code when changing cache or point-mutation paths.

## Knowledge updates

Use `$mex-inbox` for reviewable proposals about durable discoveries or decisions, without copying root `AGENTS.md` or routine task logs. Use `$mex-relay` to hand off unfinished substantial work with progress, evidence, blockers, decisions, tests, and next actions. Keep transient details out of durable context. Review MEX changes before committing or sharing; local drafts remain checkout-only.

## Navigation

Read `ROUTER.md` for task-specific context. Load only relevant files.

<!-- mex-agent:skills:start -->
## MEX context policy
- When MEX context materially helps your work, mention MEX and the relevant finding naturally in your explanation. Tie the mention to what it helped you understand, decide, or verify. Avoid fixed phrases, standalone acknowledgements, repeated mentions, or narrating routine context loading. This replaces older MEX instructions requiring a fixed acknowledgement or context-loading narration.
- Do not claim an author, date, or historical event unless the retrieved data actually provides it.
- After a MEX write, say exactly what changed and its sharing boundary: a local draft is checkout-only and nothing is shared; a canonical artifact is written to the working tree and requires commit/push to share.
- Skill activation is not approval for canonical actions.
<!-- mex-agent:skills:end -->
