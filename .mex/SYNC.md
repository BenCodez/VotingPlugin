# Keeping VotingPlugin memory current

MEX 0.8.2 does not index Java symbols, so an empty graph cannot prove architectural freshness. Check claims against current source, tests, root `AGENTS.md`, and formal contracts. Use `mex check` for scaffold health and `mex wiki rebuild-index` after reviewed memory changes.

If memory is stale, update the relevant context or propose a correction through `$mex-inbox`. Keep prior decisions only when their history matters, marking superseded decisions clearly. Do not regenerate all context from a scaffold prompt or copy root development rules into MEX.
