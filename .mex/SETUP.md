# MEX setup for this VotingPlugin checkout

This is an established Java repository. Initial MEX context was populated from `README.md`, root `AGENTS.md`, `VotingPlugin/pom.xml`, configuration resources, docs, and the current production Java packages. The fresh-project questionnaire in the packaged MEX template does not apply here.

MEX 0.8.2 was configured with `mex setup --cli` for Codex only. Its Java Code Graph contains zero source symbols because this release does not index Java. Use `.mex/ROUTER.md` and Wiki for context, then verify implementation claims through direct Java source and tests.

The CLI, Wiki, and Codex instructions work in this checkout. MEX's full Hub requires the canonical `.mex/config.json` at `HEAD`; routine CLI use does not require a Hub. Do not expose a Hub for this setup.

Useful checks: `mex check`, `mex doctor`, `mex graph status`, `mex wiki query "proxy"`, and `mex skills sync --dry-run --tool codex`.
