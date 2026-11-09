# Automatic command documentation export

VotingPlugin writes a fresh reference for its **registered Bukkit command handlers** whenever
the plugin finishes registering commands on startup. There is no new runtime command or
configuration switch. The export is diagnostic/documentation output only: failure to write
it must not prevent voting.

Files written under the plugin data folder:

- `GeneratedDocs/commands.json`: schema version 1, a stable manifest with syntax, help text,
  permission expression, parsed permission names, bulk/all-target permission metadata,
  console/player eligibility, and the command root.
- `GeneratedDocs/commands.md`: a human-readable table of the same information.

The exporter reads the registered `/vote` (including configured alternate name) and
`/av` `CommandHandler` lists. Newly registered handlers automatically appear after the
next successful startup. Output is stable and files are not rewritten when unchanged.
It never runs the commands or reads player data, config secrets, tokens, or databases.

## Scope and cautions

- This is a **runtime snapshot**, not a canonical release manifest. Enabled/configured features
  and debug-only commands can differ between servers.
- This initial implementation intentionally does **not** enumerate the separate
  BungeeCord/Velocity proxy dispatcher, plugin.yml convenience aliases, or commands from
  other plugins.
- A registered command's `permissionExpression` may include multiple permission nodes
  separated by `|`. Its interpretation can depend on multiple-permission-check settings.
- `allTargetPermissions` are extra permission nodes required for supported bulk
  player selectors; do not confuse them with ordinary command alternatives.
- The generated folder is for read-only inspection or export. Do not edit its contents.

## Updating the published wiki

Copy the JSON output from a controlled **development/test server** using the intended
configuration. A future documentation sync step can transform the JSON into the
`BenCodez/Wiki` command-reference page, compare it with the last published snapshot,
and open a focused PR when commands change.

**This PR does not yet connect a running server or CI job to the wiki.** The local export
is automatic; publishing to GitHub still requires an artifact transfer/sync workflow.

For a release-quality update, review command additions or removals, test runtime
availability, and confirm proxy commands separately before publishing.
