# Maintainer and AI-agent guide

VotingPlugin is the vote-processing data plane for Bukkit/Paper and BungeeCord/Velocity networks. The optional Control
integration is a management adapter, never a runtime dependency: vote receipt, routing, storage, rewards, joins, commands,
reload, and shutdown must keep working when Control is disabled, unreachable, incompatible, or restarting.

## Build and verification

Requirements: JDK 21+ and Maven. The Maven project lives in the `VotingPlugin/` subdirectory.

```shell
mvn -B -f VotingPlugin/pom.xml test
mvn -B -f VotingPlugin/pom.xml package
```

For a focused Control change:

```shell
mvn -B -f VotingPlugin/pom.xml -Dtest=BackendControlConnectorProtocolTest,ControlInspectionServiceTest test
```

CI runs `mvn -B -f VotingPlugin/pom.xml package`; see `.github/workflows/maven.yml`. Do not use the `dev` Maven profile in
automation because it copies a JAR into a developer-specific server directory.

## Architecture and file map

- `VotingPluginMain` is the Bukkit entry point and lifecycle owner.
- `proxy/VotingPluginProxy` and the Bungee/Velocity platform packages own proxy lifecycle and vote routing.
- `listeners/` receives Bukkit-side vote/player events; `proxy/cache/` owns proxy pending-vote queues.
- `votesites/` resolves configured service names. Be alert to the distinction between read-only resolution and paths that
  may auto-create a site.
- `user/` owns player totals, points, streaks, last-vote values, and backend offline rewards.
- `rewards/` and `specialrewards/` parse and execute rewards. A Control simulation must never invoke these executors.
- `votelog/` owns optional SQL-backed logged events and its in-game admin GUI.
- `control/BackendConfigurationService` is the bounded Bukkit YAML/quick-setup adapter.
- `control/BackendControlConnector` is the Bukkit outbound Control connector and task dispatcher.
- `control/ControlInspectionService` is the typed read-only inspection allow-list.
- `control/ControlRewardProposal` is the shared strict parser for reward simulation and reward-builder persistence.
- `control/BackendControlResultStore` journals configuration results that must survive acknowledgement failure/restart.
- `proxy/control/` contains proxy discovery/configuration, communication tests, automatic enrollment, and hosted-Control
  lifecycle.
- `VotingPlugin/src/main/resources/` contains the default Bukkit and proxy configuration.
- `docs/control-connector.md` explains deployment; `docs/control-agent-contract.md` is the exact agent/client contract.

## Runtime and security invariants

1. Control connectors initiate outbound HTTP(S); do not add an inbound admin port to VotingPlugin.
2. All connector network and database work stays off Bukkit's primary thread. Keep the dedicated inspection daemon
   separate from the presence/configuration executor; it has a five-second shutdown bound. Schedule only the minimum
   reload/runtime interaction onto the server thread, then return the bounded result to the correct connector worker.
3. Connector failure is isolated. Never block vote handling, joins, commands, reload, or shutdown on Control I/O; keep
   timeouts, body limits, daemon workers, and bounded shutdown waits.
4. Capabilities are explicit and versioned. Do not dispatch a task merely because its JSON shape looks familiar. An
   unaccepted capability must remain inactive. Control and the node both enforce fixed quick-setup preset/option schemas;
   keep phase-specific validation here even when Control already rejected the same input.
5. Configuration writes are limited to the managed VotingPlugin YAML allow-list and typed quick setups. Preserve path
   containment, no-follow reads, size limits, YAML parsing, secret masking/restoration, revision checks, atomic staging,
   `.control-backup`, reload, and rollback-on-reload-failure. Control snapshots persist this redacted read output, so new
   credential fields and sensitive comments must be covered by masking tests before release.
6. A configuration result is durable and idempotent: journal it before acknowledgement, echo the current `attemptId`, and
   do not apply the same operation twice when a lease or acknowledgement is retried.
7. Inspections are read-only, typed, bounded, and safe to retry. Never add raw SQL, table names, filesystem paths, commands,
   arbitrary placeholders, generic configuration lookup, fuzzy/all-player search, or mutable live objects.
8. Never return credentials, passwords, tokens, database/Redis/MQTT connection details, webhook URLs, raw configuration,
   raw logs, or unrestricted player records. Keep diagnostics deliberately redacted. Unexpected inspection exceptions
   return a generic external message; local logging may identify the exception class but must omit its message.
9. An inspection's `player` query is exact name or UUID lookup and must check existence before loading. Do not turn it into
   enumeration or autocomplete.
10. A reward inspection only validates/normalizes a typed proposal. It must report `wouldExecute:false` and
    `sideEffects:false`; persistence still goes through configuration preview/apply.

## Control connector lanes

Keep these paths separate:

- discovery/presence advertises current node identity and topology;
- configuration capabilities (`config.*.v1`) poll `/operations`, may read/preview/apply typed configuration, and journal
  results;
- inspection capability `data.inspect.v1` polls `/inspections`, executes only `ControlInspectionService`, and does not
  journal because a lost acknowledgement can safely repeat a read. Repeated failures back this lane off exponentially
  from one second to five minutes without changing voting or configuration availability.

Every claimed task is bound to a node session and `attemptId`. Echo both. An HTTP `204` means no work. Authentication,
protocol, or capability failure changes only connector state/backoff.

`auto-create-vote-sites` is intentionally narrower than `common-settings`: it reads/writes only
`Config.yml -> AutoCreateVoteSites`. Do not fold it back into a multi-setting update. Turning automatic creation off must
not erase detected service-site observations, and explicit administrator-created sites must remain a separate action.

`vote-logging` is also narrow: it owns only `VoteLogging.Enabled`, `VoteLogging.PurgeDays` (`-1` or `1`–`3650`), and
`VoteLogging.UseMainMySQL`. It must reject database host/name/user/password or any unknown option. Dedicated connection
credentials remain a redacted full-editor task.

`reward-builder` is PREVIEW/APPLY-only. It requires exactly one <=64 KiB `proposal` option using the inspection proposal
schema, and replaces exactly `VoteSites.<site>.Rewards`, `EverySiteReward`, or `VoteParty.Rewards`. Keep it deterministic:
do not merge stale actions, change another scope, execute a reward, expose the proposal in a result, or journal its value.

## Inspection contract

The allow-listed kinds are `overview`, `vote-site-health`, `player`, `vote-log-summary`, `vote-log-search`, `vote-trace`,
`vote-site-resolution`, `reward-simulation`, and `diagnostics`. The exact filters and result semantics are in
`docs/control-agent-contract.md`.

Maintain these global bounds unless a versioned contract deliberately replaces them:

- result JSON: 512 KiB;
- general result rows: 100 (diagnostics may report up to 128 detected plugin names);
- top lists: 20;
- lookback: 365 days;
- exact player lookup only;
- no mutation in resolution, simulation, or diagnostics.

Unknown query/filter/proposal fields must fail validation. `vote-site-resolution` must use the non-creating resolver path;
do not call a convenience method that can auto-generate configuration.

`vote-site-health` may expose at most 100 case-insensitively deduplicated persisted `GottenServiceSites` values that lack a
configured `ServiceSite`. Snapshot the stored list before iterating and keep it observational; this signal must work with
VoteLogging disabled and must never create a vote site.

## VoteLog semantics

VoteLogging is optional and SQL-backed. It may use the main MySQL connection or a dedicated one. The current quick setup
changes `Config.yml` but does not recreate or close the runtime VoteLog manager, so a server restart is required after
either `VoteLogging.Enabled` transition. Inspections must gate on the configured enabled state: disabled means unavailable
even if an old adapter remains, while newly enabled can report enabled but unavailable until restart. A dependent query
must return `UNAVAILABLE` for disabled, missing-adapter, or unreadable state rather than treating an empty result as
authoritative.

Legacy VoteLog read methods catch SQL failures and return empty/zero values, so the inspection layer must probe readability
first. Preserve the 10-second JDBC statement timeout: summary/search/trace return `UNAVAILABLE` when logging is disabled,
the adapter is missing, or the probe fails, while vote-site health exposes `voteLogReadable:false`, skips aggregates, and uses explicit unavailable or
unreadable statuses instead of `NO_RECENT_VOTES`. The probe is point-in-time; legacy methods can still return empty if the
database fails after it succeeds, so removing that race requires an explicit table error-result API.

VoteLog records selected events: vote receipt, vote milestone, vote-streak reward, top-voter reward, and vote-shop
purchase. `IMMEDIATE` and `CACHED` describe processing status. A shared `voteId` correlates written rows, but the table is
not a complete network delivery trace: it does not record every validation rejection, transport hop, duplicate decision,
reward command, command outcome, or expiry. Documentation and UI must call these **logged events**.

Queries must use the bounded methods on `VoteLogMysqlTable`. Preserve prepared parameters, exact filters, row limits, and
stable ordering. The recent service-health window is not proof that an omitted configured service has no votes; query the
at-most-100 displayed configured services through prepared exact filters. Do not accept raw SQL from Control or expose
the database/table configuration.

## Paired change and PR workflow

The server-side peer is `BenCodez/VotingPlugin-Control`. When changing a DTO, endpoint, capability, preset, error code, or
limit:

1. inspect both repositories and their root `AGENTS.md` files;
2. keep the change additive/capability-negotiated so either old side stays safe;
3. update connector/service tests here and coordinator/HTTP tests in Control;
4. update `docs/control-agent-contract.md`, `docs/control-connector.md`, and the Control management docs;
5. link the paired PRs and state a safe merge/deployment order.

Prefer one cohesive PR per repository for a paired feature, keeping its implementation, tests, and docs together. Split
further only when a part is independently deployable or has materially different review/rollback risk.

Before pushing, run the focused tests, the full Maven build, and `git diff --check`. Do not commit server runtime data,
credentials, generated JARs, dependency caches, IDE output, or unrelated formatting.

## Safe change checklist

- Trace whether the code runs on the connector worker, proxy thread, Bukkit primary thread, or a SQL executor.
- Add strict type/field/range/count validation before calling plugin services.
- Snapshot synchronized live collections before iterating; do not return mutable collections across threads.
- Distinguish “not configured/unavailable”, “not found”, and a genuine empty result.
- Test unknown fields, invalid bounds, disabled VoteLogging, oversized results, exact-player misses, non-creating resolution,
  reward no-side-effects, lease retry/idempotency, and redaction as applicable.
- Preserve connector shutdown bounds and avoid blocking waits on Bukkit lifecycle paths.
