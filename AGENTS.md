# Maintainer and AI-agent guide

VotingPlugin is the vote-processing data plane for Bukkit/Paper and BungeeCord/Velocity networks. The optional Control
integration is a management adapter, never a runtime dependency: vote receipt, routing, storage, rewards, joins, commands,
reload, and shutdown must keep working when Control is disabled, unreachable, incompatible, or restarting.

## Security threat model

For security reviews, vulnerability triage, and security-sensitive changes, read `docs/security-threat-model.md` before classifying or fixing findings. Treat it as the repository-specific attacker/trust-boundary model; verify every conclusion against current code and tests. Do not promote compatibility, trusted-operator behavior, or generic correctness bugs into security findings unless the documented boundary is actually crossed.

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

Keep the downloadable VotingPlugin JAR as small as practical. Inspect the shaded
artifact when dependencies change, avoid duplicate embedded packages, and update
the package-phase size and runtime checks when a necessary dependency increases
the artifact budget.

## Drop-in upgrade and compatibility contract

Treat compatibility as a release invariant for every new feature, refactor, fix, storage change, protocol change, and dependency change. Unless the task explicitly says otherwise, a VotingPlugin upgrade must be a **drop-in JAR replacement**: an administrator replaces the existing VotingPlugin JAR, starts/reloads as normally supported, and does not need any other deployment change.

That default contract means:

- Do not require manual edits, regenerated configs, deleted keys, renamed files, data resets, one-off conversion commands/scripts, or manual database changes. New keys must have safe defaults when absent and preserve established behavior.
- Existing YAML, vote-site definitions, rewards, user data, vote totals/streaks, cached/offline votes, logged data, and supported database state must continue to load. Required migrations must be automatic, idempotent, restart-safe, and preserve existing state.
- Do not require a simultaneous update of AdvancedCore, SimpleAPI, Votifier/VotifierPlus, PlaceholderAPI, VotingPlugin-Control, a proxy JAR, backend JARs, or every server in a network merely to keep previously working behavior working. New cross-component behavior must be additive/capability-negotiated with safe fallback for older peers.
- Preserve existing commands, permissions, placeholders, events, public/de-facto APIs, configuration semantics, proxy methods, message/reward behavior, and supported platform behavior unless the request explicitly authorizes a break.
- Mixed-version proxy/backend deployments must fail safe and retain legacy behavior for features not mutually supported; do not make upgrade order a hidden requirement.
- New optional integrations and features must default to non-disruptive behavior for existing installations and must not become mandatory runtime dependencies.
- Packaging changes must preserve the normal downloadable artifact and startup path. Do not make administrators install extra libraries or companion JARs for an upgrade unless explicitly requested.
- When a requested implementation cannot preserve drop-in compatibility, stop and clearly surface the required compatibility break before implementing it unless the request explicitly permits that break.

For compatibility-sensitive changes, add regression coverage that exercises the prior installation/state or protocol shape as well as the new behavior. Review the final diff from the perspective of an administrator upgrading only the VotingPlugin JAR on an existing installation.

## Architecture and file map

- `VotingPluginMain` is the Bukkit entry point and lifecycle owner.
- `com.bencodez.votingplugin.core` and its subpackages are platform-independent. Classes there must not import Bukkit, Paper, Fabric, Forge, or NeoForge APIs. Keep loader-specific adapters outside this package and enforce the boundary with `CorePlatformIsolationTest`.
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

## Threading and user-data invariants

- Treat AdvancedCore/VotingPlugin user-data, cache, and storage APIs as potentially blocking unless an API is explicitly documented as snapshot-only. Do not perform cache population, SQL-backed reads or writes, flush/dump/clear/remove operations, or shared-runtime admission on the Bukkit/Paper primary server thread. Capture platform-owned state there, hand user-data work to the existing persistence/storage worker, and schedule only the required Bukkit/Folia interaction back onto the platform owner.
- Preserve the shared-user lock order: shared-runtime/per-user admission before the `UserDataCache` monitor. Never hold `synchronized (UserDataCache)` while calling APIs that can acquire shared-runtime admission, including `dump()`, `clearCache()`, `removeCache()`, cache population, or storage access. Keep cache-monitor sections short and cache-local.

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
   raw logs, or unrestricted player records. Keep diagnostics deliberately redacted. Unexpected configuration/read/reload
   exceptions return fixed action-specific external text and keep their detailed cause only in the backend log. Unexpected
   inspection exceptions return a generic external message; local logging may identify the exception class but must omit
   its message.
9. An inspection's `player` query is exact name or UUID lookup and must check existence before loading. Do not turn it into
   enumeration or autocomplete.
10. A reward inspection only validates/normalizes a typed proposal. It must report `wouldExecute:false` and
    `sideEffects:false`; persistence still goes through configuration preview/apply.

## Control connector lanes

Keep these paths separate:

- discovery/presence advertises current node identity and topology;
- configuration capabilities (`config.*.v1` plus explicitly negotiated successors) poll `/operations`, may
  read/preview/apply typed configuration, and journal results;
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
- general result rows: 100 (including detected plugin names in diagnostics);
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
at-most-100 displayed configured services through prepared exact filters. Health matching and SQL aggregation use the
full, case-normalized ServiceSite (up to the 2048-character validator bound); truncate only serialized display fields,
and classify unmatched logged services against every configured site rather than only the displayed page. Do not accept
raw SQL from Control or expose the database/table configuration.

## Proxy vote lifecycle and durability invariants

Treat every proxy-side Votifier event as durable work as soon as VotingPlugin accepts it. A Java stack frame, scheduled
task, executor queue, or platform scheduler entry is **not** durable ownership.

- Before any fallible or cancellable scheduler/executor handoff, transfer the accepted vote into VotingPlugin-owned state.
  At every point afterward, the vote must be either: (a) actively processing against a runtime protected from teardown,
  (b) present in a bounded process-owned pending queue, or (c) durably persisted for restart recovery. There must be no
  gap where scheduler admission is the only thing preventing loss.
- Scheduler callbacks are wakeups, not owners. Rejection, plugin disable, proxy shutdown, `shutdownNow()`, task
  cancellation, executor replacement, or a platform scheduler refusing work must leave the vote owned in pending/durable
  state with the same vote ID.
- Preserve one stable `voteId` from Votifier acceptance through reload waiting, storage retry, durable handoff, restart,
  backend delivery, reward execution, and completion fencing. Never generate a replacement ID merely because an attempt
  was rescheduled or recovered.
- Runtime lifecycle transitions must have explicit semantics:
  - operational: votes may enter the protected runtime;
  - reloading/replacing: accepted votes wait in owned pending state and do not consume storage-failure retry budget merely
    for waiting on lifecycle replacement;
  - terminal/unavailable: do not invoke the broken/disposed runtime; hand pending work to durable recovery where possible
    and emit an explicit diagnostic if persistence fails.
- Publish a replacement runtime as operational only after required transport state, caches/storage, periodic tasks, and
  required Votifier listener registration are ready. Publish readiness before clearing the reload fence. Never expose a
  transient `reloading=false && runtimeOperational=false` state on an otherwise successful replacement that could cause
  a vote to be mistaken for terminal failure.
- Protect runtime selection and teardown with the same lifecycle fence. A vote that has selected a runtime must not race
  teardown of that runtime. Conversely, teardown must not proceed while accepted process-owned votes have no durable
  recovery path.
- Before retiring the old runtime during a full replacement, persist or otherwise durably transfer all accepted pending
  votes. If that transfer fails while the predecessor can still safely run, abort replacement and keep the predecessor.
  Final shutdown may continue only with an explicit reconciliation/error signal for any work that could not be persisted.
- Do not remove a pending vote merely because a retry was scheduled. Remove process-owned state only after the vote is
  safely processed/completed or a durable recovery record has been confirmed. Scheduling success does not imply future
  execution; a subsequent shutdown/cancellation can still prevent the callback from running.
- Exactly-once safety must be considered across totals, points, streaks, VoteParty, rewards, broadcasts, backend delivery,
  and completion acknowledgement. At-least-once transport is acceptable only when the stable vote ID and durable
  completion fences prevent duplicate externally visible vote effects.
- Bungee/Waterfall and Velocity must have equivalent lifecycle guarantees. A fix on one proxy platform is incomplete until
  the other platform is audited and either changed or explicitly proven safe with tests.
- Plugin-message queues and vote queues are separate durability concerns. Reload-abort and retained-runtime paths must
  drain/retry queued messages against the still-valid runtime; do not strand them until an unrelated future reload.
- Distinguish graceful lifecycle guarantees from unavoidable hard-crash boundaries. Do not claim graceful reload/shutdown
  safety if accepted work can still disappear through scheduler rejection or cancellation. Separately document any
  remaining process-crash boundary where an external side effect can occur before its durable completion record.

For any change touching proxy vote receipt, `VoteEventBungee`, `VoteEventVelocity`, `VotingPluginBungee`,
`VotingPluginVelocity`, `VotingPluginProxy.vote(...)`, proxy schedulers/executors, pending vote/cache state, transport
handoff, reload, runtime replacement, or shutdown, the review must explicitly trace and test these interleavings:

1. vote arrives immediately before reload starts;
2. vote arrives after the reload fence is raised but before predecessor retirement;
3. vote is accepted while replacement is in progress;
4. scheduler/executor rejects the wakeup after the vote is accepted;
5. scheduler accepts the wakeup, then shutdown/cancellation occurs before it runs;
6. replacement preparation fails while the predecessor is still safe;
7. replacement fails after predecessor retirement;
8. successful replacement drains pending votes exactly once;
9. soft reload fails and a later successful soft reload restores readiness;
10. graceful shutdown persists pending accepted votes before stopping scheduling infrastructure;
11. restart replays durable pending votes with the original vote ID and does not replay them again after completion;
12. multiple concurrent pending votes remain independently owned and cannot overwrite or collapse into one another.

Use deterministic tests with barriers, latches, fake/rejecting schedulers, or explicit lifecycle seams. Do not rely on sleeps
or timing luck to prove race safety. A green build without these interleaving checks is not sufficient evidence for a
proxy lifecycle/vote-durability change.

For transport-selection changes, additionally prove that generic reward-journal/cache IDs do not fabricate transport
provenance. Genuine retained HTTP work must survive handoff; cached reward IDs from a non-HTTP installation must not force
HTTP startup. Corrupt/missing retained listener state, parked HTTP queues, failed retained HTTP startup, undeletable
retained state, and fallback to the configured transport must be covered without requiring manual cache/config cleanup.

## Change and PR workflow

Keep changes focused and avoid unrelated formatting. Before any commit, push, PR update, review reply, or other remote change:

1. run relevant focused tests;
2. run `mvn -B -f VotingPlugin/pom.xml clean package`;
3. verify the package invocation produced a fresh downloadable JAR and that expected tests were discovered;
4. run `git diff --cached --check` and `git diff --check`;
5. inspect `git diff --cached` and `git diff`, then inspect the complete base-to-HEAD diff for compatibility, concurrency, persistence, lifecycle, security, packaging, and platform regressions.

Steps 1-3 may be skipped only for documentation/instruction-only changes that do not modify executable source, tests, build or dependency configuration, workflows, packaged resources, generated output, or runtime/deployment behavior. Record that exemption in the PR. Steps 4-5 and the review requirements below still apply.

For substantive work, obtain a fresh source-read-only `$code-review` of the exact intended change before the first push or PR update. The implementation agent verifies and fixes accepted findings, reruns all required checks, and obtains a new review of the updated snapshot. Any substantive repository change after a clean review—including source, tests, build or dependency configuration, workflow files, resources, contracts, documentation, or instructions—invalidates the previous clean verdict. Rerun applicable validation and obtain a fresh review of the exact intended snapshot; do not reuse an earlier verdict. Hosted PR review is confirmation, not the first full review, and merge still requires explicit authorization.

Do not commit server runtime data, credentials, generated JARs, dependency caches, IDE output, or unrelated formatting.

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

`config.proxy-method.v1` covers plugin messaging, Redis, MQTT, sockets, and MySQL; `config.proxy-method.v2` adds HTTP. Dispatch and validate the
exact capability for the requested method. `config.quick-setup.v2` adds `VoteParty.Enabled`; keep legacy Vote Party
payloads on v1, preserve the installed Enabled value when they omit it, and reject the `enabled` field unless v2 was
accepted. The VotingPlugin connector may deploy first and
advertise these successors without using them until Control accepts them. A newer Control deployed first must leave its
v2-only actions unavailable on older nodes. Merge the VotingPlugin capability implementation before relying on the new
Control behavior in production.

## Safe change checklist

- Trace whether the code runs on the connector worker, proxy thread, Bukkit primary thread, or a SQL executor.
- Preserve queued votes across saturation, reload, runtime replacement, scheduler rejection/cancellation, shutdown, and restart; overflow handling must be bounded, durable when promised, and observable rather than silently dropping work. Scheduler admission is never proof of durable ownership.
- Proxy-to-backend guaranteed delivery is capability negotiated and at least once. Journal a reward-bearing envelope before
  reporting transport acceptance, retain it until the matching backend completion acknowledgement is durable, persist
  completed IDs before acknowledgement for restart-safe deduplication, retire receipts only through the durable
  proxy-confirmed release handshake, retain a bounded durable tombstone for in-flight retries, and keep legacy send
  behavior for backends that do not advertise the capability.
- Treat scheduler units explicitly. Verify whether each delay is in ticks, milliseconds, or seconds, especially across Bukkit, Folia, BungeeCord, and Velocity adapters.
- Register listeners and lifecycle wakeups before producers can publish work; startup/reload ordering must not strand already-persisted or newly-arriving operations. For proxy Votifier paths, runtime readiness is not published until listener registration succeeds, and reload waiting must retain the vote independently of the scheduler.
- Protocol-mode changes must not silently broaden legacy v1/RSA acceptance when token-only operation is configured or intended; cover downgrade behavior with tests.
- Add strict type/field/range/count validation before calling plugin services.
- Snapshot synchronized live collections before iterating; do not return mutable collections across threads.
- Distinguish “not configured/unavailable”, “not found”, and a genuine empty result.
- Test unknown fields, invalid bounds, disabled VoteLogging, oversized results, exact-player misses, non-creating resolution,
  reward no-side-effects, lease retry/idempotency, and redaction as applicable.
- Preserve connector shutdown bounds and avoid blocking waits on Bukkit lifecycle paths.

<!-- mex-agent:skills:start -->
## MEX agent skills
- At the start of every session, read `.mex/AGENTS.md` and `.mex/ROUTER.md` before project work; follow `ROUTER.md` to load only the relevant context.
- Read `mex logging --json` at session start and before optional logging. Its checkout-local advisory mode is `significant` (quiet default: material decisions, risks, blockers, or durable discoveries), `checkpoints` (batch useful notes at task/session boundaries), or `manual` (no unsolicited notes). Skip routine tool calls, edits, repeated status, and empty summaries. Honor explicit user log requests in every mode; never suppress mandatory workflow Activity or recovery audit records. Report a policy read failure instead of guessing or changing the preference.
- When earlier work may inform the task, retrieve bounded relevant notes with `mex timeline --query "subject phrase" --file src/example.ts --limit 10 --json`, using the known subject or exact recorded file path, or both. Treat matches as historical evidence, not accepted current knowledge; verify conclusions before reuse or explicit promotion with their source retained.
- Use `$mex-inbox` for explicit contributions to project knowledge and `$mex-relay` for durable team handoffs. Invoke them automatically when intent clearly matches; ordinary GROW upkeep remains available without Inbox.
- When MEX context materially helps your work, mention MEX and the relevant finding naturally in your explanation. Tie the mention to what it helped you understand, decide, or verify. Avoid fixed phrases, standalone acknowledgements, repeated mentions, or narrating routine context loading. This replaces older MEX instructions requiring a fixed acknowledgement or context-loading narration.
- Do not claim an author, date, or historical event unless the retrieved data actually provides it.
- After a MEX write, say exactly what changed and its sharing boundary: a local draft is checkout-only and nothing is shared; a canonical artifact is written to the working tree and requires commit/push to share.
- Skill activation is not approval for canonical actions.
<!-- mex-agent:skills:end -->
