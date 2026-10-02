# NeoForge bootstrap status

The single `VotingPlugin.jar` contains an experimental NeoForge 21.1 entry point for Minecraft 1.21.1. Native vote ingress remains disabled. Bukkit and proxy behavior remains on its existing paths.

On server start, the entry point creates `config/votingplugin/`, installs the packaged `Config.yml`, `VoteSites.yml`, and `SpecialRewards.yml` only when missing, reads them, and opens the existing AdvancedCore SQLite user backend in `VotingPlugin.db`. The bootstrap currently accepts `DataStorage: SQLITE` only. Its separate `VotingPlugin_NeoForgeUsers` table stores the player name, all-time/monthly/daily/weekly totals, points, and per-site last-vote timestamps needed by the shared accounting step. Existing UUID-only bootstrap tables are extended in place. The accounting adapter is a storage foundation and is not wired to live vote ingress. The runtime closes the backend and clears queued tasks and online player identities on server stop.

The NeoForge bridge tracks login/logout identity and runs submitted bootstrap tasks on server ticks. Player identity access uses reflection because NeoForge's published universal Maven JAR does not include Minecraft classes on Maven's compile path; these method names were checked against the installed 1.21.1 server JAR.

An internal synthetic vote boundary now supports identity and enabled-site resolution, Bukkit-compatible vote-delay decisions, and the existing shared totals/points policy in one atomic SQLite transaction. `ACCOUNTING_ONLY` is deliberately an internal test/migration scope and returns `ACCOUNTED`, rather than claiming that a production vote completed. `COMPLETE` votes do not change timestamps, totals, or points. They are retained in an ordered SQLite inbox under their stable vote ID until NeoForge implements rewards, offline queues, vote party, broadcasts, streaks, milestones, cooldown actions, post-vote events, and placeholder updates. Repeated submission of the same vote ID is idempotent even after player or site state changes. The inbox admits at most 64 votes per player and 4,096 votes across the runtime. A full inbox returns an explicit capacity result that a future ingress must treat as non-success.

Native Votifier ingress remains disabled. VotifierPlus's hardened packet handling is currently packaged with its Bukkit, BungeeCord, and Velocity plugin runtime rather than as a platform-neutral API that this single VotingPlugin artifact can reuse. Depending on that full plugin would pull unrelated platform code into VotingPlugin, while copying its security-sensitive protocol implementation would create a second implementation.

## Proxy vote delivery

NeoForge can receive real votes from ordinary VotingPlugin installations on BungeeCord and Velocity through the existing encrypted `SOCKETS` transport. Both proxy platforms use the same VotingPlugin proxy runtime and wire protocol. The backend durably retains a COMPLETE vote before sending its delivery acknowledgement; a duplicate stable vote ID is acknowledged idempotently and cannot create another pending occurrence or repeat an already completed vote.

Copy the same `secretkey.key` to the proxy VotingPlugin directory and `config/votingplugin/` on the NeoForge server. Keep this key private. Also create a separate random 32-byte base64 socket authentication key for this backend and copy only that file to the proxy and this NeoForge backend. Do not reuse one backend's socket authentication key for another backend. On the NeoForge backend, configure `BungeeSettings.yml` with values matching the proxy's server map:

```yaml
UseBungeecord: true
BungeeMethod: SOCKETS
Server: neoforge-survival
BungeeServer:
  Name: proxy1
  Host: 127.0.0.1
  Port: 1297
SpigotServer:
  Host: 0.0.0.0
  Port: 1298
SocketAuthenticationKeyFile: neoforge-socket-auth.key
```

On either BungeeCord or Velocity, use the normal proxy `bungeeconfig.yml` socket setup and map the same backend name to the NeoForge listener:

```yaml
BungeeMethod: SOCKETS
BungeeServer:
  Host: 0.0.0.0
  Port: 1297
SpigotServers:
  neoforge-survival:
    Host: 127.0.0.1
    Port: 1298
    AuthenticationKeyFile: neoforge-socket-auth.key
BungeeManageTotals: false
```

`BungeeServer.Name` must match the proxy's `ProxyServerName`. Authentication covers the complete envelope, sender and destination identities, a timestamp, and a bounded replay nonce before NeoForge admits the vote. Existing Bukkit socket backends without `AuthenticationKeyFile` retain their legacy behavior; NeoForge refuses to start its socket receiver without this per-backend key.

`BungeeManageTotals` must remain `false` for a NeoForge target in this version because NeoForge does not yet import proxy-owned total snapshots. The NeoForge receiver rejects rather than acknowledges incompatible managed-total or batched envelopes. Presence, status probes, stable vote IDs, durable delivery acknowledgements, receipt-release acknowledgements, and restart retries use the existing proxy protocol. A full deferred inbox is not acknowledged, so the proxy retains and retries the occurrence.

NeoForge cannot execute delegated backend broadcasts. Envelopes with `vote.broadcast=true` are rejected before retention, accounting, and acknowledgement; the proxy keeps its durable delivery instead of treating an omitted broadcast as complete. `ProxyBroadcast.Enabled: false` delegates broadcasts to backends and therefore does not disable this requirement. For new votes, configure proxy broadcast routing to exclude the NeoForge backend (for example `ProxyBroadcast.Enabled: true`, `Scope.Mode: ALL_EXCEPT`, and `Scope.Servers: [neoforge]`, using its canonical backend name). Already retained proxy envelopes preserve their original broadcast flag and remain unacknowledged until that side effect can be supported or explicitly reconciled; changing configuration does not rewrite them. Backend-local `VoteBroadcast` settings remain subject to the unsupported-feature limits described below.

Current transport support is:

| Path | NeoForge status |
| --- | --- |
| BungeeCord `SOCKETS` | Supported |
| Velocity `SOCKETS` | Supported |
| Native Votifier packets | Not yet supported |
| Plugin messaging | Not yet supported |
| HTTP | Not yet supported |
| Redis / MQTT / MySQL | Not yet supported |

Only the reward subset described below can complete. Votes requiring unsupported reward or post-vote semantics remain durable and observable instead of being acknowledged as fully processed and lost.

Deferred votes can now be claimed in memory and completed through one SQLite transaction that writes a completion receipt and removes the pending payload. An abandoned claim leaves the durable payload pending, while a committed transaction leaves no pending-without-receipt window. Completion receipts are bounded to 4,096 per player and 262,144 across the runtime. They are not evicted by age because no native ingress acknowledgement can yet prove that a sender has stopped retrying. If either receipt limit is full, completion stops and keeps the original pending payload. A later ingress handshake may add safe receipt release with a bounded post-release tombstone, following the existing proxy delivery pattern. A bounded replay worker now processes at most 16 retained votes per scan. It supports one existing inline `Messages.Player` or `Commands` action per vote. Player messages and console commands run on the NeoForge server tick lane; SQLite scans and completion writes remain on the replay worker. The worker validates the entire reward tree before running any action. A vote stays pending with an observable blocked or waiting result if it requires multiple or unsupported reward actions, `AnySiteRewards`, an offline player message, vote broadcasts, vote party, streaks, milestones, monthly vote limiting, cooldown events, `CloseInventoryOnVote`, or `WaitUntilVoteDelay` processing. An ambiguous external action or completion-write result is durably quarantined and remains fenced across restart for operator reconciliation. Legacy retained rows without an accepted accounting snapshot also remain blocked instead of being recalculated with newer configuration. The packaged defaults enable some unsupported features, so administrators must explicitly disable them before retained votes can complete. Configuration changes currently take effect after a server restart.

Before a supported external action is dispatched, replay durably fences the pending occurrence. After the action succeeds, accounting, the completion receipt, and pending removal commit in one SQLite transaction. A completed vote ID remains fenced across restart and cannot rerun accounting. If the JVM or completion write fails after the pre-dispatch fence, automatic replay stays stopped for operator reconciliation, so it cannot blindly repeat a command or message. A crash between writing that fence and executing the action is indistinguishable from a crash immediately after execution and can therefore leave an action omitted unless an operator retries it; a manual retry after an uncertain outcome can instead duplicate it. The current reward APIs cannot make a Minecraft command and SQLite commit atomic, so this version does not claim exactly-once external side effects.

`SharedVoteProcessor.Operations` was reviewed operation by operation. NeoForge can currently implement its configuration, identity, site, delay, timestamp, and accounting operations. Logging and proxy-only identity fields have no processing effect at this internal non-proxy boundary. Reward delivery, offline queuing, vote party, broadcast, inventory/effects, streak, milestone, cooldown, post-event, and placeholder operations require future platform services. The adapter therefore reuses `SharedVoteIdentity`, `SharedVoteInput`, `SharedVotePolicy`, and `SharedVoteAccounting` without implementing the broad Bukkit production interface with false no-ops.

Validation for this step covers unit startup/shutdown, isolated classloader startup/shutdown from the packaged JAR, and a NeoForge 21.1.211 dedicated server smoke run. The server reached its ready state, initialized the bootstrap, and exited cleanly after `stop`. This does not verify player joins, vote receipt, or rewards.

### Proxy receipt release before deferred completion

A reliable proxy may retire its outbox entry once NeoForge durably owns the
pending occurrence. Receipt release is therefore acknowledged while a vote is
still pending: the inbox transaction records a release-request flag, without
claiming rewards or accounting completed. Pending payloads and their stable IDs
remain durable, bounded and duplicate-fenced across restart. Quarantine and
replay-context changes preserve this flag.

Replay reserves bounded released-receipt capacity before external actions.
Completion atomically writes accounting, a released completion tombstone and
pending removal. The seven-day released-tombstone retention begins at completion,
not at the earlier release request; pending occurrences never expire on that
timer. Capacity exhaustion retains pending work and prevents effect execution.
A release racing an active claim is retried after that claim completes or closes.
Existing pending v1/v2/v3 rows remain readable; release-requested rows use v4
within the existing storage column. No configuration or SQL schema change is
required. The external-action uncertainty/quarantine boundary remains unchanged.
