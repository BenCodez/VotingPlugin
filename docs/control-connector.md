# VotingPlugin Control discovery connector

This development milestone adds an optional connector from the BungeeCord and Velocity VotingPlugin proxy JAR
to the separate [VotingPlugin Control](https://github.com/BenCodez/VotingPlugin-Control) application. It discovers each
proxy and its eligible backend server names. Enrolled Bukkit nodes can also connect outbound for full configuration-file
and quick-setup control. Control includes the local WebUI. VotingPlugin may now explicitly opt in to
provisioning and supervising that application from either a proxy or a Bukkit backend as a separate child JVM; it is
never loaded into the proxy or server classloader. The
connector does not expose backend addresses. Configuration secrets are masked on every read and never enter audit records.

## Failure isolation

The connector is disabled by default. Missing `Control` keys therefore preserve existing installations exactly. When
enabled, startup performs only bounded local configuration/credential reads; network requests use Java's asynchronous HTTP
client outside proxy event-loop/main threads. There is at most one in-flight discovery cycle and no request queue.

Control being stopped, slow, malformed, unauthorized, or protocol-incompatible changes only connector status and its
rate-limited `[Control]` log. Voting, proxy startup, joins, server switches, existing plugin messages, and shutdown do not
call into or wait on Control. Retries use exponential backoff from one second to five minutes with per-process jitter.
Shutdown cancels the scheduled cycle and active future without waiting indefinitely.

The hosted-Control manager is independently disabled by default. Downloads, hashing, activation, health checks, process
startup, and restart backoff run on its own daemon worker. Hosting failure changes only its redacted status/log and never
blocks the proxy event loop, Bukkit server thread, or vote processing. Stopping VotingPlugin asks only the child process it created to terminate
and applies a bounded asynchronous force-stop fallback.

## Optional hosted WebUI

Use one hosted Control instance for the network. It may run on one BungeeCord/Velocity proxy or on one Bukkit/Paper backend.
Every other node should use its connector against that instance rather than starting a competing service. Hosting and
discovery are separate switches: `Control.Hosted.Enabled` runs Control, while proxy `Control.Enabled` or backend
`Control.Backend.Enabled` enrolls and reports that node to it.

Control releases publish `votingplugin-control.jar` with GitHub's SHA-256 asset digest. The default blank `DownloadUrl`
and `Sha256` select the official latest stable release from `BenCodez/VotingPlugin-Control`. VotingPlugin validates the
repository, stable semantic-version tag, exact asset name and immutable versioned download URL, then verifies the JAR
against GitHub's published digest before activation. With `AutoUpdate` enabled, it repeats this check every six hours.
Set both `DownloadUrl` and `Sha256` to opt out of release tracking and manually pin an exact artifact. Manual `/latest/`
URLs, partial pins, plaintext download URLs, redirects outside HTTPS, and paths outside VotingPlugin's data folder are rejected.

```yaml
Control:
  Enabled: true
  Endpoint: 'http://127.0.0.1:8080'
  NodeId: ''
  CredentialFile: 'control/control-credential.txt'
  Hosted:
    Enabled: true
    AutoDownload: true
    AutoUpdate: true
    DownloadUrl: ''
    Sha256: ''
    JarFile: 'control/votingplugin-control.jar'
    DataDirectory: 'control/data'
    Host: '127.0.0.1'
    Port: 8080
    StartupTimeoutSeconds: 30
    DownloadTimeoutSeconds: 60
```

The same hosted block may instead be placed in the backend's `Config.yml`, alongside `Control.Backend`. In this example the
backend both hosts Control and enrolls itself for full configuration management:

```yaml
Control:
  Hosted:
    Enabled: true
    AutoDownload: true
    AutoUpdate: true
    DownloadUrl: ''
    Sha256: ''
    JarFile: 'control/votingplugin-control.jar'
    DataDirectory: 'control/data'
    Host: '127.0.0.1'
    Port: 8080
    StartupTimeoutSeconds: 30
    DownloadTimeoutSeconds: 60
  Backend:
    Enabled: true
    NodeId: ''
    Endpoint: 'http://127.0.0.1:8080'
    CredentialFile: 'control/control-credential.txt'
    HeartbeatSeconds: 30
    ConnectTimeoutMillis: 3000
    RequestTimeoutMillis: 10000
```

On first start, the artifact is downloaded to a unique staging file with a 64 MiB hard limit, verified, and atomically
activated before launch with the same Java runtime as the hosting proxy or backend. The last verified official release
metadata is cached beside the hosted JAR so a temporary GitHub API outage does not prevent an already verified install
from starting. Automatic updates are staged and hash-verified while the current process remains healthy; only then is the
service restarted. The current release is retained as `.previous`. A failed process/protocol health check atomically
restores and starts that previous release, retaining the failed candidate as `.failed`. A quarantined digest is not retried
until GitHub publishes a newer release. Unexpected exits use bounded restart backoff.

The child's explicit management inputs are its bind host, port, contained data directory, a random per-launch ID, and the
supervising process ID so it can stop when its parent disappears. The launcher clears the inherited environment and copies
only a fixed allow-list of locale, temporary-directory, and Windows runtime variables. Health must echo the launch ID,
preventing an unrelated Control process on the same port from satisfying startup checks. It does not receive VotingPlugin
configuration or credentials in release metadata. Process output is written beside the hosted JAR; before a child launch,
an existing log larger than 1 MiB is moved to the single previous-log slot.

On first start, Control creates an owner-readable `web-setup-code.txt` inside `Control.Hosted.DataDirectory`. Open the
WebUI, copy that one-time value using the server file manager, and choose the WebUI password in the browser. The code is
consumed immediately, and no server command is required. Password rotation invalidates every existing browser session.
Use HTTPS or a private authenticated tunnel when exposing the listener outside a trusted private network.

Manual pinning remains supported: set an immutable versioned `DownloadUrl` and its exact `Sha256`. For a fully offline
manual installation, put the JAR at `JarFile`, leave `DownloadUrl` blank, configure its exact `Sha256`, and set both
`AutoDownload` and `AutoUpdate` false. VotingPlugin will verify and supervise it without network access.

## Enrollment and configuration

The easiest network setup hosts one Control instance on the proxy. Configure `bungeeconfig.yml` as below, using a Control
release that supports the `enroll-verifier` owner command. On first start VotingPlugin creates a random proxy credential
under `plugins/VotingPlugin/control/`, installs only its SHA-256 verifier into the hosted Control data directory, and starts
the connector. No enrollment command or credential copy is required.

Configure `bungeeconfig.yml`:

   ```yaml
   Control:
     Enabled: true
     Endpoint: 'http://127.0.0.1:8080'
     NodeId: ''
     CredentialFile: 'control/control-credential.txt'
     HeartbeatSeconds: 30
     ConnectTimeoutMillis: 3000
     RequestTimeoutMillis: 5000
   ```

`NodeId` may be blank to reuse the existing `ProxyServerName`, but every simultaneously connected proxy must have a unique,
stable identity. Automatic enrollment is enabled only when the connector endpoint addresses that same hosted listener by
loopback or by its configured bind address and port. `CredentialFile` must resolve inside VotingPlugin's data directory.
Its missing parent directories are created automatically; credentials in URLs and parent-directory traversal are rejected.
The generated credential is never logged or passed to the child process.

The endpoint must be an `http` or `https` origin without embedded credentials, query, fragment, or path. Plain HTTP is
suitable only for loopback or a trusted private network. Use HTTPS or a private authenticated tunnel when crossing an
untrusted network. Control authentication does not itself encrypt traffic.

Proxy discovery-connector timing bounds are intentional:

- heartbeat: 10–300 seconds;
- connect/request timeout: 500–30,000 milliseconds;
- response body: at most 64 KiB;
- backend snapshot: at most 4096 entries.

Do not raise these by patching around validation; correct the topology or connectivity problem instead.

### Bukkit full-configuration enrollment

Each backend is separately opt-in and receives its own credential so one node cannot impersonate another. With
`BungeeMethod: PLUGINMESSAGING`, configure each backend's `Config.yml` and point `Endpoint` at the proxy-hosted listener:

```yaml
Control:
  Backend:
    Enabled: true
    NodeId: ''
    Endpoint: 'http://<proxy-private-address>:8080'
    CredentialFile: 'control/control-credential.txt'
    HeartbeatSeconds: 30
    ConnectTimeoutMillis: 3000
    RequestTimeoutMillis: 10000
```

The Bukkit connector uses the same 10–300-second heartbeat and 500–30,000-millisecond timeout ranges, but its bounded
response-body limit is 4 MiB so it can receive managed-file/configuration tasks. Do not apply the proxy connector's 64 KiB
discovery-response bound to this lane.

Use an address the backend itself can reach, normally the proxy VM/private IP. Proxy-mediated enrollment deliberately
rejects `localhost`, `127.0.0.0/8`, and IPv6 loopback because those addresses resolve to the backend rather than the proxy
when the processes run on different machines.

Blank `NodeId` reuses `BungeeSettings.Server`. The backend generates the raw credential locally and sends only its SHA-256
verifier over the plugin-message channel. Velocity/BungeeCord binds the request to the actual backend server connection,
requires that identity to equal `BungeeSettings.Server`, installs the verifier in its hosted Control, and returns a
non-secret acknowledgement. The pending marker is removed only after the backend also authenticates to its configured
Control endpoint, so an incorrect endpoint cannot strand a newly generated credential. The raw credential never leaves the
backend. Requests retry safely after restarts and while no player is available to carry plugin messages.

If a Bukkit server hosts its own Control and its endpoint addresses that local listener, the same generation and verifier
installation happen locally. External Control installations, custom backend node IDs that differ from
`BungeeSettings.Server`, and non-plugin-message transports retain manual WebUI/owner-command enrollment; an existing
nonblank credential file is always treated as manually managed and is never replaced.

The Bukkit connector owns separate single-thread daemon executors for presence/configuration work and read-only
inspections, and performs no Control I/O on the server thread. The inspection worker is cancelled on shutdown with a
bounded five-second wait, so a slow database read does not hold the configuration lane or shutdown indefinitely. The
connector reports a bounded list of installed plugin names for WebUI command suggestions and negotiates
`config.files.v1`, `config.quick-setup.v1`, and the separate read-only `data.inspect.v1` capability. It polls configuration
operations and inspections over distinct outbound queues. Repeated inspection transport or protocol failures use bounded
exponential backoff from one second to five minutes, while the configuration and voting paths remain available. File apply
schedules the VotingPlugin reload on the Bukkit thread and waits only on the connector worker. Control failure never blocks votes,
joins, commands, or plugin shutdown. A successful `Config.yml` apply reports its result first, then recreates the connector
so changes to `Control.Backend` take effect without a full server restart. If `Control.Hosted` changed, a dedicated daemon
lifecycle worker waits for the existing child to stop only after that result is acknowledged, then starts the replacement
with the new settings. This prevents the old and new children from racing for the same listener. Invalid host settings leave
the currently running Control child unchanged. If the backend restarts with a pending result, the durable journal also
restores the previous hosted settings until that result is acknowledged, preserving the recovery connector's endpoint.

## Discovery semantics

Both platforms use the same connector implementation and protocol version `1`. Each proxy process creates a new session
ID, registers, and sends full replacement backend snapshots with a monotonically increasing sequence. Registration and
retries are idempotent. A Control restart is detected by a 404 heartbeat/presence response and causes re-registration.

Backend identity preserves VotingPlugin's existing eligible server name when it is already a protocol-safe identifier;
other configured names receive a deterministic SHA-256-derived ID while retaining a bounded safe display name. For transports that support VotingPlugin's backend
presence protocol, the existing `BackendPlayerPresenceTracker` supplies availability and player count. Where that protocol
has no authoritative observation, the backend remains discoverable with `presenceKnown: false`; the connector does not
invent an online state from a server address or arbitrary wall-clock timestamp.

The connector requires `presence.snapshot` and advertises `config.proxy-routing.v1` when its typed adapter is available.
The latter permits reads and preview/apply of only `SendVotesToAllServers` and `BlockedServers`. Every preview validates
blocked names as exact, case-preserving matches against that proxy's configured backends and returns a deterministic
SHA-256 revision. Apply rejects stale
revisions, writes a local `.control-backup`, requires atomic activation of a staged YAML file, and soft-reloads the
proxy. A reload failure triggers immediate backup restoration and another reload attempt; Control receives the per-node
reload/rollback result. Task results are cached by operation ID so a leased retry cannot apply the same change twice.

For enrolled Bukkit nodes, the WebUI can read and edit all six user-facing files: `Config.yml`, `VoteSites.yml`,
`SpecialRewards.yml`, `GUI.yml`, `Shop.yml`, and `BungeeSettings.yml`. YAML is parsed before preview; each apply uses an
exact SHA-256 revision, stages and atomically installs the file, retains `.control-backup`, reloads, and restores the backup
if reload fails. Returned YAML is normalized and masks password/secret/token/API-key/authorization/webhook-secret paths
with `__VOTINGPLUGIN_CONTROL_REDACTED__`; leaving the marker unchanged preserves the local value. A replacement secret may
be submitted through the authenticated preview, but is never returned or audited.

Control configuration snapshots store the redacted managed-file content returned by this read path, not raw credentials.
Restore resolves unchanged markers against each target's current secrets during preview/apply. Protect Control's data
directory anyway because snapshots contain complete managed configuration structure and operational values.

Quick setups cover standalone backend mode, proxy-connected backend mode with an explicit server identity, adding/updating
a vote site, an easy per-site or every-site command/message reward, six common operational toggles, a dedicated
`auto-create-vote-sites` switch that changes only `Config.yml` → `AutoCreateVoteSites`, a non-secret `vote-logging` setup,
vote-party basics, and a typed `reward-builder`. Disabling auto-creation affects only inbound unknown-service generation;
explicit admin command/GUI site creation remains available, and the health inspection can still list at most 100 persisted
detected-but-unconfigured service names. The logging setup owns only enabled state, purge retention (`-1` disables or
`1`–`3650` days), and whether to reuse the main MySQL connection. It rejects `0`/other negatives and never accepts or
exposes connection credentials. Its apply reloads configuration but does not recreate or close the VoteLog manager;
restart VotingPlugin after changing `VoteLogging.Enabled`. Inspections immediately gate disabled logging even if a stale
adapter remains, while a newly enabled instance reports enabled but unavailable until restart initializes the adapter.

The reward builder is PREVIEW/APPLY-only and accepts exactly one <=64 KiB serialized proposal using the same strict schema
as reward simulation. It deterministically replaces only `VoteSites.<site>.Rewards`, `EverySiteReward`, or
`VoteParty.Rewards`; all unrelated sites/settings/scopes remain intact, and it never executes the reward. Control strips
the proposal from public operation views and durable history; the connector's result and pending-result journal keep only
the safe derived target file, never the proposal.

Detected Essentials/EssentialsX, CMI, and LuckPerms installations add editable command suggestions alongside generic
Minecraft rewards. Plugin detection does not inspect third-party configuration or versions. Every shortcut uses the same
preview, revision, approval, backup, reload, and rollback path—not a bypass.
Control rejects presets/options outside its fixed schema, and the node independently applies phase-specific validation.
The WebUI settings catalog is a static versioned reference, not an arbitrary key/value write API.

The inspection lane provides typed overview, vote-site health (including persisted unconfigured-service observations),
exact-player data with bounded per-site last votes, VoteLog summary/search/correlation trace, side-effect-free vote-site
resolution, reward-proposal simulation, and redacted diagnostics. Overview/diagnostics distinguish VoteLog configuration
from current readability. Results are capped at 512 KiB, general rows at 100, detected diagnostic plugin names at 128, and
lookbacks at 365 days. It does not expose SQL, arbitrary user enumeration, raw configuration/logs,
commands, reward execution, or writes. The exact schemas and safety invariants are documented in
[the Control agent contract](control-agent-contract.md).

Inspection filters are string values on the wire and are parsed by the selected kind's strict schema. The connector runs
the handlers, including bounded VoteLog/player storage reads, on the dedicated inspection daemon rather than Bukkit's
primary thread or the configuration executor. VoteLog statements use a 10-second JDBC timeout. Reward and vote-site
inspections are dry runs: they do not call reward execution or auto-creating resolution paths.

VoteLogging is optional and SQL-backed. Control labels its output **logged events** because the table records selected vote,
milestone, streak, top-voter, and shop events—not every validation decision, transport hop, reward command, or command
outcome. A correlation-ID trace is therefore a timeline of retained rows sharing one `voteId`, not proof of every network
delivery step. Configured enabled, enabled-and-adapter-available, and readable are separate states. The bounded
`voteLogReadable` probe distinguishes a current database failure from an authoritative empty table:
summary/search/trace return `UNAVAILABLE` when logging is disabled, its adapter is absent, or the probe fails, while
vote-site health labels SQL state and skips aggregates. It is a point-in-time probe; a database failure after it succeeds
can still hit the legacy query API's empty fallback.

An admin must select capable online nodes, preview the change, and confirm the
single-use approval generated for that exact successful preview. Nodes claim work over their existing outbound connection,
so no inbound listener is added to VotingPlugin. Authentication failures, unavailable required capabilities,
and protocol mismatches back off only the connector. Current redacted diagnostic status is one of `DISABLED`, `STARTING`,
`CONNECTED`, `AUTHENTICATION_FAILED`, `INCOMPATIBLE`, `UNAVAILABLE`, or `STOPPED`.

## Rotation, disabling, and troubleshooting

- Rotate with Control's `enroll <nodeId>` command, replace the credential file, then reload/restart VotingPlugin.
- Revoke immediately with Control's `revoke <nodeId>` command.
- Disable by setting `Control.Enabled: false` and reloading/restarting; deleting all new keys also returns to the disabled
  default.
- `AUTHENTICATION_FAILED`: confirm exact node-ID enrollment and replace a revoked/rotated credential.
- `INCOMPATIBLE`: the two JARs do not share protocol version/capability support; upgrade the older side.
- `UNAVAILABLE`: verify endpoint routing, Control health, TLS trust, and timeout settings.
- Backend listed with unknown presence: this is expected when the selected existing VotingPlugin transport does not provide
  backend-presence observations.

Arbitrary console commands, direct backup-rollback endpoints, topology persistence, cloud relay, raw support archives,
signed remote release manifests, and remote support remain later milestones. Automatic release tracking trusts GitHub's authenticated
release metadata and published asset digest for the official repository. Administrators who require an independently
reviewed trust pin can continue to supply `DownloadUrl` and `Sha256` locally.
