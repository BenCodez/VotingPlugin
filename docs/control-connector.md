# VotingPlugin Control discovery connector

This development milestone adds an optional connector from the BungeeCord and Velocity VotingPlugin proxy JAR
to the separate [VotingPlugin Control](https://github.com/BenCodez/VotingPlugin-Control) application. It discovers each
proxy and its eligible backend server names. Control includes the local WebUI. VotingPlugin may now explicitly opt in to
provisioning and supervising that application as a separate child JVM; it is never loaded into the proxy classloader. The
connector does not expose backend addresses, secrets, or unrestricted configuration and does not connect directly from
Bukkit backend nodes.

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
blocks the proxy event loop or vote processing. Stopping VotingPlugin asks only the child process it created to terminate
and applies a bounded asynchronous force-stop fallback.

## Optional hosted WebUI

Use one hosted Control instance for the network. Other proxies should use the connector against that instance rather than
each starting a competing service. Hosting and discovery are separate switches: `Control.Hosted.Enabled` runs Control,
while `Control.Enabled` reports this proxy to it.

Control releases publish `votingplugin-control.jar` and `votingplugin-control.jar.sha256`. Configure an immutable versioned
asset URL and copy its exact digest into `Sha256`; `/latest/` URLs, unpinned artifacts, plaintext download URLs, redirects
outside HTTPS, and paths outside VotingPlugin's data folder are rejected.

```yaml
Control:
  Enabled: true
  Endpoint: 'http://127.0.0.1:8080'
  NodeId: 'proxy-a'
  CredentialFile: 'control-credential.txt'
  Hosted:
    Enabled: true
    AutoDownload: true
    AutoUpdate: false
    DownloadUrl: 'https://github.com/BenCodez/VotingPlugin-Control/releases/download/v0.1.0/votingplugin-control.jar'
    Sha256: '<64-character SHA-256 from the pinned release>'
    JarFile: 'control/votingplugin-control.jar'
    DataDirectory: 'control/data'
    Host: '127.0.0.1'
    Port: 8080
    StartupTimeoutSeconds: 30
    DownloadTimeoutSeconds: 60
```

On first start, the artifact is downloaded to a unique staging file with a 64 MiB hard limit, verified, and atomically
activated before launch with the same Java runtime as the proxy. An existing matching manual installation is launched
without downloading. `AutoUpdate` is false by default; when explicitly enabled and the configured digest changes, the
current release is retained as `.previous`. A failed process/protocol health check atomically restores and starts that
previous release, retaining the failed candidate as `.failed`. Unexpected exits use bounded restart backoff.

The child receives only its bind host, port, and contained data directory. It does not receive VotingPlugin configuration
or credentials in release metadata. Process output is written beside the hosted JAR and rotated at 1 MiB. Configure the
WebUI password directly against the hosted data directory (the prompt is not echoed and the password is not an argument):

```shell
java -jar plugins/VotingPlugin/control/votingplugin-control.jar web-password plugins/VotingPlugin/control/data
```

The WebUI is then available at `http://127.0.0.1:8080/`. Password rotation invalidates every existing browser session.
To expose a non-loopback listener, create either the WebUI password or an admin API token in
`Control.Hosted.DataDirectory` first and use HTTPS or a private tunnel; Control otherwise refuses the bind.

Manual installation remains supported: put a Control JAR at `JarFile`, configure its exact SHA-256, set `AutoDownload` and
`AutoUpdate` false, and enable hosting. VotingPlugin will verify and supervise it without network access.

## Enrollment and configuration

1. Build/run Control and use its owner command to enroll the exact stable node ID:

   ```shell
   java -jar votingplugin-control-0.1.0-SNAPSHOT-all.jar enroll proxy-a data
   ```

2. On that proxy, create `plugins/VotingPlugin/control-credential.txt` (or the platform-equivalent VotingPlugin proxy data
   directory) containing only the printed credential. Restrict filesystem access to the proxy service account.

3. Configure `bungeeconfig.yml`:

   ```yaml
   Control:
     Enabled: true
     Endpoint: 'http://127.0.0.1:8080'
     NodeId: 'proxy-a'
     CredentialFile: 'control-credential.txt'
     HeartbeatSeconds: 30
     ConnectTimeoutMillis: 3000
     RequestTimeoutMillis: 5000
   ```

`NodeId` may be blank to reuse the existing `ProxyServerName`, but every simultaneously connected proxy must have a unique,
stable enrolled identity. Explicitly setting it is recommended. `CredentialFile` must resolve inside VotingPlugin's data
directory; credentials in URLs and parent-directory traversal are rejected. The credential is never logged or placed in
plugin messages.

The endpoint must be an `http` or `https` origin without embedded credentials, query, fragment, or path. Plain HTTP is
suitable only for loopback or a trusted private network. Use HTTPS or a private authenticated tunnel when crossing an
untrusted network. Control authentication does not itself encrypt traffic.

Timing bounds are intentional:

- heartbeat: 10–300 seconds;
- connect/request timeout: 500–30,000 milliseconds;
- response body: at most 64 KiB;
- backend snapshot: at most 4096 entries.

Do not raise these by patching around validation; correct the topology or connectivity problem instead.

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
blocked names against that proxy's configured backends and returns a deterministic SHA-256 revision. Apply rejects stale
revisions, writes a local `.control-backup`, requires atomic activation of a staged YAML file, and soft-reloads the
proxy. A reload failure triggers immediate backup restoration and another reload attempt; Control receives the per-node
reload/rollback result. Task results are cached by operation ID so a leased retry cannot apply the same change twice.

The WebUI never sends raw YAML. An admin must select capable online proxies, preview the typed change, and confirm the
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

Unrestricted configuration, manual rollback, topology persistence, cloud relay, diagnostics downloads, signed remote
release manifests, and remote support remain later milestones. Hosted downloads currently require the SHA-256 trust pin
to be supplied in local configuration; VotingPlugin never trusts a remotely fetched checksum by itself.
