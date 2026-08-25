# VotingPlugin Control discovery connector

This development milestone adds an optional, read-only connector from the BungeeCord and Velocity VotingPlugin proxy JAR
to the separate [VotingPlugin Control](https://github.com/BenCodez/VotingPlugin-Control) application. It discovers each
proxy and its eligible backend server names. It does not expose backend addresses or configuration and does not connect
directly from Bukkit backend nodes.

## Failure isolation

The connector is disabled by default. Missing `Control` keys therefore preserve existing installations exactly. When
enabled, startup performs only bounded local configuration/credential reads; network requests use Java's asynchronous HTTP
client outside proxy event-loop/main threads. There is at most one in-flight discovery cycle and no request queue.

Control being stopped, slow, malformed, unauthorized, or protocol-incompatible changes only connector status and its
rate-limited `[Control]` log. Voting, proxy startup, joins, server switches, existing plugin messages, and shutdown do not
call into or wait on Control. Retries use exponential backoff from one second to five minutes with per-process jitter.
Shutdown cancels the scheduled cycle and active future without waiting indefinitely.

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

The connector advertises and requires `presence.snapshot`. Authentication failures, unavailable required capabilities,
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

Configuration reads/writes, a WebUI, cloud relay, diagnostics downloads, automatic Control distribution, and remote support
remain separate later milestones.
