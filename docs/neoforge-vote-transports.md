# NeoForge vote ingress

This optional extension adds Redis, MQTT and MySQL proxy ingress
to the existing authenticated SOCKETS adapter. Direct Votifier ingress is not included. It does not add plugin messaging,
change SQLite user storage, or expand the restricted NeoForge reward subset.
Treat this feature as experimental until its composed source review, hosted
checks and assigned native-server acceptance are complete. Headless Java,
loopback and packaged-JAR tests are not a live Minecraft deployment test.

| Method | Configuration | Authentication and delivery |
| --- | --- | --- |
| SOCKETS | Existing `BungeeSettings.yml` socket keys | Existing independent socket authentication and optional encryption; wire behavior is retained |
| REDIS | `BungeeMethod: REDIS`, `Redis` settings | Pinned proxy-origin signature, SDK-compatible channels, captured credentials/DB/prefix, verified TLS when enabled, separate shared-envelope authentication/encryption |
| MQTT | `BungeeMethod: MQTT`, `MQTT` settings | Pinned proxy-origin signature, SDK-compatible topics, captured credentials/prefix, separate shared-envelope authentication/encryption |
| MYSQL | `BungeeMethod: MYSQL`, `Config.yml` `Database` settings | Pinned proxy-origin signature and exact queue routing; optional envelope encryption |

## Proxy transports

Keep `DataStorage: SQLITE`. Enable `UseBungeecord` in `BungeeSettings.yml`, choose
one proxy transport with `BungeeMethod`, and give `Server` its matching proxy
backend identity. The established `UseBungeecoord` spelling is also recognized;
an explicit `UseBungeecord: false` takes precedence. Inactive transport settings
are ignored. Unsupported methods fail configuration validation.

Redis uses `Redis.Host`, `Port`, `Username`, `Password`, `Db-Index`, `SSL` and
`Prefix`. MQTT uses `MQTT.BrokerURL`, `Username`, `Password`, `Prefix` and optional
`ClientID` (defaulting to `Server`). Preserve prefixes exactly on both sides.
MQTT supports the SDK's TCP/TLS/WebSocket broker URL forms. Broker credentials
and prefixes establish a routing domain; they do not alone establish an
independent proxy identity.

All three new methods require `BungeeServer.Name` matching the proxy's
`ProxyServerName`, plus `ProxyVerificationKeyFile` in the NeoForge
`BungeeSettings.yml`. This is a base64 DER X509 Ed25519 **public** key, pinned
through an authorized channel. On the proxy, configure the matching base64 DER
PKCS8 Ed25519 **private** key with
`SpigotServers.<backend>.ProxySigningKeyFile`. Paths are relative to each plugin's
data directory. Keep the signing file private and only on the proxy; never copy
it or configure the network-wide `secretkey.key` as this key. The plugin does not
generate, download or enroll these keys automatically.

The proxy signs the complete outbound envelope, including any encrypted payload,
schema, subchannel, exact backend identity, proxy identity, transport domain and
issuance time. NeoForge verifies that proof before decryption or durable vote
admission. A database writer, broker publisher or another holder of the shared
network key cannot mint proxy-origin messages using only that access. A backend
receives only the public verifier. Missing, malformed or unavailable verification
settings fail startup; absent or invalid message proofs are rejected. Update the
proxy and configure its signing entry before enabling the new NeoForge receiver.
Other proxy backend entries without a signing setting retain their existing
wire behavior, and SOCKETS keeps its existing independent authentication.

Proxy-origin signing requires `VoteCacheUseMySQL: false` and a proxy-private local
vote cache. This restricts pending-work storage, not the MySQL message transport.
A backend-accessible SQL cache can copy a valid admission record to another
target or restore it after consumption; its signature cannot establish current
delivery ownership. Startup and reload reject signing with configured or active
SQL vote caching before retiring the working runtime. Changing configuration
alone cannot reclassify an existing SQL cache owner. No cached data is deleted,
migrated or automatically re-attested. Existing SQL-cache setups without the new
signing option retain their established behavior. The new signing mode does not
support shared SQL vote-cache recovery.
SQL-imported server, online and timed rows lose internal admission evidence before
any private-file fallback; numeric totals and original occurrence identities remain.
Moving a shared row into a local file cannot promote it to proxy-owned authority.

Proxy-origin signing also requires `MultiProxySupport: false`. Both current multi-proxy
methods use the network-wide `secretkey.key`, which a backend may also possess;
forwarding their votes through a private signer would incorrectly upgrade network
membership into proxy-only authority. Startup and reload reject this combination
before replacing working security or retiring a runtime. Single-proxy setups
with private caches support the new signing option. A proxy instance that has activated multi-proxy
ingress cannot enable signing later through a soft reload, even after disabling
that ingress: admitted work may still exist. A restart alone does not classify
retained votes as locally admitted. Local admission evidence is signed with an already configured proxy-only key and
persisted with vote totals through offline, timed and emergency recovery. It binds
the occurrence ID, player/UUID, service/time, real-vote and online/delay decisions,
and numeric totals. Shared-wire claims are cleared before admission. A plain flag
or altered stored fields cannot supply authority. This internal evidence is removed
from peer payloads before encryption/signing. Final signed vote publication requires
valid evidence under the still-configured issuing key,
including after full runtime replacement or restart. Older or shared-origin
records without it remain retained for reconciliation and cannot acquire signing
authority from new configuration. Installing or replacing keys does not retroactively
attest old work; retain the issuing key while its records remain pending. Unknown
or unverifiable records keep their original IDs and payloads for reconciliation;
unsigned legacy targets retain their existing delivery path. The plugin never
restarts a process. Existing setups without a signing entry retain their transport
behavior.

Full runtime-replacement preflight also reads every configured active signing key.
Missing or malformed replacement files leave the predecessor's captured keys and
runtime available; preflight does not install the prospective keys.

Origin proofs are valid for at most 24 hours, with two minutes of permitted
future clock skew. This is shorter than the inbox's seven-day released-receipt
retention, preventing ancient captured deliveries from outliving duplicate
fences. Within that interval the same signed SQL row can be retried after failed
acknowledgement or capacity refusal. The signature does not consume a nonce before
durable admission; stable vote IDs and completion receipts retain that ownership.
The proxy signs retransmissions with the current issuance time. Expired SQL rows
remain refused and retained rather than deleted; recovery requires a fresh
authorized proxy delivery, preserving the vote ID. Synchronize proxy/backend
clocks. Broker membership freshness/replay checks remain an additional outer
layer. This change does not add independent backend-to-proxy identity to the
existing shared-key ACK/presence protocol.

For Redis/MQTT, configure `BungeeServer.Name` to the proxy identity. Use
`SharedTransportAuthentication: REQUIRED` once all peers support it, and copy
the existing private network `secretkey.key` through an authorized private
channel. The compatibility default permits legacy unsigned broker traffic for
rolling upgrades. Shared-envelope authentication establishes network membership
and message integrity/freshness, not isolation from other holders of that shared
key. `CommunicationEncryption` is a separate optional confidentiality layer.

MySQL uses `Database.Host`, `Port`, `Database`, `Username`, `Password`, `UseSSL`,
`PublicKeyRetrieval`, `MaxConnections` and optional `DbType` (`MYSQL` or
`MARIADB`). These are transport settings even though user storage remains SQLite.
The adapter bundles a privately relocated MariaDB Connector/J 3.5.10; no companion
server plugin or separately installed JDBC JAR is required. Enabled SSL verifies
the server certificate and hostname. Credentials are passed as opaque driver
fields rather than concatenated into URL options. TCP and certificate trust
still require the operator's normal database configuration.

The MySQL adapter creates the existing `VotingPlugin_message_queue` schema if
needed, so its database account needs the corresponding schema/read/write
permissions. Backend `Server` names must fit that schema's 36-character fields.
Rows are read in bounded 50-row pages under the existing SDK named lock; a row
is deleted only after application acceptance. Invalid or refused rows remain
available for investigation. Full pages advance before revisiting refused rows
so one blocked player does not permanently hide later rows. Database writers
share a routing domain; a `source` column grants no proxy-origin authority.
The pinned signature is required even when the row claims `source='proxy'` and
optional network-wide encryption is enabled.

## JSON compatibility

Broker ingress uses the Gson 2.10.1 APIs supplied by Minecraft 1.21.1, with
explicit strict JSON string validation. Isolated unit and packaged-JAR checks
load that exact library separately from Maven's newer provided compile
dependency. No additional Gson library is bundled or required.

## Durability and limits

Every adapter feeds the existing complete-vote inbox, replay, accounting,
duplicate-recognition and ambiguous-effect quarantine rules. Unsupported rewards
stay unsupported; acknowledging durable ownership does not mean every reward
has run. Capacity exhaustion refuses new acceptance and preserves existing votes
and receipts. Invalid input, failed persistence, shutdown and retry must not
silently become successful delivery. Completion receipt limits and explicit
retirement rules remain unchanged.

Choose one proxy delivery path for each real vote. Different occurrence IDs
are not cross-protocol deduplication. No automatic database deployment, production endpoint replacement,
native server launch, release or artifact distribution is part of this setup.
