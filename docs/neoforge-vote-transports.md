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
| REDIS | `BungeeMethod: REDIS`, `Redis` settings | SDK-compatible binary channels, captured credentials/DB/prefix, verified TLS when enabled, optional shared-envelope authentication/encryption |
| MQTT | `BungeeMethod: MQTT`, `MQTT` settings | SDK-compatible topics and payloads, captured broker/client credentials and prefix, optional shared-envelope authentication/encryption |
| MYSQL | `BungeeMethod: MYSQL`, `Config.yml` `Database` settings | Shared database credentials and exact queue source/destination routing; optional envelope encryption |

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
share a trust domain; a `source` column is not cryptographic per-node identity.

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
