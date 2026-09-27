# Shared transport authentication

VotingPlugin can authenticate every ordinary envelope carried over Redis, MQTT, or multi-proxy Redis before routing
it. Authentication is enforced in `REQUIRED` mode. The authentication key is derived from the existing
`secretkey.key`; every proxy and backend in one network must use the same copied file. Networks sharing a broker should
use different key files and different `Redis.Prefix` values.

`SharedTransportAuthentication` has two modes:

- `COMPATIBILITY` is the upgrade-safe default. It keeps outbound messages unsigned and temporarily accepts wholly
  unsigned legacy messages. For multi-proxy Redis with a nonempty prefix, it also publishes and subscribes on the
  legacy unprefixed channel so upgraded and legacy peers can communicate. Both subscriptions pass through the same authentication
  verifier. It logs a warning because an untrusted broker publisher can forge unsigned legacy messages and the legacy
  channel does not isolate networks by prefix.
- `REQUIRED` rejects missing, invalid, stale, or replayed authentication before message handling.

For a rolling upgrade, leave the default `COMPATIBILITY` mode active while upgrading all nodes. This deliberately
keeps traffic unsigned so independently generated node keys cannot interrupt an existing network. Distribute the same
`secretkey.key`, confirm communication, then change every node to `REQUIRED` and reload or restart. Do not leave a network in
`COMPATIBILITY` mode after the rollout.

The MAC covers a distinct Redis, MQTT, or multi-proxy Redis protocol domain, schema, sender identity, subchannel,
complete payload, timestamp, and a random message ID. Exact authenticated replays are rejected by a bounded in-memory
cache sized for sustained broker traffic across the complete freshness window. Vote retries retain their existing
stable vote ID and receive fresh transport authentication for each publish.

`CommunicationEncryption` is a separate, optional confidentiality layer for complete proxy/backend and multi-proxy
envelopes across every configured communication method. It uses AES-256-GCM and keys derived for separate
proxy/backend and multi-proxy domains. HTTP retains mutually authenticated TLS as its outer transport; Redis and MQTT
retain the mandatory MAC above around the encrypted envelope. `PluginMessageEncryption` remains only as a deprecated
plugin-message framing compatibility setting.

Every node creates `secretkey.key` at startup even while `CommunicationEncryption` is disabled. Copy the proxy's file
to every backend and other proxy, then enable `CommunicationEncryption` everywhere and restart. An enabled receiver
rejects plaintext envelopes. A disabled upgraded receiver can decrypt encrypted envelopes, which permits verification
before the coordinated enable/restart step. Startup logs recommend this setup without printing key material.

Multi-proxy Redis channels use the ordinary `Redis.Prefix` namespace:

```text
<Redis.Prefix>VotingPluginProxy_<proxy-name>
```

The publisher and subscriber derive that name through the same channel helper, including when multi-proxy support
reuses the ordinary Redis connection. `REQUIRED` mode uses only this prefixed channel. The temporary legacy channel
bridge described above is active only in `COMPATIBILITY` mode.
