# VotingPlugin security threat model

This document is the repository-specific security model for code review and Codex Security scans. Read it with current source, tests, `AGENTS.md`, `docs/shared-transport-authentication.md`, `docs/control-connector.md`, and `docs/control-agent-contract.md`. Current code wins when older scan history or documentation disagrees.

The goal is to find exploitable trust-boundary failures, not to relabel every correctness, compatibility, or trusted-operator configuration bug as security.

## Security objectives

VotingPlugin runs in Bukkit/Paper/Folia servers and BungeeCord/Velocity proxies and can coordinate votes, rewards, persistent user state, vote parties, webhooks, cross-server delivery, and optional VotingPlugin Control management.

The highest-value properties are:

1. **Vote authenticity:** a lower-trust actor must not forge, replay, multiply, redirect, or transform one legitimate vote into extra rewards, points, totals, vote-party progress, commands, or webhook effects.
2. **Deduplication within the documented at-least-once contract:** ordinary retries, reconnects, broker duplication, reloads, restarts, and mixed-version handoffs should not repeat a reward-bearing vote once the protocol can safely prove completion. Two documented ambiguous cases are not exactly-once guarantees: (a) a backend crash after reward side effects but before the completion receipt becomes durable, and (b) capability loss after completion was journaled but its ACK was lost, where the proxy deliberately drains the already-admitted entry once through the legacy path and retains at-least-once semantics. Stronger atomic reward execution across those cases would require a different reward/storage or upgrade-transition design.
3. **Cross-node identity and membership:** transports with independent per-node credentials must prevent one authenticated node from impersonating another. Shared-key Redis/MQTT/multi-proxy authentication instead proves possession of the network-wide key and message integrity/freshness; it does **not** provide cryptographic isolation between nodes that legitimately possess that shared key.
4. **Authorization at the final sink:** permissions, ownership, balances, limits, node/session ownership, and deployment leases must be revalidated when delayed work actually executes.
5. **Trust-boundary preservation:** attacker-derived strings must remain data and must not become PlaceholderAPI expressions, console commands, YAML/file paths, SQL syntax, URL authority, JSON/Discord syntax, or another interpreter's code through a second parsing pass.
6. **Durability and recovery safety on reliable routes:** for acknowledgement-capable routes and entries admitted to the durable vote-delivery outbox, accepted reward-bearing work must not be silently lost, duplicated, resurrected, or acknowledged in a state that disagrees with durable side effects. Legacy/non-negotiated routes retain their documented historical delivery semantics and are not implicitly upgraded by this model.
7. **Control-plane integrity:** Control may inspect, configure, or deploy only what the authenticated node/session/capability permits. Credentials, revisions, attempts, operation IDs, redacted secrets, journals, and recovery state must remain bound to the correct actor and target.
8. **Lifecycle-appropriate bounds on attacker-influenced state:** queues, replay caches, snapshots, placeholders, webhooks, retries, logs, and response bodies must have useful count/byte/time bounds where expiry is safe. Accepted reliable-delivery state is different: durable outbox entries and completed receipts awaiting acknowledgement/release must remain bounded by admission/capacity and protocol retirement, not by a TTL that can discard the only retry or deduplication record before the required handshake completes.

## Trust boundaries

Treat as attacker-controlled or potentially attacker-influenced:

- Votifier/NuVotifier fields after the vote listener accepts a packet: username, service/site, address, timestamp, and related metadata;
- normal player commands, GUI/dialog actions, sign interactions, and PlaceholderAPI requests reachable through installed plugins;
- player names from offline-mode, proxy-forwarded, Bedrock/Geyser, or unusual identity setups unless the exact source already enforces Java username rules;
- delayed, duplicated, reordered, stale, or replayed legitimate proxy/backend envelopes;
- a compromised backend or proxy attempting actions outside the role that node is trusted to perform, including impersonating another node where independent identity binding exists or originating message types its role should not be allowed to create;
- Redis/MQTT publishers when broker credentials or publish access are compromised;
- malformed or hostile external HTTP responses;
- a Control user/node limited to the exact credential, capability, session, operation, or deployment lease it holds;
- uploaded/staged plugin artifacts until identity, size, digest, session/attempt, and destination checks all pass.

Do **not** assume "internal transport" means trusted.

A full filesystem/plugin administrator is trusted to edit YAML, configure console reward commands, install plugins, choose broker/database endpoints, and intentionally enable compatibility modes. Those choices are not vulnerabilities by themselves. A lower-privileged in-game admin, Control user, backend, broker publisher, or one authenticated node is not equivalent to unrestricted host access.

For shared-key Redis/MQTT/multi-proxy transports, possession of the network key establishes membership in that trust domain. A member is therefore trusted to originate only the message families and actions appropriate to its configured role. Security review should flag a key-holding member when it can exceed that role (for example a backend originating proxy-only vote ingress, a node performing Control-like administration, or one role invoking another role's privileged subchannel), not merely because it can authenticate a message under its own legitimate shared-key membership.

## Proxy transport and message security

Review every communication method according to its actual guarantees.

### Shared transport authentication

Redis, MQTT, and multi-proxy Redis have application-layer authentication where implemented. `SharedTransportAuthentication=REQUIRED` is the hardened state. Missing, invalid, stale, replayed, cross-domain, or wrong-recipient envelopes must be rejected before state mutation.

These transports use a network-wide shared `secretkey.key`. Their MAC binds the declared sender and destination against outsiders or broker-only publishers that do not know the key, but any compromised node that legitimately possesses the same key can generate a valid MAC containing another sender identity. Treat this control as **network-membership authentication plus integrity/replay protection**, not independent per-node authentication. Per-node impersonation is a security failure only where a transport or higher-level protocol actually supplies distinct node credentials/identity binding.

`COMPATIBILITY` intentionally supports rolling upgrades and may accept legacy unsigned traffic. Do not report that documented compatibility behavior by itself. Instead look for:

- a subchannel or message family bypassing authentication while REQUIRED is configured;
- authentication after side effects or expensive attacker-controlled processing;
- MAC coverage that omits declared sender, recipient, schema, message type, full payload, timestamp, nonce/message ID, or protocol domain;
- replay-cache poisoning before semantic validation;
- replay acceptance across restart beyond the intended freshness model;
- cross-protocol or cross-network reuse of authenticators;
- old/new runtime overlap enforcing different policies;
- accidental persistent downgrade from REQUIRED to COMPATIBILITY;
- compatibility channels remaining active in REQUIRED mode;
- Redis prefix/namespace mistakes that let separate networks sharing a broker consume each other's messages.

`CommunicationEncryption` is a separate confidentiality layer. Disabled optional encryption alone is not vote forgery if integrity/authentication still holds.

### Reliable reward-bearing delivery

Apply this section to acknowledgement-capable routes and to entries already admitted to the durable delivery outbox. Reliable at-least-once delivery is capability-negotiated; older backends and legacy multi-proxy paths retain their documented legacy semantics. Do not classify expected loss/duplication from a route that never negotiated the reliable-delivery contract as a violation of that contract unless code incorrectly treated the route as reliable.

There is also a documented capability-loss transition for an already-admitted reliable entry: if the backend durably completed the vote, its acknowledgement was lost, and a later backend generation stops advertising acknowledgement support, the proxy drains that entry once through the legacy send path before moving it toward receipt retirement. That transition intentionally retains at-least-once semantics and can repeat an external reward whose completion was already journaled. Treat the existence of this downgrade window as part of the compatibility contract, not a security failure by itself; review whether code sends more than once, loses the retirement state, applies the downgrade when capability was not actually lost, or lets a lower-trust actor force/amplify the transition.

Trace a logical vote end to end:

ingress -> proxy accounting -> routing -> durable/outbox admission -> publish -> backend admission -> duplicate reservation -> semantic validation -> identity lookup -> reward/accounting effects -> durable completion -> acknowledgement -> retirement.

High-value failures include:

- generating a new vote ID during retry, cache restore, multi-proxy forwarding, VoteDelayRejected, or compatibility conversion;
- ACK before effects are durable;
- reserving attacker-chosen IDs before required fields are validated, allowing poisoning of a future legitimate vote;
- reservations not released after failed processing;
- behavior that widens, makes attacker-controllable, or incorrectly classifies the documented ambiguous crash window between reward side effects and durable completion;
- deleting outbox/journal state before ACK is durable;
- partial fan-out followed by replay of already-completed destinations outside documented capability-loss/legacy-drain behavior;
- retirement of dedupe fences while retries can still arrive;
- a peer or route being treated as ACK-capable when that capability was not negotiated, or older peers bypassing guarantees that the sender incorrectly assumed applied;
- accepted queue entries that cannot actually be persisted;
- oversized entries poisoning bounded durable queues;
- cached votes removed before confirmed delivery;
- transport-switch migration losing or duplicating accepted work.

Treat proxy/backend crashes, restarts, reloads, disconnects, capability loss, and duplicate broker delivery as normal adversarial lifecycle events. Do not report either documented at-least-once ambiguity by itself: a crash after an external reward side effect but before durable completion may retry, and a lost completion ACK followed by capability loss may cause the already-admitted entry to drain once through the legacy path. A security finding should show a violation outside those contracts, an attacker-controlled way to force or amplify them, more than the documented one-time legacy drain, premature acknowledgement/retirement, or a dedupe failure after the protocol should have safely retired the entry.

### Presence and routing

Presence influences cached/online reward routing. Search for:

- stale backend generations becoming current after proxy restart;
- old login/logout/start/stop/snapshot messages overriding newer state;
- missing generation/monotonicity checks;
- snapshot chunks from different requests or incarnations being combined;
- case-normalization mismatch in server identities;
- disconnect/reconnect races routing a reward to the wrong backend;
- whitelist/blocked-server policy differences between normal routing, retries, broadcasts, presence, Control discovery, and recovery;
- work marked delivered merely because a player/server appears present when the destination cannot actually receive it.

## VotingPlugin Control

Control is a first-class security boundary, not merely an optional HTTP client.

Relevant surfaces include discovery, backend enrollment, hosted Control, full YAML READ/PREVIEW/APPLY, quick setup, redaction/restoration, inspections, durable results, reload/rollback, verified plugin deployment, Control self-update, recovery-only connectors, credentials, sessions, revisions, capabilities, operations, and deployment attempts.

### Configuration and YAML

Search for:

- traversal, symlink tricks, alternate separators/casing, rename races, and validated-path-to-string TOCTOU;
- reads/writes outside managed VotingPlugin paths;
- secrets leaking through READ, PREVIEW, diffs, comments, logs, backups, snapshots, inspections, errors, or audits;
- a redacted marker being written literally instead of restoring the existing secret;
- secret mutation through a field intended to be masked/immutable;
- stale revision checks after mutation;
- PREVIEW and APPLY resolving different state;
- malformed YAML causing partial replacement;
- rollback restoring only part of one logical operation;
- a node applying work leased to another node/session/attempt;
- operation-ID reuse suppressing a distinct operation;
- reported rollback success while changed files remain active;
- restart recovery acknowledging work that never fully applied.

### Enrollment and credentials

Search for raw credentials crossing nodes where only verifiers should, verifier substitution, replayed challenges/results, node/endpoint/request confusion, broker/database observers enrolling themselves without proof, credential leakage in URLs/arguments/environment/logs/diagnostics, credential-file traversal or symlink replacement, stale credential survival after rotation, recovery using an old endpoint, and automatic replacement of a manually managed credential.

### Plugin deployment

Treat deployment as privileged code installation. Search for:

- artifact authorization not bound to node + session + deployment + attempt;
- stale attempts/results being accepted;
- digest verification over bytes different from those installed;
- TOCTOU between verification and activation;
- JAR bombs, duplicate entries, excessive entries, or path tricks;
- wrong plugin identity or nested/fake `plugin.yml`;
- symlink/path replacement of update/plugin destinations;
- proxy replacement without a usable rollback;
- markers acknowledging a different artifact;
- crash states where filesystem state and reported result disagree;
- replay of completed deployment;
- recovery-only connectors advertising deployment;
- HTTP artifact download becoming eligible on a route allowed only for normal Control traffic.

An administrator intentionally selecting a custom JAR is trusted. The security question is whether a stale, unauthorized, or differently scoped actor can install something else.

## Rewards, placeholders, commands, Discord and webhooks

Operator-authored reward commands are intentionally powerful. Do not report a trusted administrator explicitly configuring a console command or arbitrary webhook URL as a vulnerability by itself.

Trace whether lower-trust data can change the command, reward, URL, or syntax that executes. Important inputs include player names, vote service/site values, PlaceholderAPI results, database values, Discord text, and Control-supplied non-secret fields.

Prioritize **multi-pass interpretation**:

1. attacker-derived text is inserted into a trusted template;
2. the result is then parsed by PlaceholderAPI, command dispatch, MiniMessage/chat formatting, JSON/YAML, URL parsing, or another interpreter.

Review normal votes, vote-party commands, random-player rewards, DiscordSRV rewards, webhooks, reminders, broadcasts, VoteSite placeholders, milestones/streaks/cooldowns, offline cached rewards, and delayed/rejected-vote rewards.

## Player authorization and transactional state

Permission checks must hold at execution time, not only when a GUI is constructed.

Review chest/dialog GUIs, confirmation flows, category navigation, shop callbacks, admin editors, self-vs-other player views, aliases, signs, and scheduled callbacks. Test config/permission reload while a GUI is open, direct construction of child GUIs, and identity ambiguity.

Treat points, VoteShop, and transfers as one transaction:

balance read -> authorization/limit check -> reservation/debit -> reward/credit side effect -> durable completion/compensation.

Search stale shared-MySQL state, lost updates, overflow/negative arithmetic, double-click/concurrent purchase, cross-server concurrent spend, recipient disconnect, crash between debit and credit, compensation queued without durable state, mutation APIs that bypass global limits, and stale confirmation state after reload.

## Resource exhaustion

Require a credible lower-trust path and meaningful amplification. Prioritize:

- envelope size before decode/authentication;
- durable overflow queue bytes and entries;
- replay/dedupe caches;
- presence snapshots/chunks;
- reload/replacement queues;
- PlaceholderAPI auto-cache and forced live lookup variants;
- webhook queue/retries/response bodies;
- external HTTP response size;
- VoteLog/statistics SQL bounds and execution thread;
- Control request/response/result journals and backup/history counts;
- malformed-input log amplification.

Check count, bytes, eviction/retirement rules, cancellation, and restart persistence. Use TTLs only where expiration is semantically safe. Do not require accepted reliable-delivery outbox entries or completed receipts awaiting release to expire before the acknowledgement/release protocol has durably retired them; those states should instead be admission/capacity bounded and retained until protocol completion.

## Storage and database boundaries

Bind lower-trust SQL values. Treat dynamic identifiers, table prefixes, database names, ORDER BY fragments, migrations, and DDL separately because bind parameters do not protect SQL syntax.

Operator-controlled identifiers are normally hardening rather than remote SQL injection. Security-relevant storage failures include lower-trust values reaching SQL syntax, shared-storage races producing duplicate rewards/purchases, stale cache writes overwriting newer state, fail-open authorization on database errors, or partial multi-step accounting.

## Reload, shutdown and runtime replacement

This is high priority. Old/new listeners, workers, caches, sessions, queues, and subscriptions can overlap.

For every replacement path verify:

- the old listener is fenced before the new one can consume the same message;
- accepted work transfers exactly once;
- queues are not cleared before durable transfer;
- close failure cannot leave two active consumers;
- replacement failure preserves the last healthy runtime;
- authentication/encryption cannot temporarily downgrade;
- Redis/MQTT subscriptions and HTTP/socket listeners retire safely;
- old retries cannot mutate new runtime state;
- stale Control sessions/leases/credentials cannot operate after replacement;
- interruption cannot skip required persistence/fencing.

## Compatibility and scan calibration

VotingPlugin requires drop-in compatibility. Legacy protocol/config compatibility is not automatically a security flaw.

Report compatibility behavior when it bypasses a security mode explicitly configured as REQUIRED, creates a stable downgrade, loses stable vote identity, accepts unauthenticated traffic outside the documented upgrade state, or lets an old peer request newly privileged behavior.

Do not repeatedly classify these as security by themselves:

- malformed trusted YAML causing an exception;
- API/ABI regressions;
- wrong totals with no attacker advantage;
- null checks on administrator-only paths;
- intentionally configured powerful rewards/webhooks;
- development SNAPSHOT dependencies under an accepted development policy;
- cosmetic formatting problems;
- future platform support that is not wired to live ingress/rewards.

Previously heavily reviewed areas include shared-transport authentication, Redis namespaces, vote replay IDs, durable delivery, presence, VoteShop races, GUI permissions, PlaceholderAPI/service-site injection, Control enrollment/journals/deployment, webhook bounds, CI permissions, and unbounded caches/queues. Do not suppress regressions, but prefer a **new invariant violation, downgrade, cross-feature interaction, or crash/recovery window** over restating an old family.

## High-value attack stories

1. A valid vote arrives while a backend/proxy transport is replaced.
2. A valid Redis/MQTT authenticator for one destination/type is redirected elsewhere.
3. COMPATIBILITY -> REQUIRED migration occurs while old/new runtimes overlap.
4. Proxy crashes after publish but before ACK.
5. Exercise both documented ambiguous delivery windows: backend crash after reward effects but before durable completion, and lost completion ACK followed by capability loss/legacy drain; verify no lower-trust actor can force, amplify, repeat, or extend either case beyond its documented at-least-once behavior.
6. Multi-proxy fan-out partially succeeds and retries after restart.
7. Player opens an authorized shop/admin GUI, permissions/config reload, then clicks stale state.
8. Two servers concurrently spend the same shared-MySQL points.
9. Control APPLY changes disk but reload/result acknowledgement is interrupted.
10. Control credential/session rotates while old results or recovery connectors remain.
11. Deployment crashes at each verify/stage/marker/activate/ack transition.
12. Hostile player/service text crosses two interpreters before reaching a command, Discord, webhook, or broadcast sink.
13. A very large but schema-valid authenticated envelope traverses every queue/cache/persistence layer.
14. Communication method changes while accepted votes are outstanding.
15. A current node communicates with an older peer during rolling upgrade.

## Severity calibration

**Critical:** remote unauthenticated or ordinary-player arbitrary server/OS/plugin code execution; unauthorized installation of an attacker-selected JAR; broad Control/admin authentication bypass enabling arbitrary privileged commands/configuration.

**High:** forged/replayed rewards despite the configured hardened boundary, including a shared-key member exceeding its authorized message role; repeatable reward/economy duplication outside the documented ambiguous at-least-once crash and capability-loss drain windows and outside routes that never negotiated reliable delivery; cross-node impersonation where independent per-node identity is actually promised (for example Control or another per-node credentialed protocol); player authorization bypass to privileged actions; Control cross-node operation/deployment; practical attacker-triggered persistent resource exhaustion; repeatable theft/duplication through shared-storage races. Possession of the network-wide shared transport key by one compromised Redis/MQTT node is not, by itself, a per-node-authentication bypass because that mode authenticates shared network membership rather than isolating key-holding nodes.

**Medium:** prerequisite-heavy integrity failures; occasional lost/duplicate rewards; sensitive token disclosure to a limited actor; meaningful lower-privilege SSRF; message/placeholder injection with configuration-dependent privileged effect; timing-dependent reload boundary failures.

**Low:** bounded log/format injection, limited operational disclosure, difficult low-impact resource amplification, or defense-in-depth hardening with a concrete lower-trust path.

For every reported security issue identify the attacker capability, exact trust boundary crossed, concrete privileged effect, and whether current master still exposes the path.
