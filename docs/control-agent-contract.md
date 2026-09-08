# VotingPlugin Control agent contract

This is the compact source of truth for an AI agent or Control client implementing the Bukkit integration. The connector
has two separate lanes:

- configuration operations use the negotiated `config.files.v1` / `config.quick-setup.v1` contract and may write only
  managed VotingPlugin YAML after preview and approval;
- inspections use the optional `data.inspect.v1` contract and are always read-only.

Do not translate an inspection request into a configuration operation. Do not add raw SQL, arbitrary commands, player
enumeration, database browsing, filesystem paths, or generic key/value reads to either contract.

## Proxy file contract (`config.proxy-files.v1`)

This is a proxy-only capability, advertised by an enrolled BungeeCord or Velocity node. It is separate from
`config.proxy-routing.v1`, `config.proxy-method.v1`, and the backend `config.files.v1` capability. The only managed file
is the proxy's top-level `bungeeconfig.yml` in the VotingPlugin data folder. `fileName` must be exactly
`bungeeconfig.yml`; paths, subdirectories, and other filenames are rejected. The file and proposed UTF-8 content are
bounded at 512 KiB. YAML must be a mapping, use string keys and supported scalar/map/list values, and may not contain
aliases/merge keys, duplicate keys, invalid UTF-8, NULs, or nesting deeper than 50.

The operation is carried in the normal authenticated node operation queue. Control claims an operation with
`POST /api/v1/nodes/{nodeId}/operations` and `{"sessionId":"<connector-session-uuid>"}`. A claimed task has
`operationId`, `attemptId`, `type`, and, for APPLY, `expectedRevision`, plus this configuration object:

```json
{
  "domain": "file",
  "fileName": "bungeeconfig.yml",
  "content": "...masked YAML..."
}
```

`content` is omitted for READ. The node submits the result through the normal operation-result endpoint; a result has
`success`, `code`, `message`, `revision` (on success), `configuration` (on success), `changes`, `reloaded`, and
`rolledBack`, and includes the claimed `attemptId`. A successful configuration object contains `domain`, `fileName`,
and masked `content`. `changes` is a deterministic, lexicographically ordered list of at most 20 flattened YAML paths,
using `added`, `changed`, or `removed` prefixes. The complete operation-result request and connector HTTP response retain
the shared 4 MiB protocol bound; the stricter 512 KiB limit applies to the managed YAML content itself.

The node posts that result to `POST /api/v1/nodes/{nodeId}/operations/{operationId}/result` with the same
`sessionId`; there is no second proxy-file-specific envelope. `attemptId` is retained in the result so Control can
match the leased attempt. Control's HTTP success response acknowledges receipt; a `409` `TASK_LEASE_EXPIRED` leaves
the result journaled for recovery/retry, while `404` `OPERATION_NOT_FOUND` is treated as an acknowledgement because
the Control-side operation is already gone. Malformed or non-success transport responses affect only this connector
queue and are retried with its normal bounded backoff.

READ returns the current masked document and its SHA-256 revision. PREVIEW parses and validates the proposal, resolves
unchanged redacted secret markers against the local document, returns the current revision and changes, and does not
write. APPLY requires the exact revision returned by the preview (and checks it again around staging/installation),
writes through a staged file and atomic activation, and retains `bungeeconfig.yml.control-backup`. A stale or changed
revision returns `success:false`, `code:"STALE_REVISION"`, with no configuration payload. Invalid file names, YAML,
content, or redaction use `VALIDATION_ERROR`; unavailable/read or save failures use the fixed `APPLY_FAILED` result;
an installation failure returns `APPLY_FAILED` and reports whether rollback succeeded in `rolledBack`.

Proxy-file APPLY does not reload the proxy in this connector (`reloaded:false`); the success message instructs an
operator to restart the proxy for general settings to take effect. It is not the `proxy-method` runtime-replacement
operation. A failed installation attempts to restore the backup atomically. The result is journaled by operation ID
until Control acknowledges it, so a leased retry does not apply the change twice; on node recovery an unfinished APPLY
is either recognized as already installed by revision or reported as `RECOVERY_ABORTED`.

Control must authenticate as the enrolled node and must include `config.proxy-files.v1` in `acceptedCapabilities`
before assigning these tasks. If the capability was not negotiated, the node returns `UNSUPPORTED` without reading or
writing the file. Proxy and backend nodes may be enrolled against the same Control instance, but each node has its own
identity, credential/session, operation lease, revision, and result journal: a backend capability does not authorize a
proxy-file task, and one peer's approval or revision cannot be used for another peer. Control should therefore present
this editor only for a capable proxy node and require its normal authenticated admin preview/approval flow.

All reads mask secrets. The mask covers password/secret/token/API-key/authorization/webhook URL fields and selected
database, Redis, MQTT, proxy-host, and Control infrastructure fields; JDBC-style and credential-bearing URLs are also
masked. A submitted redaction marker preserves the local secret; replacement secrets may be submitted in an authenticated
operation but are never returned or journaled.

## Easy automatic vote-site toggle

The `auto-create-vote-sites` quick-setup preset owns exactly one setting:

```json
{"domain":"quick-setup","preset":"auto-create-vote-sites","options":{"enabled":"false"}}
```

`READ` uses an empty `options` object and returns `options.enabled`; unknown READ options are rejected. `PREVIEW` and
`APPLY` use the normal revision/approval workflow. The preset changes only
`Config.yml` → `AutoCreateVoteSites`. Use it for the prominent “Automatically create unknown vote sites” switch instead of
submitting every field in `common-settings`. The setting gates only automatic creation from an inbound unknown service.
An administrator's explicit `/av VoteSite <site> Create` or admin-GUI creation remains available when the toggle is off.
`vote-site-health` still reports a bounded `detectedUnconfiguredServices` list from persisted `GottenServiceSites`
observations, so turning automatic creation off does not hide new service names. This is read-only discovery, not an
approve/create action.

## Vote-logging setup

The `vote-logging` quick-setup preset owns exactly three non-secret settings:

```json
{
  "domain": "quick-setup",
  "preset": "vote-logging",
  "options": {"enabled":"true", "purgeDays":"30", "useMainMySQL":"true"}
}
```

`purgeDays` is exactly `-1` (disable automatic purge) or an integer from 1 through 3650; `0` and other negative values are
invalid, and READ round-trips `-1`. The preset rejects unknown options and never reads/writes a hostname, database name,
username, password, or other connection field. Choosing a dedicated connection therefore remains a full
redacted-editor task; turning `useMainMySQL` off alone does not invent credentials. `READ` accepts no options and rejects
unknown fields. `READ` returns current typed state and its revision, `PREVIEW` computes changes and produces an approval,
and only the approved `APPLY` writes atomically, reloads, and rolls back on reload failure, like every other quick setup.

## Inspection transport

An enrolled Bukkit node advertises `data.inspect.v1`. Once Control includes it in `acceptedCapabilities`, the node polls:

```http
POST /api/v1/nodes/{nodeId}/inspections
Content-Type: application/json

{"sessionId":"<connector-session-uuid>"}
```

A `204` means no work. A `200` assignment is:

```json
{
  "inspectionId": "<uuid>",
  "attemptId": "<opaque attempt id>",
  "query": {"kind":"overview","filters":{}}
}
```

The node posts the result to `/api/v1/nodes/{nodeId}/inspections/{inspectionId}/result`:

```json
{
  "sessionId": "<connector-session-uuid>",
  "attemptId": "<same attempt id>",
  "success": true,
  "code": "OK",
  "message": "Inspection completed",
  "data": {
    "schemaVersion": 1,
    "kind": "overview",
    "generatedAt": "2026-08-30T12:00:00Z",
    "result": {}
  }
}
```

For compatibility, a successful result may use `"code":"OK"` as above or omit/set `code` to `null`. It must contain a
JSON object whose `schemaVersion` is the JSON integer `1` (not a string), `kind` exactly matches the assigned query,
`generatedAt` parses as an ISO-8601 instant, and `result` is a JSON object. Failures omit `data` and use
`VALIDATION_ERROR`, `UNAVAILABLE`, `RESULT_TOO_LARGE`, or `INSPECTION_FAILED`.

Data is limited to 512 KiB. General rows and diagnostics plugin inventories are limited to 100, top lists to 20,
and lookback windows to 365 days. Summary top lists order vote counts descending, then names case-insensitively and by
exact spelling; the database applies the same name tie-break before its limit. The connector performs inspections on a
dedicated single-thread daemon executor, separate from presence and configuration work and never on the Bukkit primary
thread. Database reads therefore cannot
block vote handling, server ticks, or configuration polling; one long query serializes only later inspections. Shutdown
cancels this lane and waits at most five seconds for its worker. Inspections have no write-ahead journal because retrying
a read is safe. A persistent missing route, transport failure, server error, or malformed response backs this polling lane
off exponentially from one second to a five-minute cap; a successful poll or newly accepted capability resets the delay.

## Query allow-list

Unknown kinds and unknown filter fields are rejected. The versioned `filters` contract is a string-to-string object; parse
each allowed value strictly after selecting the kind. Numeric and boolean examples are `"days":"30"`, `"limit":"25"`,
and `"includeDisabled":"false"`. Control's current JSON mapper may coerce some scalar admin-request values before it
creates the task, but clients must not rely on that implementation detail: connector assignments contain strings and the
VotingPlugin handler requires text. Ordinary filter values are limited to 500 UTF-8 bytes at Control. Only `reward-simulation`'s
encoded `proposal` may be larger, with a 64 KiB hard limit.

| Kind | Allowed filters | Meaning |
| --- | --- | --- |
| `overview` | none | Versions, configuration health, bounded data-storage mode, proxy mode, vote-site counts, and configured/readable VoteLog state |
| `vote-site-health` | string `days` (1–365, default 30) | Configured site status, bounded aggregate last-vote/count data, unmatched logged services, and persisted unconfigured service observations |
| `player` | exactly one of `name` (1–16 characters) or `uuid` (canonical 36-character UUID) | Exact existing-player lookup; totals, points, streaks, up to 100 per-site last-vote rows, and pending vote count saturated at 100,000; never lists players |
| `vote-log-summary` | string `days` (1–365, default 30) | Vote totals, immediate/cached split, unique voters, and bounded top-service (`service`, canonical `count`) and top-server (`server`, canonical `count`) rows; schema-v1 nodes also emit the equal legacy `votes` alias for staggered dashboard upgrades |
| `vote-log-search` | at most one of exact `player` (1–16 characters), `service` (1–64), or `server` (1–64); optional `event` and string `days`/`limit` | Bounded recent event rows; `limit` is 1–100 and defaults to 25 |
| `vote-trace` | required canonical 36-character UUID `voteId`; optional string `days`/`limit` | Chronological VoteLog events sharing one correlation ID |
| `vote-site-resolution` | required valid `serviceSite` (1–64 characters); optional string boolean `includeDisabled` | Dry-runs existing resolution and reports whether auto-create would be attempted; never calls the creating resolver |
| `reward-simulation` | required `proposal`, a JSON object encoded as one filter string | Validates and normalizes typed actions, reports the plan, and never invokes `RewardBuilder` |
| `diagnostics` | none | Bounded redacted environment/configuration status, configured/readable VoteLog state, and up to 100 detected plugin names with an explicit truncation indicator |

Valid VoteLog `event` values are `VOTE_RECEIVED`, `VOTEMILESTONE`, `VOTE_STREAK_REWARD`, `TOP_VOTER_REWARD`, and
`VOTESHOP_PURCHASE`. Search values are exact, not substrings. VoteLog summary/search/trace return `UNAVAILABLE` when the
optional table is disabled, its adapter is absent, or its bounded readability probe fails.

### Reward proposal

After parsing the `filters.proposal` string as JSON, the typed object accepts only:

```json
{
  "scope": "site",
  "site": "PMC",
  "commands": ["eco give %player% 100"],
  "playerMessages": ["Thanks for voting"],
  "broadcastMessages": [],
  "items": [{"material":"DIAMOND","amount":2}],
  "money": 0,
  "permissions": [],
  "chancePercent": 100,
  "onlineOnly": false
}
```

For example, a client produces the request with the equivalent of:

```javascript
const query = {
  kind: 'reward-simulation',
  filters: {proposal: JSON.stringify(proposal)}
};
```

Use a standard JSON serializer. Never construct the escaped proposal by concatenating user-controlled strings.

`scope` is `site`, `every-site`, or `vote-party`. For `site`, `site` must match `[A-Za-z0-9_-]{1,64}` and already exist in
`VoteSites.yml`; for a global scope it must be omitted, null, or empty. Unknown proposal and item fields are invalid. The remaining typed limits
are:

| Field | Contract |
| --- | --- |
| `commands`, `playerMessages`, `broadcastMessages` | Optional arrays of at most 20 nonblank, single-line strings; each string is at most 500 characters |
| `permissions` | Optional array of at most 20 nonblank, single-line strings; each string is at most 200 characters |
| `items` | Optional array of at most 20 objects containing only `material` and `amount`; material is uppercased, must match `[A-Z0-9_]{1,80}`, resolve through Bukkit `Material.matchMaterial`, and be an item material; amount is an integer 1–64 and defaults to 1 |
| `money` | Optional finite JSON number from 0 through 1,000,000,000; defaults to 0 and only a positive value counts as an action |
| `chancePercent` | Optional finite JSON number from 0 through 100; defaults to 100 |
| `onlineOnly` | Optional native JSON boolean; defaults to false |

At least one command, message, item, permission, or positive money value is required. This endpoint validates a proposal
for the UI: it does not evaluate arbitrary requirement code, choose random outcomes, execute commands, grant
items/currency/permissions, or persist YAML. Configuration still requires the normal preview/apply lane. The complete
encoded proposal filter is limited to 64 KiB.

### Reward-builder persistence

The configuration lane can persist the same typed object with the PREVIEW/APPLY-only `reward-builder` preset:

```json
{
  "domain": "quick-setup",
  "preset": "reward-builder",
  "options": {"proposal":"{\"scope\":\"site\",...}"}
}
```

`options` must contain only `proposal`, encoded with a standard JSON serializer and limited to 64 KiB of UTF-8. There is
no READ operation for this preset. Control strips the proposal from public operation views, and its durable journal stores
only the redacted domain/preset. The node result and pending-result journal retain only the safe derived `targetFile`
(`VoteSites.yml` or `SpecialRewards.yml`), never the proposal. PREVIEW validates the proposal and reports deterministic
path changes; APPLY remains bound to that preview's revision and one-time approval.

The preset clears and rebuilds exactly one selected reward subtree:

| Scope | Managed file | Replaced path |
| --- | --- | --- |
| `site` | `VoteSites.yml` | `VoteSites.<site>.Rewards`; `<site>` must already be configured |
| `every-site` | `VoteSites.yml` | `EverySiteReward` |
| `vote-party` | `SpecialRewards.yml` | `VoteParty.Rewards` |

The mapping is deterministic: `commands` → `Commands`; player/broadcast messages → `Messages.Player` /
`Messages.Broadcast`; items → numbered `Items.ControlItemN.Material` and `.Amount`; positive money → `Money`;
permissions → numbered `AdvancedRewards.ControlPermissionN.TempPermission.Permission` with `Expiration: 2147483647`;
chance → `Chance`; and online-only → `RewardType: ONLINE` (otherwise `BOTH`). At least one action is required. The
preset neither executes rewards nor changes a different site's rewards, the other global reward scope, or unrelated
VoteParty settings. Atomic staging, reload, rollback, and stale-revision protection are unchanged.

### Detected unconfigured services and player last votes

`vote-site-health.result.detectedUnconfiguredServices` is a case-insensitive, deduplicated, sorted array of at most 100
sanitized service names copied from VotingPlugin's persisted `GottenServiceSites` list when no configured `ServiceSite`
matches. `detectedUnconfiguredServicesTruncated` reports whether more values existed. This list remains available without
VoteLogging. Keep it distinct from `unmatchedLoggedServices`, which is derived from retained VoteLog aggregates and is
empty/non-authoritative unless `voteLogReadable` is true. An enabled row with a configured service uses
`VOTE_LOG_UNAVAILABLE` or `VOTE_LOG_UNREADABLE`, not `NO_RECENT_VOTES`, when aggregates cannot be read; `DISABLED` and
`SERVICE_SITE_MISSING` keep their higher-priority configuration status. The at-most-100 configured sites shown in the
response are queried through a separate bounded prepared filter, so falling outside the 100 most recently active services
cannot be misreported as zero votes. Matching and aggregates use the complete case-normalized ServiceSite, including
valid names longer than the 64-character display field; output truncation never changes lookup identity. Case variants
are combined into one aggregate, and `unmatchedLoggedServices` excludes all configured ServiceSites, including configured
rows beyond the displayed 100-site page.

An exact player result includes at most 100 `lastVotes` rows with `siteKey`, `displayName`, `serviceSite`, and `time`, plus
`lastVotesTruncated`. These are stored last-vote values for sites that currently resolve as enabled; disabled, invalid, or
unloaded site keys are not returned. They are not log enumeration or an end-to-end delivery history.
`pendingOfflineVotes` is a bounded count saturated at 100,000 rather than a detailed queue view.

### Exact-player storage fields (schema version 1)

The `player` result additionally contains `storageRowAvailable`, `storage`, `columns`, and `columnsTruncated`.
`storageRowAvailable` is true only when the exact loaded player has user data and a configured storage type that can be
read. When false, `columns` is an empty array, `columnsTruncated` is false, and `storage` is omitted. `storage` is the
storage enum name (for example `SQLITE`), not a connection or table description.

Each `columns` entry is exactly `{ "name": <string>, "type": <string>, "value": <string> }`. `value` is a bounded
string rendering: integer values are decimal text, booleans are `true`/`false` text, and string values are returned as
text (a null string renders as empty text). `type` is exactly the underlying stored `DataValue.getType().name()`;
the allow-list accepts only the corresponding string, boolean, or integer value type described below. Only these fields
are eligible:

- Static string names (must have a string value): `UUID`, `PlayerName`, `LastOnline`, `DayVoteStreakLastUpdate`,
  `VoteRemindersLast`.
- Static boolean names (native boolean, or a string exactly matching `true`/`false`, case-insensitively): `TopVoterIgnore`,
  `Reminded`, `DisableBroadcast`.
- Static integer names (integer value): `VotePartyVotes`, `MonthTotal`, `AllTimeTotal`, `DailyTotal`, `WeeklyTotal`,
  `Points`, `DayVoteStreak`, `BestDayVoteStreak`, `WeekVoteStreak`, `BestWeekVoteStreak`, `MonthVoteStreak`,
  `BestMonthVoteStreak`, `HighestDailyTotal`, `HighestMonthlyTotal`, `HighestWeeklyTotal`, `LastMonthTotal`,
  `LastWeeklyTotal`, `LastDailyTotal`.
- Dynamic integer names: `MonthTotal-<MONTH>-<YYYY>` where `<MONTH>` is an uppercase English month name and `<YYYY>` is
  exactly four digits; and `VoteShopLimit<suffix>` where `<suffix>` is 1–64 characters from `[A-Za-z0-9_-]`.
- Four runtime-derived exact names: the configured cooldown flag (`CoolDownCheck` or `CoolDownCheck_<server-storage-name>`,
  boolean), cooldown-site list (`CoolDownCheck_Sites` or `CoolDownCheck_<server-storage-name>_Sites`, string),
  all-sites day (`AllSitesLast` or `AllSitesLast_<server-storage-name>`, integer), and almost-all-sites day
  (`AlmostAllSitesLast` or `AlmostAllSitesLast_<server-storage-name>`, integer). The runtime names are compared exactly;
  they are not wildcards.

Other stored keys—including serialized offline/reward payloads, plugin-specific keys, malformed dynamic spellings, and
allow-listed names with the wrong value type—are omitted. Entries are sorted by `name` case-insensitively, then by exact
spelling, and at most 100 eligible entries are returned. A 101st eligible entry or a value larger than 16 KiB sets
`columnsTruncated:true`; oversized values themselves are omitted. The complete result is bounded at 512 KiB. No
credential, secret, raw payload, SQL metadata, or arbitrary storage key is exposed.

## Data and security invariants

- Inspection results may contain only the typed fields documented above. Never echo a credential, password, token,
  database or transport host, Control endpoint, webhook URL, raw configuration, or raw server log.
- Unexpected managed configuration read, preview, apply, and reload exceptions retain their action-specific result code but
  return fixed external text; their detailed cause is logged only on the backend and is never copied into a Control result.
- An unexpected handler exception returns only generic `INSPECTION_FAILED` text. The backend log may identify its exception
  class, but omits the exception message because it can contain storage endpoints, users, or filesystem paths.
- `diagnostics` explicitly lists sensitive categories it omitted. It is a status report, not a support archive.
- VoteLog access goes through bounded methods on `VoteLogMysqlTable`; Control never supplies SQL or a table name.
- VoteLog queries execute on the connector worker and use prepared, bounded table methods. Never move them to the Bukkit
  primary thread or return database/table connection settings. Preserve the 10-second JDBC statement timeout.
- Player lookup requires an exact UUID or valid Minecraft name and checks existence before loading. There is no “all
  players”, prefix search, or pagination cursor.
- `vote-site-resolution` uses `resolveVoteSite`, not `getVoteSite`; the latter can auto-create configuration.
- Reward simulation returns `wouldExecute:false` and `sideEffects:false`.
- Reward-builder preview/apply shares the simulation parser but replaces only the selected subtree and never invokes a
  reward executor.
- Capability negotiation is authoritative. An older Control that does not accept `data.inspect.v1` must not receive
  inspection polling.

## VoteLog interpretation

VoteLogging is optional and SQL-backed; a dependent query returns `UNAVAILABLE` when it is disabled, lacks an initialized
adapter, or fails its readability probe. The quick setup updates and reloads `Config.yml`, but it does not recreate or
close the runtime VoteLog manager. A server restart is therefore required after either `VoteLogging.Enabled` transition.
Until then, disabling immediately gates any stale adapter and reports unavailable/readable false, while enabling a
previously disabled instance can report enabled true but available false.
The exposed rows are selected **logged events**: `VOTE_RECEIVED`, `VOTEMILESTONE`, `VOTE_STREAK_REWARD`,
`TOP_VOTER_REWARD`, and `VOTESHOP_PURCHASE`. `IMMEDIATE` and `CACHED` describe recorded processing status.

`overview`, `diagnostics`, and vote-site health distinguish configured, enabled-and-adapter-available, and currently readable state.
`voteLogReadable` runs a bounded probe with a 10-second JDBC statement timeout. Existing VoteLog table methods catch SQL
failures and return empty/zero values, so the inspection layer probes first: summary/search/trace fail `UNAVAILABLE` when
the probe fails, and health skips aggregates and labels each applicable row unavailable/unreadable. The probe is
point-in-time; a later SQL failure can still trigger a legacy empty/zero fallback, so it is not a transactional health
guarantee.

A `voteId` correlates rows written with the same identifier. It is not a complete delivery trace: VoteLog does not promise
an entry for every validation rejection, network hop, duplicate decision, reward command, command outcome, or expiry. UI
and support output must say “logged events” and must not claim end-to-end delivery proof.
