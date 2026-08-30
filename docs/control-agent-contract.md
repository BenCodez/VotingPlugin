# VotingPlugin Control agent contract

This is the compact source of truth for an AI agent or Control client implementing the Bukkit integration. The connector
has two separate lanes:

- configuration operations use the negotiated `config.files.v1` / `config.quick-setup.v1` contract and may write only
  managed VotingPlugin YAML after preview and approval;
- inspections use the optional `data.inspect.v1` contract and are always read-only.

Do not translate an inspection request into a configuration operation. Do not add raw SQL, arbitrary commands, player
enumeration, database browsing, filesystem paths, or generic key/value reads to either contract.

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

Data is limited to 512 KiB. General rows are limited to 100, top lists to 20, diagnostics to 128 detected plugin names,
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
| `vote-log-summary` | string `days` (1–365, default 30) | Vote totals, immediate/cached split, unique voters, top services, and top servers |
| `vote-log-search` | at most one of exact `player` (1–16 characters), `service` (1–64), or `server` (1–64); optional `event` and string `days`/`limit` | Bounded recent event rows; `limit` is 1–100 and defaults to 25 |
| `vote-trace` | required canonical 36-character UUID `voteId`; optional string `days`/`limit` | Chronological VoteLog events sharing one correlation ID |
| `vote-site-resolution` | required valid `serviceSite` (1–64 characters); optional string boolean `includeDisabled` | Dry-runs existing resolution and reports whether auto-create would be attempted; never calls the creating resolver |
| `reward-simulation` | required `proposal`, a JSON object encoded as one filter string | Validates and normalizes typed actions, reports the plan, and never invokes `RewardBuilder` |
| `diagnostics` | none | Bounded redacted environment/configuration status, configured/readable VoteLog state, and detected plugin names |

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

## Data and security invariants

- Inspection results may contain only the typed fields documented above. Never echo a credential, password, token,
  database/Redis/MQTT host, webhook URL, raw configuration, or raw server log.
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
