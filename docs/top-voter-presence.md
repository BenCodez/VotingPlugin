# Optional top-voter presence display

`HideTopVoterIfNotPlayedBefore: false` in `Config.yml` preserves existing displays by default.
When enabled, only identities with confirmed `NEVER_JOINED` history are omitted from current and
historical top-voter displays. Joined, pending and unknown history remain visible. This setting
does not remove vote totals, modify stored rankings or change the internal top-voter award inputs.
Existing blacklist and ignore-permission decisions still apply upstream.

Chest/dialog menus, chat pages, top-list/rank placeholders, signs and Discord use display snapshots.
Visible rows have consecutive display positions and keep their original vote totals. Global total
placeholders and the raw ranking APIs remain unchanged. Display-specific positions should not be
treated as award eligibility: internal awards continue using the raw ranking.

Checks run through AdvancedCore's checked asynchronous presence API. Display requests do not load
user storage or invoke server/profile history loaders on the server owner. Offline identity evidence
uses the exact native name spelling; uncertain normalized, migrated or unsupported identities stay
visible. Current UUID and retained offline UUID history can preserve positive migration evidence.
This cannot reconstruct deleted history or establish ownership of unrelated historical names.
Online negatives and offline rows with a known online migration alias additionally require a readable stable history scope without unmapped offline/non-online
records or observed joins. A current-name cache entry alone cannot prove that a former offline name never
joined. Ordinary online-only history still supports negative results; unmapped migration evidence remains
visible. Older SDKs preserve positive history but cannot certify this wider negative scope.

Pending results remain visible until a later display refresh. Evidence is cached for up to 30 seconds,
with at most 128 requests in flight. Evidence capacity starts at 8,192 identities and grows to the
row count already loaded across current, last-month and historical ranking maps. Unlimited boards
therefore retain fresh checked evidence without cycling it out on each projection; removed boards
reduce the capacity on later projections. Saturation and failures retain rows.
Negative evidence revalidates the exact current cache-only identity scope, including uncertainty markers,
at completion and display. Joins invalidate matching negative/pending evidence; reload and shutdown retire the display generation.
Checked historical aliases are included in join invalidation even after a name change. Discord prepares
its snapshot asynchronously before publishing, with a five-second evidence deadline and one pending
preparation per leaderboard. Overlapping requests coalesce without restarting that deadline; their current visible projection is published
when the original preparation finishes. Stale reload callbacks are discarded, and
unavailable evidence stays visible without a duplicate preliminary post.
One snapshot supplies each rendered row's name and vote count. Historical menus inspect only their
selected month rather than resolving every saved board.

Focused verification:

```sh
mvn -B -f VotingPlugin/pom.xml -Dtest=TopVoterDisplayFilterTest,TopVoterPresenceIdentityTest,TopVoterDisplayIntegrationTest,TopVoterStateTest,TopVoterLoaderTest test
```

Before enabling on a server, check current and historical menus with synthetic joined, never-joined,
unknown and migrated identities. Confirm chest/dialog navigation, chat page size, placeholders, signs
and Discord positions; verify ordinary awards and blacklist/ignore behavior still use their original
rankings. Join with an identity previously omitted, reload the option, and confirm stale callbacks do
not re-hide it. Automated focused tests are separate from a live server/gameplay check.

Sign refreshes wait once for their admitted presence checks (with the same bounded preparation deadline), then render current evidence on the sign location's owning scheduler. Overlapping refreshes coalesce; reload, disable or changed sign metadata discards the older completion. A sign-only interval longer than the evidence TTL therefore still repaints after checks complete. Sign lines use a detached render. Skull updates run on each skull location’s own scheduler, recheck the current projected position, and discard superseded frames or changed metadata. Preferred and nearby skull discovery also dispatches each block access to its owning location.

Discord also rechecks current visible evidence when queued publication actually runs, including when no second update requested coalescing. A join or identity-scope change between preparation and publication cannot preserve a stale omitted-row snapshot. These final checks do not restart the asynchronous deadline or alter raw rankings and awards.

When filtering is enabled, the five player-position placeholders project current evidence on each request instead of reusing a persistent rank result. Their work remains bounded and cache-only; the configured placeholder cache behavior remains unchanged for other placeholders and when filtering is disabled. This keeps ranks aligned with list displays after completion, evidence expiry, joins and filter retirement.

Discord deleted-message recovery returns to the existing worker, rebuilds the replacement from current evidence and checks the display generation. Initial sends, edits and recovery share distinct publication ownership, so late or duplicate callbacks cannot overwrite a successor’s message ID or retire its request. Pending sends coalesce another refresh; recovery reprojects after blocking ID persistence, and persistence rechecks ownership after acquiring the save monitor. New-message mode keeps its existing persistence policy. Publication is checked when admitted to the Discord request; joins or lifecycle changes after the external request is sent cannot atomically cancel Discord's network delivery.

An enabled filtered sign with no row at its position clears the associated skull profile on its owning scheduler, together with its empty text projection. Existing empty-sign skull behavior remains unchanged when the option is false.
