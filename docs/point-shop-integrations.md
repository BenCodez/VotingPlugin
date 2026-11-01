# Point balances and external shops

`%votingplugin_points_nocache%` bypasses VotingPlugin's rendered-placeholder
cache. It does not guarantee a new MySQL read or reserve points, and another
server may change a shared balance immediately after the value is read.

An external shop that grants a reward before a separate point-removal command
has confirmed payment cannot make the two actions atomic. Concurrent clicks or
requests on different servers can grant rewards even when later debits fail.
Use VotingPlugin's transaction-backed VoteShop when confirmed reservation is
required, or use an integration that grants rewards only after an authoritative
payment succeeds.

Point placeholders fail closed while a point mutation's outcome is uncertain.
They can remain unavailable even after a reload, a later successful operation or
a snapshot refresh: those events do not confirm the original mutation. The
current recovery interface does not provide the operation-bound terminal
evidence needed to clear that condition automatically. Do not repeat an uncertain
debit as a cache workaround.

## Direct Java API balances

`VotingPluginUser.getPoints()` remains an `int` for existing integrations such as
EconomyShopGUI. When called on a Bukkit/Folia-owned thread with MySQL storage,
it reads only a published point snapshot and returns **0** instead of throwing
`Shared user data is still loading` when the snapshot is unavailable. This
matches the legacy integer default but is a **fail-closed compatibility value**:
0 does not establish that the player really has no points.

New integrations should call `VotingPluginUser.getPointsIfLoaded()` to
distinguish a known zero (`OptionalInt.of(0)`) from loading or uncertain state
(`OptionalInt.empty()`). In MySQL mode this method is a snapshot-only read,
never a blocking SQL lookup; a cache miss schedules bounded worker recovery
when that state can safely be refreshed. Other storage types retain their
existing read behavior.

An invalidated snapshot, a pending debit or an indeterminate mutation must
not be replaced with a remembered balance. The direct API shares the
point-placeholder mutation fence. Display balances are advisory and are not
reservations: purchases must use confirmed, transaction-backed debits before
granting their rewards. An unchanged shop integration may now reject a click
as insufficient funds while the user's points load, but should not crash on
the transient read failure. Retry after the snapshot is published.

Avoid using `UserDataFetchMode.CACHE_ONLY` as a publication check: it may
return the integer default even when the shared snapshot is not published,
without distinguishing missing data from a genuine zero.
