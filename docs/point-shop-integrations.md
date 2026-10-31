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
