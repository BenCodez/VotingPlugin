# Interrupted offline-vote and NameMC rewards

VotingPlugin retains a pending marker before these asynchronous reward chains
start. If delivery or its final storage acknowledgment fails, the marker prevents
an automatic replay that could repeat money, items, or external commands. A
restart or timeout does not prove that a reward was never delivered.

These console-only commands inspect and reconcile that state. They do not grant
rewards or change vote totals. They require `VotingPlugin.Admin` or the respective
`VotingPlugin.Commands.AdminVote.OfflineVoteRecovery` /
`VotingPlugin.Commands.AdminVote.NameMCLikeRecovery` permission. Use the exact
stored player UUID, rather than a player name.

## Before making a recovery change

1. Stop new reward processing and let accepted work finish. In a network, stop
   **all** backends using the same user database, including pending callbacks.
   Stopping only the server where the command runs is insufficient.
2. Check console logs and any affected economy, inventory, or external command
   system. Establish whether the entire batch was delivered, no effects were
   delivered, or delivery was partial/unknown.
3. Request a fresh status preview. Its token expires in five minutes, is
   single-use, and is bound to the reviewed pending state. Changes to that state
   require another preview.
4. Apply only the resolution supported by the evidence. Do not blindly retry a
   partial or unknown delivery. These commands cannot undo external effects or
   guarantee exactly-once execution across different systems.

The local in-flight check blocks recovery while this server is delivering the
reward. With shared user storage, the first valid mutation command explains the
precautions and makes **no changes**. After taking those precautions, repeat the
**identical command** from the same console within 30 seconds to confirm. No
extra argument or separate command is needed. The shared-storage check also
applies to the current shared SQLite runtime.

Confirmation is an **operator acknowledgement**, not a distributed lock or
server-verified proof that other backends are idle. Keep other database writers
and vote/reward ingress stopped until reconciliation finishes. The existing
five-minute, single-use recovery preview token remains mandatory; confirmation
cannot bypass its state checks. A different action, UUID, or token needs a fresh
confirmation. Expired confirmations, reloads, and restarts do not authorize a
mutation. At most 128 pending console identities are retained per confirmation
owner; excess requests fail without changing data until capacity is available.

## Offline vote batches

Run from the server console (omit the leading slash there):

```text
/av OfflineVoteRecovery <uuid> status
/av OfflineVoteRecovery <uuid> delivered <token>
/av OfflineVoteRecovery <uuid> retry <token>
/av OfflineVoteRecovery <uuid> already-cleared <token>
```

- `delivered`: after verifying all effects were delivered, remove only the
  reviewed original queue prefix and its pending marker in one storage write.
  Votes appended after that batch remain queued.
- `retry`: after verifying **no** effects were delivered, clear the pending
  marker while retaining the original votes for normal reward processing.
- `already-cleared`: remove only an orphaned marker when audit evidence shows
  that the original prefix was already removed. A nonmatching prefix alone is
  not evidence of delivery. The command rejects a still-matching prefix.

Legacy queue entries contain site names, not unique vote occurrence IDs. If new
votes from the same sites resemble an old prefix, do not guess which votes to
remove. Retain the quarantine and reconcile using independent evidence.

`clearOfflineVotes()`, `ClearOfflineVotes`, and `ClearOfflineVoteRewards` cannot
replace a queue containing an unresolved batch. The bulk commands check stored
users before clearing any queue. Reconcile the pending batch first; do not
manually delete its database marker. Non-MySQL bulk clears without a pending batch keep
their existing single-invocation behavior. Bulk clears on a network likewise require operational
quiescence; the preflight is not a cross-process lock.

## NameMC claims

```text
/av NameMCLikeRecovery <uuid> status
/av NameMCLikeRecovery <uuid> delivered <token>
/av NameMCLikeRecovery <uuid> retry <token>
```

- `delivered`: confirm the claim before clearing the pending marker, after
  verifying that the complete reward was delivered.
- `retry`: clear the pending marker only after verifying no effects were
  delivered. An already-completed claim cannot be reset for retry.

Recovery changes are logged with the UUID, operator, and chosen resolution.
Failures retain the available recovery evidence; inspect the console and request
another preview before taking further action. A restored healthy storage worker
or a plugin reload alone never authorizes a replay of an uncertain reward.

### Bulk clear and replay concurrency

The legacy void join/background replay entry point now admits work to the user
storage worker and uses the same durable asynchronous completion protocol as
backend replay. Returning from that void method does not confirm delivery.
Login rewards, the public background-update hook, and dependent generic offline rewards
continue only after confirmed replay. The player-aware completion API captures permissions
on the entity owner and returns dependent storage work to the storage worker. Disabled
replay and absent players remain successful no-ops; failed replay leaves dependent work
unexecuted and logs the pending failure. Non-shared-storage proxy updates also wait
for replay before writing the vote timestamp and acknowledging the ordered update.
Both entry points acquire the same local per-player fence before reading a queue
or its pending marker. Empty queues and
failed replay release that fence; asynchronous delivery retains it through confirmed
storage completion. This fence coordinates one JVM, not multiple backends.

`ClearOfflineVotes` and the vote-queue portion of `ClearOfflineVoteRewards` require
a console confirmation when the authoritative active storage type is MySQL.
SQLite retains its existing single-invocation bulk-clear behavior, including
AdvancedCore’s shared-runtime SQLite adapter. A scan followed by a bulk deletion
cannot safely exclude a concurrently starting remote reward batch.

Use the existing commands (including on a single-backend MySQL installation):

```text
/av ClearOfflineVotes
/av ClearOfflineVoteRewards
```

On MySQL the first invocation makes no changes and asks you to stop other writers
and vote/reward ingress. Repeat the same command from the same console within
30 seconds after doing so. Changing from one bulk-clear command to the other
replaces the previous prompt and cannot confirm it. Confirmation is consumed
before the attempted write; a failure requires a fresh confirmation. No automatic
retry is performed. `ClearOfflineVoteRewards` clears both the vote queue and the
separate generic offline reward queue, only after the vote-queue safeguards pass.

This acknowledgement does not detect or lock other JVMs. Confirming while another
backend can modify the database can race reward delivery or delete concurrent
votes. Local bulk clearing continues to reject active or unresolved batches and
excludes new local replay admissions until the bulk write finishes. No monitor is
held during storage work; competing replay fails or defers rather than waiting.
Individual audited recovery uses the same repeat-command acknowledgement above;
it does not automatically coordinate other JVMs.
