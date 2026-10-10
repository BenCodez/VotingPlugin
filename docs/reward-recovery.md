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

1. Stop new reward processing and let accepted work finish. In a network, quiesce
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
reward. With shared user storage, mutating commands also require the final
argument `all-backends-quiesced`. This is an **operator assertion**, not a
server-verified distributed lock or proof that another backend is idle. Keep the
network quiesced until reconciliation finishes. The shared storage check also
applies to the current shared SQLite runtime.

## Offline vote batches

Run from the server console (omit the leading slash there):

```text
/av OfflineVoteRecovery <uuid> status
/av OfflineVoteRecovery <uuid> delivered <token> all-backends-quiesced
/av OfflineVoteRecovery <uuid> retry <token> all-backends-quiesced
/av OfflineVoteRecovery <uuid> already-cleared <token> all-backends-quiesced
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
manually delete its database marker. Normal clears without a pending batch keep
their existing behavior. Bulk clears on a network likewise require operational
quiescence; the preflight is not a cross-process lock.

## NameMC claims

```text
/av NameMCLikeRecovery <uuid> status
/av NameMCLikeRecovery <uuid> delivered <token> all-backends-quiesced
/av NameMCLikeRecovery <uuid> retry <token> all-backends-quiesced
```

- `delivered`: confirm the claim before clearing the pending marker, after
  verifying that the complete reward was delivered.
- `retry`: clear the pending marker only after verifying no effects were
  delivered. An already-completed claim cannot be reset for retry.

Recovery changes are logged with the UUID, operator, and chosen resolution.
Failures retain the available recovery evidence; inspect the console and request
another preview before taking further action. A restored healthy storage worker
or a plugin reload alone never authorizes a replay of an uncertain reward.
