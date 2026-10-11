# PlayerVoteEvent cancellation

`PlayerVoteEvent` implements Bukkit `Cancellable`. Integrations may reject a vote before the MONITOR listener by setting `setCancelled(true)` at an earlier event priority. The native vote consumer then skips totals and reward processing. An uncancelled vote follows its existing processing path; cancelling is not a refund mechanism for work already processed.

The existing cancellation accessors and asynchronous event context are unchanged. Integrations should follow Bukkit priority rules and must not change cancellation state at MONITOR. This correction requires only replacing the VotingPlugin JAR; no configuration or data migration is needed.
