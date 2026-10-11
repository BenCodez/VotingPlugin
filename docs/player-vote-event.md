# PlayerVoteEvent cancellation

`PlayerVoteEvent` implements Bukkit `Cancellable`. Integrations may reject a vote before the MONITOR listener by setting `setCancelled(true)` at an earlier event priority. The native vote consumer then skips totals and reward processing. An uncancelled vote follows its existing processing path; cancelling is not a refund mechanism for work already processed.

The existing cancellation accessors and asynchronous event context are unchanged. Integrations should follow Bukkit priority rules and must not change cancellation state at MONITOR. This correction requires only replacing the VotingPlugin JAR; no configuration or data migration is needed.

For proxy deliveries, cancellation also suppresses backend `PerServerPoints`. A cancelled reliable delivery is acknowledged as handled and retained in the existing replay cache, so transport retries do not re-run a rejected vote. This is a backend decision; it does not undo totals or rewards already processed on the originating proxy or another server. Existing `bungeeVotePluginMessaging` overloads remain available; the additive event-returning dispatch method lets the router observe cancellation.
