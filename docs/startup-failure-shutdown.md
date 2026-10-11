# Startup failure and shutdown

VotingPlugin may be unloaded after a partial startup. Shutdown therefore treats each optional subsystem independently:
a failed close does not prevent the remaining queues, reminders, cooldown checks, signs, handlers, or executors from being
cleaned up. The vote timer uses a bounded termination wait, forces cancellation if it does not terminate, and restores
the interrupt flag when shutdown is interrupted. Repeated unload callbacks are safe after the first unload completes.

## Building the complete SQL failure fix

The shutdown change works independently, but the MySQL authentication-failure regression also exercises the corrected
SimpleAPI and AdvancedCore constructors. SimpleAPI PR #111 is merged; AdvancedCore PR #361 supplies the remaining SQL
initialization fix. The POM pins published SimpleAPI `1.0.2-20261011.161601-40`, so no manual SimpleAPI source installation is needed. CI builds AdvancedCore revision
`d09f0d891127952f0787aef879380a0b8ee0dde8` while that PR remains under review.

With that AdvancedCore revision checked out in a sibling directory, run:

```shell
mvn -B -f ../AdvancedCore/AdvancedCore/pom.xml clean install
mvn -B -nsu -f VotingPlugin/pom.xml clean package
```

`-nsu` preserves the locally installed AdvancedCore snapshot for this build. The CI source pin can be removed once that
fix is merged and published. This is a build dependency only: the distributable VotingPlugin JAR includes the required
libraries, and server owners do not need to install separate SimpleAPI or AdvancedCore JARs.
Invalid MySQL credentials still abort startup; no storage fallback or configuration migration is introduced.

Timed-vote shutdown publication shares an admission fence with queue additions and cache-clearing callbacks. A task that
outlives the executor's bounded termination wait persists its late timed vote through that same owner. Successfully
saved prefixes are retired; an unsuccessful write retains its vote and cache index for a later unload retry, without
repeating unrelated cleanup. Timed-cache writes report I/O failures rather than relying on the legacy file helper's
log-only error handling. Ordinary `TimeQueueHandler.save()` remains a checkpoint; `saveForShutdown()` seals processing.
The existing ServerData file format and persistence model are retained: this is not a new transactional reward system or
an exactly-once guarantee across arbitrary vote event handlers, chat, rewards and disk writes.

A timed vote remains owned while its event is being prepared. If shutdown wins before event dispatch, it is saved for ordinary timed-cache recovery. Once event dispatch starts, external handlers may already have acted: shutdown or a thrown dispatch error retains that occurrence under `VotingPlugin.UncertainTimedVotes.<id>` in `ServerData.yml` and logs a warning. These records contain the original name, service and timestamp and are **not automatically replayed**. An administrator must compare them with voting data/logs before deciding whether any recovery is needed; a completed event can still leave a conservative review record when shutdown raced it. Repeated persistence attempts use the same ID. Shutdown does not wait indefinitely for external event handlers.
