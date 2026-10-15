# Startup failure and shutdown

VotingPlugin may be unloaded after a partial startup. Shutdown therefore treats each optional subsystem independently:
a failed close does not prevent the remaining queues, reminders, cooldown checks, signs, handlers, or executors from being
cleaned up. Each unload step is marked complete only after it returns successfully. A repeated unload retries steps
that propagated a failure and skips successful steps, while timed persistence continues through its existing coalesced
retry owner. A timer whose shutdown fails retains its reference for the next cleanup attempt. The vote timer uses a bounded termination wait, forces cancellation if it does not terminate, and restores
the interrupt flag when shutdown is interrupted. Concurrent unload callbacks do not start a second cleanup pass; repeated callbacks remain safe.

## Building the complete SQL failure fix

The MySQL authentication-failure regression exercises the corrected SimpleAPI and AdvancedCore constructors.
SimpleAPI PR #111 and AdvancedCore PR #361 are merged, and the corrected dependencies are published. The POM pins
SimpleAPI `1.0.2-20261011.161601-40`; normal Maven resolution supplies AdvancedCore. CI no longer builds an older
AdvancedCore source checkout before testing VotingPlugin.

Run the focused lifecycle and timed-vote regressions, then the required clean package:

```shell
mvn -B -f VotingPlugin/pom.xml -Dtest=VotingPluginMainShutdownTest,TimeQueueShutdownPersistenceTest,TimeQueueRecoveryCheckpointTest,TimeQueueHandlerRejectionTest,VoteTaskAdmissionTest,VotiferEventDisabledVoteSiteTest,VotifierVoteOverflowQueueTest,CorePlatformIsolationTest test
mvn -B -f VotingPlugin/pom.xml clean package
```

The distributable VotingPlugin JAR includes the required libraries. Server owners do not need a separate SimpleAPI or
AdvancedCore installation, storage fallback, configuration migration or schema migration. Invalid MySQL credentials
still abort startup. The automated SQL test injects an in-process JDBC authentication rejection; it does not establish
live external MySQL or native Paper/Folia startup behavior.

Timed-vote shutdown publication shares an admission fence with queue additions and cache-clearing callbacks. A task that
outlives the executor's bounded termination wait persists its late timed vote through that same owner. Shutdown saves retire successfully saved prefixes; an unsuccessful write retains its vote and cache index for a later
unload retry, without repeating unrelated cleanup. Timed-cache writes report I/O failures rather than relying on the legacy file helper's
log-only error handling. Ordinary `TimeQueueHandler.save()` checkpoints pending votes while keeping their live queue entries; `saveForShutdown()` seals processing.
The existing ServerData file format and persistence model are retained: this is not a new transactional reward system or
an exactly-once guarantee across arbitrary vote event handlers, chat, rewards and disk writes.

A timed vote remains owned while its event is being prepared. If shutdown wins before event dispatch, it is saved for ordinary timed-cache recovery. Once event dispatch starts, external handlers may already have acted: shutdown or a thrown dispatch error retains that occurrence under `VotingPlugin.UncertainTimedVotes.<id>` in `ServerData.yml` and logs a warning. These records contain the original name, service and timestamp and are **not automatically replayed**. An administrator must compare them with voting data/logs before deciding whether any recovery is needed; a completed event can still leave a conservative review record when shutdown raced it. Repeated persistence attempts use the same ID. Shutdown does not wait indefinitely for external event handlers.

Before each event handoff, the queue saves its undispatched suffix and a conservative in-flight review record together.
A confirmed normal event return clears only that occurrence's review record. Cancellation also checkpoints any later
admissions, then continues draining other queued votes. A later checkpoint failure retains the undispatched suffix
both live and recoverable. Direct and scheduled processing use the same recovery boundary; neither
clears the whole recovery cache before the first event. Failed preparation does not invoke the event, and failed
completion publication does not automatically repeat an event that already returned. These writes use the existing
ServerData YAML persistence model, not a crash-atomic transaction or an exactly-once external-effects guarantee.

A successful quarantine releases the finished dispatch's live guard so unaffected votes can resume. A failed quarantine
keeps its original identity until a later processing or unload attempt can retain it; that attempt does not invoke the
ambiguous event again. In-flight records are transient during normal processing, and a crash before actual dispatch can
leave a conservative record too. Stop processing before manually reconciling retained records; their presence alone does
not prove a reward ran or failed.

The stopped vote timer remains visible until overflow and event-handler cleanup finishes. A missing timer is treated as
rejected admission, allowing a late Votifier callback to use the existing overflow path rather than fail on a null
reference. No accepted task is run inline on the event thread as a fallback.

Timed-vote shutdown persistence has one retained daemon worker per handler. Each shutdown call waits at most one second
for that worker, including when a preparation/completion write holds the queue monitor or the shutdown write itself
stalls. Repeated calls join the same pending work; a completed failure remains retryable with the original queue/index.
An interrupted wait preserves the caller's interrupt flag and does not cancel persistence. A timeout reports pending
ownership, **not confirmed recovery**: if the process exits before storage resumes, new pending writes may not finish.
Stopping is published before waiting, so a stalled preparation cannot authorize a new event when it resumes after stop.
This bound covers the timed-queue persistence handoff; it does not promise a deadline for every other subsystem's I/O.

Participating VotingPlugin generations reserve the server-data path in the JVM before AdvancedCore initializes or reads
its file. A replacement fails before startup if the previous generation still owns a write or shutdown handoff. It does
not wait for that filesystem operation on the lifecycle thread. The pending worker keeps its reservation through the
entire write and until stopped vote producers terminate, including an empty initial snapshot followed by late admission
and an initial publication failure. Disable also retains one coalesced retirement reservation independently of the timed
queue, covering AdvancedCore's heartbeat, login, inventory and time-checker executors until they actually terminate.
Failed startup remains reserved until its disable cleanup runs; a missing timed queue does not imply those writers stopped.
SDK server-data saves also retain the generation directly at both normal and durable time-transition publication hooks,
so platform-scheduled callbacks outside those executors, including the SDK version update, cannot overwrite a successor.
The adapter delegates to the existing SDK configuration/file rather than keeping a copied YAML state. VotingPlugin's
server-data wrapper remains bound to its original file and generation even if the plugin object is enabled again.
Lazy first access validates ownership before exposing mutable configuration. A retained SDK file keeps its predecessor
fence during re-enable until AdvancedCore actually creates the replacement file.
After that work ends, normal startup can proceed. Stale generations cannot publish through these server-data save methods
after a successor owns the path; failed shutdown writes remain retryable until such replacement.
This coordinates participating generations at the same normalized path within a JVM, including separate class loaders;
it is not a multi-process or filesystem-alias coordination contract. Existing YAML, dependencies and configuration remain
unchanged. A full process restart clears the old JVM's workers and in-memory reservations.

Normal preparation, completion and quarantine/checkpoint failures retain their existing vote/identity and request one
coalesced retry with bounded backoff. Retry admission does not reset failure backoff; successful processing does. The
processing guard is released before requesting another attempt. Shutdown fences those callbacks, and ambiguous completed
events are quarantined without being dispatched again. The installed SimpleAPI asynchronous delay is in seconds; retries
start at one second and cap at sixty seconds. Rejected retry scheduling reports retained ownership, not durable
recovery that has not been confirmed.
