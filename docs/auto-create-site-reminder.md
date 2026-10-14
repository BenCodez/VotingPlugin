# Automatic vote-site setup reminder

VotingPlugin logs an owner-facing console reminder when `AutoCreateVoteSites`
remains enabled after the setup period. It advises setting
`AutoCreateVoteSites: false` in `Config.yml` after the intended sites are
configured. The reminder never changes that setting, creates a site, grants a
reward, or changes vote matching. Existing explicit administrative site creation
continues to work when automatic creation is disabled.

## Configuration

Existing installations need no manual migration. Missing settings use these
defaults in `Config.yml`:

```yaml
AutoCreateVoteSites: true
AutoCreateVoteSitesReminder:
  Enabled: true
  AfterDays: 7
  RepeatDays: 7
```

The intervals are positive integer days, at most 106751. Invalid intervals disable
only this reminder until corrected and reloaded. `Enabled: false` suppresses the
console reminder. `AutoCreateVoteSites: false` also suppresses it. Configuration
reload updates a snapshot; it does not add another timer or reset the clock.
There are no new commands or permissions, and no player messages.

## Timing and persistence

The clock measures cumulative active plugin runtime using monotonic elapsed
time. Offline time never counts. Existing installations start accumulating when
they first run a build containing this feature; historical uptime is not guessed.
Runtime accumulates even while the reminder or automatic creation is disabled.
Changing settings or restarting does not mint a new clock or reminder allowance.

A single task on a dedicated bounded daemon worker checkpoints the runtime once
per minute and checks the configured threshold. A busy executor can delay both
the checkpoint and the message; it does not produce a catch-up burst. The worker
is separate from the vote-processing timer so checkpoint I/O cannot hold that
queue. The marker
is persisted before the console warning, so a restart after that write suppresses
a repeated warning. A crash between the write and logging can omit that warning;
the next configured repeat remains eligible. Settings changed during a write or
shutdown suppress a stale warning conservatively.

The checkpoint belongs to one server JVM per plugin data directory; sharing
that directory between multiple server JVMs is unsupported. A nonblocking
JVM-local file-lock admission keeps one plugin instance responsible for checkpoint
publication, including its final write. This does not promise cross-process
mutual exclusion: closing another channel can release a native lock on some
platforms even while Java retains its overlapping-lock bookkeeping. During a full plugin disable
and re-enable in the same JVM, a successor retries on its background timer until
the retiring instance releases the lease. It then loads the completed checkpoint;
an older writer cannot overwrite the successor. This wait never blocks gameplay
or plugin startup/shutdown. The fixed empty `.auto-create-sites-reminder.lock`
file is retained; a crash releases the JVM
and native lock state automatically.

The fixed internal `.auto-create-sites-reminder.dat` file contains only two
runtime counters, a version header and checksum. Checked atomic publication uses
the existing file durability helper. An unreadable or corrupt checkpoint pauses
this reminder without overwriting it or interrupting voting. A failed checkpoint
does not advance the in-memory clock or issue a warning. Correct the storage
problem and reload or restart to retry; do not remove the checkpoint to resolve a
vote-processing problem.

Shutdown cancels the periodic task and queues a final background checkpoint
before shutting down that worker. It adds no synchronous file operation or
extra shutdown wait. A hard crash or an interrupted final write can lose runtime
since the last successful checkpoint, including any executor delay. This is a
setup reminder, not an exact uptime meter or a fix for unsafe site-name rendering.

## Verification

Automated tests exercise the monotonic clock, restart and duplicate markers,
strict checkpoint format, failed/uncertain writes, deferred worker execution and
task lifecycle. Server testing should check startup/reload/stop, verify that only
the console receives warnings, and confirm that normal votes and explicit admin
site creation retain their existing behavior. Actual server verification must be
reported separately from unit and mocked service tests.
