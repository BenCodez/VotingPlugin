# Automatic vote-site setup nag

While `AutoCreateVoteSites` remains true, VotingPlugin occasionally prints a brief
console nag:

> After voting-site setup, set AutoCreateVoteSites: false in Config.yml.

There is no new configuration section, command, permission, or player message.
After configuring the intended sites, set the existing `AutoCreateVoteSites`
option to false and reload. This stops the nag while preserving configured sites
and explicit administrative site creation. The nag never changes configuration,
vote matching, totals, or rewards.

The first nag appears after seven cumulative active plugin days; another may
appear after each further seven active days while the option remains enabled.
Downtime is excluded. The internal checkpoint keeps reloads/restarts from
resetting progress or repeating a recent nag. Existing installations begin
counting when first using this feature; historical uptime is not guessed.
Earlier experimental reminder-specific keys are ignored and may be removed.

A dedicated bounded background worker checks and saves progress once per minute;
file work never holds the vote-processing timer or adds a synchronous shutdown
wait. The marker is published before logging. A crash between publication and
logging can omit one nag, and a crash can lose uncheckpointed runtime. Invalid
or unverified checkpoints pause the nag without changing voting. Repair and
reload rather than deleting an existing checkpoint to reset history.

The internal checkpoint belongs to one server JVM per plugin data directory.
Plugin instances coordinate retirement and replacement inside that JVM; sharing
the directory across server JVMs is unsupported. No database/config migration or
companion plugin is required.

Automated tests cover timing, persisted markers, reload/retirement, first-write
failure, invalid/missing records and uncertain publication. Live server testing
should separately confirm normal votes, console-only output, reload/stop and
continued explicit admin site creation when automatic creation is disabled.
