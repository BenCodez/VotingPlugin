# VotingPlugin Fabric bootstrap

This standalone server mod targets Minecraft 1.21.1, Java 21, Fabric Loader
0.16.14 or later, and Fabric API 0.116.5+1.21.1 or later for Minecraft 1.21.1.

From the repository root, run `gradle -p VotingPlugin-Fabric clean build` with
Gradle 8.8 and JDK 21. Install the remapped VotingPlugin-Fabric JAR from
`VotingPlugin-Fabric/build/libs/` and the matching Fabric API JAR in the
dedicated server's `mods/` directory. The separate Fabric artifact requires
no Bukkit, proxy, or NeoForge libraries.

On first startup the mod creates `config/votingplugin/fabric.properties`.
Set `logLifecycle=false` and restart to silence its startup and shutdown
messages. Existing settings are preserved. A malformed setting causes a
configuration initialization error.

The bootstrap supports dedicated-server lifecycle and in-memory lookup of
an online player's UUID and profile name. Lookups queued from any thread
complete on the server thread, return an empty result for offline players,
and fail if the runtime is inactive, stops before lookup, or reaches its
queue limit. Never wait for a queued lookup on the server thread. Use an
explicit executor for completion work that must happen off the server thread.

## Optional vote-site rules

Place a UTF-8 `VoteSites.yml` next to `fabric.properties` and restart to load
read-only site rules. The mod does not create this file.

```yaml
VoteSites:
  Example:
    Enabled: true
    ServiceSite: example.test
    Name: Example Site
    Priority: 10
    WaitUntilVoteDelay: true
    VoteDelay: 24h
```

Missing or empty configuration leaves the server bootstrap active with no site
rules. A malformed file disables the entire set and logs a short diagnostic.
The loader accepts a single mapping, at most 64 KiB and 256 sites. Aliases,
anchors, duplicate keys, and custom tags are unsupported. Supported site fields
are the ones shown above plus `VoteDelayMin`, `VoteDelayDaily`, and
`VoteDelayDailyHour` (0–23). Sites default to disabled, names to their key,
and priority and delay values to zero. Enabled sites require a service name.

Lookups match enabled service names, keys, or display names case-insensitively,
using descending priority and declaration order for ties. Case-variant site
keys are rejected. Ambiguous aliases produce a warning and still follow the
same ordering.

Numeric `VoteDelay` is hours and numeric `VoteDelayMin` is minutes. Use decimal
integers without leading zeros, since YAML allows other bases for integer
scalars. A formatted
`VoteDelay` such as `1h 30m` takes precedence. Duration strings support
milliseconds, seconds, minutes, hours, days, weeks, and 30-day months. The
read-only delay gate allows the first vote and requires the configured boundary
to have strictly elapsed for later votes. A gated zero interval blocks later
votes. Daily mode uses the reset hour in the explicitly supplied zone. Both
interval and daily comparisons use local wall time in that zone, including
clock jumps and repeated hours during daylight-saving transitions.

Vote receipt, persistence, commands, proxy delivery, and rewards are future
work. The present VotifierPlus candidate callback supplies neither a stable
occurrence ID nor a storage acknowledgement. This mod does not register a
vote callback or count votes.
