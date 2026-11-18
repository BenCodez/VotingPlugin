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

Future work covers shared vote processing and persistence, commands and
permissions, then proxy delivery and native VotifierPlus integration once
VotifierPlus provides a Fabric callback API. Rewards, offline vote replay,
vote parties, streaks, milestones, and broadcasts require further work.
This bootstrap does not receive votes or register voting commands.
