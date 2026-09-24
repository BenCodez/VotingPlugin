# JAR packaging contract

VotingPlugin consumes AdvancedCore's normal artifact and applies a narrow
artifact-specific filter so both the currently published SNAPSHOT and the
coordinated slimmer successor produce the same dependency ownership. VotingPlugin
directly declares the Jedis and Paho libraries used by its Redis and MQTT
transports. Gson is platform-supplied and is therefore `provided`.

AdvancedCore already contains the relocated Rhino implementation needed by its
JavaScript support, so VotingPlugin excludes the second unrelocated Rhino
dependency. The Bukkit and Bungee descriptors ask their platform library loaders
for the three Bouncy Castle 1.85 artifacts used by `HttpTlsIdentity`. Velocity,
which has no descriptor-level Maven library support, downloads those same exact
artifacts from Paper's Maven Central mirror, verifies pinned SHA-256 digests,
caches them under the plugin data directory, and attaches them before the proxy
runtime starts. The downloadable plugin JAR therefore does not duplicate the
crypto payload.

The package phase runs `PackagedArtifactTest` after shading. It opens the actual
downloadable JAR, checks plugin resources and required relocated classes, and
rejects duplicate Rhino, raw Hikari/Folia, embedded Bouncy Castle payloads,
Checker Framework annotations, and optional Jedis module clients. It creates
both server and client TLS identities with the declared external crypto
libraries. VotingPlugin keeps the versioned SQLite driver classes in the JAR,
then downloads the exact driver artifact once, verifies its pinned SHA-256, and
extracts only the current operating system and architecture's native library.
This preserves Xerial's supported targets without shipping every native in every
plugin download.
The test also caps the downloadable artifact at 10 MiB so dependency growth must
be reviewed explicitly. Release/deployment profiles
reuse this Shade setup; the artifact check follows their configured JAR name.

Keep the downloadable VotingPlugin JAR as small as practical. Before adding a
runtime dependency, inspect the shaded artifact and assign one owner for each
embedded package. Prefer platform-provided APIs where every supported loader
supplies them, and filter unused native targets or duplicate transitive classes
only when the retained runtime paths are covered by packaging and startup tests.
