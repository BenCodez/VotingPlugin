# JAR packaging contract

VotingPlugin consumes AdvancedCore's normal artifact and applies a narrow
artifact-specific filter so both the currently published SNAPSHOT and the
coordinated slimmer successor produce the same dependency ownership. VotingPlugin
directly declares the Jedis and Paho libraries used by its Redis and MQTT
transports. Gson is platform-supplied and is therefore `provided`.

AdvancedCore already contains the relocated Rhino implementation needed by its
JavaScript support, so VotingPlugin excludes the second unrelocated Rhino
dependency. SimpleAPI's HTTP identity uses the JDK cryptography APIs, so the
downloadable plugin does not bundle Bouncy Castle.

The package phase runs `PackagedArtifactTest` after shading. It opens the actual
downloadable JAR, checks plugin resources and required relocated classes, and
rejects duplicate Rhino, raw Hikari/Folia, external crypto providers, uncommon
SQLite native targets, and Checker Framework annotations. It creates both
server and client TLS identities from the packaged JDK-only implementation.

SQLite keeps the Linux x86_64 native in the plugin for offline startup on the
common server platform. When SQLite is selected on another supported target,
VotingPlugin downloads the pinned `sqlite-jdbc` 3.53.4.0 artifact, verifies its
SHA-256 digest, extracts only that target's native into the plugin data folder,
loads it, and restores the JVM-wide Xerial loader properties. MySQL installations
and Linux x86_64 SQLite installations do not make this request.

The test caps the downloadable artifact at 10 MiB so dependency growth must be
reviewed explicitly. Release/deployment profiles
reuse this Shade setup; the artifact check follows their configured JAR name.

Keep the downloadable VotingPlugin JAR as small as practical. Before adding a
runtime dependency, inspect the shaded artifact and assign one owner for each
embedded package. Prefer platform-provided APIs where every supported loader
supplies them, and filter unused native targets or duplicate transitive classes
only when the retained runtime paths are covered by packaging and startup tests.
