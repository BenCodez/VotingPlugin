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
rejects duplicate Rhino, raw Hikari/Folia, external crypto providers and Checker
Framework annotations. It verifies every SQLite native target against the pinned
official driver artifact. It creates both
server and client TLS identities from the packaged JDK-only implementation.

SQLite retains all 20 native targets from the official `sqlite-jdbc` 3.54.1.0
artifact in the downloadable plugin. Windows, macOS, Linux (including ARM and
musl), and FreeBSD installations can replace the plugin JAR and start offline;
no new driver download, cache deletion, or manual provisioning is required.
Each embedded native is checked against a pinned SHA-256 digest, extracted to a
unique load directory under the plugin libraries directory, and loaded with the
existing temporary Xerial properties. The JVM-wide properties are restored.
Native loading does not depend on a writable or executable JVM temp directory.
A corrupt or unexpected embedded native fails validation rather than silently
switching to a download.

The verified-driver fallback remains available if a native resource is absent
from a custom/repackaged installation. It accepts only the official
`sqlite-jdbc-3.54.1.0.jar` (SHA-256
`f8dce420a6016278497f4f0560f65602b9db858de141b541cf91a4644ebe4639`)
in the VotingPlugin libraries directory, or downloads and verifies that exact
artifact. The normal downloadable plugin never needs this fallback for a
supported target. Explicit `org.sqlite.lib.path` / `org.sqlite.lib.name` JVM
properties retain their existing operator-controlled behavior. Extracted natives
keep unique load directories for replacement classloaders and best-effort
stale-file cleanup.

Retaining all supported natives adds about 11 MiB of compressed resources compared
to keeping only Linux x86_64. This is necessary for offline drop-in upgrades;
the size target remains advisory, while missing, duplicated, or mismatched natives
fail the package check. Resource verification across all targets does not claim
native execution on foreign operating systems; startup is exercised on the build
host by the isolated packaged NeoForge runtime test with an unusable JVM temp
directory.

The package-phase test reports a prominent warning when the downloadable artifact
exceeds the 10 MiB size target. Size alone does not fail the build; dependency
growth should still be reviewed. Required runtime resources, duplicate libraries,
linkage, and other packaging-safety checks remain build failures. Release/deployment profiles
reuse this Shade setup; the artifact check follows their configured JAR name.

Keep the downloadable VotingPlugin JAR as small as practical. Before adding a
runtime dependency, inspect the shaded artifact and assign one owner for each
embedded package. Prefer platform-provided APIs where every supported loader
supplies them, and filter unused native targets or duplicate transitive classes
only when the retained runtime paths are covered by packaging and startup tests.
