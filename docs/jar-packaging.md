# JAR packaging contract

VotingPlugin consumes AdvancedCore's normal artifact and applies a narrow
artifact-specific filter so both the currently published SNAPSHOT and the
coordinated slimmer successor produce the same dependency ownership. VotingPlugin
directly declares the Jedis and Paho libraries used by its Redis and MQTT
transports. Gson is platform-supplied and is therefore `provided`.

AdvancedCore already contains the relocated Rhino implementation needed by its
JavaScript support, so VotingPlugin excludes the second unrelocated Rhino
dependency. VotingPlugin bundles the relocated Bouncy Castle base provider used
by `BouncyCastleProvider` and `HttpTlsIdentity`, while excluding its unused
multi-release payloads and unrelated timestamping and other
protocol stacks. HTTP transport support explicitly owns its TLS implementation
and the remaining crypto classes; it does not rely on AdvancedCore to provide
them.

The package phase runs `PackagedArtifactTest` after shading. It opens the actual
downloadable JAR, checks plugin resources and required relocated classes, and
rejects duplicate Rhino, raw Hikari/Folia, unused multi-release crypto payloads,
unused Bouncy Castle protocol packages, and Checker Framework annotations. It
creates both server and client TLS identities
from the packaged crypto classes. SQLite keeps its complete native platform set
so packaging changes do not narrow existing installations.
The test also caps the downloadable artifact at 30 MiB so dependency growth must
be reviewed explicitly. Release/deployment profiles
reuse this Shade setup; the artifact check follows their configured JAR name.

Keep the downloadable VotingPlugin JAR as small as practical. Before adding a
runtime dependency, inspect the shaded artifact and assign one owner for each
embedded package. Prefer platform-provided APIs where every supported loader
supplies them, and filter unused native targets or duplicate transitive classes
only when the retained runtime paths are covered by packaging and startup tests.
