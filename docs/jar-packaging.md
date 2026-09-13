# JAR packaging contract

VotingPlugin consumes AdvancedCore's normal artifact and directly declares
SimpleAPI because it uses APIs outside the subset embedded by AdvancedCore.
AdvancedCore publishes that embedded input as optional so downstream shaders do
not pull the unfiltered self-contained SimpleAPI JAR back into their output.
VotingPlugin applies narrow artifact-specific filters so both the currently
published SNAPSHOT and the coordinated slimmer successor produce the same
dependency ownership. It directly declares the Jedis and Paho libraries used by
its Redis and MQTT transports. Gson is platform-supplied and is therefore
`provided`.

AdvancedCore already contains the relocated Rhino implementation needed by its
JavaScript support, so VotingPlugin excludes the second unrelocated Rhino
dependency. The default branch has no HTTP transport and therefore does not
bundle Bouncy Castle. Adding HTTP transport support must explicitly own its TLS
implementation and crypto dependencies; it must not rely on the non-HTTP
AdvancedCore artifact to provide them.

The package phase runs `PackagedArtifactTest` after shading. It opens the actual
downloadable JAR, checks plugin resources and required relocated classes, and
rejects duplicate Rhino, raw Hikari/Folia, unused Bouncy Castle, and unsupported
Java 25 versioned payload. Release/deployment profiles reuse this Shade setup;
the artifact check follows their configured JAR name.
