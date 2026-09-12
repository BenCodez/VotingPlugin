# JAR packaging contract

VotingPlugin consumes the coordinated AdvancedCore artifact, whose SimpleAPI
dependency uses the `thin` classifier. VotingPlugin directly declares the
Jedis and Paho libraries used by its Redis and MQTT transports instead of
receiving them accidentally through SimpleAPI. Gson is platform-supplied and is
therefore `provided`.

AdvancedCore already contains the relocated Rhino implementation needed by its
JavaScript support, so VotingPlugin excludes the second unrelocated Rhino
dependency. The default branch has no HTTP transport and therefore does not
bundle Bouncy Castle. Adding HTTP transport support must explicitly own its TLS
implementation and crypto dependencies; it must not rely on the non-HTTP
AdvancedCore artifact to provide them.

The package phase runs `PackagedArtifactTest` after shading. It opens the actual
downloadable JAR, checks plugin resources and required relocated classes, and
rejects duplicate Rhino, raw Hikari/Folia, unused Bouncy Castle, and unsupported
Java 25 versioned payload. Release/deployment profiles reuse this Shade setup.
