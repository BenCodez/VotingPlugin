# Standalone vote core

`VotingPluginCore/pom.xml` builds a JDK 21-only JAR from the same
`com.bencodez.votingplugin.core.vote` Java sources compiled into the existing
`VotingPlugin` plugin. The sources stay in one place so the two artifacts cannot
silently acquire different vote policies. The standalone artifact has no Bukkit,
proxy, or AdvancedCore compile dependency.

```sh
mvn -B -f VotingPlugin/pom.xml clean package
mvn -B -f VotingPluginCore/pom.xml clean package
```

The first command remains the Bukkit/proxy plugin build. The second produces
`VotingPluginCore/target/votingplugin-vote-core-7.1.2-SNAPSHOT.jar`; its package
test checks that only shared vote classes are present and that they load without
platform libraries. CI builds both artifacts. Keep the version in both POMs
aligned when changing the plugin version.

This JAR is a library boundary for future platform adapters. It contains no
Fabric or NeoForge entry point, vote ingress, storage adapter, or reward executor.
The Bukkit listener still owns its existing vote and reward behavior. In
particular, `SharedVoteProcessor` is not the production Bukkit path; its
prepared-plan and keyed-delivery contracts must not be mistaken for guarantees
of the existing reward system. Future platform work must preserve live reward
configuration behavior and verify the native adapter against current code and
tests before changing production ownership.
