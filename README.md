# VotingPlugin
Plugin on SpigotMC
https://www.spigotmc.org/resources/votingplugin.15358/

Development documentation: [optional VotingPlugin Control discovery connector](docs/control-connector.md) and the
[Control agent/protocol contract](docs/control-agent-contract.md).

Maintainers and coding agents should read [AGENTS.md](AGENTS.md) before changing runtime, storage, rewards, proxy, or
Control code. The project requires JDK 21; CI builds it with:

```shell
mvn -B -f VotingPlugin/pom.xml package
```

VotingPlugin Control is an optional management plane. Voting, routing, rewards, joins, and shutdown do not depend on it;
connectors use outbound requests and capability negotiation so either repository can be upgraded independently.

## Optional Control feature set

- Authenticated outbound discovery/configuration connectors for Bukkit, BungeeCord, and Velocity nodes.
- Revisioned YAML and typed setup preview/apply with one-time approval, local backup, reload, and rollback on reload failure.
- Narrow setup for automatic vote-site creation and VoteLogging, plus a typed reward builder that replaces only the
  selected reward subtree and never executes it.
- Read-only `data.inspect.v1` handlers for operational overview, configured/detected vote-site health, exact-player data,
  bounded logged-event summary/search/correlation, non-creating service resolution, reward simulation, and redacted
  diagnostics.

## Control boundaries

The connector does not add an inbound admin listener or expose arbitrary commands, raw SQL, database credentials, generic
files/settings, raw logs, fuzzy/all-player search, or reward execution. VoteLog views contain selected retained **logged
events**, not proof of every transport hop or reward-command outcome. A Control outage or incompatible capability affects
only management availability.

The VoteLogging toggle changes configuration but not the runtime manager lifecycle. Restart VotingPlugin after enabling
or disabling it; inspections gate disabled state immediately and report a newly enabled logger unavailable until restart.

## Maven dependency

```xml
<repository>
    <id>BenCodez Repo</id>
    <url>https://nexus.bencodez.com/repository/maven-public/</url>
</repository>

<dependency>
    <groupId>com.bencodez</groupId>
    <artifactId>votingplugin</artifactId>
    <version>LATEST</version>
    <scope>provided</scope>
</dependency>
```

`LATEST` resolves to the latest stable release.
