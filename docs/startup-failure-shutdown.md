# Startup failure and shutdown

VotingPlugin may be unloaded after a partial startup. Shutdown therefore treats each optional subsystem independently:
a failed close does not prevent the remaining queues, reminders, cooldown checks, signs, handlers, or executors from being
cleaned up. The vote timer uses a bounded termination wait, forces cancellation if it does not terminate, and restores
the interrupt flag when shutdown is interrupted. Repeated unload callbacks are safe after the first unload completes.

## Building the complete SQL failure fix

The shutdown change works independently, but the MySQL authentication-failure regression also exercises the corrected
SimpleAPI and AdvancedCore constructors. SimpleAPI PR #111 is merged; AdvancedCore PR #361 supplies the remaining SQL
initialization fix. Build these dependencies in that order before building VotingPlugin. CI pins the merged SimpleAPI
revision `fdbf26eec885ec3f1c1dbd937a243a0acfaa0a5e` and AdvancedCore revision
`a68cc2dfe6e44c1c0854e659c614708492fcd58d` so an older published snapshot cannot silently replace the tested fixes.

With those source revisions checked out in sibling directories, run:

```shell
mvn -B -f ../SimpleAPI/SimpleAPI/pom.xml clean install
mvn -B -nsu -f ../AdvancedCore/AdvancedCore/pom.xml clean install
mvn -B -nsu -f VotingPlugin/pom.xml clean package
```

`-nsu` preserves the locally installed snapshot dependencies for these builds. The CI pins can be changed deliberately
once corrected dependencies are reliably published. This is a build dependency only: the distributable VotingPlugin JAR
includes the required libraries, and server owners do not need to install separate SimpleAPI or AdvancedCore JARs.
Invalid MySQL credentials still abort startup; no storage fallback or configuration migration is introduced.
