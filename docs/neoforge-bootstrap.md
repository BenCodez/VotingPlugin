# NeoForge bootstrap status

The single `VotingPlugin.jar` now contains an experimental NeoForge 21.1 entry point for Minecraft 1.21.1. This bootstrap does not receive votes, update totals or points, or execute rewards. Bukkit and proxy behavior remains on its existing paths.

On server start, the entry point creates `config/votingplugin/`, installs the packaged `Config.yml` and `VoteSites.yml` only when missing, reads them, and opens the existing AdvancedCore SQLite user backend in `VotingPlugin.db`. The bootstrap currently accepts `DataStorage: SQLITE` only. Its `VotingPlugin_NeoForgeUsers` table has no vote fields yet and is separate from Bukkit user storage. It closes the backend and clears queued tasks and online player identities on server stop.

The NeoForge bridge tracks login/logout identity and runs submitted bootstrap tasks on server ticks. Player identity access uses reflection because NeoForge's published universal Maven JAR does not include Minecraft classes on Maven's compile path; these method names were checked against the installed 1.21.1 server JAR. A future vote-processing step should use a supported mapped Minecraft compile setup and add the actual user schema, vote ingress, reward adapters, and storage parity.

Validation for this step covers unit startup/shutdown, isolated classloader startup/shutdown from the packaged JAR, and a NeoForge 21.1.211 dedicated server smoke run. The server reached its ready state, initialized the bootstrap, and exited cleanly after `stop`. This does not verify player joins, vote receipt, or rewards.
