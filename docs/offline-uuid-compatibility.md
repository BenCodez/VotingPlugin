# Offline UUID compatibility

VotingPlugin intentionally derives offline UUIDs from lowercase player names. This keeps voting data consistent when vote sites send different capitalization. Minecraft can use a different UUID because its offline calculation preserves case. Existing voting records and the UUID algorithm remain unchanged.

With `OnlineMode: false`, `getVotingPluginUser(UUID)` accepts a confidently matched, currently online player's server UUID and uses that player's captured VotingPlugin storage UUID. Identity capture uses the existing presence tracker at join and player-owned refresh boundaries; asynchronous API lookups read captured values without Bukkit player calls or profile/database searches. Delayed login may update only the current owner. Disconnect, replacement, reload and disable invalidate stale captures.

An actual correction logs one WARNING per affected storage identity per plugin runtime, including the player name and both UUIDs. Reload and reconnect do not reset this warning limit. The warning describes third-party compatibility without identifying an unknown caller.

Online mode, unknown/offline/ambiguous identities and already-canonical UUIDs retain their identity. No stored rows are moved, merged, deleted or rewritten by this workaround. A refresh whose entity capture has not completed cannot resolve a new alias yet and conservatively retains the supplied UUID.

The `getVotingPluginUser(UUID, boolean loadName)` overload remains an **exact storage UUID** lookup for record iteration, recovery and administrative operations. It does not apply aliases, regardless of `loadName`. The UUID-and-name and player overloads retain their existing offline name-derived behavior. Constructors and name-based lookups retain their existing contracts. Callers must still follow existing user-storage threading requirements when reading or mutating balances.

This improves compatibility with integrations using the affected UUID-only API. EconomyShopGUI Premium is closed-source and has not been tested with this change; the reporter must confirm its purchase/balance behavior. Deterministic API tests do not establish live Paper/Folia or third-party integration acceptance.
