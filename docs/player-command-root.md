# Player command root

`Config.yml` supports two settings, with defaults that preserve existing installations:

```yaml
VoteCommandEnabled: true
VoteCommandName: vote
```

These settings and `LoadCommandAliases` are captured when VotingPlugin starts. Restart the server after changing them; `/av reload` keeps the registered root, help links, tab completion and navigation on the current startup settings.

With `VoteCommandName: ballot`, use `/ballot`, `/ballot help`, `/ballot gui`, and the other existing player subcommands. Permissions remain `VotingPlugin.Commands.Vote.*`. Standalone player aliases such as `/votegui` and `/vgui` retain their existing names when `LoadCommandAliases` is enabled; no `/ballotgui` aliases are generated. Disabling alias loading removes the standalone aliases, while keeping the player root and `/adminvote` and `/av`.

`VoteCommandEnabled: false` removes VotingPlugin's player root, standalone player aliases, and their namespaced registrations. Admin commands and admin aliases remain available according to `LoadCommandAliases`. The toggle changes command access only: vote receipt, processing, rewards, storage and proxy routing keep working.

A root is a single label without a leading slash or namespace. It is normalized to lowercase, must start with a letter or digit, and may contain letters, digits, underscores or hyphens up to 64 characters. Invalid names and names reserved for VotingPlugin's admin commands or standalone aliases retain `/vote` with a warning. If another plugin owns `/ballot`, VotingPlugin preserves that owner and registers `/votingplugin:ballot`; generated help and navigation use that namespaced root. If `votingplugin:ballot` is already occupied, VotingPlugin keeps its original `/vote` registration with a warning instead of replacing the occupant. Removing VotingPlugin's old root restores any unrelated command that remains registered under another namespace.

Generated player help, permission-guide command labels, the invalid-command help hint, GUI help navigation and the vote URL dialog's Back action use the active startup root. Generated navigation does nothing while player commands are disabled. Operator-authored GUI `Command` actions, reward commands and general display text remain as configured; update explicit `/vote` references there when choosing another root. The invalid-command hint also accepts `%votecommand%`, including the slash, and adapts existing `/vote help` hints.
