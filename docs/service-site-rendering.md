# Voting-service identifiers and displayed names

VotingPlugin displays ordinary configured names directly, without WORD JOINER or
other invisible formatting guards. Existing administrator-authored `DisplayName`
colors remain supported. The exact accepted `ServiceSite` is still used for vote
matching; no escaping or rewriting is applied to that identifier.

External vote-service identifiers must be bounded, visible names without
interpreter delimiters. `&`, `%`, the section-sign color character, braces,
quotes, brackets, backticks, backslashes and control/format characters are
rejected by the shared validator before vote scheduling or automatic site
creation. This applies to Bukkit Votifier ingress, backend/proxy messages,
NeoForge ingress and the existing validation of persisted ingress queues.
A rejected service does not receive vote credit or rewards.

Use a plain service identifier such as `PlanetMinecraft.com` or
`Minecraft Server List`. The service field is not the voting URL: encoded URLs
or query strings containing `%` or `&` are rejected too. Keep the clickable
voting URL in the site's `VoteURL` setting, whose URL behavior is unchanged.
These restrictions do not rewrite or migrate configured sites or saved player
records. A previously accepted delimiter-bearing service must be corrected at
its vote source before new votes from that source can be accepted. Malformed
persisted ingress queues retain their existing fail-closed recovery behavior;
no unsafe records are silently awarded or reinterpreted.

This is an intentional ingress compatibility restriction instead of the old
invisible display guards. Live client rendering has not been visually verified.
