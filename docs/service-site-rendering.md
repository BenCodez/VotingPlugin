# Voting-service identifiers and displayed names

Ordinary voting-site names retain their text, without WORD JOINER or other
font-dependent guards. External display values have an ASCII space on each side
to prevent adjacent template text from completing a formatting token. For example,
`[%SiteName%]` renders `[ Javascript=1 ]` for the literal service `Javascript=1`,
rather than creating an executable rich-format token. Administrator-authored display-name colors and placeholders
remain supported. The exact service identifier is preserved for matching,
action placeholders, and queued-vote recovery.

Display-only values originating from external service identifiers or fallback
site keys use printable ASCII labels for interpreter delimiters: `&` becomes
`(ampersand)`, `%` becomes `(percent)`, `§` becomes `(section)`, and braces become
`(open brace)` / `(close brace)`. This avoids executing color/placeholder syntax
without relying on font support for hidden Unicode guards. For example, a legacy
`R&D` identifier matches exactly as before but its fallback display is
`R(ampersand)D`. An explicit trusted `DisplayName` can provide a preferred label.

This also protects already-persisted auto-created sites. It does not rename site
keys or change reward action identifiers. Existing service-name acceptance and
persisted ingress journals remain compatible: names containing `&` or `%`,
including legacy URL-shaped identifiers, remain replayable. Structural delimiters
such as quotes, brackets, backticks and backslashes, control characters and
oversized identifiers retain their existing rejection rules. The same validation
applies before delayed-vote reward site/user lookup. `VoteURL` is unchanged.

No configuration or data migration is required. Minecraft client rendering of
the new printable labels has not been visually verified; automated tests cover
literal display values, trusted labels, queued-vote recovery and reward routing.
