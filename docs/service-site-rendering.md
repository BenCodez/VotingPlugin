# Voting-service identifiers and displayed names

Bukkit/Paper site-name formatting follows the GitHub VotingPlugin `7.1.1` release baseline
(`39f4717f363beb1d338b353a060630eba6b60cbb`). Configured display names and
fallback site keys enter the existing formatter unchanged. No extra leading or
trailing spaces, invisible separators, or printable delimiter substitutions are
inserted. For example, `&7Site: &e%SiteName%` with `Minecraft Menu` displays
`Site: Minecraft Menu`, and `[%SiteName%]` displays `[Minecraft Menu]`.
Administrator-authored colors, placeholders and intentional whitespace retain
the release behavior. `R&D` remains `R&D` in formatting input.

The exact service identifier remains unchanged for matching, actions, storage
and proxy communication. Compatibility accessors `getDisplayNameForFormatting`,
`getServiceSiteForFormatting` remain available; their historical names do not
promise literal insertion or interpreter isolation.
All Bukkit GUI, book, dialog, broadcast, cooldown, reward and reminder callers
use the original raw getters. Display-only reward/reminder overrides introduced
with the guard are removed. Compatibility accessors simply return those values;
they are not an active formatting path. No AdvancedCore literal-rendering
extension is required.

This deliberately restores the release's formatting trust semantics. The later
external-identifier guards are removed: external or fallback text can once again
participate in the configured color, placeholder or rich-text interpretation.
This change does not claim the previous protection against tokens formed across
placeholder boundaries. Input validation, bounded single-line logging, raw vote
identity, asynchronous reward ordering and delivery bookkeeping are retained.
The separate NeoForge replay adapter was not part of release `7.1.1`; its guarded
expansion and the shared `inertForFormatting` helper are unchanged.

New auto-created sites use the release's original reward-message template,
`&aThanks for voting on %ServiceSite%!`. Existing site files and configured
messages are preserved; no configuration or database migration is required.

Proxy receipt logs sanitize player names using the Minecraft-username sanitizer
and service identifiers using the separate service-site sanitizer. Service dots,
hyphens, spaces and ordinary punctuation survive; controls and unsupported
formatting characters cannot add log lines, and output remains bounded.
These display/log representations do not alter the received vote's identity.

Automated component tests check final visible text and the actual BungeeCord and
Velocity receipt-log paths. Native Minecraft client rendering is not claimed.
