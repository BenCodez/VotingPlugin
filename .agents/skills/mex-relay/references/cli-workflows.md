# Relay CLI workflows

Use JSON mode for deterministic agent work. Keep request and preview files temporary and avoid displaying them unless troubleshooting.

## Resolve only the needed contract

Run:

```text
mex relay contract --action <command-id> --json
```

Use one of these command IDs:

- `relay.draft.save`
- `relay.draft.delete`
- `relay.publish`
- `relay.acknowledge`
- `relay.close`

Treat this bounded action result as the exact runtime source for the request shape, available examples, constraints, preview command, and apply command. Do not dump `mex capabilities --json` or the full Relay contract during ordinary execution. Write request and preview JSON only to ordinary regular files inside the checkout or an approved temporary directory; do not use symlinks.

## Choose an audience and optional context

1. For an open handoff or “save for whoever picks it up,” use `audience: "team"` and `recipients: []`. Do not enumerate today's Members; active Members who join later must remain eligible. This setting does not share the local draft.
2. For named people, use `audience: "members"`. Resolve names with `mex member list --active --limit 100 --json` and bounded cursors. Local drafts may retain an empty recipient array while names are unresolved; record the intended name in `unresolvedQuestions` and resolve it before publication.
3. Use `mex member show <member-id> --json` for disambiguation or exact publication revisions. Ask when named recipients remain ambiguous; never reinterpret a named subset such as “the backend team” as every project Member.
4. Default to no Workstream lookup. If the user named a Workstream or the handoff clearly belongs to an existing one, run `mex workstream list --json` and `mex workstream show <workstream-id> --json` to resolve it exactly.
5. In Relay v3, preserve a relevant Workstream as typed entity evidence when the selected contract supports it; do not author the legacy top-level Workstream field.

Never fabricate recipient IDs, entity IDs, revisions, commits, code fingerprints, paths, URLs, or provenance.

## Save a checkout-local draft

1. Resolve `relay.draft.save`.
2. Write one temporary JSON object containing the draft content. Include a concise summary, the intended audience, and only useful non-empty context sections. Prefer accurate omissions over invented completeness.
3. For a new draft, run `mex relay draft save --from <draft.json> --operation-id <unique-id> --json`. This validates the content, obtains a signed local preview, and applies that exact preview internally. It never publishes and needs no Member resolution. Do not combine `--from` with a request file or `--apply`.
4. Require `ok: true`, `mode: "apply"`, and `data.applied: true`; read the draft ID from `data.localChanges` and return `/relays?view=drafts&draft=<id>`. The shortcut retains the original preview under `.mex/local/relay-previews/` before applying and removes it after success. An interrupted save can resume with the same operation ID and unchanged content, or the exact `problem.recovery` command. Do not replace the receipt or create another operation to work around an interrupted/conflicting result; if recovery is refused, report it and inspect the existing draft state.

For example, a new open handoff may contain:

```json
{
  "audience": "team",
  "recipients": [],
  "summary": "Continue the parser correction.",
  "completed": ["Reproduced the failure with the nested-import fixture."],
  "nextActions": ["Check whether the same failure affects exported aliases."]
}
```

For an existing draft update, read it with `mex relay draft show <draft-id> --json`, preserve its content, and use its exact local revision in the structured request. Preview with `mex relay draft save <request-file> --json`; require `ok: true`, `mode: "preview"`, and `data.preview.valid: true`. Apply the complete captured envelope unchanged with `mex relay draft save --apply <preview-envelope> --json`. An explicit save/update request needs no additional confirmation for this checkout-local write.

The apply writes only checkout-local draft state in `.mex/local/team.db`. It does not create a canonical Relay or Activity record, deliver a handoff, commit, push, or notify anyone. Apply before the preview expires; if anything changes or the preview becomes stale, preview again instead of reconstructing it.

## Delete a local draft

1. Read the exact draft and current local revision.
2. Resolve and preview `relay.draft.delete`.
3. Explain that the checkout-local draft will be deleted and wait for fresh confirmation.
4. Apply the captured preview unchanged with `mex relay draft delete --apply <preview-envelope> --json`.

## Publish a Relay

1. Read the exact draft. For an open-to-team audience, no recipient Member list is needed. For named recipients, require at least one and read each current active Member; an empty named draft must be completed before publication.
2. Resolve `relay.publish`. Team publication expects only the exact local draft revision; named publication also expects every recipient Member revision. Preview with `mex relay publish <request-file> --json`. The service verifies the current active sender in both cases.
3. Explain that applying replaces the private local draft with canonical Git-tracked Relay and Activity records in the working tree, records the service-observed branch/HEAD/dirty repository state without copying dirty source contents, and does not deliver through a notification service or share before Git commit/push and teammate pull/refresh.
4. Wait for fresh explicit confirmation.
5. Apply the exact preview with `mex relay publish --apply <preview-envelope> --json`.
6. Return `/relays?view=sent&state=open&relay=<relay-id>`.

## Take a Relay

1. Resolve the exact published Relay with `mex relay show <relay-id> --json`. A team audience allows the current active project Member; a named audience requires the current Member to be listed. An omitted audience on an older Relay means named recipients.
2. Resolve `relay.acknowledge` and preview with `mex relay acknowledge <request-file> --json`.
3. Explain that applying records the current Member as claimant and writes canonical Relay/Activity state in the working tree. A synchronized claim prevents another take; separate offline claims still require Git conflict resolution. There is no unclaim or reassignment action, and it does not assign or start a task elsewhere.
4. Wait for fresh explicit confirmation, then apply the exact preview with `mex relay acknowledge --apply <preview-envelope> --json`.
5. Return `/relays?view=mine&state=open&relay=<relay-id>`.

## Close a Relay

1. Resolve the exact acknowledged Relay and its current revision.
2. Resolve `relay.close` and preview with `mex relay close <request-file> --json`.
3. Explain that applying irreversibly marks only the handoff as no longer needing attention and writes canonical Relay/Activity state in the working tree.
4. Wait for fresh explicit confirmation, then apply the exact preview with `mex relay close --apply <preview-envelope> --json`.
5. Return `/relays?view=all&state=closed&relay=<relay-id>`.

Closing does not complete a linked task, issue, pull request, or Workstream. No lifecycle command stages, commits, pushes, pulls, or sends a notification.
