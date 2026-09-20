# Inbox CLI workflows

Use JSON mode for deterministic agent work. Keep request and preview files temporary and avoid displaying them unless troubleshooting.

## Resolve only the needed contract

Run:

```text
mex inbox contract --action <command-id> --json
```

Use one of these command IDs:

- `inbox.draft.save`
- `inbox.draft.delete`
- `inbox.publish`
- `inbox.proposal.approve`
- `inbox.proposal.reject`
- `inbox.proposal.withdraw`
- `inbox.proposal.mark-stale`
- `inbox.proposal.repair`

Treat this bounded action result as the exact runtime source for the request shape, available examples, constraints, preview command, and apply command. Do not dump `mex capabilities --json` or the full Inbox contract during ordinary execution. Write request and preview JSON only to ordinary regular files inside the checkout or an approved temporary directory; do not use symlinks.

## Find existing knowledge and resolve a correction

1. Use `mex wiki query <subject> --limit 10 --json` or `mex wiki list --type <kind> --limit 25 --json` to find related records before creating another.
2. Read the best candidate with `mex inbox target <entity-id> --json`. This bounded read returns `target`, `version`, `sourcePath`, and the existing body from the current Wiki index, without initializing or repairing it.
3. For `knowledge.update`, copy `target` and use `version.contentHash` as the target revision and `version.semanticRevision` as the semantic revision. Select a section entity when only that section should change. The patch body replaces the target's body; preserve its relevant existing content. A target can fit the read response but exceed the 16 KiB update-body limit; choose a smaller section or a title/summary-only patch when appropriate, never truncate existing knowledge to fit.
4. If no suitable record exists, use `knowledge.create` with the appropriate existing kind. MEX chooses a path in `context/` or `patterns/`; do not invent an Inbox knowledge category or supply an arbitrary destination path.
5. If several targets remain plausible after reading them, ask which one the user intends. If a read reports a stale or unavailable index, address its explicit maintenance requirement before relying on it; never fabricate revisions or silently refresh an index as part of a read.

For create-time topics, find them with Wiki reads, then resolve each with `mex inbox target <topic-id> --json` for current revisions. Topics can be dependencies but are not Inbox correction targets. Omit topics when none are needed; normal knowledge creates have no relation editor.

## Resolve a legacy Spec update

1. Use `mex spec list --json` to identify candidates.
2. Use `mex spec show <entity-id> --json` for the exact candidate.
3. Match the requested durable claim to one exact entity and kind.
4. Copy `version.contentHash` as the entity revision and `version.semanticRevision` as the semantic revision exactly as the selected contract requires. For nested requirements, constraints, or acceptance criteria, take both from the enclosing `mex spec show` projection.
5. Stop and ask for target clarification when multiple candidates remain plausible.

Do not invent target IDs, relation endpoints, topic IDs, or revisions. For a create request with relations or topics, resolve every referenced entity and its current revisions first.

## Save a checkout-local draft

1. Resolve `inbox.draft.save`.
2. Create a unique operation ID and a request containing one `knowledge.create` or `knowledge.update` draft (or a Spec change for actual Spec-family intent).
3. For a new draft, provide no unrelated expectations. For an existing draft update, read it with `mex inbox draft show <draft-id> --json` and use its exact current local revision.
4. Preview with `mex inbox draft save <request-file> --json` and capture the complete successful JSON wrapper unchanged. Require `ok: true`, `mode: "preview"`, and `data.preview.valid: true`.
5. Summarize the proposed local effect. If the user asked to create/save/draft, apply with `mex inbox draft save --apply <preview-envelope> --json` without another confirmation.
6. Read the returned draft ID and respond with `/inbox?view=drafts&draft=<id>`.

The apply writes only checkout-local draft state. It does not create a canonical proposal, Activity record, commit, push, or notification. Apply before the preview expires; if anything changes or the preview becomes stale, preview again instead of reconstructing it.

## Delete a local draft

1. Read the exact draft and current local revision.
2. Resolve and preview `inbox.draft.delete`.
3. Explain that the checkout-local draft will be deleted and wait for fresh confirmation.
4. Apply the captured preview unchanged with `mex inbox draft delete --apply <preview-envelope> --json`.

## Publish a draft

1. Read the exact draft and resolve `inbox.publish`.
2. Build the request with the exact local draft revision and preview using `mex inbox publish <request-file> --json`.
3. Explain that applying writes a canonical proposal and Activity artifact into the working tree while leaving Git commit/push separate.
4. Wait for fresh explicit confirmation.
5. Apply the exact preview with `mex inbox publish --apply <preview-envelope> --json`.
6. Return `/inbox?view=review&proposal=<proposal-id>`.

Publishing removes the exact local draft after creating the pending proposal. It does not approve the proposal or change accepted knowledge. The proposal is Markdown in the working tree; teammates receive it through Git.

## Review canonical proposals

Use `mex inbox proposal list --json` and `mex inbox proposal show <proposal-id> --json` for read-only review. For approve, reject, withdraw, mark-stale, or repair:

1. Resolve the matching command ID.
2. Use the proposal's exact current artifact revision and any action-specific rationale or replacement draft.
3. Preview with the corresponding `mex inbox proposal <action> <request-file> --json` command.
4. Explain the semantic outcome and which canonical Git-tracked MEX records would change.
5. Wait for fresh explicit confirmation.
6. Apply the exact preview with the same command plus `--apply <preview-envelope> --json`.

Approval writes the proposed knowledge change, proposal decision, Wiki ledger, and Activity records to the working tree. The proposal remains review history. Reject and withdraw make a terminal proposal decision without changing knowledge. Mark stale changes a pending proposal to stale only when MEX proves dependency drift. Repair replaces stale intent, clears prior review, and returns the proposal to pending without changing knowledge. These canonical transitions write Activity records; none commits, pushes, pulls, stages, or notifies teammates.
