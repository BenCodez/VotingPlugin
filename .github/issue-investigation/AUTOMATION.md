# GitHub issue investigator

`run.py` and `post_comment.py` use shared policy and evidence processing from
`engine.py` and project metadata from `profiles.json`. The hosted GitHub workflow
is the existing VotingPlugin entrypoint. AdvancedCore, SimpleAPI, VotifierPlus,
VotingPlugin-Control, VotingPluginEditor, and Wiki are offline profiles with
hosted runs and publication disabled. Wiki follows the Markdown source on
`BenCodez/Wiki` and uses documentation checks.

## Future hosted event ownership and admission

The deployed issue-opened workflow uses the repository variable
`ISSUE_INVESTIGATION_ENVIRONMENT`, which must refer to the existing GitHub
Environment holding `OPENAI_API_KEY`. Failed Environment or workflow count
lookups stop before model work. Keep the existing secret name and permissions.

The future hosted design requires an explicit `ISSUE_INVESTIGATION_OWNER`.
No value or `cezar` leaves automatic GitHub work skipped. Select `github` only
after reconciling the existing Cezar intake for openings and updates. The worker
binds the explicit owner to the current repository, issue number, and evidence
revision. This repository supplies no Cezar
Job1 rollout, remote ownership registry, or shared production storage.

`ISSUE_INVESTIGATION_UPDATES=true` opts the GitHub owner into edited and reopened
issues, evidence-like comments from the reporter, and maintainer comments.
Pull requests and bots are excluded from update admission. The preflight sums
automatic issue and issue-comment workflow runs for the UTC day and admits at
most the first five based on GitHub's visible counts. This is a soft throttle.
The automatic model timeout remains 10 minutes. Manual runs keep the selected
5–30 minute timeout, allow an explicit force rerun, and produce draft
workflow artifacts. Model timeout does not represent a spending cap.

Automatic event preflight runs before an issue enters the investigation
concurrency group. GitHub may replace an older eligible pending job with a
newer eligible job. The worker obtains current issue, comments, and reopen
history on start.

## Evidence and publication

The runner classifies feature, bug, support, documentation, and insufficient
evidence using type, labels, text, and relevant discussion. For bugs, only an
unambiguous stable version can resolve to a tag automatically. A reporter or
maintainer's explicit later version correction can supersede a candidate while
earlier reports remain in the evidence record. Exact tag source and the pinned
development SHA remain distinct from a deployed artifact. Custom or snapshot
builds without attested revision metadata stay unverified.

The evidence revision covers issue content, state, labels, reopen history and
ordered human comments. The prompt has bounded text, with early, later, and
maintainer comments selected when needed. Issue material stays in the untrusted
context, with common credentials redacted before prompt submission. Treat
redaction as best-effort and use synthetic or public sanitized evidence for
pilots. Issue and attachment content never supplies setup commands. The hosted
agent stays read-only, uses a digest-checked Temurin download for the existing
VotingPlugin environment, and records evidence limitations and observed checks.

Hosted model output is labeled as an unreviewed draft. Source checks confirm
the existence of a cited pinned file. An issue citation identifies the subject.
Neither establishes that the prose contains a factual observation or a concrete
missing input. Disclosure checking decides whether draft bytes may leave the
runner and is separate from semantic review.

The proposed workflow has no publication job or issue-write permission, and
the publisher blocks before accessing credentials or GitHub. Future publication
requires a trusted reviewer independent of the model to establish mode-specific
observations or concrete missing inputs, inspect the exact report, and bind
approval to its digest, issue destination, and evidence revision. This pilot
has no such authority. A future publisher must also preserve the exact outgoing
disclosure check and the revision and duplicate-marker read immediately before
POST. GitHub provides no conditional comment POST.

### Disclosure and durable claims

Before hosted requests or exporting artifacts, the runner requires a locally
provisioned disclosure checker. The worker checks each allowlisted draft
artifact again before upload. No raw issue or session
response, sandbox file, tool output, position identifier, or recovery state is
uploaded. The report remains agent-generated. Its cited commit identities are
model-reported observations; the runner does not attest runtime verification.

The approved executable must exist at the absolute path
`ISSUE_INVESTIGATION_DISCLOSURE_CHECKER` and match the SHA-256 configured in
`ISSUE_INVESTIGATION_DISCLOSURE_SHA256`. It receives a purpose or destination
argument and exact payload on stdin. Success requires exit zero and a JSON
object containing only `verdict: allow`, the requested `destination`, and the
exact input `sha256`. Unavailable, timed-out, mismatched, or refusing checkers
fail closed. Neither the approved checker nor a deployment mechanism was
installed by this pilot. Scan alone does not replace authorization and
content review; the checker is the administrator-provisioned policy boundary.

`ISSUE_INVESTIGATION_LEDGER` optionally selects an approved persistent SQLite
store shared by the participating runner instances. The store atomically
claims a revision before session creation and records a draft digest after
generation and disclosure checks, independent of semantic approval. A completed claim
or an active/ambiguous claim suppresses replay. With a durable store, explicit
force can replace a completed claim once per evidence revision. It cannot
replace an active or ambiguous claim. Explicit retries can claim a known
failed pre-dispatch attempt. A lost response or interrupted attempt
after dispatch intentionally remains pending reconciliation. The receipt
records draft readiness; it does not attest investigation completion or Actions upload. Without
shared persistence, hosted work still uses accepted bot markers and a no-post
result may be analyzed again on another delivery. GitHub-hosted runners and
Cezar Job1 have no shared ledger in this pilot.

## Offline runner

Use a sanitized issue JSON fixture and a trusted local Git
checkout of the selected repository. A full GitHub snapshot contains an
`issue` record and full REST `comments` and `events` arrays. A bare REST issue
record is accepted only if it has no comments. The compact normalized fixture
shape used in earlier pilots is still accepted. For example:

```sh
python3 .github/issue-investigation/offline.py \
  --profile AdvancedCore --issue fixture.json --checkout /path/to/AdvancedCore \
  --sha <full-comparison-sha> --source README.md --ledger /path/to/evidence.sqlite3
```

The runner performs read-only Git lookups in the verified repository origin,
reads explicitly supplied source paths at the matching tag and comparison
commit, and records commands and outcomes. A related checkout supplied with
`--dependency` may resolve a reported dependency tag. The SQLite ledger
deduplicates the same issue, evidence revision and comparison SHA across runs.
It is local evidence storage, separate from the GitHub publication markers.
Runtime and application tests require independent evidence.

## Proposed Job1 shared admission contract

This is an offline integration proposal. The enabled Job1 and all deployment
settings are outside this patch. The draft-only boundary above applies to
both ingress paths.

1. Treat a GitHub event or Job1 poll hit as a wakeup. Fetch the issue plus all
   comment and event pages before admission. Use the same normalization as the
   investigator. GitHub comment IDs determine ordering. Bot text and bot report
   markers are excluded from the revision. A human correction, edit or reopen
   can produce a new revision. Incomplete or stale cached snapshots must be
   refetched. The ledger can reject a previously claimed snapshot replay, but
   cannot establish whether a never-seen snapshot is current. Before either
   ingress claims, a valid Actions-bot report marker for the exact revision
   suppresses automatic work, even on a fresh ledger. A legacy, stale or
   other-author marker does not suppress it. Explicit investigator force still
   requires the durable ledger and obeys its existing claim fences.
2. Take the owner from one trusted routing configuration shared by both
   consumers. Keys are the canonical repository, decimal issue number and
   evidence revision. Each ingress supplies its actual lane to
   `Claims.claim_ingress`. Only the configured owner may attempt the claim.
   Atomically reserve the attempt before dispatching work. The stored owner
   cannot be changed for that revision. A policy switch affects new revisions.
   Conflicting configuration must stop admission during rollout. The ledger
   fences duplicate claims but cannot tell which conflicting config was intended.
3. Keep running and ambiguous attempts fenced. A known pre-dispatch failure
   can use explicit retry. Persist the draft digest only after draft checks.
   If storage independently returns a confirmed artifact ID, correlate it with
   `record_artifact` using the current attempt and matching draft digest.
   Repeating the same storage acknowledgement is idempotent. An out-of-date
   attempt, wrong digest, or conflicting ID is refused. A draft receipt without
   a storage confirmation is draft-ready only. A storage confirmation is still
   a draft and grants no publishing authority. Storage callback wiring is
   deliberately absent from the proposed workflow.

For offline replay, save a sanitized fixture as an object with `issue`,
`comments`, and `events`, using the GitHub REST shapes and full lists.

```sh
python3 .github/issue-investigation/job1_adapter.py \
  --snapshot fixture.json --repo BenCodez/VotingPlugin --issue 42 \
  --owner cezar --ingress cezar --ledger /path/to/disposable.sqlite3
```

The command emits claim identity, owner and revision, and never dispatches.
Use `--ingress github` against the same configuration and database to check
non-owner behavior. Replay the first command to check deduplication.

### Integration, rollout and rollback

The SQLite file demonstrates transactions between local consumers. An Actions
runner's local disk and a Job1 local file are not shared durability. Before
activation, select one approved transactional store reachable from both
runtimes. Validate identity and access control for the callers, atomic compare
and set, attempt fencing, backup/recovery, and artifact storage acknowledgements.
Use the same schema/version and canonical snapshot tests on both sides. Do not
point separate runners at a generic network filesystem and assume SQLite
locking works. Unavailable or conflicting configuration or storage must stop
new shared admission before dispatch. Current optional hosted ledger behavior
is not production shared admission.

After approval, first compare revision parity using sanitized fixtures or a
read-only shadow comparison that never claims or launches work. At cutover,
stop admitting on the outgoing path, reconcile in-flight attempts and draft
receipts, and confirm one configured owner for new revisions before enabling
shared admission. Rollback likewise stops new shared claims first and waits
for or explicitly reconciles outstanding work before restoring the designated
single path. Preserve the ledger and receipts during rollback. Do not clear
ambiguous claims or reassign an existing revision to manufacture a retry.
Hosted deployment stays blocked on trusted checker, shared store, agreed Job1
cutover and explicit approval. Publication separately requires the independent
trusted semantic approval described above.

## Offline pilot entrypoints

Run the same `offline.py` from VotingPlugin for VotingPlugin, AdvancedCore and
SimpleAPI. Pass the matching local checkout and immutable comparison commit for
each invocation. For example, from this repository:

```sh
python3 .github/issue-investigation/offline.py \
  --profile VotingPlugin --issue fixture.json --checkout . \
  --sha "$(git rev-parse HEAD)" --source README.md
```

Use `--profile AdvancedCore` or `--profile SimpleAPI` with that repository's local
checkout and exact comparison SHA. VotifierPlus, VotingPlugin-Control,
VotingPluginEditor and Wiki require `--validate-profile` and a synthetic fixture.
No engine or workflow is installed in those repositories. No cross-repository
engine download or mutable reference is needed.

The proposed Actions entrypoint runs unit checks only on explicit dispatch.
The repository's separate pull request test workflow continues to run tests.
There is no issue event trigger, hosted model step, artifact upload or posting
path in the pilot workflow. The retained hosted implementation and Job1
adapter are local integration work. A later activation needs a separate,
coordinated review. This draft does not alter the active default-branch workflow.
