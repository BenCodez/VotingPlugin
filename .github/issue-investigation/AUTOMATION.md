# GitHub issue investigator

`run.py` and `post_comment.py` use shared policy and evidence processing from
`engine.py` and project metadata from `profiles.json`. The hosted GitHub workflow
is the existing VotingPlugin entrypoint. AdvancedCore, SimpleAPI, VotifierPlus,
VotingPlugin-Control, VotingPluginEditor, and Wiki are offline profiles with
hosted runs and publication disabled. Wiki follows the Markdown source on
`BenCodez/Wiki` and uses documentation checks.

## Hosted admission

VotingPlugin keeps the existing GitHub-hosted runner, `issues.opened` and
manual dispatch. Automatic runs use the existing Environment configured by
`ISSUE_INVESTIGATION_ENVIRONMENT`. Manual dispatch selects an existing
Environment and a 5–30 minute limit. Both retain `OPENAI_API_KEY` and the
existing OpenAI project selection and fallback. The workflow only admits work
on the repository's default branch. GitHub is the default VotingPlugin
workflow owner. An explicit non-GitHub `ISSUE_INVESTIGATION_OWNER` stops
hosted admission.

Automatic admission rejects bots, pull requests and other issue activities.
It accepts at most the first five automatic runs by the visible UTC day
count, with a ten-minute model timeout. A failed Environment or run-count
lookup blocks model work. The count is a throttle, not a provider spending
cap. Manual dispatch does not consume the automatic allowance.

Read-only preflight runs before the per-issue concurrency group. The worker
reads current issue, full comments and reopen history and suppresses accepted
reports for the same evidence revision before creating a model session.
GitHub may replace an older pending eligible job with a newer pending job.

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

A completed model turn and corroborated references are structural evidence
only. Before sending the exact session setup and bounded issue message, the
producer requires a disclosure decision. It also checks the exact bytes of
the report, evidence, completed-turn receipt and bounded usage before
uploading those files. The hosted artifact contains an unreviewed model
report. It carries no permission to post. Raw session or issue exports and
recovery IDs are excluded.

The separate posting job has issue-write permission and downloads only the
producing job's artifact ID. It verifies the run identity, evidence revision,
source references and report digest. Before publication it requires an
independent substantive approval for the exact report, rendered comment,
destination issue, revision and source SHA. A separate disclosure decision
covers the exact comment bytes. The publisher then rereads current evidence
and accepted bot markers immediately before POST. A refusal, missing
approval, changed revision or existing equivalent report prevents a new
comment. GitHub provides no conditional POST. Automatic eligible openings
post after these checks. Manual runs default to artifact-only and require
explicit opt-in for posting.

Both jobs remain on `ubuntu-latest`. They load a trusted HTTPS checker from
`ISSUE_INVESTIGATION_POLICY_URL` only if the downloaded executable matches
`ISSUE_INVESTIGATION_DISCLOSURE_SHA256`. Provisioning this policy is separate
from editing the workflow. Exact disclosure receipts use verdict `allow`.
Substantive approval has a distinct `approve-substantive-report` verdict and
must be derived from independent review, not from the model or a generic
scanner. Each receipt binds the exact destination and input SHA-256. If no
such checker or independently reviewed decision exists, these operations
stop. A protected, owner-reviewed policy is a deployment prerequisite.
The model receives no credentials and cannot choose the policy or publish.

An optional `ISSUE_INVESTIGATION_LEDGER` can fence claims in compatible
callers. Without shared persistent storage, manual no-post replay or
concurrent model runs across consumers may still consume budget. The
per-issue job group and fresh accepted-bot markers limit duplicate posting.
These are not exactly-once admission or a hard monetary cap.

Session cleanup cancels active work and retries deletion of the known
session. The fallback cleanup step uses the current run's recovery file.
Recovery state stays out of the upload. Lost create responses and forced
termination can still require provider-side reconciliation.

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

Before enabling both consumers, use sanitized fixtures or read-only shadow
comparison to verify revision parity. Reconcile existing intake ownership,
budget and in-flight sessions. Shared transactional storage remains a
separate prerequisite for cross-system exactly-once admission claims.
Rollback stops admission on the outgoing path and reconciles in-flight
sessions before restoring the designated owner. Preserve receipts and
never clear ambiguous claims to manufacture a retry. The local correction
changes no deployed workflow, settings, credentials or ownership.

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

The separate `issue-investigation-offline.yml` workflow runs unit checks
only on explicit dispatch. It has no model or publishing permissions. The
existing pull request test workflow also remains in place.
