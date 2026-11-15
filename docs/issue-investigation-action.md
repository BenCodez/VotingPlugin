# Investigate an issue with GitHub Actions

The **Investigate issue** workflow runs the OpenAI-hosted issue investigation agent
through curl. It is manual-only: it never comments, opens a PR, edits the repository,
or starts automatically for public issue submissions. The GitHub token has only
contents/issue read permissions and stays on the Actions runner. The OpenAI key is
also kept on the runner, outside the hosted investigation environment.

## Setup and use

1. In repository **Settings → Environments**, use an existing environment or create
   one. Add an environment secret named `OPENAI_API_KEY` for the OpenAI project.
   Existing environment protection and approval rules still apply.
2. The default project is `proj_aRse4SXfv5N60VlakdZGzVV5`. To override it, set
   the environment variable `OPENAI_PROJECT_ID` in that GitHub Environment.
3. After this workflow is merged to the default branch, open **Actions → Investigate
   issue → Run workflow**. Enter the issue number, select the environment containing
   the key, and optionally change the runtime limit (5–30 minutes; default 15).
4. Download the `issue-investigation-<run-id>-<attempt>` artifact from the run page.
   It is retained for 14 days. `report.md` is the final response, `usage.json` is API
   accounting, and the session/turn/items JSON provides failure diagnostics. Published
   sandbox outputs are in `artifacts/`, indexed by ID in `artifacts.json`.

No key is committed or installed by the workflow. The key must have Agents API
access in the configured OpenAI project. The model is `gpt-6.1-sol`, with medium
reasoning and a medium hosted container; edit `.github/issue-investigation/agent.json`
to change agent behavior. Each run creates one session with this definition inline,
so no pre-created saved-agent ID is required.

The public repository is fetched at the exact commit selected by the workflow's
branch/ref. Issue text and all comments are retrieved as data before session creation.
Private repository checkout is not supported. JDK 21 and Maven are explicitly
installed in the hosted environment because requesting packages alone did not
provide Java in the initial live test. The agent follows repository instructions,
may write temporary probes outside the checkout, and must report blocked checks.

## Runtime, cost, and recovery

The agent runs asynchronously in OpenAI's environment, not on the Actions runner.
Both GitHub Actions usage and OpenAI API/tool/container charges may apply. The
runtime limit is **not a hard dollar or token cap**. Investigations are serialized;
starting a new run does not cancel the currently running investigation. GitHub can
replace an older pending run when several runs queue in the same concurrency group.

A completed turn, not an idle session, determines success. Failed/cancelled turns,
unsupported required actions, missing final responses, and runtime expiry fail the
job. The runner briefly polls for delayed usage; unavailable accounting is recorded
as `null`, never invented or treated as zero. A report does not imply all tests passed.

The runner attempts remote cancellation/deletion in `finally`; an `always()` step
retries cleanup before uploading diagnostics. A failed deletion leaves the exact ID
in `session-id.txt`. If cleanup remains unsuccessful, retrieve that artifact and run
`python3 .github/issue-investigation/run.py --cleanup` from a directory containing
`investigation-results/session-id.txt`, with the same OpenAI key/project in the
environment. Runner loss or a hard kill can prevent both cleanup paths; check the
OpenAI project for orphaned sessions. If the create request loses its response, it
is deliberately not retried because the session may already exist remotely. Session
creation contains no investigation input: the ID is saved before the task is sent
as a separate user-message event. A lost create response may still leave a provisioning
or idle environment, but cannot start an untracked investigation turn. If task
submission loses its response, the recorded ID is used for cancellation and deletion;
the task is never automatically resubmitted.

Downloaded files are saved before deletion. No GitHub or OpenAI secrets are supplied
to the agent. Reports and API items may still contain public issue text or excerpts
from linked evidence: treat them as untrusted content. The workflow does not render
agent output into Actions commands or the job summary.

## Local checks

```sh
python3 -m unittest discover -s .github/issue-investigation/tests -v
```

These checks use fake API responses and local curl stubs, not a paid agent run.
