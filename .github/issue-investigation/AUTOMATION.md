# Automatic issue investigation (Luna)

This workflow investigates newly opened **VotingPlugin** issues and, when the agent completes successfully, posts its report as an issue comment from github-actions[bot]. Manual runs are still supported, with public posting **off** by default.

## One-time setup before enabling automatic replies

1. In VotingPlugin → Settings → Secrets and variables → Actions → Variables, add the **repository** variable `ISSUE_INVESTIGATION_ENVIRONMENT` with the exact name of the existing GitHub Actions Environment containing the `OPENAI_API_KEY` secret. This is the environment selected when manually running "Investigate issue." The variable stores a name, not the API key. Never put the API key in repository variables.
2. Confirm the selected Environment provides `OPENAI_API_KEY` and, if needed, `OPENAI_PROJECT_ID`. An Environment requiring reviewer approval will delay automatic investigations until approved.
3. After merging this PR, open a non-sensitive test issue and verify the investigation and separate post-report job. GitHub only triggers issue-opened workflows from the default branch.

Without the repository variable, automatic investigations **skip before calling OpenAI**. The preflight also checks that the named GitHub Environment exists; a missing Environment skips and a failed lookup stops automatic work rather than creating an unprotected environment. The existing `OPENAI_API_KEY` secret is not read or changed by this check. Manual runs still use the Environment selected in their form.

## Automatic behavior and cost safeguards

- Only `issues.opened` triggers automatic runs. Pull requests, bot-authored issues, edits, and other events are ineligible.
- Before opening a paid session, a read-only preflight verifies the Environment by GitHub's read-only Environment API (using the job's `actions: read` permission), then checks the Actions workflow run count for the current UTC day. It permits at most the first **five** issue-triggered runs per day. Missing or invalid counts fail closed; skipped issues may be investigated manually.
- Automatic sessions have a 10-minute agent timeout; manual runs retain the selected 5–30-minute timeout. This is **not** a dollar spending cap.
- Automatic input is capped at 300 title characters, 14,000 body characters, and five comments of 2,000 characters each. Truncation is disclosed to the model; the issue URL is retained.
- The agent uses **gpt-6-luna** with medium reasoning and no live web search. The investigation stays read-only in an isolated OpenAI-hosted sandbox. Usage, diagnostics, and reports are recorded as workflow artifacts.
- Only completed, nonempty reports are published. The separate post-report job has `issues: write` permission and **no** OpenAI key. Run-ID markers prevent duplicate comments when rerunning the same workflow run.
- Issue and comment text are untrusted evidence. The comment includes hypotheses and verification limits; it is not a confirmed fix. User-controlled issue text never enters workflow shell commands.

To pause automatic paid work without changing workflow code, remove the `ISSUE_INVESTIGATION_ENVIRONMENT` repository variable. Preflight skips paid automatic investigations; manual runs remain available.

**Limitations:** Daily workflow-run counting is a soft admission throttle, not an account-wide spending cap. It counts all issue-triggered workflow runs, even skipped or failed ones, and can be affected by concurrent creation and GitHub API indexing delay. Use OpenAI project spend limits/alerts as an additional safeguard. Luna's lower cost does not guarantee the same investigation quality as Sol; compare actual reports before increasing the daily limit.
