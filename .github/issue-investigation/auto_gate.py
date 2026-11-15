#!/usr/bin/env python3
"""Fail-closed, read-only admission check for public issue-opened events."""
import json
import os
from datetime import datetime, timezone
from pathlib import Path
import re
import sys
from urllib.parse import quote, urlencode

from run import ApiError, Curl

# A new public issue can be opened by anyone. Limit billable investigations.
MAX_DAILY_AUTOMATIC_ISSUES = 5
WORKFLOW = 'investigate-issue.yml'


def eligible(event, github, repository, environment_name, today=None):
    """Return (admitted, reason). No API session is created here."""
    if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repository):
        raise RuntimeError('Invalid repository')
    if not isinstance(event, dict) or event.get('action') != 'opened':
        return False, 'Not an issue-opened event'
    issue = event.get('issue')
    if not isinstance(issue, dict) or 'pull_request' in issue:
        return False, 'Not a regular issue'
    number = issue.get('number')
    if isinstance(number, bool) or not isinstance(number, int) or number < 1:
        raise RuntimeError('Invalid issue number')
    author = issue.get('user') or {}
    if author.get('type') == 'Bot' or str(author.get('login', '')).endswith('[bot]'):
        return False, 'Bot-generated issue'
    if not environment_name or not environment_name.strip():
        return False, 'Set repository variable ISSUE_INVESTIGATION_ENVIRONMENT to the existing key environment'

    # A typo must not cause GitHub to create a new, unprotected environment.
    environment_path = '/repos/' + repository + '/environments/' + quote(environment_name, safe='')
    try:
        environment = github.request('GET', environment_path)
    except ApiError as error:
        if error.status == 404:
            return False, 'Configured GitHub Environment does not exist'
        raise
    if not isinstance(environment, dict) or (
            not isinstance(environment.get('name'), str) or
            environment['name'].casefold() != environment_name.casefold()):
        raise RuntimeError('Could not verify the configured GitHub Environment')

    day = today or datetime.now(timezone.utc).date()
    query = urlencode({
        'event': 'issues',
        'created': day.isoformat() + '..' + day.isoformat(),
        'per_page': '1',
    })
    path = '/repos/' + repository + '/actions/workflows/' + WORKFLOW + '/runs?' + query
    result = github.request('GET', path)
    count = result.get('total_count') if isinstance(result, dict) else None
    if isinstance(count, bool) or not isinstance(count, int) or count < 0:
        raise RuntimeError('Could not verify daily Actions run count')
    if count > MAX_DAILY_AUTOMATIC_ISSUES:
        return False, 'Daily automatic issue limit reached (' + str(count) + ' runs)'
    return True, 'New issue #' + str(number) + ' within daily limit (' + str(count) + '/' + str(MAX_DAILY_AUTOMATIC_ISSUES) + ')'


def main():
    try:
        event = json.loads(Path(os.environ['GITHUB_EVENT_PATH']).read_text(encoding='utf-8'))
        github = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        admitted, reason = eligible(event, github, os.environ['GITHUB_REPOSITORY'],
                                    os.environ.get('AUTO_ENVIRONMENT_NAME', ''))
        with Path(os.environ['GITHUB_OUTPUT']).open('a', encoding='utf-8') as output:
            output.write('allowed=' + ('true' if admitted else 'false') + '\n')
        print(('Admitted: ' if admitted else 'Skipped: ') + reason)
        return 0
    except Exception:
        # A failed preflight must never fall back to running a billable session.
        print('Automatic issue preflight failed closed; no model work authorized.', file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
