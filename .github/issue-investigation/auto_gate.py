#!/usr/bin/env python3
"""Fail-closed, read-only admission for hosted investigations."""
import json
import os
from datetime import datetime, timezone
from pathlib import Path
import re
import sys
from urllib.parse import quote, urlencode

from engine import is_bot
from run import ApiError, Curl, validate_inputs

# A new public issue can be opened by anyone. Limit billable investigations.
MAX_DAILY_AUTOMATIC_ISSUES = 5
WORKFLOW = 'investigate-issue.yml'


def eligible(event, github, repository, environment_name, today=None,
             event_name='issues', owner=None, updates=False):
    """Return (admitted, reason). No API session is created here."""
    if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repository):
        raise RuntimeError('Invalid repository')
    if owner != 'github':
        return False, 'GitHub is not the selected automatic investigation owner'
    if not isinstance(event, dict):
        return False, 'Invalid event'
    action = event.get('action')
    issue = event.get('issue')
    if not isinstance(issue, dict) or 'pull_request' in issue:
        return False, 'Not a regular issue'
    if (event_name, action) != ('issues', 'opened'):
        if not updates:
            return False, 'Automatic issue updates are disabled'
        if event_name == 'issue_comment' and action in {'created', 'edited'}:
            comment = event.get('comment') or {}
            association = comment.get('author_association')
            author = (comment.get('user') or {}).get('login')
            reporter = author and author == (issue.get('user') or {}).get('login')
            body = comment.get('body') or ''
            trusted = association in {'OWNER', 'MEMBER', 'COLLABORATOR'}
            evidence = re.search(r'(?im)\b(?:reproduc\w*|logs?|version|build|stack trace|actual result)\b|^```', body)
            if is_bot(comment.get('user') or {}) or not (trusted or (reporter and evidence)):
                return False, 'Comment does not qualify as an evidence update'
        elif event_name != 'issues' or action not in {'edited', 'reopened'}:
            return False, 'Unsupported issue event'
    if is_bot(event.get('sender') or {}) or is_bot(issue.get('user') or {}):
        return False, 'Bot-generated event'
    number = issue.get('number')
    if isinstance(number, bool) or not isinstance(number, int) or not 1 <= number <= 999999999:
        raise RuntimeError('Invalid issue number')

    available, reason = verified_environment(github, repository, environment_name)
    if not available:
        return False, reason

    day = today or datetime.now(timezone.utc).date()
    count = 0
    for automatic_event in (('issues', 'issue_comment') if updates else ('issues',)):
        query = urlencode({'event': automatic_event, 'created': day.isoformat() + '..' + day.isoformat(),
                           'per_page': '1'})
        path = '/repos/' + repository + '/actions/workflows/' + WORKFLOW + '/runs?' + query
        result = github.request('GET', path)
        event_count = result.get('total_count') if isinstance(result, dict) else None
        if isinstance(event_count, bool) or not isinstance(event_count, int) or event_count < 0:
            raise RuntimeError('Could not verify daily Actions run count')
        count += event_count
    if count > MAX_DAILY_AUTOMATIC_ISSUES:
        return False, 'Daily automatic issue limit reached (' + str(count) + ' runs)'
    return True, 'Issue #' + str(number) + ' within daily limit (' + str(count) + '/' + str(MAX_DAILY_AUTOMATIC_ISSUES) + ')'


def verified_environment(github, repository, environment_name):
    if not environment_name or not environment_name.strip():
        return False, 'Set repository variable ISSUE_INVESTIGATION_ENVIRONMENT to the existing key environment'

    if len(environment_name) > 255 or not environment_name.isprintable():
        raise RuntimeError('Invalid environment name')

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

    return True, 'Configured environment verified'


def manual_admission(github, repository, environment_name, owner, number, minutes):
    if owner != 'github':
        return False, 'GitHub is not the selected investigation owner'
    validate_inputs(number, minutes)
    return verified_environment(github, repository, environment_name)


def main():
    try:
        event = json.loads(Path(os.environ['GITHUB_EVENT_PATH']).read_text(encoding='utf-8'))
        github = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        repository = os.environ['GITHUB_REPOSITORY']
        environment = os.environ.get('AUTO_ENVIRONMENT_NAME', '')
        owner = os.environ.get('ISSUE_INVESTIGATION_OWNER')
        event_name = os.environ['GITHUB_EVENT_NAME']
        if event_name == 'workflow_dispatch':
            number = os.environ.get('MANUAL_ISSUE_NUMBER', '')
            admitted, reason = manual_admission(github, repository, environment, owner,
                                                number, os.environ.get('MANUAL_TIMEOUT_MINUTES', ''))
        else:
            admitted, reason = eligible(event, github, repository, environment,
                                        event_name=event_name, owner=owner)
            number = str(event['issue']['number']) if admitted else ''
        with Path(os.environ['GITHUB_OUTPUT']).open('a', encoding='utf-8') as output:
            output.write('allowed=' + ('true' if admitted else 'false') + '\n')
            if admitted:
                output.write('issue_number=' + number + '\n')
                output.write('environment=' + environment + '\n')
        print(('Admitted: ' if admitted else 'Skipped: ') + reason)
        return 0
    except Exception:
        # A failed preflight must never fall back to running a billable session.
        print('Hosted issue preflight failed closed; no model work authorized.', file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
