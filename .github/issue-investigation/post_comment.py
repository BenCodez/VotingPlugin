#!/usr/bin/env python3
"""Publish an explicitly selected completed report; never calls OpenAI."""
import json
import os
from pathlib import Path
import re
import sys

from run import Curl

RESULTS = Path('investigation-results')


def report_links(report, repository, sha, artifacts_url):
    # The hosted sandbox paths do not exist for issue readers. Pin source links.
    def source(match):
        path = match.group(1)
        line = re.search(r':([0-9]+)$', path)
        if line:
            path = path[:line.start()] + '#L' + line.group(1)
        return '](' + f'https://github.com/{repository}/blob/{sha}/' + path + ')'
    report = re.sub(r'\]\(/workspace/repos/VotingPlugin/([^\s)]+)\)', source, report)
    return re.sub(r'\]\(/workspace/outputs/[^\s)]+\)', '](' + artifacts_url + ')', report)


def comment_body(report, repository, sha, run_id):
    run_url = f'https://github.com/{repository}/actions/runs/{run_id}'
    report = report_links(report, repository, sha, run_url + '#artifacts')
    # Bound UTF-8 bytes as well as characters, leaving room for the heading/links.
    encoded = report.encode('utf-8')
    if len(encoded) > 55000:
        report = encoded[:55000].decode('utf-8', errors='ignore') + '\n\n[Report shortened; download the complete report from the workflow artifacts.]'
    return (f'<!-- votingplugin-issue-investigation:{run_id} -->\n'
            '## Automated issue investigation\n\n'
            f'[Workflow run and complete artifacts]({run_url}) · Checkout `{sha}`\n\n'
            'This is an automated analysis, not confirmation that every check passed. '
            'Verification results and limitations are included below.\n\n' + report.strip())


def publish(api, repository, number, sha, run_id):
    turn = json.loads((RESULTS / 'turn.json').read_text(encoding='utf-8'))
    if turn.get('status') != 'completed':
        raise RuntimeError('Refusing to post an incomplete investigation')
    report = (RESULTS / 'report.md').read_text(encoding='utf-8')
    if not report.strip():
        raise RuntimeError('Refusing to post an empty report')
    issue_path = f'/repos/{repository}/issues/{number}'
    issue = api.request('GET', issue_path)
    if 'pull_request' in issue:
        raise RuntimeError('Choose an issue, not a pull request')
    marker = f'<!-- votingplugin-issue-investigation:{run_id} -->\n'
    # Rerunning the posting job/run must not duplicate an accepted comment,
    # including when a prior POST succeeded but its response was lost.
    for page in range(1, 101):
        batch = api.request('GET', issue_path + f'/comments?per_page=100&page={page}')
        for comment in batch:
            if (comment.get('user', {}).get('login') == 'github-actions[bot]'
                    and comment.get('body', '').startswith(marker)):
                print('This workflow run already posted a report; no duplicate comment created.')
                return
        if len(batch) < 100:
            break
    else:
        raise RuntimeError('Comment pagination limit reached; refusing to risk a duplicate')
    api.request('POST', issue_path + '/comments', {'body': comment_body(report, repository, sha, run_id)})
    print('Posted the completed report to issue #' + number + '.')


def main():
    # A second explicit gate protects against accidental direct invocation.
    if os.environ.get('POST_COMMENT') != 'true':
        print('Issue posting is disabled.')
        return 0
    try:
        repository = os.environ['GITHUB_REPOSITORY']
        number = os.environ['ISSUE_NUMBER']
        sha = os.environ['GITHUB_SHA']
        run_id = os.environ['GITHUB_RUN_ID']
        if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repository):
            raise RuntimeError('Invalid repository')
        if not re.fullmatch(r'[1-9][0-9]{0,8}', number):
            raise RuntimeError('Invalid issue number')
        if not re.fullmatch(r'[0-9a-f]{40}', sha):
            raise RuntimeError('Invalid checkout SHA')
        if not re.fullmatch(r'[1-9][0-9]{0,19}', run_id):
            raise RuntimeError('Invalid workflow run ID')
        api = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        publish(api, repository, number, sha, run_id)
        return 0
    except Exception as error:
        message = str(error) if isinstance(error, RuntimeError) else type(error).__name__
        print('Could not post the report: ' + message, file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
