#!/usr/bin/env python3
"""Publish an explicitly selected completed report; never calls OpenAI."""
import json
import os
from pathlib import Path
import re
import sys
from urllib.parse import quote, unquote

from run import Curl

RESULTS = Path('investigation-results')
LATEST_BUILD_URL = 'https://bencodez.com/job/VotingPlugin/lastSuccessfulBuild/'
JENKINS_JOB_URL = 'https://bencodez.com/job/VotingPlugin/'


def report_links(report, repository, sha, artifacts_url):
    """Replace sandbox-only Markdown links with public, pinned URLs.

    Markdown accepts both [label](/path) and [label](</path with spaces>).
    Output files are included in the downloadable workflow artifact ZIP, not
    reachable at their original /workspace/outputs locations.
    """
    def rewrite(match):
        target = match.group(1) or match.group(2)
        if target.startswith('/workspace/outputs/'):
            return '](' + artifacts_url + ')'

        prefix = '/workspace/repos/VotingPlugin/'
        if not target.startswith(prefix):
            return match.group(0)
        path = target[len(prefix):]
        line = ''
        # GitHub-style fragments are anchors, not part of the filename to encode.
        if '#' in path:
            path, anchor = path.split('#', 1)
            if not re.fullmatch(r'L[1-9][0-9]*(?:-L[1-9][0-9]*)?', anchor):
                return match.group(0)
            line = '#' + anchor
        else:
            path_with_line = re.fullmatch(r'(.+):([1-9][0-9]*)', path)
            if path_with_line:
                path, line = path_with_line.group(1), '#L' + path_with_line.group(2)

        # Normalize existing percent escapes once, then re-encode the file path.
        # Validate the decoded path so %2e%2e and %2f cannot bypass the
        # traversal checks. Do not guess at ambiguous double-encoded paths.
        if re.search(r'%(?![0-9A-Fa-f]{2})', path):
            return match.group(0)
        try:
            decoded_path = unquote(path, encoding='utf-8', errors='strict')
        except UnicodeDecodeError:
            return match.group(0)
        if (not decoded_path
                or any(part in ('', '.', '..') for part in decoded_path.split('/'))
                or any(ord(ch) < 32 or ord(ch) == 127 for ch in decoded_path)
                or '\\' in decoded_path
                or re.search(r'%[0-9A-Fa-f]{2}', decoded_path)):
            return match.group(0)
        url = (f'https://github.com/{repository}/blob/{sha}/'
               + quote(decoded_path, safe='/._-~') + line)
        return '](' + url + ')'

    # Bracketed URLs may contain spaces, but never consume another tag or
    # paragraph if a closing > is missing; unbracketed URLs cannot contain spaces.
    return re.sub(
        r'\]\((?:<(/workspace/(?:repos/VotingPlugin|outputs)/[^<>\r\n]{1,2048})>'
        r'|(/workspace/(?:repos/VotingPlugin|outputs)/[^\s)<>]{1,2048}))\)',
        rewrite, report)


def comment_body(report, repository, sha, run_id):
    run_url = f'https://github.com/{repository}/actions/runs/{run_id}'
    report = report_links(report, repository, sha, run_url + '#artifacts')
    # Bound UTF-8 bytes as well as characters, leaving room for the heading/links.
    encoded = report.encode('utf-8')
    if len(encoded) > 55000:
        report = encoded[:55000].decode('utf-8', errors='ignore') + '\n\n[Report shortened; download the complete report from the workflow artifacts.]'
    # Every published report supplies an evergreen Jenkins permalink rather than
    # a stale build number or a guessed artifact filename. The same permalink is
    # used by VotingPlugin-Control's Jenkins artifact integration.
    retest = '' if LATEST_BUILD_URL in report else (
        '\n\n**Optional development-build retest:** If you can safely test on a '
        'backed-up/staging server, download the VotingPlugin JAR under the '
        f'[latest successful development build]({LATEST_BUILD_URL}) '
        f'([build history]({JENKINS_JOB_URL})). '
        'This is a development build, not a verified fix or stable release. '
        'Check plugin compatibility and fully restart before retesting; '
        'please share the build number and whether the same steps reproduce.')
    return (f'<!-- votingplugin-issue-investigation:{run_id} -->\n'
            '## Automated issue investigation\n\n'
            f'[Workflow run]({run_url}) · [Download investigation artifacts]({run_url}#artifacts) · Checkout `{sha}`\n\n'
            'This is an automated analysis, not confirmation that every check passed. '
            'Verification results and limitations are included below.\n\n'
            + report.strip() + retest)


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
