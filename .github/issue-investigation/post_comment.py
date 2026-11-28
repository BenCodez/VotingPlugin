#!/usr/bin/env python3
"""Publish checked reports from the trusted VotingPlugin workflow."""
import hashlib
import json
import os
from pathlib import Path
import re
import sys
from urllib.parse import quote, unquote

from report import report_completion, report_fingerprint
from engine import evidence_revision, profile_for
from disclosure import payload_bytes, require_disclosure, require_substantive_approval
from run import Curl, github_source_exists, issue_context

RESULTS = Path('investigation-results')


def report_links(report, repository, sha, artifacts_url):
    """Replace sandbox-only Markdown links with public, pinned URLs.

    Markdown accepts both [label](/path) and [label](</path with spaces>).
    Output files are included in the downloadable workflow artifact ZIP, not
    reachable at their original /workspace/outputs locations.
    """
    workspace = profile_for(repository)['workspace']
    source_path = re.escape(workspace.removeprefix('/workspace/'))

    def rewrite(match):
        target = match.group(1) or match.group(2)
        if target.startswith('/workspace/outputs/'):
            return '](' + artifacts_url + ')'

        prefix = workspace + '/'
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
        rf'\]\((?:<(/workspace/(?:{source_path}|outputs)/[^<>\r\n]{{1,2048}})>'
        rf'|(/workspace/(?:{source_path}|outputs)/[^\s)<>]{{1,2048}}))\)',
        rewrite, report)


def comment_body(report, repository, sha, run_id, revision=None, report_digest=None):
    profile = profile_for(repository)
    build_url = profile['development']
    run_url = f'https://github.com/{repository}/actions/runs/{run_id}'
    report = report_links(report, repository, sha, run_url + '#artifacts')
    # Bound UTF-8 bytes as well as characters, leaving room for the heading/links.
    encoded = report.encode('utf-8')
    if len(encoded) > 55000:
        report = encoded[:55000].decode('utf-8', errors='ignore') + '\n\n[Report shortened; download the complete report from the workflow artifacts.]'
    # Keep the development-build retest guidance even when the model already
    # linked the permalink. Omit only the duplicate URL in that case.
    if build_url in report:
        build_reference = 'the latest successful development build mentioned above'
    else:
        build_reference = (f'the [latest successful development build]({build_url}) '
                           f'([build history]({build_url[:-len('lastSuccessfulBuild/')]}))')
    retest = (
        '\n\n**Optional development-build retest:** If you can safely test on a '
        f'backed-up/staging server, download the {profile["checkout"]} JAR from '
        f'{build_reference}. '
        'This is a development build, not a verified fix or stable release. '
        'Check plugin compatibility and fully restart before retesting; '
        'please share the build number and whether the same steps reproduce.')
    marker = (f'<!-- issue-investigation:v1:{run_id}:{revision}:{report_digest} -->\n'
              if revision else f'<!-- votingplugin-issue-investigation:{run_id} -->\n')
    if not build_url.endswith('/lastSuccessfulBuild/'):
        retest = ''
    return (marker +
            '## Automated issue investigation\n\n'
            f'[Workflow run]({run_url}) · [Download investigation artifacts]({run_url}#artifacts) · Development checkout `{sha}` (not necessarily the reported version)\n\n'
            'This is an automated analysis, not confirmation that every check passed. '
            'Verification results and limitations are included below.\n\n'
            + report.strip() + retest)




def require_semantic_review():
    # A model turn, source lookup and disclosure approval do not attest the
    # findings. No trusted semantic review authority is wired in this pilot.
    raise RuntimeError('Publication requires trusted exact-report semantic review')


def publish(api, repository, number, sha, run_id, *, hosted=False):
    if not hosted:
        require_semantic_review()
    if not profile_for(repository)['post']:
        raise RuntimeError('Publication is disabled for this profile')
    turn = json.loads((RESULTS / 'turn.json').read_text(encoding='utf-8'))
    if turn.get('status') != 'completed' or (RESULTS / 'error.json').exists():
        raise RuntimeError('Refusing to post an incomplete investigation')
    report_file = (RESULTS / 'report.md').read_bytes()
    fingerprint = report_fingerprint(report_file.decode())
    metadata = json.loads((RESULTS / 'evidence.json').read_text(encoding='utf-8'))
    completed = json.loads((RESULTS / 'completed.json').read_text(encoding='utf-8'))
    revision = metadata['revision']
    if (metadata['owner'] != 'github'
            or (not hosted and os.environ.get('GITHUB_EVENT_NAME') not in {None, 'workflow_dispatch'}
                and os.environ.get('ISSUE_INVESTIGATION_OWNER') != 'github')):
        raise RuntimeError('This evidence revision is not routed to GitHub')
    if not all(re.fullmatch(r'[a-f0-9]{64}', value)
               for value in (revision, turn['digest'], completed['turn'])):
        raise RuntimeError('Invalid evidence receipt')
    if (metadata['repository'].casefold(), metadata['number'], metadata['development_sha'],
            completed['revision'], completed['turn'], completed['report_sha256']) != (
            repository.casefold(), number, sha, revision, turn['digest'],
            hashlib.sha256(report_file).hexdigest()):
        raise RuntimeError('Investigation receipt does not match the selected report')
    verify_source = (lambda commit, path: github_source_exists(api, repository, commit, path)) if hosted else None
    if metadata.get('observations') != report_completion(report_file.decode(), metadata, verify_source):
        raise RuntimeError('Structured report completion does not match the selected report')
    if hosted:
        if metadata.get('report_status') != 'unreviewed' or completed.get('run_id') != run_id:
            raise RuntimeError('Artifact does not match the producing hosted run')
        fingerprint = hashlib.sha256((revision + fingerprint).encode()).hexdigest()
    issue_path = f'/repos/{repository}/issues/{number}'
    report = comment_body(report_file.decode(), repository, sha, run_id, revision, fingerprint)
    payload = {'body': report}
    destination = 'github-comment:' + repository + '#' + number
    outgoing = payload_bytes(payload)
    if hosted:
        approval = {'repository': repository, 'issue': number, 'revision': revision,
                    'source_sha': sha, 'report': report_file.decode(), 'report_sha256': hashlib.sha256(report_file).hexdigest(),
                    'comment_sha256': hashlib.sha256(outgoing).hexdigest(), 'comment': payload}
        require_substantive_approval(payload_bytes(approval), destination)
    require_disclosure(outgoing, destination)
    issue = issue_context(api, repository, number, track_reopens=True)
    for marker in issue['report_markers']:
        if marker[0] == run_id or (len(marker) == 3 and
                                  (marker[1] == revision or marker[2] == fingerprint)):
            print('Equivalent investigation already posted.')
            return
    if revision != evidence_revision(issue):
        print('Evidence changed during the investigation; stale report was withheld.')
        return
    api.request('POST', issue_path + '/comments', payload)
    print('Posted the completed report to issue #' + number + '.')


def main():
    # A second explicit gate protects against accidental direct invocation.
    if os.environ.get('POST_COMMENT') != 'true':
        print('Issue posting is disabled.')
        return 0
    if sys.argv[1:] != ['--hosted']:
        print('Issue posting blocked: trusted exact-report review is required.', file=sys.stderr)
        return 1
    try:
        repository = os.environ['GITHUB_REPOSITORY']
        number = os.environ['ISSUE_NUMBER']
        sha = os.environ['GITHUB_SHA']
        run_id = os.environ['GITHUB_RUN_ID']
        if (repository != 'BenCodez/VotingPlugin'
                or os.environ.get('GITHUB_EVENT_NAME') not in {'issues', 'workflow_dispatch'}
                or not re.fullmatch(r'[1-9][0-9]{0,8}', number)
                or not re.fullmatch(r'[a-f0-9]{40}', sha)
                or not re.fullmatch(r'[1-9][0-9]{0,19}', run_id)):
            raise RuntimeError('Invalid hosted posting identity')
        api = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        publish(api, repository, number, sha, run_id, hosted=True)
        return 0
    except Exception as error:
        print('Could not post the hosted report: ' + type(error).__name__, file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
