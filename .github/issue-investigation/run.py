#!/usr/bin/env python3
"""Hosted Actions runner. All HTTP uses curl; credentials stay on this runner."""
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time
from urllib.parse import quote, urlencode

from claims import Claims
from disclosure import payload_bytes, require_disclosure
from report import report_completion
from engine import (DEFAULT, GUIDANCE, PROFILES, evidence, github_snapshot, has_accepted_revision, profile_for,
                    route_revision, version_mentions)

ROOT = Path(__file__).resolve().parent
RESULTS = Path('investigation-results')
PROJECT = 'proj_aRse4SXfv5N60VlakdZGzVV5'


class ApiError(RuntimeError):
    def __init__(self, status):
        super().__init__(f'HTTP request failed (status {status}); response body withheld')
        self.status = status


class Curl:
    def __init__(self, base, key, project=None):
        self.base, self.key, self.project = base, key, project

    def request(self, method, path, payload=None, binary=False):
        headers = {'Authorization': f'Bearer {self.key}', 'Accept': 'application/json'}
        if self.project:
            headers.update({'OpenAI-Beta': 'agents=v1', 'OpenAI-Project': self.project})
        if payload is not None:
            headers['Content-Type'] = 'application/json'
        config = ''.join('header = ' + json.dumps(f'{k}: {v}') + '\n'
                         for k, v in headers.items())
        # Secret headers travel over stdin, never process arguments or artifact files.
        with tempfile.TemporaryDirectory() as directory:
            response = Path(directory) / 'response'
            args = ['curl', '--silent', '--show-error', '--connect-timeout', '10',
                    '--max-time', '30', '--config', '-', '--request', method,
                    '--output', str(response), '--write-out', '%{http_code}', self.base + path]
            if payload is not None:
                data = Path(directory) / 'request.json'
                data.write_bytes(payload_bytes(payload))
                args += ['--data-binary', '@' + str(data)]
            child_env = {k: v for k, v in os.environ.items()
                         if k not in {'OPENAI_API_KEY', 'GH_TOKEN', 'GITHUB_TOKEN'}}
            result = subprocess.run(args, input=config, text=True, capture_output=True,
                                    timeout=35, env=child_env)
            if result.returncode:
                raise RuntimeError(f'curl transport failed (exit {result.returncode}); not retried')
            status = int(result.stdout)
            if not 200 <= status < 300:
                raise ApiError(status)
            raw = response.read_bytes()
            return raw if binary else (json.loads(raw) if raw else {})


def session_state():
    return Path(os.environ.get('RUNNER_TEMP', str(RESULTS))) / 'issue-investigation-session-id.txt'


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n', encoding='utf-8')


def resource_id(value, prefix):
    if not isinstance(value, str) or not re.fullmatch(prefix + r'_[A-Za-z0-9_-]+', value):
        raise RuntimeError(f'Invalid {prefix} ID')
    return value


def pages(api, path):
    rows = []
    after = None
    for _ in range(100):
        query = {'order': 'asc', 'limit': 100}
        if after:
            query['after'] = after
        page = api.request('GET', path + '?' + urlencode(query))
        rows.extend(page['data'])
        if not page.get('has_more'):
            return rows
        cursor = page.get('last_id')
        if not cursor or cursor == after:
            raise RuntimeError('Invalid pagination cursor')
        after = cursor
    raise RuntimeError('Pagination exceeded 100 pages; output incomplete')


def issue_rows(github, path):
    for page in range(1, 101):
        batch = github.request('GET', path + f'?per_page=100&page={page}')
        yield from batch
        if len(batch) < 100:
            return
    raise RuntimeError('Issue pagination bound reached')


def issue_context(github, repository, number, track_reopens=False):
    path = f'/repos/{repository}/issues/{number}'
    issue = github.request('GET', path)
    if 'pull_request' in issue:
        raise RuntimeError('Choose an issue, not a pull request')
    events = issue_rows(github, path + '/events') if track_reopens else ()
    return github_snapshot(issue, issue_rows(github, path + '/comments'), events)


def github_tag(github, repository, tag):
    try:
        # GitHub resolves lightweight and annotated tags to the commit here.
        row = github.request('GET', f'/repos/{repository}/commits/' + quote('tags/' + tag, safe=''))
        return row.get('sha')
    except ApiError as error:
        if error.status != 404 and error.status != 422:
            raise
        return None


def github_source_exists(github, repository, sha, path):
    endpoint = f'/repos/{repository}/contents/{quote(path)}?ref={sha}'
    try:
        value = github.request('GET', endpoint)
    except ApiError as error:
        if error.status == 404:
            return False
        raise
    return isinstance(value, dict) and value.get('type') == 'file' and value.get('path') == path


def session_payload(repository, sha):
    profile = profile_for(repository)
    if not profile['hosted'] or profile['jdk'] != 21:
        raise RuntimeError('Hosted investigations are disabled for this profile')
    workspace = profile['workspace']
    jdk = profile['jdk']
    # Validate before interpolating trusted repository/commit metadata into shell.
    if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repository):
        raise RuntimeError('Invalid repository')
    if not re.fullmatch(r'[0-9a-f]{40}', sha):
        raise RuntimeError('Invalid checkout SHA')
    setup = (
        'set -eu; apt-get update -qq; '
        'apt-get install -y git curl ca-certificates maven ripgrep; '
        f'mkdir -p /opt/issue-jdk{jdk} {workspace}; '
        'curl -fLsS --connect-timeout 15 --max-time 120 '
        'https://github.com/adoptium/temurin21-binaries/releases/download/jdk-21.0.13.1%2B2/'
        'OpenJDK21U-jdk_x64_linux_hotspot_21.0.13.1_2.tar.gz -o /tmp/issue-jdk21.tar.gz; '
        'echo "bb8a93f9ae6e7a693b2b721e85dbeff26ae1a77447985a8d7afcdd9c7cf6222e  /tmp/issue-jdk21.tar.gz" '
        '| sha256sum -c -; '
        f'tar -xzf /tmp/issue-jdk21.tar.gz -C /opt/issue-jdk{jdk} --strip-components=1; '
        f'ln -sf /opt/issue-jdk{jdk}/bin/java /usr/local/bin/java; '
        f'ln -sf /opt/issue-jdk{jdk}/bin/javac /usr/local/bin/javac; '
        f'java -version; mvn -version; cd {workspace}; '
        f'git init; git remote add origin https://github.com/{repository}.git; '
        f'git fetch --depth=1 origin {sha}; git checkout --detach FETCH_HEAD')
    return {'agent': json.loads((ROOT / 'agent.json').read_text()),
            'environment': {'type': 'openai_hosted', 'container_size': 'medium',
                            'network': {'access': 'enabled'},
                            'env': {'JAVA_HOME': f'/opt/issue-jdk{jdk}'},
                            'setup_commands': [{'command': setup, 'cwd': '/workspace'}]},
            'stream': False}



def redact_issue_text(value):
    if not isinstance(value, str):
        return value
    value = re.sub(r'(?is)-----BEGIN [^-]*PRIVATE KEY-----.*?-----END [^-]*PRIVATE KEY-----',
                   '[Redacted key]', value)
    value = re.sub(r'(?im)^.*notifications/unsubscribe.*$', '[Redacted link]', value)
    value = re.sub(r'(?i)\b(?:github_pat_|gh[pousr]_|sk-(?:proj-)?)\S{16,}', '[Redacted token]', value)
    return re.sub(r'(?im)\b(?:password|passwd|secret|token|api[-_ ]?key|authorization)'
                  r'\s*[:=]\s*[^\r\n]+', '[Redacted credential]', value)


def limit_automatic_context(issue):
    """Cap untrusted issue payload size before creating a billable session."""
    result = dict(issue)
    truncated = False
    for key, limit in [('title', 300), ('body', 14000)]:
        value = redact_issue_text(result.get(key))
        if key in result:
            result[key] = value
        if isinstance(value, str) and len(value) > limit:
            result[key] = value[:limit] + '\n[Truncated for automatic investigation; see issue URL.]'
            truncated = True
    comments = result.get('comments') or []
    if len(comments) > 20:
        truncated = True
        middle = comments[5:-10]
        maintainer = [row for row in middle if row.get('association') in {'OWNER', 'MEMBER', 'COLLABORATOR'}]
        correction_sources = {row['source'] for profile_name in PROFILES
                              for row in version_mentions(issue, profile_name) if row['correction']}
        ranked = sorted(enumerate(middle), key=lambda pair: (
            pair[1].get('url') in correction_sources, pair[1] in maintainer,
            bool(re.search(r'(?i)reproduc|stack trace|version|actual result', pair[1].get('body') or '')),
            pair[0]))
        selected = {id(row) for _, row in ranked[-5:]}
        comments = comments[:5] + [row for row in middle if id(row) in selected] + comments[-10:]
    bounded = []
    for comment in comments:
        copy = dict(comment)
        body = redact_issue_text(copy.get('body'))
        if 'body' in copy:
            copy['body'] = body
        if isinstance(body, str) and len(body) > 2000:
            matches = list(re.finditer(r'(?im)^.*(?:version correction|actually running|stack trace|actual result).*$', body))
            corrections = [match for match in matches if any(
                mention['correction'] for profile_name in PROFILES
                for mention in version_mentions(
                    {'author': issue.get('author'), 'comments': [{**comment, 'body': match.group()}]},
                    profile_name))]
            relevant = corrections or matches
            if relevant and relevant[-1].start() > 1000:
                start = max(0, relevant[-1].start() - 100)
                copy['body'] = body[:800] + '\n[Excerpt]\n' + body[start:start + 1100]
            else:
                copy['body'] = body[:1900]
            copy['body'] += '\n[Comment truncated; see linked comment.]'
            truncated = True
        bounded.append(copy)
    result['comments'] = bounded
    if truncated:
        result['context_truncated'] = True
    return result


def session_input(sha, issue, minutes, metadata=None, profile=None):
    name = profile['checkout'] if profile else DEFAULT
    versions = list(dict.fromkeys(row['version'] for row in version_mentions(issue, name)))[:10]
    if len(versions) > 1:
        focus = (f'The issue mentions multiple {name} versions (' + ', '.join(versions)
                 + '). Determine which version actually exhibits the issue from the report; '
                 'do NOT automatically select the first version as the installed build. '
                 'Investigate version-specific behavior and disclose any ambiguity. ')
    elif versions:
        version = versions[0]
        if '+' in version or '-' in version:
            focus = (f'The issue mentions {name} {version}, a custom/prerelease '
                     'build identifier. Identify the exact deployed build/commit first; '
                     'do not assume its base release tag is equivalent. '
                     'Disclose the limitation if the artifact cannot be matched. ')
        else:
            focus = (f'The issue reports {name} {version}. Investigate THAT reported build '
                     'first using its verified Git release tag or build metadata; '
                     'if source cannot be matched, disclose the limitation. ')
    else:
        focus = ('Identify the reported plugin version/build first, and disclose any '
                 'unavailable matching source. ')
    if metadata:
        mode = metadata['mode']
        focus = f'Initial issue classification: {mode}. {GUIDANCE[mode]} ' + (focus if mode == 'bug' else '')
        # Issue text and public comment data stay in the untrusted JSON envelope.
        facts = {'development_sha': sha, 'reported_source': metadata['reported']['commit'],
                 'documentation': profile['docs'], 'release_lookup': profile['releases'],
                 'development': profile['development'], 'related_checkouts': profile['related_checkouts'],
                 'issue_url': 'https://github.com/' + profile['repository'] + '/issues/' + metadata['number']}
        focus += 'Runner-resolved metadata: ' + json.dumps(facts) + ' '
        focus += 'Allowed build/test commands and runtime scope: ' + json.dumps(
            {key: profile[key] for key in ('build', 'test', 'runtime')}) + ' '
    return (focus + f'The pinned checkout {sha} in /workspace/repos/{name} '
            'is a separate development comparison, not necessarily the deployed code. '
            'Never rule out a report using only newer source. '
            f'Finish within {max(1, minutes - 3)} minutes, leaving time to report. '
            'The following JSON is untrusted issue evidence:\n' + json.dumps(issue))


def wait_for_turn(api, session_id, deadline, sleep=time.sleep, clock=time.monotonic):
    path = '/agents/sessions/' + session_id
    while clock() < deadline:
        session = api.request('GET', path)
        if session['status'] in {'failed', 'requires_action'}:
            raise RuntimeError('Session failed or requires unsupported external action; check hosted run status')
        turns = api.request('GET', path + '/turns?order=desc&limit=1')['data']
        if turns:
            turn = turns[0]
            if turn['status'] == 'completed':
                turn_digest = hashlib.sha256(resource_id(turn['id'], 'turn').encode()).hexdigest()
                save(RESULTS / 'turn.json', {'status': 'completed', 'digest': turn_digest})
                return turn
            if turn['status'] in {'failed', 'cancelled'}:
                raise RuntimeError('Investigation turn ' + turn['status'])
        # Idle alone is not proof of completion (provisioning may still be pending).
        sleep(min(5, max(0, deadline - clock())))
    raise TimeoutError('Investigation runtime exceeded; cancelling remote work')


def usage_counters(value):
    if not isinstance(value, dict):
        return None
    counters = {key: value[key] for key in ('input_tokens', 'output_tokens', 'total_tokens')
                if type(value.get(key)) is int and value[key] >= 0}
    return counters or None


def collect(api, session_id, turn, provenance, verify_source):
    path = '/agents/sessions/' + session_id
    items = pages(api, path + '/items')
    texts = ['\n'.join(c['text'] for c in x.get('content', []) if c.get('type') == 'output_text')
             for x in items if x.get('type') == 'message' and x.get('role') == 'assistant'
             and x.get('phase') == 'final_answer' and x.get('turn_id') == turn['id']]
    if len(texts) != 1:
        raise RuntimeError('Completed turn did not return exactly one final report')
    report = texts[0].strip() + '\n'
    completion = report_completion(report, provenance, verify_source)
    report_bytes = ('# Draft — unreviewed model report\n\n' + report).encode()
    require_disclosure(report_bytes, 'github-artifact:draft.md')
    (RESULTS / 'draft.md').write_bytes(report_bytes)
    # Accounting can lag the completion event. Null must never be reported as zero.
    usage = None
    for attempt in range(6):
        session = api.request('GET', path)
        usage = usage_counters(session.get('usage'))
        if usage is not None:
            break
        if attempt < 5:
            time.sleep(5)
    save(RESULTS / 'usage.json', {'status': 'available' if usage is not None else 'unavailable',
                                 'usage': usage})
    print('Report and bounded usage counters saved.')
    return completion


def cleanup(api):
    state = session_state()
    if not state.exists():
        return
    sid = resource_id(state.read_text().strip(), 'sess')
    path = '/agents/sessions/' + sid
    # Retain state if cleanup fails, so the always() step can retry the exact session.
    try:
        session = api.request('GET', path)
    except ApiError as error:
        if error.status != 404:
            raise
        session = None
    if session is not None:
        usage_path = RESULTS / 'usage.json'
        usage = usage_counters(session.get('usage'))
        if usage is not None or not usage_path.exists():
            save(usage_path, {'status': 'available' if usage is not None else 'unavailable',
                             'usage': usage})
        if session['status'] in {'in_progress', 'requires_action'}:
            api.request('POST', path + '/events', {'events': [{'type': 'agent.session.input.cancel'}]})
        for attempt in range(3):
            try:
                api.request('DELETE', path)
                break
            except ApiError as error:
                if error.status == 404:
                    break
                if error.status != 409 or attempt == 2:
                    raise
                time.sleep(2)
    save(RESULTS / 'cleanup.json', {'deleted': True})
    state.unlink()


def interrupted(signum, frame):
    raise InterruptedError('Runner interrupted; attempting remote cancellation and deletion')


def check_upload():
    files = {name: (RESULTS / name).read_bytes()
             for name in ('draft-receipt.json', 'evidence.json', 'draft.md', 'turn.json', 'usage.json')}
    receipt = json.loads(files['draft-receipt.json'])
    evidence = json.loads(files['evidence.json'])
    turn = json.loads(files['turn.json'])
    if (receipt.get('status') != 'draft'
            or receipt.get('draft_sha256') != hashlib.sha256(files['draft.md']).hexdigest()
            or not re.fullmatch(r'[a-f0-9]{64}', receipt.get('revision') or '')
            or receipt['revision'] != evidence.get('revision')
            or not re.fullmatch(r'[a-f0-9]{64}', receipt.get('turn') or '')
            or turn.get('status') != 'completed' or receipt['turn'] != turn.get('digest')):
        raise RuntimeError('Draft receipt does not match the draft, evidence, or completed turn')
    for name, content in files.items():
        require_disclosure(content, 'github-artifact:' + name)


def main():
    os.umask(0o077)
    RESULTS.mkdir(exist_ok=True)
    api = None
    ledger = None
    claim = None
    dispatched = False
    try:
        if sys.argv[1:] == ['--prepare-upload']:
            check_upload()
            with Path(os.environ['GITHUB_OUTPUT']).open('a') as output:
                output.write('draft_ready=true\n')
            return 0
        key = os.environ.get('OPENAI_API_KEY')
        if not key:
            raise RuntimeError('OPENAI_API_KEY is missing from the selected GitHub Environment')
        api = Curl('https://api.openai.com/v1', key, os.environ.get('OPENAI_PROJECT_ID', PROJECT))
        if sys.argv[1:] == ['--cleanup']:
            cleanup(api)
            return 0
        if sys.argv[1:]:
            raise RuntimeError('Only --cleanup is supported')
        number = os.environ.get('ISSUE_NUMBER', '')
        minutes = os.environ.get('AGENT_TIMEOUT_MINUTES', '15')
        if not re.fullmatch(r'[1-9][0-9]{0,8}', number):
            raise RuntimeError('Issue number must be a positive integer')
        if not minutes.isascii() or not minutes.isdigit() or not 5 <= int(minutes) <= 30:
            raise RuntimeError('Runtime must be between 5 and 30 minutes')
        if session_state().exists():
            raise RuntimeError('A previous session requires cleanup; refusing to start another')
        repository = os.environ['GITHUB_REPOSITORY']
        github = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        profile = profile_for(repository)
        sha = os.environ['GITHUB_SHA']
        issue = issue_context(github, repository, number, track_reopens=True)
        metadata = evidence(issue, profile, sha, lambda tag: github_tag(github, repository, tag))
        force = os.environ.get('FORCE_INVESTIGATION') == 'true'
        if not force and has_accepted_revision(issue, metadata['revision']):
            print('This evidence revision already has a published investigation.')
            return 0
        issue.pop('report_markers', None)
        metadata['number'] = number
        owner = (os.environ.get('ISSUE_INVESTIGATION_OWNER', '')
                 if os.environ.get('GITHUB_EVENT_NAME') not in {None, 'workflow_dispatch'} else 'github')
        route = route_revision(repository, number, metadata['revision'], owner)
        if route['owner'] != 'github':
            raise RuntimeError('This evidence revision is routed to Cezar')
        # Export only fixed-shape provenance. Raw issue and API envelopes stay in memory.
        provenance = {**route, 'development_sha': sha, 'mode': metadata['mode'],
                      'reported_commit': metadata['reported']['commit'], 'report_status': 'draft'}
        evidence_bytes = (json.dumps(provenance, indent=2) + '\n').encode()
        require_disclosure(evidence_bytes, 'github-artifact:evidence.json')
        (RESULTS / 'evidence.json').write_bytes(evidence_bytes)
        payload = session_payload(repository, sha)
        require_disclosure(payload_bytes(payload), 'openai-investigation-setup')
        issue = limit_automatic_context(issue)
        task = session_input(sha, issue, int(minutes), metadata, profile)
        event = {'events': [{'type': 'agent.session.input.message',
                            'input': [{'role': 'user', 'content': [{'type': 'input_text', 'text': task}]}]}]}
        require_disclosure(payload_bytes(event), 'openai-investigation-input')
        ledger_path = os.environ.get('ISSUE_INVESTIGATION_LEDGER')
        if force and not ledger_path:
            raise RuntimeError('Forced investigation requires a durable claim store')
        if ledger_path:
            ledger = Claims(ledger_path)
            claim = ledger.claim_ingress(route, 'github', retry_failed=force, force_completed=force)
            if claim is None:
                (RESULTS / 'evidence.json').unlink()
                print('Evidence revision already claimed or completed')
                return 0
        deadline = time.monotonic() + int(minutes) * 60
        dispatched = True
        session = api.request('POST', '/agents/sessions', payload)
        sid = resource_id(session['id'], 'sess')
        session_state().write_text(sid + '\n', encoding='utf-8')
        api.request('POST', '/agents/sessions/' + sid + '/events', event)
        print('Investigation session started', flush=True)
        turn = wait_for_turn(api, sid, deadline)
        provenance['observations'] = collect(
            api, sid, turn, provenance,
            lambda commit, path: github_source_exists(github, repository, commit, path))
        save(RESULTS / 'evidence.json', provenance)
        receipt = hashlib.sha256((RESULTS / 'draft.md').read_bytes()).hexdigest()
        save(RESULTS / 'draft-receipt.json', {
            'status': 'draft',
            'revision': metadata['revision'],
            'turn': hashlib.sha256(turn['id'].encode()).hexdigest(),
            'draft_sha256': receipt,
        })
        # A failed disclosure check cannot settle draft generation.
        check_upload()
        if claim and not ledger.settle(claim, receipt):
            raise RuntimeError('Could not settle the investigation claim')
        return 0
    except Exception as error:
        # Fixed local errors only; do not echo untrusted issue text or API error bodies.
        if claim and not dispatched:
            ledger.settle(claim)
        message = type(error).__name__
        save(RESULTS / 'error.json', {'error': message})
        print(message, file=sys.stderr)
        return 1
    finally:
        if api is not None:
            signal.signal(signal.SIGINT, signal.SIG_IGN)
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            try:
                cleanup(api)
            except Exception:
                save(RESULTS / 'cleanup.json', {'deleted': False, 'error': 'Cleanup failed; retry --cleanup with session-id.txt'})
                print('Remote cleanup failed; session-id.txt retained for retry.', file=sys.stderr)
                return 1


if __name__ == '__main__':
    signal.signal(signal.SIGINT, interrupted)
    signal.signal(signal.SIGTERM, interrupted)
    sys.exit(main())
