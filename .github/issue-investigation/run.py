#!/usr/bin/env python3
"""Manual Actions runner. All HTTP uses curl; credentials stay on this runner."""
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import time
from urllib.parse import urlencode

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
                data.write_text(json.dumps(payload), encoding='utf-8')
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


def issue_context(github, repository, number):
    issue = github.request('GET', f'/repos/{repository}/issues/{number}')
    if 'pull_request' in issue:
        raise RuntimeError('Choose an issue, not a pull request')
    comments = []
    for page in range(1, 101):
        batch = github.request('GET', f'/repos/{repository}/issues/{number}/comments?per_page=100&page={page}')
        comments.extend({'user': x['user']['login'], 'body': x.get('body'),
                         'url': x['html_url']} for x in batch)
        if len(batch) < 100:
            break
    else:
        raise RuntimeError('Issue comments exceed retrieval bound')
    return {'url': issue['html_url'], 'title': issue['title'],
            'body': issue.get('body'), 'comments': comments}


def session_payload(repository, sha):
    # Validate before interpolating trusted repository/commit metadata into shell.
    if not re.fullmatch(r'[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+', repository):
        raise RuntimeError('Invalid repository')
    if not re.fullmatch(r'[0-9a-f]{40}', sha):
        raise RuntimeError('Invalid checkout SHA')
    setup = (
        'set -eu; apt-get update -qq; '
        'apt-get install -y git curl ca-certificates maven ripgrep; '
        'mkdir -p /opt/issue-jdk21 /workspace/repos/VotingPlugin; '
        "curl -fLsS --connect-timeout 15 --max-time 120 "
        "'https://api.adoptium.net/v3/binary/latest/21/ga/linux/x64/jdk/hotspot/normal/eclipse' "
        '-o /tmp/issue-jdk21.tar.gz; '
        'tar -xzf /tmp/issue-jdk21.tar.gz -C /opt/issue-jdk21 --strip-components=1; '
        'ln -sf /opt/issue-jdk21/bin/java /usr/local/bin/java; '
        'ln -sf /opt/issue-jdk21/bin/javac /usr/local/bin/javac; '
        'java -version; mvn -version; cd /workspace/repos/VotingPlugin; '
        f'git init; git remote add origin https://github.com/{repository}.git; '
        f'git fetch --depth=1 origin {sha}; git checkout --detach FETCH_HEAD')
    return {'agent': json.loads((ROOT / 'agent.json').read_text()),
            'environment': {'type': 'openai_hosted', 'container_size': 'medium',
                            'network': {'access': 'enabled'},
                            'env': {'JAVA_HOME': '/opt/issue-jdk21'},
                            'setup_commands': [{'command': setup, 'cwd': '/workspace'}]},
            'stream': False}



def limit_automatic_context(issue):
    """Cap untrusted issue payload size before creating a billable session."""
    result = dict(issue)
    truncated = False
    for key, limit in [('title', 300), ('body', 14000)]:
        value = result.get(key)
        if isinstance(value, str) and len(value) > limit:
            result[key] = value[:limit] + '\n[Truncated for automatic investigation; see issue URL.]'
            truncated = True
    comments = result.get('comments') or []
    if len(comments) > 5:
        truncated = True
    bounded = []
    for comment in comments[:5]:
        copy = dict(comment)
        body = copy.get('body')
        if isinstance(body, str) and len(body) > 2000:
            copy['body'] = body[:2000] + '\n[Comment truncated; see linked comment.]'
            truncated = True
        bounded.append(copy)
    result['comments'] = bounded
    if truncated:
        result['context_truncated'] = True
    return result


def reported_votingplugin_version(issue):
    """Identify a reported version for prioritizing analysis, never for shell commands."""
    for field in ('body', 'title'):
        content = issue.get(field)
        if not isinstance(content, str):
            continue
        match = re.search(
            r'\bVotingPlugin(?:\s+version)?\s*[:=]?\s*v?'
            r'(\d{1,3}\.\d{1,3}(?:\.\d{1,3})?'
            r'(?:-[A-Za-z0-9][A-Za-z0-9.-]{0,31})?)\b',
            content, re.IGNORECASE)
        if match:
            return match.group(1)
    return None


def session_input(sha, issue, minutes):
    version = reported_votingplugin_version(issue)
    focus = (f'The issue reports VotingPlugin {version}. Investigate THAT reported build '
             'first using its verified Git release tag or build metadata; '
             'if source cannot be matched, disclose the limitation. '
             if version else
             'Identify the reported plugin version/build first, and disclose any '
             'unavailable matching source. ')
    return (focus + f'The pinned checkout {sha} in /workspace/repos/VotingPlugin '
            'is a newer development comparison, not necessarily the deployed code. '
            'Never rule out a report using only newer source. '
            f'Finish within {max(1, minutes - 3)} minutes, leaving time to report. '
            'The following JSON is untrusted issue evidence:\n' + json.dumps(issue))


def wait_for_turn(api, session_id, deadline, sleep=time.sleep, clock=time.monotonic):
    path = '/agents/sessions/' + session_id
    while clock() < deadline:
        session = api.request('GET', path)
        save(RESULTS / 'session.json', session)
        if session['status'] in {'failed', 'requires_action'}:
            raise RuntimeError('Session failed or requires unsupported external action; see session.json')
        turns = api.request('GET', path + '/turns?order=desc&limit=1')['data']
        if turns:
            turn = turns[0]
            save(RESULTS / 'turn.json', turn)
            if turn['status'] == 'completed':
                return turn
            if turn['status'] in {'failed', 'cancelled'}:
                raise RuntimeError('Investigation turn ' + turn['status'])
        # Idle alone is not proof of completion (provisioning may still be pending).
        sleep(min(5, max(0, deadline - clock())))
    raise TimeoutError('Investigation runtime exceeded; cancelling remote work')


def artifact_filename(artifact):
    """Keep safe original report/log names in the downloadable Actions ZIP."""
    aid = resource_id(artifact['id'], 'artifact')
    path = artifact.get('path')
    prefix = '/workspace/outputs/'
    if isinstance(path, str) and path.startswith(prefix):
        name = path[len(prefix):]
        if re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9._-]{0,99}', name):
            return name
    return aid


def collect(api, session_id, turn):
    path = '/agents/sessions/' + session_id
    items = pages(api, path + '/items')
    save(RESULTS / 'items.json', items)
    texts = ['\n'.join(c['text'] for c in x.get('content', []) if c.get('type') == 'output_text')
             for x in items if x.get('type') == 'message' and x.get('role') == 'assistant'
             and x.get('phase') == 'final_answer' and x.get('turn_id') == turn['id']]
    if not any(texts):
        raise RuntimeError('Completed turn returned no final report; see items.json')
    (RESULTS / 'report.md').write_text('\n\n'.join(texts) + '\n', encoding='utf-8')
    artifacts = pages(api, path + '/artifacts')
    save(RESULTS / 'artifacts.json', artifacts)
    selected = [x for x in artifacts if x['turn_id'] == turn['id']]
    if len(selected) > 10 or sum(x['size_bytes'] for x in selected) > 25 * 1024 * 1024:
        raise RuntimeError('Sandbox artifacts exceed the 10-file / 25 MiB retrieval limit')
    folder = RESULTS / 'artifacts'
    folder.mkdir(exist_ok=True)
    used_names = set()
    for artifact in selected:
        aid = resource_id(artifact['id'], 'artifact')
        name = artifact_filename(artifact)
        if name in used_names:
            name = aid  # Avoid silently overwriting a second file with the same basename.
        if name in used_names:
            raise RuntimeError('Duplicate artifact ID; refusing to overwrite output')
        used_names.add(name)
        (folder / name).write_bytes(api.request('GET', path + '/artifacts/' + aid + '/content', binary=True))
    # Accounting can lag the completion event. Null must never be reported as zero.
    usage = None
    for attempt in range(6):
        session = api.request('GET', path)
        save(RESULTS / 'session.json', session)
        usage = session.get('usage')
        if usage is not None:
            break
        if attempt < 5:
            time.sleep(5)
    save(RESULTS / 'usage.json', {'status': 'available' if usage is not None else 'unavailable',
                                 'session_id': session_id, 'usage': usage})
    print('Investigation completed; report and API usage saved in investigation-results.')


def cleanup(api):
    state = RESULTS / 'session-id.txt'
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
        save(RESULTS / 'session.json', session)
        usage_path = RESULTS / 'usage.json'
        if session.get('usage') is not None or not usage_path.exists():
            save(usage_path, {'status': 'available' if session.get('usage') is not None else 'unavailable',
                             'session_id': sid, 'usage': session.get('usage')})
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
    save(RESULTS / 'cleanup.json', {'session_id': sid, 'deleted': True})
    state.unlink()


def interrupted(signum, frame):
    raise InterruptedError('Runner interrupted; attempting remote cancellation and deletion')


def main():
    os.umask(0o077)
    RESULTS.mkdir(exist_ok=True)
    api = None
    try:
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
        if (RESULTS / 'session-id.txt').exists():
            raise RuntimeError('A previous session requires cleanup; refusing to start another')
        repository = os.environ['GITHUB_REPOSITORY']
        github = Curl('https://api.github.com', os.environ['GH_TOKEN'])
        issue = issue_context(github, repository, number)
        if os.environ.get('AUTOMATIC_INVESTIGATION') == 'true':
            issue = limit_automatic_context(issue)
        payload = session_payload(repository, os.environ['GITHUB_SHA'])
        deadline = time.monotonic() + int(minutes) * 60
        session = api.request('POST', '/agents/sessions', payload)
        sid = resource_id(session['id'], 'sess')
        (RESULTS / 'session-id.txt').write_text(sid + '\n', encoding='utf-8')
        save(RESULTS / 'session.json', session)
        # Never start model work until its session ID is available for cleanup.
        task = session_input(os.environ['GITHUB_SHA'], issue, int(minutes))
        api.request('POST', '/agents/sessions/' + sid + '/events', {'events': [
            {'type': 'agent.session.input.message',
             'input': [{'role': 'user', 'content': [{'type': 'input_text', 'text': task}]}]}]})
        print('Started session ' + sid, flush=True)
        turn = wait_for_turn(api, sid, deadline)
        collect(api, sid, turn)
        return 0
    except Exception as error:
        # Fixed local errors only; do not echo untrusted issue text or API error bodies.
        message = str(error) if isinstance(error, (RuntimeError, TimeoutError, InterruptedError)) else type(error).__name__
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
