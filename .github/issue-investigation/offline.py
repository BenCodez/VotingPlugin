#!/usr/bin/env python3
"""Offline evidence collection from an issue fixture and local Git objects."""
import argparse
from contextlib import closing
import json
from pathlib import Path
import re
import sqlite3
import subprocess
from urllib.parse import quote

from engine import PROFILES, SHA, digest, evidence, github_snapshot, is_bot, profile_for, version_evidence

PILOT_PROFILES = frozenset({'VotingPlugin', 'AdvancedCore', 'SimpleAPI'})


def git(checkout, *args):
    result = subprocess.run(['git', '-C', str(checkout), *args], capture_output=True, timeout=15)
    return result.stdout if result.returncode == 0 else None


def checkout_commit(checkout, profile, ref):
    expected = profile['clone_url']
    remote_url = git(checkout, 'remote', 'get-url', 'origin')
    if remote_url is None:
        raise RuntimeError('Missing origin for the evidence checkout')
    origin = remote_url.decode().strip()
    slug = expected.removeprefix('https://github.com/').removesuffix('.git')
    accepted = {f'{prefix}{slug}{suffix}' for prefix in
                ('https://github.com/', 'git@github.com:', 'ssh://git@github.com/')
                for suffix in ('.git', '')}
    if origin not in accepted:
        raise RuntimeError('Checkout origin does not match the selected profile')
    value = git(checkout, 'rev-parse', '--verify', '--end-of-options', ref + '^{commit}')
    if not value or not SHA.fullmatch(value.decode().strip()):
        raise RuntimeError('Comparison revision is unavailable in the evidence checkout')
    return value.decode().strip()


def tag_sha(checkout, tag):
    value = git(checkout, 'rev-parse', '--verify', '--end-of-options', f'refs/tags/{tag}^{{commit}}')
    return value.decode().strip() if value else None


def source_record(checkout, profile, sha, path, lane):
    if not re.fullmatch(r'[A-Za-z0-9_./ -]+', path) or any(
            part in {'', '.', '..'} for part in path.split('/')):
        raise RuntimeError('Invalid source path')
    argv = ['git', '-C', str(checkout), 'show', f'{sha}:{path}']
    blob = git(checkout, *argv[3:])
    if blob is None:
        return {'lane': lane, 'sha': sha, 'path': path, 'url': None,
                'command': argv, 'outcome': 'unavailable'}
    home = 'https://github.com/' + profile['repository']
    url = home + '/blob/' + sha + '/' + quote(path)
    return {'lane': lane, 'sha': sha, 'path': path, 'url': url,
            'command': argv, 'outcome': 'source-read', 'bytes': len(blob)}


def analyze(issue, profile, checkout, sha, source_paths, dependencies):
    if 'issue' in issue:
        record, comments = issue['issue'], issue['comments']
        count = record.get('comments')
        if isinstance(count, int) and count > len(comments):
            raise RuntimeError('Use a snapshot with full comment and event rows')
        issue = github_snapshot(record, comments, issue['events'])
    elif isinstance(issue.get('comments'), int) or 'html_url' in issue:
        if issue.get('comments', 0) != 0:
            raise RuntimeError('Use a snapshot with full comment and event rows')
        issue = github_snapshot(issue, (), ())
    issue = dict(issue)
    if 'pull_request' in issue:
        raise RuntimeError('Choose an issue, not a pull request')
    issue['comments'] = [
        row for row in issue.get('comments', [])
        if not is_bot(row['user'] if isinstance(row.get('user'), dict)
                      else {'login': row.get('user') or ''})
    ]
    result = evidence(issue, profile, sha, lambda tag: tag_sha(checkout, tag))
    for name, dependency_checkout in dependencies.items():
        if name not in profile['related']:
            raise RuntimeError('Dependency is outside the selected profile')
        peer = PROFILES[name]
        checkout_commit(dependency_checkout, peer, 'HEAD')
        result['dependencies'][name] = version_evidence(
            issue, name, lambda tag: tag_sha(dependency_checkout, tag))
    revisions = [('development', sha)]
    if result['reported']['commit']:
        revisions.insert(0, ('reported', result['reported']['commit']))
    for lane, revision in revisions:
        for path in source_paths:
            observation = source_record(checkout, profile, revision, path, lane)
            result['commands'].append({'argv': observation['command'], 'result': observation['outcome']})
            result['source_references'].append(observation)
    result['limits'] = ['No application build, runtime verification, or model calls were performed.',
                        'Local release tags identify source only. The deployed artifact was not verified.']
    return result


def persist(path, result):
    key = digest({k: result[k] for k in ('repository', 'profile', 'issue', 'revision', 'development_sha')})
    payload = json.dumps(result, sort_keys=True)
    with closing(sqlite3.connect(path)) as db, db:
        db.execute('CREATE TABLE IF NOT EXISTS evidence (key TEXT PRIMARY KEY, payload TEXT NOT NULL)')
        cursor = db.execute(
            'INSERT INTO evidence VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET payload=excluded.payload '
            'WHERE evidence.payload <> excluded.payload', (key, payload))
        return cursor.rowcount == 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--profile', required=True, choices=PROFILES)
    parser.add_argument('--issue', required=True, type=Path)
    parser.add_argument('--checkout', required=True, type=Path)
    parser.add_argument('--sha', required=True)
    parser.add_argument('--source', action='append', default=[])
    parser.add_argument('--dependency', action='append', default=[], metavar='NAME=CHECKOUT')
    parser.add_argument('--ledger', type=Path)
    parser.add_argument('--validate-profile', action='store_true',
                        help='Use a synthetic fixture to check a non-pilot profile')
    args = parser.parse_args()
    if args.validate_profile == (args.profile in PILOT_PROFILES):
        parser.error('Non-pilot profiles require --validate-profile; pilot profiles use normal offline mode')
    profile = profile_for(PROFILES[args.profile]['repository'], args.profile)
    if not SHA.fullmatch(args.sha) or checkout_commit(args.checkout, profile, args.sha) != args.sha:
        parser.error('--sha must be a full SHA in the profile checkout')
    fixture = json.loads(args.issue.read_text())
    dependencies = dict(value.split('=', 1) for value in args.dependency)
    result = analyze(fixture, profile, args.checkout, args.sha, args.source, dependencies)
    result['scope'] = 'validation-only' if args.validate_profile else 'offline-pilot'
    if args.ledger:
        result['replay'] = not persist(args.ledger, result)
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
