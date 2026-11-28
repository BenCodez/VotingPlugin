#!/usr/bin/env python3
"""Offline proposal for Job1 admission using a complete GitHub snapshot.

A delivery is a wakeup only. The caller must obtain a fresh, fully paginated
snapshot and pass ownership from trusted configuration, never from issue text.
"""
import argparse
import json
from pathlib import Path

from claims import Claims
from engine import evidence_revision, github_snapshot, has_accepted_revision, route_revision


def admit(snapshot, repository, number, owner, ingress, ledger):
    issue = snapshot['issue']
    if str(issue['number']) != str(number):
        raise ValueError('Snapshot issue does not match requested issue')
    context = github_snapshot(issue, snapshot['comments'], snapshot['events'])
    route = route_revision(repository, number, evidence_revision(context), owner)
    expected_url = f"https://github.com/{route['repository']}/issues/{route['number']}"
    if (issue.get('html_url') or '').casefold() != expected_url.casefold():
        raise ValueError('Snapshot URL does not match requested repository and issue')
    claim = None
    if not has_accepted_revision(context, route['revision']):
        claim = ledger.claim_ingress(route, ingress)
    return {'owner': route['owner'], 'revision': route['revision'],
            'status': 'claimed' if claim else 'skipped',
            'claim': claim}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--snapshot', required=True, type=Path)
    parser.add_argument('--repo', required=True)
    parser.add_argument('--issue', required=True)
    parser.add_argument('--owner', required=True, choices=('cezar', 'github'))
    parser.add_argument('--ingress', required=True, choices=('cezar', 'github'))
    parser.add_argument('--ledger', required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(admit(json.loads(args.snapshot.read_bytes()), args.repo, args.issue,
                           args.owner, args.ingress, Claims(args.ledger))))


if __name__ == '__main__':
    main()
