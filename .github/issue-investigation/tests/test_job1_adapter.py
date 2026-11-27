from concurrent.futures import ThreadPoolExecutor
import copy
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import unittest

from claims import Claims
from engine import evidence_revision, github_snapshot, has_accepted_revision
from job1_adapter import admit
from run import issue_context

REPO = 'BenCodez/VotingPlugin'
ROOT = Path(__file__).resolve().parents[1]


def fixture():
    return {'issue': {'number': 42, 'html_url': f'https://github.com/{REPO}/issues/42',
                      'title': 'Feature request', 'body': 'Add a count', 'state': 'open'},
            'comments': [{'id': 10, 'user': {'login': 'reporter'}, 'body': 'History needed'}],
            'events': []}


class OfflineIngressTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.folder = Path(directory.name)
        self.ledger = Claims(self.folder / 'claims.sqlite3')

    def test_two_ingresses_share_one_owner_and_one_concurrent_claim(self):
        for number, owner in ((42, 'cezar'), (43, 'github')):
            with self.subTest(owner=owner), ThreadPoolExecutor(max_workers=4) as pool:
                snapshot = fixture()
                snapshot['issue'].update(number=number, html_url=f'https://github.com/{REPO}/issues/{number}')
                wrong = 'github' if owner == 'cezar' else 'cezar'
                unused = Claims(self.folder / f'nonowner-{number}.sqlite3')
                self.assertEqual(admit(snapshot, REPO, number, owner, wrong, unused)['status'], 'skipped')
                self.assertFalse(unused.path.exists())
                barrier = threading.Barrier(4)

                def delivery(ingress):
                    barrier.wait(timeout=5)
                    return admit(snapshot, REPO.lower(), number, owner, ingress,
                                 Claims(self.ledger.path))

                results = list(pool.map(delivery, ('cezar', 'github', 'cezar', 'github')))
                self.assertEqual(len({item['revision'] for item in results}), 1)
                (winner, claimed), = [(i, item) for i, item in enumerate(results)
                                      if item['status'] == 'claimed']
                self.assertEqual(winner % 2, 0 if owner == 'cezar' else 1)
                self.assertEqual(claimed['owner'], owner)
                wrong_ingress = 'github' if owner == 'cezar' else 'cezar'
                disagreed = admit(snapshot, REPO, number, wrong_ingress, wrong_ingress, self.ledger)
                self.assertEqual(disagreed['status'], 'skipped')

    def test_snapshot_parity_bot_filter_late_correction_and_replay(self):
        initial = fixture()
        first = admit(initial, REPO, 42, 'cezar', 'cezar', self.ledger)
        self.assertEqual(first['status'], 'claimed')
        updated = copy.deepcopy(initial)
        updated['comments'].append({'id': 12, 'user': {'login': 'github-actions[bot]'},
                                    'body': '<!-- votingplugin-issue-investigation:26 -->\nDraft'})
        bot_replay = admit(updated, REPO, 42, 'cezar', 'cezar', self.ledger)
        self.assertEqual(bot_replay['revision'], first['revision'])
        self.assertEqual(bot_replay['status'], 'skipped')
        updated['comments'].insert(0, {'id': 15, 'user': {'login': 'maintainer'},
                                       'author_association': 'OWNER', 'body': 'Correction: latest version'})
        result = admit(updated, REPO, 42, 'github', 'github', self.ledger)
        self.assertNotEqual(result['revision'], first['revision'])
        self.assertEqual(result['status'], 'claimed')

        class MockGitHub:
            def request(self, method, url):
                return updated['comments'] if '/comments?' in url else (
                    updated['events'] if '/events?' in url else updated['issue'])

        hosted = issue_context(MockGitHub(), REPO, '42', track_reopens=True)
        self.assertEqual(result['revision'], evidence_revision(hosted))
        self.assertEqual([row['id'] for row in hosted['comments']], [10, 15])
        self.assertEqual(hosted, github_snapshot(updated['issue'], reversed(updated['comments']), []))
        self.assertEqual(admit(updated, REPO, 42, 'github', 'github', self.ledger)['status'], 'skipped')
        self.assertEqual(admit(initial, REPO, 42, 'cezar', 'cezar', self.ledger)['status'], 'skipped')

    def test_accepted_marker_on_fresh_ledger_and_real_correction(self):
        initial = fixture()
        revision = evidence_revision(github_snapshot(initial['issue'], initial['comments'], []))
        marker = f'<!-- issue-investigation:v1:1:{revision}:' + 'a' * 64 + ' -->'
        initial['comments'].append({'id': 12, 'user': {'login': 'github-actions[bot]'}, 'body': marker + '\n'})
        for owner in ('github', 'cezar'):
            with self.subTest(owner=owner):
                result = admit(initial, REPO, 42, owner, owner, self.ledger)
                self.assertEqual(result['revision'], revision)
                self.assertEqual(result['status'], 'skipped')
        self.assertFalse(self.ledger.path.exists())

        corrected = copy.deepcopy(initial)
        corrected['comments'].append({'id': 13, 'user': {'login': 'maintainer'},
                                      'author_association': 'OWNER', 'body': 'Correction: version 6.19'})
        result = admit(corrected, REPO, 42, 'cezar', 'cezar', self.ledger)
        self.assertNotEqual(result['revision'], revision)
        self.assertEqual(result['status'], 'claimed')

    def test_stale_legacy_and_other_bot_markers_do_not_suppress(self):
        snapshot = fixture()
        revision = evidence_revision(github_snapshot(snapshot['issue'], snapshot['comments'], []))
        for i, (login, marker) in enumerate((
                ('github-actions[bot]', '<!-- votingplugin-issue-investigation:26 -->'),
                ('github-actions[bot]', '<!-- issue-investigation:v1:2:' + 'b' * 64 + ':' + 'a' * 64 + ' -->'),
                ('unrelated[bot]', f'<!-- issue-investigation:v1:3:{revision}:' + 'a' * 64 + ' -->'))):
            snapshot['comments'].append({'id': 20 + i, 'user': {'login': login}, 'body': marker + '\n'})
        normalized = github_snapshot(snapshot['issue'], snapshot['comments'], [])
        self.assertEqual(evidence_revision(normalized), revision)
        self.assertFalse(has_accepted_revision(normalized, revision))
        self.assertEqual(admit(snapshot, REPO, 42, 'cezar', 'cezar', self.ledger)['status'], 'claimed')

    def test_draft_and_storage_receipts_are_attempt_fenced_and_idempotent(self):
        result = admit(fixture(), REPO, 42, 'cezar', 'cezar', self.ledger)
        claim = result['claim']
        draft = hashlib.sha256(b'# Draft').hexdigest()
        self.assertFalse(self.ledger.record_artifact(claim, draft, 'artifact-1'))
        self.assertTrue(self.ledger.settle(claim, draft))
        self.assertFalse(self.ledger.record_artifact(claim, 'b' * 64, 'artifact-1'))
        self.assertTrue(self.ledger.record_artifact(claim, draft, 'artifact-1'))
        self.assertTrue(self.ledger.record_artifact(claim, draft, 'artifact-1'))
        self.assertFalse(self.ledger.record_artifact(claim, draft, 'artifact-2'))
        route = {'repository': REPO, 'number': '42', 'revision': result['revision'], 'owner': 'cezar'}
        retry = self.ledger.claim_ingress(route, 'cezar', force_completed=True)
        self.assertFalse(self.ledger.record_artifact(claim, draft, 'artifact-1'))
        self.assertFalse(self.ledger.record_artifact(retry, draft, 'artifact-2'))
        self.assertTrue(self.ledger.settle(retry, draft))
        self.assertTrue(self.ledger.record_artifact(retry, draft, 'artifact-2'))

    def test_cli_replay_and_reject_mismatched_repository(self):
        snapshot = self.folder / 'snapshot.json'
        snapshot.write_text(json.dumps(fixture()))
        argv = [sys.executable, str(ROOT / 'job1_adapter.py'), '--snapshot', str(snapshot),
                '--repo', REPO, '--issue', '42', '--owner', 'cezar', '--ingress', 'cezar',
                '--ledger', str(self.ledger.path)]
        for expected in ('claimed', 'skipped'):
            result = subprocess.run(argv, text=True, capture_output=True, check=True, timeout=10)
            self.assertEqual(json.loads(result.stdout)['status'], expected)
        with self.assertRaisesRegex(ValueError, 'Snapshot URL'):
            admit(fixture(), 'BenCodez/AdvancedCore', 42, 'cezar', 'cezar', self.ledger)
