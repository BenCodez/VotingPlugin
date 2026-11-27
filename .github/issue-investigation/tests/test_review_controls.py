import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from claims import Claims
from disclosure import payload_bytes, require_disclosure
from engine import evidence, profile_for, route_revision
from report import report_completion, report_fingerprint
from run import limit_automatic_context


class ReviewControls(unittest.TestCase):
    def test_checker_requires_pinned_executable_and_exact_destination_and_bytes(self):
        body = payload_bytes({'body': 'Approved example text'})
        destination = 'github-comment:BenCodez/VotingPlugin#1'
        with patch.dict(os.environ, {}, clear=True), self.assertRaisesRegex(RuntimeError, 'unavailable'):
            require_disclosure(body, destination)
        with tempfile.TemporaryDirectory() as temp:
            checker = Path(temp) / 'checker'
            checker.write_bytes(b'fixture mock')
            env = {'ISSUE_INVESTIGATION_DISCLOSURE_CHECKER': str(checker),
                   'ISSUE_INVESTIGATION_DISCLOSURE_SHA256': hashlib.sha256(checker.read_bytes()).hexdigest(),
                   'OPENAI_API_KEY': 'fictional', 'GH_TOKEN': 'fictional'}
            receipt = {'verdict': 'allow', 'destination': destination,
                       'sha256': hashlib.sha256(body).hexdigest()}
            result = SimpleNamespace(returncode=0, stdout=json.dumps(receipt).encode())
            with patch.dict(os.environ, env, clear=True), patch('disclosure.subprocess.run', return_value=result) as run:
                require_disclosure(body, destination)
                self.assertEqual(run.call_args.kwargs['input'], body)
                self.assertNotIn('OPENAI_API_KEY', run.call_args.kwargs['env'])
                self.assertNotIn('GH_TOKEN', run.call_args.kwargs['env'])
                with self.assertRaisesRegex(RuntimeError, 'refused'):
                    require_disclosure(body + b' ', destination)
                with self.assertRaisesRegex(RuntimeError, 'refused'):
                    require_disclosure(body, destination.replace('#1', '#2'))
                checker.write_bytes(b'changed')
                with self.assertRaisesRegex(RuntimeError, 'digest'):
                    require_disclosure(body, destination)

    def test_parallel_claim_and_receipt_block_no_post_replay_and_wrong_owner(self):
        route = route_revision('BenCodez/VotingPlugin', '1', 'a' * 64, 'github')
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / 'claim.sqlite3'
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
                attempts = list(pool.map(lambda _: Claims(path).claim(route), range(2)))
            accepted, = [claim for claim in attempts if claim]
            self.assertIsNone(Claims(path).claim({**route, 'owner': 'cezar'}, retry_failed=True))
            self.assertIsNone(Claims(path).claim(route, retry_failed=True))
            self.assertTrue(Claims(path).settle(accepted))
            self.assertIsNone(Claims(path).claim(route))
            retry = Claims(path).claim(route, retry_failed=True)
            self.assertEqual(retry[1], accepted[1] + 1)
            self.assertFalse(Claims(path).settle(accepted, 'older attempt'))
            self.assertTrue(Claims(path).settle(retry, 'artifact-only receipt'))
            self.assertIsNone(Claims(path).claim(route, retry_failed=True))

    def test_force_consumes_only_one_completed_claim_and_fences_old_attempt(self):
        route = route_revision('BenCodez/VotingPlugin', '4', 'b' * 64, 'github')
        with tempfile.TemporaryDirectory() as temp:
            ledger = Claims(Path(temp) / 'claims.db')
            initial = ledger.claim(route)
            self.assertIsNone(ledger.claim(route, force_completed=True))
            self.assertTrue(ledger.settle(initial, 'initial receipt'))
            self.assertIsNone(ledger.claim({**route, 'owner': 'cezar'}, force_completed=True))
            rerun = ledger.claim(route, force_completed=True)
            self.assertEqual(rerun[1], initial[1] + 1)
            self.assertIsNone(ledger.claim(route, retry_failed=True, force_completed=True))
            self.assertFalse(ledger.settle(initial, 'stale receipt'))
            self.assertTrue(ledger.settle(rerun, 'new receipt'))
            self.assertIsNone(ledger.claim(route, force_completed=True))

            legacy = {**route, 'number': '5'}
            first = ledger.claim(legacy, force_completed=True)
            self.assertTrue(ledger.settle(first, 'legacy revision rerun'))
            self.assertIsNone(ledger.claim(legacy, force_completed=True))

    def test_structured_completion_per_mode_and_missing_source(self):
        sha = 'a' * 40
        provenance = {'repository': 'BenCodez/VotingPlugin', 'number': '1',
                      'development_sha': sha}
        root = 'https://github.com/BenCodez/VotingPlugin'
        template = ('**Finding**: Reviewed the reported behavior.\n'
                    '**Evidence**: Consulted [project record]({ref}).\n'
                    '**Verification**: Read only confirmed references.\n'
                    '**Suggested next step**: Obtain the installed version.\n'
                    '**Limitations**: Runtime evidence is missing.')
        for mode in ('bug', 'docs', 'feature', 'support', 'insufficient'):
            with self.subTest(mode=mode):
                source_mode = mode in {'bug', 'docs'}
                ref = f'{root}/blob/{sha}/README.md' if source_mode else f'{root}/issues/1'
                meta = {**provenance, 'mode': mode}
                completion = report_completion(template.format(ref=ref), meta,
                                               lambda commit, path: (commit, path) == (sha, 'README.md'))
                self.assertEqual(completion['mode'], mode)
                self.assertFalse(completion['runtime_verified'])
                self.assertEqual(completion['issue_cited'], not source_mode)
                with self.assertRaisesRegex(RuntimeError, 'lacks corroborated'):
                    report_completion(template.format(ref=root + '/issues/99'), meta)
        with self.assertRaisesRegex(RuntimeError, 'could not be verified'):
            report_completion(template.format(ref=f'{root}/blob/{sha}/missing.md'),
                              {**provenance, 'mode': 'bug'}, lambda commit, path: False)

    def test_authoritative_middle_correction_survives_both_bounds(self):
        comments = [{'id': i, 'url': f'comment/{i}', 'user': 'reviewer', 'association': 'OWNER',
                     'body': 'Discussion'} for i in range(30)]
        comments[6].update(body=('intro\n' * 400) + 'Version correction: VotingPlugin 7.2.1\n')
        bounded = limit_automatic_context({'comments': comments})
        selected = next(row for row in bounded['comments'] if row['id'] == 6)
        self.assertIn('Version correction: VotingPlugin 7.2.1', selected['body'])
        self.assertLess(len(selected['body']), 2050)
        self.assertLessEqual(len(bounded['comments']), 20)

    def test_placeholders_and_unvalidated_preamble_fail(self):
        sections = ('Finding', 'Evidence', 'Verification', 'Suggested next step', 'Limitations')
        stub = '\n'.join(f'**{name}**: TODO' for name in sections)
        with self.assertRaisesRegex(RuntimeError, 'incomplete'):
            report_fingerprint(stub)
        repeated = stub.replace('TODO', 'No investigation was performed.')
        with self.assertRaisesRegex(RuntimeError, 'repeats'):
            report_fingerprint(repeated)
        complete = repeated
        with self.assertRaisesRegex(RuntimeError, 'unexpected'):
            report_fingerprint('Extra commentary\n' + complete)

    def test_issue_title_does_not_appear_in_exported_search_urls(self):
        title = 'Fictional private name'
        meta = evidence({'title': title}, profile_for('BenCodez/VotingPlugin'), 'a' * 40)
        self.assertNotIn('Fictional', json.dumps(meta['resources']))
        with self.assertRaisesRegex(RuntimeError, 'explicit owner'):
            route_revision('BenCodez/VotingPlugin', '1', meta['revision'], '')

    def test_validation_only_profile_cannot_enter_pilot_mode(self):
        from unittest.mock import patch
        import offline

        argv = ['offline.py', '--profile', 'Wiki', '--issue', 'fixture.json',
                '--checkout', '.', '--sha', 'a' * 40]
        with patch.object(offline.argparse.ArgumentParser, 'error', side_effect=RuntimeError) as error, \
             patch.object(offline, 'checkout_commit') as checkout, patch.object(offline, 'profile_for'), \
             patch('sys.argv', argv), self.assertRaises(RuntimeError):
            offline.main()
        self.assertIn('--validate-profile', error.call_args.args[0])
        checkout.assert_not_called()
