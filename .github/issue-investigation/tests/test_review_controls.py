import concurrent.futures
import hashlib
from io import BytesIO
import json
import os
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from claims import Claims
from disclosure import install_checker, payload_bytes, require_disclosure, require_substantive_approval
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
                with self.assertRaisesRegex(RuntimeError, 'refused'):
                    require_substantive_approval(body, destination)
                receipt['verdict'] = 'approve-substantive-report'
                result.stdout = json.dumps(receipt).encode()
                require_substantive_approval(body, destination)
                with self.assertRaisesRegex(RuntimeError, 'refused'):
                    require_substantive_approval(body + b' ', destination)
                with self.assertRaisesRegex(RuntimeError, 'refused'):
                    require_substantive_approval(body, destination.replace('#1', '#2'))
                checker.write_bytes(b'changed')
                with self.assertRaisesRegex(RuntimeError, 'digest'):
                    require_disclosure(body, destination)

    def test_hosted_policy_install_rejects_unverified_bytes(self):
        data = b'example executable'
        with tempfile.TemporaryDirectory() as temp:
            checker = Path(temp) / 'checker'
            env = {'ISSUE_INVESTIGATION_POLICY_URL': 'https://example.com/policy',
                   'ISSUE_INVESTIGATION_DISCLOSURE_CHECKER': str(checker),
                   'ISSUE_INVESTIGATION_DISCLOSURE_SHA256': hashlib.sha256(data).hexdigest()}
            with patch.dict(os.environ, env, clear=True), patch('disclosure.build_opener') as opener:
                opener.return_value.open.return_value = BytesIO(b'different payload')
                with self.assertRaisesRegex(RuntimeError, 'digest'):
                    install_checker()
                self.assertFalse(checker.exists())
                opener.return_value.open.return_value = BytesIO(data)
                install_checker()
                self.assertEqual(checker.read_bytes(), data)
                self.assertEqual(checker.stat().st_mode & 0o777, 0o700)

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
        comments[6].update(body=('intro\n' * 400) + 'Version correction: VotingPlugin 7.2.1\n' + ('later\n' * 400) + 'Actual result: still open\n')
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

    def test_offline_rest_snapshot_preserves_labels_identity_and_correction(self):
        import offline
        profile = profile_for('BenCodez/VotingPlugin')
        issue = {'title': 'Problem', 'body': 'VotingPlugin 7.1.0',
                 'labels': [{'name': 'bug'}], 'comments': 1,
                 'html_url': 'https://github.com/BenCodez/VotingPlugin/issues/1'}
        snapshot = {'issue': issue, 'events': [], 'comments': [
            {'id': 1, 'user': {'login': 'maintainer'}, 'author_association': 'OWNER',
             'body': 'Version correction: VotingPlugin 7.2.1'}]}
        with patch.object(offline, 'tag_sha', return_value=None):
            result = offline.analyze(snapshot, profile, Path('.'), 'a' * 40, [], {})
            self.assertEqual(result['mode'], 'bug')
            self.assertEqual(result['issue'], issue['html_url'])
            self.assertEqual(result['reported']['reported_version'], '7.2.1')
            with self.assertRaisesRegex(RuntimeError, 'full comment'):
                offline.analyze(issue, profile, Path('.'), 'a' * 40, [], {})
            with self.assertRaisesRegex(RuntimeError, 'full comment'):
                offline.analyze({**snapshot, 'issue': {**issue, 'comments': 2}},
                                profile, Path('.'), 'a' * 40, [], {})
            bare = offline.analyze({**issue, 'comments': 0}, profile, Path('.'), 'a' * 40, [], {})
            self.assertEqual(bare['issue'], issue['html_url'])

    def test_markdown_heading_and_citation_must_be_structural(self):
        meta = {'repository': 'BenCodez/VotingPlugin', 'number': '1',
                'development_sha': 'a' * 40, 'mode': 'insufficient'}
        url = 'https://github.com/BenCodez/VotingPlugin/issues/1'
        template = ('## Finding\nReviewed the reported behavior.\n'
                    '**Evidence**: Consulted the project record {ref}.\n'
                    '**Verification**: Read the supplied records.\n'
                    '**Suggested next step**: Obtain the installed version.\n'
                    '**Limitations**: Runtime evidence is missing.')
        valid = template.format(ref=f'[issue]({url})')
        self.assertTrue(report_completion(valid, meta)['issue_cited'])
        with self.assertRaisesRegex(RuntimeError, 'required evidence sections'):
            report_completion(valid.replace('## Finding', '## Findingstone'), meta)
        self.assertTrue(report_completion(template.format(ref=f'\n\n   [issue]({url})\n'),
                                          meta)['issue_cited'])
        for reference in (url, f'`[issue]({url})`', f'\n~~~\n[issue]({url})\n~~~\n',
                          f'\n\n    [issue]({url})\n', f'\n\n\t[issue]({url})\n'):
            with self.assertRaisesRegex(RuntimeError, 'lacks corroborated'):
                report_completion(template.format(ref=reference), meta)

    def test_fenced_evidence_requires_a_matching_closer(self):
        meta = {'repository': 'BenCodez/VotingPlugin', 'number': '1',
                'development_sha': 'a' * 40, 'mode': 'insufficient'}
        link = '[issue](https://github.com/BenCodez/VotingPlugin/issues/1)'
        report = ('**Finding**: Reviewed the reported behavior.\n'
                  '**Evidence**: Consulted the supplied evidence.\n'
                  '{opener}\nexample\n{closer}\n{link}\n'
                  '**Verification**: Read the supplied records.\n'
                  '**Suggested next step**: Obtain the installed version.\n'
                  '**Limitations**: Runtime evidence is missing.')
        for opener, closer in [('~~~', '~~~`'), ('```', '```~'), ('~~~~', '~~~'), ('~~~', '```')]:
            with self.subTest(opener=opener, closer=closer):
                with self.assertRaisesRegex(RuntimeError, 'lacks corroborated'):
                    report_completion(report.format(opener=opener, closer=closer, link=link), meta)
        for opener, closer in [('~~~', '  ~~~~ \t'), ('```', '````')]:
            with self.subTest(opener=opener, closer=closer):
                self.assertTrue(report_completion(
                    report.format(opener=opener, closer=closer, link=link), meta)['issue_cited'])

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
