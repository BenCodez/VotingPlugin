import json
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from auto_gate import eligible
from engine import PROFILES, evidence, evidence_revision, profile_for
from offline import persist
from post_comment import report_fingerprint, report_links
from run import issue_context, limit_automatic_context


REPO = 'BenCodez/VotingPlugin'
VP = profile_for(REPO)
SHA = 'a' * 40


class EngineTests(unittest.TestCase):
    def test_issue_modes(self):
        for issue, mode in (
            ({'labels': ['Possible Feature'], 'body': 'Describe the bug'}, 'feature'),
            ({'type': 'Bug'}, 'bug'),
            ({'comments': [{'association': 'OWNER', 'body': 'Where did you set the command?'}]}, 'support'),
            ({'title': 'Documentation typo'}, 'docs'),
            ({'title': 'It happened'}, 'insufficient'),
        ):
            with self.subTest(mode=mode):
                self.assertEqual(evidence(issue, VP, SHA)['mode'], mode)
        self.assertEqual(evidence({}, PROFILES['Wiki'], SHA)['mode'], 'docs')

    def test_reported_tag_retains_reported_identity_and_comparison(self):
        def resolve(tag):
            return 'b' * 40 if tag == '7.1.1' else None

        result = evidence({'labels': ['Bug'], 'body': 'VotingPlugin 7.1.1; AdvancedCore 3.8.2-SNAPSHOT'},
                          VP, SHA, resolve)
        self.assertEqual(result['reported']['commit'], 'b' * 40)
        self.assertEqual(result['development_sha'], SHA)
        self.assertIsNone(result['reported']['artifact_sha256'])
        self.assertEqual(result['dependencies']['AdvancedCore']['status'], 'unverified')

    def test_snapshot_is_never_resolved_to_base_tag(self):
        def unexpected_lookup(tag):
            self.fail('Custom build must not consult a base release tag')

        result = evidence({'body': 'VotingPlugin 7.1.1-custom.42.'}, VP, SHA, unexpected_lookup)
        self.assertEqual(result['reported']['status'], 'unverified')
        self.assertEqual(result['reported']['reported_version'], '7.1.1-custom.42')
        self.assertIsNone(result['reported']['commit'])

    def test_late_explicit_reporter_correction_and_replay(self):
        issue = {'url': 'fixture://late-correction', 'author': 'reporter', 'body': 'VotingPlugin 7.1.0',
                 'comments': [{'id': number, 'body': f'context {number}'} for number in range(1, 24)]}
        before = evidence(issue, VP, SHA)
        issue['comments'].append({'id': 24, 'url': 'fixture://comment/24', 'user': 'reporter',
                                  'body': 'Version correction: VotingPlugin 7.1.1'})
        revised = evidence(issue, VP, SHA)
        bounded = limit_automatic_context(issue)
        self.assertEqual(revised['reported']['reported_version'], '7.1.1')
        self.assertEqual(len(revised['reported']['mentions']), 2)
        self.assertNotEqual(before['revision'], revised['revision'])
        self.assertEqual(bounded['comments'][0]['id'], 1)
        self.assertEqual(bounded['comments'][-1]['id'], 24)
        self.assertTrue(bounded['context_truncated'])
        with tempfile.TemporaryDirectory() as work:
            ledger = Path(work) / 'evidence.sqlite3'
            self.assertTrue(persist(ledger, before))
            self.assertTrue(persist(ledger, revised))
            self.assertFalse(persist(ledger, revised))

    def test_bot_marker_and_reopen_are_independent_of_untrusted_input(self):
        class Github:
            def request(self, method, path):
                if '/comments?' in path:
                    return [{'id': 10, 'user': {'login': 'github-actions[bot]'},
                             'body': '<!-- votingplugin-issue-investigation:10 -->\nBot text'},
                            {'id': 11, 'user': {'login': 'human'},
                             'body': '<!-- votingplugin-issue-investigation:12 -->\nHuman text'}]
                if '/events?' in path:
                    return [{'event': 'reopened', 'id': 30}]
                return {'title': 'Issue'}

        latest = issue_context(Github(), REPO, '1', track_reopens=True)
        self.assertEqual(latest['report_markers'], [('10',)])
        self.assertEqual([row['id'] for row in latest['comments']], [11])
        self.assertNotEqual(evidence_revision(latest), evidence_revision({**latest, 'reopened': None}))

    def test_comment_gate_rejects_pr_bots_and_other_owner_without_http(self):
        class Github:
            def request(self, method, path):
                raise AssertionError('Ineligible event must skip before network')

        event = {'action': 'created', 'issue': {'number': 9, 'user': {'login': 'reporter'}},
                 'comment': {'user': {'login': 'github-actions[bot]'}, 'body': 'version corrected',
                             'author_association': 'MEMBER'}}
        for changes in ({}, {'owner': 'cezar'}, {'updates': False}):
            with self.subTest(changes=changes):
                options = {'event_name': 'issue_comment', 'updates': True, **changes}
                self.assertFalse(eligible(event, Github(), REPO, 'env', **options)[0])
        event['issue']['pull_request'] = {}
        event['comment']['user']['login'] = 'reporter'
        self.assertFalse(eligible(event, Github(), REPO, 'env', event_name='issue_comment', updates=True)[0])

    def test_incomplete_report_and_equivalent_reports(self):
        report = ('**Finding**: Source evidence suggests this was addressed in development.\n'
                  '**Evidence**: revision-pinned source has a new branch.\n'
                  '**Verification**: source inspection only.\n**Suggested next step**: retest on staging.\n'
                  '**Limitations**: runtime and exact fixed build are unconfirmed.\n')
        self.assertEqual(report_fingerprint(report), report_fingerprint(report.replace('source inspection',
                                                                                      'SOURCE  inspection')))
        with self.assertRaisesRegex(RuntimeError, 'required evidence sections'):
            report_fingerprint(report.rsplit('**Limitations**', 1)[0])

    def test_profile_links_and_offline_only_policy(self):
        for checkout, profile in PROFILES.items():
            with self.subTest(checkout=checkout):
                self.assertEqual(profile['checkout'], checkout)
                self.assertEqual(profile['hosted'], checkout == 'VotingPlugin')
                self.assertEqual(profile['post'], checkout == 'VotingPlugin')
        report = '[Source](/workspace/repos/AdvancedCore/AdvancedCore/src/Foo.java:42)'
        rewritten = report_links(report, 'BenCodez/AdvancedCore', SHA, 'unused')
        self.assertIn(f'/AdvancedCore/blob/{SHA}/AdvancedCore/src/Foo.java#L42', rewritten)

    def test_untrusted_secrets_and_whitespace_sensitive_evidence(self):
        issue = {'body': 'Authorization: fictitious-token\nparent:\n  child: value'}
        self.assertNotIn('fictitious-token', json.dumps(limit_automatic_context(issue)))
        self.assertNotEqual(evidence_revision(issue),
                            evidence_revision({'body': issue['body'].replace('  child', 'child')}))


if __name__ == '__main__':
    unittest.main()
