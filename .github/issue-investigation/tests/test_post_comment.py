import importlib.util
import hashlib
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
with patch.object(sys, 'path', [str(ROOT), *sys.path]):
    SPEC = importlib.util.spec_from_file_location('post_comment', ROOT / 'post_comment.py')
    post = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(post)


LATEST_BUILD_URL = post.profile_for('BenCodez/VotingPlugin')['development']


class PostingTests(unittest.TestCase):
    def setUp(self):
        self.enterContext(patch.dict(os.environ, {'GITHUB_EVENT_NAME': 'workflow_dispatch'}))
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.folder = Path(self.temp.name)
        result_patch = patch.object(post, 'RESULTS', self.folder)
        result_patch.start()
        self.addCleanup(result_patch.stop)
        (self.folder / 'turn.json').write_text(json.dumps({'status': 'completed', 'digest': 'd' * 64}))
        report = ('**Finding**: no defect confirmed. Tests blocked.\n**Evidence**: compared tagged source [README](https://github.com/BenCodez/VotingPlugin/blob/' + 'a' * 40 + '/README.md).\n'
                  '**Verification**: inspected the source.\n**Suggested next step**: retest on staging.\n'
                  '**Limitations**: no runtime coverage.')
        (self.folder / 'report.md').write_text(report)
        revision = post.evidence_revision({})
        provenance = {'repository': 'BenCodez/VotingPlugin', 'number': '1583', 'mode': 'bug',
                      'development_sha': 'a' * 40, 'revision': revision, 'owner': 'github'}
        provenance['observations'] = post.report_completion(report, provenance)
        (self.folder / 'evidence.json').write_text(json.dumps(provenance))
        (self.folder / 'completed.json').write_text(json.dumps({
            'revision': revision, 'turn': 'd' * 64,
            'report_sha256': hashlib.sha256(report.encode()).hexdigest()}))
        self.calls = []
        checker = patch.object(post, 'require_disclosure')
        self.checker = checker.start()
        self.addCleanup(checker.stop)

    def request(self, method, path, payload=None):
        self.calls.append((method, path, payload))
        return [] if method == 'GET' and ('/comments?' in path or '/events?' in path) else {}

    def publish(self):
        from types import SimpleNamespace
        # Exercise the preserved transport ordering after a hypothetical trusted review.
        with patch.object(post, 'require_semantic_review'):
            post.publish(SimpleNamespace(request=self.request), 'BenCodez/VotingPlugin', '1583', 'a' * 40, '123')

    def test_default_off_never_reads_credentials_or_calls_api(self):
        for value in [None, 'false', '1', 'True']:
            with patch.dict(os.environ, {} if value is None else {'POST_COMMENT': value}, clear=True), \
                    patch.object(post, 'publish') as curl:
                self.assertEqual(post.main(), 0)
                curl.assert_not_called()

    def test_posts_actual_completed_report_to_selected_issue(self):
        self.publish()
        method, path, payload = self.calls[-1]
        self.assertEqual((method, path), ('POST', '/repos/BenCodez/VotingPlugin/issues/1583/comments'))
        self.assertTrue(payload['body'].startswith('<!-- issue-investigation:v1:123:'))
        self.assertIn('no defect confirmed. Tests blocked.', payload['body'])
        self.assertIn('/actions/runs/123', payload['body'])
        self.assertIn(LATEST_BUILD_URL, payload['body'])

    def test_link_to_latest_successful_build_is_stable_and_not_duplicated(self):
        body = post.comment_body('Reported release appears affected.', 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('[latest successful development build](' + LATEST_BUILD_URL + ')', body)
        self.assertIn('backed-up/staging server', body)
        self.assertIn('build number', body)
        existing = '[Existing latest build](' + LATEST_BUILD_URL + ')'
        body = post.comment_body(existing, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertEqual(body.count(LATEST_BUILD_URL), 1)
        self.assertIn('Optional development-build retest', body)
        self.assertIn('backed-up/staging server', body)
        self.assertIn('This is a development build, not a verified fix or stable release.', body)
        self.assertIn('build number and whether the same steps reproduce', body)
        self.assertIn('mentioned above', body)

        plain_url = 'I tested ' + LATEST_BUILD_URL
        body = post.comment_body(plain_url, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertEqual(body.count(LATEST_BUILD_URL), 1)
        self.assertIn('Optional development-build retest', body)

    def test_existing_bot_comment_prevents_duplicate_on_rerun(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [{'user': {'login': 'github-actions[bot]'},
                     'body': '<!-- votingplugin-issue-investigation:123 -->\nold report'}] if '/comments?' in path else ([] if '/events?' in path else {})
        self.request = reply
        self.publish()
        self.assertFalse(any(x[0] == 'POST' for x in self.calls))

    def test_user_copied_marker_does_not_suppress_post(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [{'user': {'login': 'reporter'},
                     'body': '<!-- votingplugin-issue-investigation:123 -->\nfake'}] if '/comments?' in path else ([] if '/events?' in path else {})
        self.request = reply
        issue = post.issue_context(self, 'BenCodez/VotingPlugin', '1583', track_reopens=True)
        revision = post.evidence_revision(issue)
        for filename in ('evidence.json', 'completed.json'):
            path = self.folder / filename
            data = json.loads(path.read_text())
            data['revision'] = revision
            path.write_text(json.dumps(data))
        self.calls.clear()
        self.publish()
        self.assertEqual(sum(x[0] == 'POST' for x in self.calls), 1)

    def test_prior_evidence_revision_blocks_cross_run_duplicate(self):
        metadata = json.loads((self.folder / 'evidence.json').read_text())
        marker = '<!-- issue-investigation:v1:122:' + metadata['revision'] + ':' + 'b' * 64 + ' -->\n'
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            if '/comments?' in path:
                return [{'user': {'login': 'github-actions[bot]'}, 'body': marker + 'Old report'}]
            return [] if '/events?' in path else {}

        self.request = reply
        self.publish()
        self.assertTrue(all(method == 'GET' for method, _, _ in self.calls))

    def test_stale_evidence_and_tampered_report_do_not_post(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [] if '?' in path else {'body': 'New reproduction details'}

        self.request = reply
        self.publish()
        self.assertTrue(all(method == 'GET' for method, _, _ in self.calls))
        self.calls.clear()
        with (self.folder / 'report.md').open('a') as report:
            report.write('Modified report')
        with self.assertRaisesRegex(RuntimeError, 'receipt does not match'):
            self.publish()
        self.assertEqual(self.calls, [])

    def test_disclosure_refusal_receives_exact_outgoing_json_and_prevents_post(self):
        self.checker.side_effect = RuntimeError('Refused')
        with self.assertRaisesRegex(RuntimeError, 'Refused'):
            self.publish()
        self.assertTrue(all(method == 'GET' for method, _, _ in self.calls))
        payload, destination = self.checker.call_args.args
        self.assertIn('Automated issue investigation', json.loads(payload)['body'])
        self.assertEqual(destination, 'github-comment:BenCodez/VotingPlugin#1583')

    def test_checker_time_revision_change_is_withheld(self):
        changed = False
        def approve(*args):
            nonlocal changed
            changed = True
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [] if '?' in path else {'body': 'New material after approval began'} if changed else {}

        self.request = reply
        self.checker.side_effect = approve
        self.publish()
        self.checker.assert_called_once()
        self.assertTrue(self.calls)
        self.assertTrue(all(method == 'GET' for method, _, _ in self.calls))

    def test_incomplete_and_empty_reports_are_not_posted(self):
        for status, report in [('failed', 'report'), ('in_progress', 'report'), ('completed', ' ')] :
            (self.folder / 'turn.json').write_text(json.dumps({'status': status}))
            (self.folder / 'report.md').write_text(report)
            with self.assertRaises(RuntimeError):
                self.publish()
        self.assertEqual(self.calls, [])

    def test_pr_target_is_rejected(self):
        self.request = lambda *args: {'pull_request': {}}
        with self.assertRaisesRegex(RuntimeError, 'not a pull request'):
            self.publish()

    def test_rewrites_sandbox_source_links_and_output_links(self):
        text = '[Source](/workspace/repos/VotingPlugin/VotingPlugin/src/Foo.java:42) [Log](/workspace/outputs/log.txt)'
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('/blob/' + 'a' * 40 + '/VotingPlugin/src/Foo.java#L42', body)
        self.assertIn('/actions/runs/123#artifacts', body)
        self.assertNotIn('/workspace/', body)

    def test_rewrites_angle_bracket_artifact_links_from_issue_1760(self):
        report = '\n'.join([
            '[Investigation report](</workspace/outputs/votingplugin-issue-1760-report.md>)',
            '[Commands and observed results](</workspace/outputs/votingplugin-issue-1760-commands.txt>)',
            '[Maven output](</workspace/outputs/votingplugin-issue-1760-maven.log>)',
        ])
        body = post.comment_body(report, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        url = 'https://github.com/BenCodez/VotingPlugin/actions/runs/123#artifacts'
        for label in ['Investigation report', 'Commands and observed results', 'Maven output']:
            self.assertIn('[' + label + '](' + url + ')', body)
        self.assertNotIn('](</workspace/outputs/', body)
        self.assertIn('[Download investigation artifacts](' + url + ')', body)

    def test_rewrites_angle_bracket_source_links_with_lines_and_spaces(self):
        text = '[Source](</workspace/repos/VotingPlugin/VotingPlugin/src/File name.java:42>)'
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('/blob/' + 'a' * 40 + '/VotingPlugin/src/File%20name.java#L42', body)
        self.assertNotIn('/workspace/repos/', body)

    def test_preserves_github_single_line_and_line_range_anchors(self):
        root = '/workspace/repos/VotingPlugin/VotingPlugin/src/'
        for anchor in ('#L42', '#L42-L45'):
            for suffix in (
                    '[Source](' + root + 'Foo.java' + anchor + ')',
                    '[Source](<' + root + 'Foo.java' + anchor + '>)'):
                body = post.comment_body(suffix, 'BenCodez/VotingPlugin', 'a' * 40, '123')
                expected = '/blob/' + 'a' * 40 + '/VotingPlugin/src/Foo.java' + anchor
                self.assertIn(expected, body)
                self.assertNotIn('%23L', body)
                self.assertNotIn('/workspace/repos/', body)

    def test_anchor_and_spaced_filename_are_preserved_together(self):
        text = '[Source](</workspace/repos/VotingPlugin/VotingPlugin/src/File name.java#L5-L9>)'
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('/VotingPlugin/src/File%20name.java#L5-L9', body)
        self.assertNotIn('%23', body)

    def test_invalid_fragment_does_not_create_misleading_source_link(self):
        text = '[Source](/workspace/repos/VotingPlugin/VotingPlugin/src/Foo.java#untrusted)'
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn(text, body)
        self.assertNotIn('%23untrusted', body)

    def test_existing_percent_escaped_spaces_are_not_double_encoded(self):
        root = '/workspace/repos/VotingPlugin/VotingPlugin/src/'
        examples = [
            '[Source](' + root + 'File%20name.java:42)',
            '[Source](<' + root + 'File%20name.java#L5-L9>)',
            '[Source](<' + root + 'File name.java:42>)',
        ]
        for source in examples:
            with self.subTest(source=source):
                body = post.comment_body(source, 'BenCodez/VotingPlugin', 'a' * 40, '123')
                self.assertIn('/VotingPlugin/src/File%20name.java#L', body)
                self.assertNotIn('File%2520name', body)

    def test_encoded_traversal_control_chars_and_bad_percent_escapes_are_rejected(self):
        root = '/workspace/repos/VotingPlugin/'
        invalid_paths = (
            '%2e%2e/secret.txt',
            'safe/%2e%2e/secret.txt',
            'safe%2f..%2fsecret.txt',
            'safe%5c..%5csecret.txt',
            'safe/%0aevil.txt',
            'safe/%ffevil.txt',
            'safe/File%GGname.java',
            'safe/%252e%252e/secret.txt',
        )
        for path in invalid_paths:
            with self.subTest(path=path):
                source = '[Source](' + root + path + ')'
                self.assertIn(source, post.comment_body(source, 'BenCodez/VotingPlugin',
                                                        'a' * 40, '123'))

    def test_unfinished_angle_link_cannot_consume_following_paragraphs(self):
        text = ('[Log](</workspace/outputs/run.log)\n\n'
                '**Finding**: confirmed bug.\n\n'
                '[Source](</workspace/repos/VotingPlugin/Foo.java:42>)\n\n'
                'Final paragraph.')
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('[Log](</workspace/outputs/run.log)', body)
        self.assertIn('**Finding**: confirmed bug.', body)
        self.assertIn('/blob/' + 'a' * 40 + '/Foo.java#L42', body)
        self.assertIn('Final paragraph.', body)
        self.assertIn('**Finding**: confirmed bug.\n\n[Source]', text)

    def test_nested_angle_brackets_do_not_absorb_later_links(self):
        text = ('[Bad](</workspace/outputs/no<close>) '
                '[Valid](/workspace/outputs/report.md)')
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('[Bad](</workspace/outputs/no<close>)', body)
        self.assertIn('[Valid](https://github.com/BenCodez/VotingPlugin/actions/runs/123#artifacts)', body)

    def test_does_not_rewrite_external_or_traversal_source_links(self):
        external = '[External](https://example.com/docs)'
        traversal = '[Outside](</workspace/repos/VotingPlugin/../secret.txt>)'
        body = post.comment_body(external + ' ' + traversal,
                                 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn(external, body)
        self.assertIn(traversal, body)
        self.assertNotIn('/blob/' + 'a' * 40 + '/../', body)

    def test_oversized_report_is_shortened_with_full_artifact_link(self):
        for text in ['x' * 70000, '\U0001f600' * 20000]:
            body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
            self.assertLess(len(body.encode('utf-8')), 65536)
            self.assertIn('Report shortened', body)

    def test_manual_opt_in_is_blocked_without_loading_or_publishing_report(self):
        with patch.dict(os.environ, {'POST_COMMENT': 'true'}, clear=True), \
             patch.object(post, 'publish') as publish:
            self.assertEqual(post.main(), 1)
        publish.assert_not_called()

    def test_all_modes_remain_unpublishable_even_with_valid_anchors_and_filler(self):
        from types import SimpleNamespace
        from unittest.mock import Mock

        root = 'https://github.com/BenCodez/VotingPlugin'
        sha = 'a' * 40
        template = ('**Finding**: No investigation was performed.\n'
                    '**Evidence**: No inspection was performed on [this reference]({anchor}).\n'
                    '**Verification**: No result was independently checked.\n'
                    '**Suggested next step**: No recommendation was evaluated.\n'
                    '**Limitations**: Inputs are missing; no investigation occurred.\n')
        for mode in ('bug', 'docs', 'feature', 'support', 'insufficient'):
            with self.subTest(mode=mode):
                anchor = f'{root}/blob/{sha}/README.md' if mode in {'bug', 'docs'} else f'{root}/issues/1583'
                report = template.format(anchor=anchor)
                provenance = json.loads((self.folder / 'evidence.json').read_text())
                provenance['mode'] = mode
                provenance['observations'] = post.report_completion(report, provenance, lambda *_: True)
                (self.folder / 'evidence.json').write_text(json.dumps(provenance))
                (self.folder / 'report.md').write_text(report)
                receipt_path = self.folder / 'completed.json'
                receipt = json.loads(receipt_path.read_text())
                receipt['report_sha256'] = hashlib.sha256(report.encode()).hexdigest()
                receipt_path.write_text(json.dumps(receipt))
                api = SimpleNamespace(request=Mock())
                with self.assertRaisesRegex(RuntimeError, 'trusted exact-report'):
                    post.publish(api, 'BenCodez/VotingPlugin', '1583', sha, '123')
                api.request.assert_not_called()
        self.checker.assert_not_called()

    def test_ambiguous_post_failure_is_not_retried(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            if method == 'POST':
                raise RuntimeError('Lost response')
            return [] if '/comments?' in path else ([] if '/events?' in path else {})
        self.request = reply
        with self.assertRaisesRegex(RuntimeError, 'Lost response'):
            self.publish()
        self.assertEqual(sum(x[0] == 'POST' for x in self.calls), 1)
