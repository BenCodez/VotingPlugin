import importlib.util
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


class PostingTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.folder = Path(self.temp.name)
        result_patch = patch.object(post, 'RESULTS', self.folder)
        result_patch.start()
        self.addCleanup(result_patch.stop)
        (self.folder / 'turn.json').write_text(json.dumps({'status': 'completed'}))
        (self.folder / 'report.md').write_text('Result: no defect confirmed. Tests blocked.')
        self.calls = []

    def request(self, method, path, payload=None):
        self.calls.append((method, path, payload))
        return [] if method == 'GET' and '/comments?' in path else {}

    def publish(self):
        with patch.object(post.Curl, 'request', side_effect=self.request):
            post.publish(post.Curl('https://api.github.com', 'test'), 'BenCodez/VotingPlugin', '1583', 'a' * 40, '123')

    def test_default_off_never_reads_credentials_or_calls_api(self):
        for value in [None, 'false', '1', 'True']:
            with patch.dict(os.environ, {} if value is None else {'POST_COMMENT': value}, clear=True), \
                    patch.object(post, 'Curl') as curl:
                self.assertEqual(post.main(), 0)
                curl.assert_not_called()

    def test_posts_actual_completed_report_to_selected_issue(self):
        self.publish()
        method, path, payload = self.calls[-1]
        self.assertEqual((method, path), ('POST', '/repos/BenCodez/VotingPlugin/issues/1583/comments'))
        self.assertTrue(payload['body'].startswith('<!-- votingplugin-issue-investigation:123 -->\n'))
        self.assertIn('no defect confirmed. Tests blocked.', payload['body'])
        self.assertIn('/actions/runs/123', payload['body'])

    def test_existing_bot_comment_prevents_duplicate_on_rerun(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [{'user': {'login': 'github-actions[bot]'},
                     'body': '<!-- votingplugin-issue-investigation:123 -->\nold report'}] if '/comments?' in path else {}
        self.request = reply
        self.publish()
        self.assertFalse(any(x[0] == 'POST' for x in self.calls))

    def test_user_copied_marker_does_not_suppress_post(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            return [{'user': {'login': 'reporter'},
                     'body': '<!-- votingplugin-issue-investigation:123 -->\nfake'}] if '/comments?' in path else {}
        self.request = reply
        self.publish()
        self.assertEqual(sum(x[0] == 'POST' for x in self.calls), 1)

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
        text = ('[Log](</workspace/outputs/run.log)\\n\\n'
                '**Finding**: confirmed bug.\\n\\n'
                '[Source](</workspace/repos/VotingPlugin/Foo.java:42>)\\n\\n'
                'Final paragraph.')
        body = post.comment_body(text, 'BenCodez/VotingPlugin', 'a' * 40, '123')
        self.assertIn('[Log](</workspace/outputs/run.log)', body)
        self.assertIn('**Finding**: confirmed bug.', body)
        self.assertIn('/blob/' + 'a' * 40 + '/Foo.java#L42', body)
        self.assertIn('Final paragraph.', body)
        self.assertIn('**Finding**: confirmed bug.\\n\\n[Source]', text)

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

    def test_explicit_opt_in_invokes_publisher_with_selected_issue(self):
        env = {'POST_COMMENT': 'true', 'GH_TOKEN': 'test', 'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin',
               'ISSUE_NUMBER': '1583', 'GITHUB_SHA': 'a' * 40, 'GITHUB_RUN_ID': '123'}
        with patch.dict(os.environ, env, clear=True), patch.object(post, 'publish') as publish:
            self.assertEqual(post.main(), 0)
        self.assertEqual(publish.call_args.args[1:], ('BenCodez/VotingPlugin', '1583', 'a' * 40, '123'))

    def test_invalid_issue_is_rejected_before_any_api_call(self):
        env = {'POST_COMMENT': 'true', 'GH_TOKEN': 'test', 'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin',
               'ISSUE_NUMBER': '1583/comments', 'GITHUB_SHA': 'a' * 40, 'GITHUB_RUN_ID': '123'}
        with patch.dict(os.environ, env, clear=True), patch.object(post, 'Curl') as curl:
            self.assertEqual(post.main(), 1)
        curl.assert_not_called()

    def test_ambiguous_post_failure_is_not_retried(self):
        def reply(method, path, payload=None):
            self.calls.append((method, path, payload))
            if method == 'POST':
                raise RuntimeError('Lost response')
            return [] if '/comments?' in path else {}
        self.request = reply
        with self.assertRaisesRegex(RuntimeError, 'Lost response'):
            self.publish()
        self.assertEqual(sum(x[0] == 'POST' for x in self.calls), 1)
