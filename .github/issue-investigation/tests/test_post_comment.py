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
