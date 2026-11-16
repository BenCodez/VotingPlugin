import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location('investigate', Path(__file__).resolve().parents[1] / 'run.py')
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class FakeApi:
    def __init__(self, handler):
        self.handler, self.calls = handler, []

    def request(self, method, path, payload=None, binary=False):
        self.calls.append((method, path, payload))
        return self.handler(method, path, payload)


class RunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.output = Path(self.temp.name)
        self.results = patch.object(runner, 'RESULTS', self.output)
        self.results.start()
        self.addCleanup(self.results.stop)

    def test_issue_context_paginates_without_using_body_as_code(self):
        body = '$(touch /tmp/not-executed)\n::error::untrusted'
        def reply(method, path, payload):
            if '/comments?' not in path:
                return {'html_url': 'https://github.com/BenCodez/VotingPlugin/issues/1747',
                        'title': body, 'body': body}
            count = 100 if path.endswith('page=1') else 1
            return [{'user': {'login': 'reporter'}, 'body': body, 'html_url': 'url'}] * count
        api = FakeApi(reply)
        issue = runner.issue_context(api, 'BenCodez/VotingPlugin', '1747')
        self.assertEqual(len(issue['comments']), 101)
        self.assertEqual(issue['body'], body)
        payload = runner.session_payload('BenCodez/VotingPlugin', 'a' * 40)
        self.assertNotIn(body, payload['environment']['setup_commands'][0]['command'])
        self.assertNotIn('input', payload)
        self.assertNotIn('name', payload['agent'])
        self.assertEqual(json.loads(runner.session_input('a' * 40, issue, 15).split('\n', 1)[1]), issue)
        self.assertEqual(payload['environment']['env'], {'JAVA_HOME': '/opt/issue-jdk21'})
        self.assertIn('maven ripgrep', payload['environment']['setup_commands'][0]['command'])
        self.assertFalse(payload['stream'])

    def test_automatic_issue_context_is_bounded_and_labeled(self):
        issue = {'url': 'https://github.com/BenCodez/VotingPlugin/issues/1',
                 'title': 'X' * 500, 'body': 'B' * 20000,
                 'comments': [{'body': 'C' * 4000, 'user': 'u'}] * 9}
        trimmed = runner.limit_automatic_context(issue)
        self.assertTrue(trimmed['context_truncated'])
        self.assertLess(len(trimmed['body']), 15000)
        self.assertLess(len(trimmed['title']), 400)
        self.assertEqual(len(trimmed['comments']), 5)
        self.assertIn('Truncated', trimmed['body'])
        self.assertEqual(len(issue['comments']), 9)

    def test_automatic_issue_context_preserves_short_issue(self):
        issue = {'title': 'Issue', 'body': 'Description', 'comments': []}
        self.assertEqual(runner.limit_automatic_context(issue), issue)

    def test_rejects_pr_before_spending_api_usage(self):
        api = FakeApi(lambda *args: {'pull_request': {}})
        with self.assertRaisesRegex(RuntimeError, 'not a pull request'):
            runner.issue_context(api, 'BenCodez/VotingPlugin', '1')

    def test_rejects_shell_injection_in_repository_and_sha(self):
        for repo, sha in [('BenCodez/VotingPlugin;id', 'a' * 40), ('BenCodez/VotingPlugin', '$(id)')]:
            with self.assertRaises(RuntimeError):
                runner.session_payload(repo, sha)

    def test_idle_during_provisioning_is_not_success(self):
        states = iter([[], [{'id': 'turn_1', 'status': 'completed'}]])
        api = FakeApi(lambda m, p, x: {'status': 'idle'} if '/turns?' not in p else {'data': next(states)})
        sleeps = []
        turn = runner.wait_for_turn(api, 'sess_1', 100, sleeps.append, lambda: 0)
        self.assertEqual(turn['id'], 'turn_1')
        self.assertEqual(sleeps, [5])

    def test_failed_and_cancelled_turns_are_errors(self):
        for status in ['failed', 'cancelled']:
            api = FakeApi(lambda m, p, x: {'status': 'idle'} if '/turns?' not in p
                          else {'data': [{'id': 'turn_1', 'status': status}]})
            with self.assertRaisesRegex(RuntimeError, status):
                runner.wait_for_turn(api, 'sess_1', 100, lambda _: None, lambda: 0)

    def test_failed_provisioning_and_required_action_are_errors(self):
        for status in ['failed', 'requires_action']:
            api = FakeApi(lambda *args: {'status': status})
            with self.assertRaisesRegex(RuntimeError, 'Session failed'):
                runner.wait_for_turn(api, 'sess_1', 100, lambda _: None, lambda: 0)

    def test_timeout_does_not_claim_success(self):
        with self.assertRaises(TimeoutError):
            runner.wait_for_turn(FakeApi(lambda *args: {}), 'sess_1', 1, clock=lambda: 2)

    def test_pagination_recovers_final_output_beyond_first_page(self):
        api = FakeApi(lambda m, p, x: {'data': [{'id': 'msg_1'}], 'has_more': True, 'last_id': 'msg_1'}
                      if 'after=' not in p else {'data': [{'id': 'msg_2'}], 'has_more': False})
        self.assertEqual([x['id'] for x in runner.pages(api, '/items')], ['msg_1', 'msg_2'])

    def collect_api(self, usage):
        def reply(method, path, payload):
            if '/items?' in path:
                return {'data': [
                    {'type': 'message', 'role': 'assistant', 'phase': 'commentary', 'turn_id': 'turn_1',
                     'content': [{'type': 'output_text', 'text': 'working'}]},
                    {'type': 'message', 'role': 'assistant', 'phase': 'final_answer', 'turn_id': 'turn_1',
                     'content': [{'type': 'output_text', 'text': 'Actual final report'}]}], 'has_more': False}
            if '/artifacts?' in path:
                return {'data': [{'id': 'artifact_1', 'turn_id': 'turn_1', 'size_bytes': 3}], 'has_more': False}
            if path.endswith('/content'):
                return b'log'
            return {'status': 'idle', 'usage': usage()}
        return FakeApi(reply)

    def test_final_answer_artifacts_and_delayed_usage_are_saved(self):
        values = iter([None, {'input_tokens': 100, 'output_tokens': 20, 'total_tokens': 120}])
        with patch.object(runner.time, 'sleep'):
            runner.collect(self.collect_api(lambda: next(values)), 'sess_1', {'id': 'turn_1'})
        self.assertEqual((self.output / 'report.md').read_text(), 'Actual final report\n')
        self.assertEqual((self.output / 'artifacts/artifact_1').read_bytes(), b'log')
        self.assertEqual(json.loads((self.output / 'usage.json').read_text())['usage']['total_tokens'], 120)

    def test_agent_instructions_require_evidence_and_bounded_searches(self):
        agent = json.loads((runner.ROOT / 'agent.json').read_text(encoding='utf-8'))
        self.assertEqual(agent['model'], 'gpt-6-luna')
        instructions = agent['instructions']
        for heading in ('**Finding**', '**Evidence**', '**Verification**',
                        '**Suggested next step**', '**Limitations**'):
            self.assertIn(heading, instructions)
        self.assertIn('12 shell commands', instructions)
        self.assertIn('one focused Maven test command', instructions)
        self.assertIn('Do not include raw sandbox output paths', instructions)
        self.assertIn('VERSION FIRST', instructions)
        self.assertIn('reported version', instructions.lower())
        self.assertIn('tag-or-commit', instructions)

    def test_issue_1764_reported_release_is_primary_and_latest_sha_is_comparison(self):
        issue = {'title': 'Globally linked Floodgate account',
                 'body': '**Versions**\nVotingPlugin 7.1.1 | AdvancedCore 3.8.2-SNAPSHOT',
                 'comments': []}
        self.assertEqual(runner.reported_votingplugin_version(issue), '7.1.1')
        prompt = runner.session_input('a' * 40, issue, 10)
        self.assertIn('reports VotingPlugin 7.1.1', prompt)
        self.assertIn('first using its verified Git release tag', prompt)
        self.assertIn('newer development comparison', prompt)
        self.assertEqual(json.loads(prompt.split('\n', 1)[1]), issue)

    def test_unversioned_and_snapshot_issues_do_not_invent_release_tags(self):
        self.assertIsNone(runner.reported_votingplugin_version({'body': 'Vote rewards do not run'}))
        self.assertEqual(runner.reported_votingplugin_version(
            {'body': 'VotingPlugin: 7.1.2-SNAPSHOT'}), '7.1.2-SNAPSHOT')
        prompt = runner.session_input('b' * 40, {'body': 'No version'}, 10)
        self.assertIn('Identify the reported plugin version/build first', prompt)

    def test_artifact_filenames_preserve_safe_names_only(self):
        self.assertEqual(runner.artifact_filename({
            'id': 'artifact_123', 'path': '/workspace/outputs/report.md'}), 'report.md')
        self.assertEqual(runner.artifact_filename({
            'id': 'artifact_124', 'path': '/workspace/outputs/run.log'}), 'run.log')
        for path in ('/workspace/outputs/../secrets',
                     '/workspace/outputs/subfolder/file.txt',
                     '/etc/passwd', '/workspace/outputs/invalid name.txt'):
            self.assertEqual(runner.artifact_filename({'id': 'artifact_125', 'path': path}),
                             'artifact_125')

    def test_collected_artifacts_retain_readable_names_and_avoid_overwrite(self):
        def reply(method, path, payload):
            if '/items?' in path:
                return {'data': [{
                    'type': 'message', 'role': 'assistant', 'phase': 'final_answer',
                    'turn_id': 'turn_1', 'content': [{'type': 'output_text', 'text': 'Finding'}]
                }], 'has_more': False}
            if '/artifacts?' in path:
                return {'data': [
                    {'id': 'artifact_1', 'turn_id': 'turn_1', 'size_bytes': 3,
                     'path': '/workspace/outputs/report.md'},
                    {'id': 'artifact_2', 'turn_id': 'turn_1', 'size_bytes': 3,
                     'path': '/workspace/outputs/report.md'},
                ], 'has_more': False}
            if path.endswith('artifact_1/content'):
                return b'one'
            if path.endswith('artifact_2/content'):
                return b'two'
            return {'status': 'idle', 'usage': {'total_tokens': 1}}
        runner.collect(FakeApi(reply), 'sess_1', {'id': 'turn_1'})
        self.assertEqual((self.output / 'artifacts/report.md').read_bytes(), b'one')
        self.assertEqual((self.output / 'artifacts/artifact_2').read_bytes(), b'two')

    def test_missing_usage_remains_unknown(self):
        with patch.object(runner.time, 'sleep'):
            runner.collect(self.collect_api(lambda: None), 'sess_1', {'id': 'turn_1'})
        usage = json.loads((self.output / 'usage.json').read_text())
        self.assertEqual(usage['status'], 'unavailable')
        self.assertIsNone(usage['usage'])

    def test_cleanup_cancels_before_delete_and_retries_conflict(self):
        (self.output / 'session-id.txt').write_text('sess_1\n')
        deletes = []
        def reply(method, path, payload):
            if method == 'GET':
                return {'status': 'in_progress', 'usage': {'total_tokens': 20}}
            if method == 'DELETE':
                deletes.append(path)
                if len(deletes) == 1:
                    raise runner.ApiError(409)
            return {}
        api = FakeApi(reply)
        with patch.object(runner.time, 'sleep'):
            runner.cleanup(api)
        self.assertEqual([x[0] for x in api.calls], ['GET', 'POST', 'DELETE', 'DELETE'])
        self.assertEqual(api.calls[1][2], {'events': [{'type': 'agent.session.input.cancel'}]})
        self.assertFalse((self.output / 'session-id.txt').exists())

    def test_cleanup_preserves_available_usage_and_recovery_id_on_failure(self):
        (self.output / 'session-id.txt').write_text('sess_1\n')
        runner.save(self.output / 'usage.json', {'status': 'available', 'usage': {'total_tokens': 8}})
        def reply(method, path, payload):
            if method == 'GET':
                return {'status': 'idle', 'usage': None}
            raise runner.ApiError(500)
        with self.assertRaises(runner.ApiError):
            runner.cleanup(FakeApi(reply))
        self.assertTrue((self.output / 'session-id.txt').exists())
        self.assertEqual(json.loads((self.output / 'usage.json').read_text())['usage']['total_tokens'], 8)

    def test_cleanup_is_idempotent_after_remote_deletion(self):
        (self.output / 'session-id.txt').write_text('sess_1\n')
        def reply(*args):
            raise runner.ApiError(404)
        runner.cleanup(FakeApi(reply))
        self.assertTrue(json.loads((self.output / 'cleanup.json').read_text())['deleted'])

    def test_curl_keeps_secrets_out_of_arguments_and_child_environment(self):
        def invoke(args, **kwargs):
            self.assertNotIn('test-secret', ' '.join(args))
            self.assertIn('Bearer test-secret', kwargs['input'])
            self.assertNotIn('OPENAI_API_KEY', kwargs['env'])
            self.assertNotIn('GH_TOKEN', kwargs['env'])
            Path(args[args.index('--output') + 1]).write_text('{"ok":true}')
            class Result:
                returncode, stdout = 0, '200'
            return Result()
        with patch.object(runner.subprocess, 'run', side_effect=invoke), patch.dict(os.environ, {'OPENAI_API_KEY': 'hidden', 'GH_TOKEN': 'hidden'}):
            self.assertEqual(runner.Curl('https://api.openai.com/v1', 'test-secret').request('GET', '/agents'), {'ok': True})

    def test_main_failure_after_create_still_cleans_exact_session(self):
        def reply(method, path, payload):
            if method == 'POST':
                if path == '/agents/sessions':
                    self.assertNotIn('input', payload)
                else:
                    self.assertEqual((self.output / 'session-id.txt').read_text(), 'sess_1\n')
                    self.assertEqual(payload['events'][0]['type'], 'agent.session.input.message')
                return {'id': 'sess_1', 'status': 'in_progress'}
            if '/turns?' in path:
                return {'data': [{'id': 'turn_1', 'status': 'failed'}]}
            return {'status': 'idle', 'usage': {'total_tokens': 2}}
        api = FakeApi(reply)
        env = {'OPENAI_API_KEY': 'test', 'GH_TOKEN': 'test', 'ISSUE_NUMBER': '1747',
               'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin', 'GITHUB_SHA': 'a' * 40}
        with patch.dict(os.environ, env), patch.object(runner.sys, 'argv', ['run.py']), \
             patch.object(runner, 'Curl', return_value=api), patch.object(runner, 'issue_context', return_value={}), \
             patch.object(runner.signal, 'signal'):
            self.assertEqual(runner.main(), 1)
        self.assertTrue(any(m == 'DELETE' and p.endswith('/sess_1') for m, p, _ in api.calls))

    def test_lost_create_response_never_submits_task(self):
        def reply(method, path, payload):
            self.assertEqual((method, path), ('POST', '/agents/sessions'))
            self.assertNotIn('input', payload)
            raise RuntimeError('Simulated lost create response')
        api = FakeApi(reply)
        env = {'OPENAI_API_KEY': 'test', 'GH_TOKEN': 'test', 'ISSUE_NUMBER': '1747',
               'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin', 'GITHUB_SHA': 'a' * 40}
        with patch.dict(os.environ, env), patch.object(runner.sys, 'argv', ['run.py']), \
             patch.object(runner, 'Curl', return_value=api), patch.object(runner, 'issue_context', return_value={}), \
             patch.object(runner.signal, 'signal'):
            self.assertEqual(runner.main(), 1)
        self.assertEqual(len(api.calls), 1)
        self.assertFalse((self.output / 'session-id.txt').exists())

    def test_lost_task_response_cancels_recorded_session_without_resubmitting(self):
        def reply(method, path, payload):
            if (method, path) == ('POST', '/agents/sessions'):
                return {'id': 'sess_1', 'status': 'idle'}
            if method == 'GET':
                return {'status': 'in_progress', 'usage': None}
            if method == 'POST' and payload['events'][0]['type'] == 'agent.session.input.message':
                self.assertEqual((self.output / 'session-id.txt').read_text(), 'sess_1\n')
                raise RuntimeError('Simulated lost task response')
            return {}
        api = FakeApi(reply)
        env = {'OPENAI_API_KEY': 'test', 'GH_TOKEN': 'test', 'ISSUE_NUMBER': '1747',
               'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin', 'GITHUB_SHA': 'a' * 40}
        with patch.dict(os.environ, env), patch.object(runner.sys, 'argv', ['run.py']), \
             patch.object(runner, 'Curl', return_value=api), patch.object(runner, 'issue_context', return_value={}), \
             patch.object(runner.signal, 'signal'):
            self.assertEqual(runner.main(), 1)
        events = [payload['events'][0]['type'] for method, path, payload in api.calls
                  if method == 'POST' and path.endswith('/events')]
        self.assertEqual(events, ['agent.session.input.message', 'agent.session.input.cancel'])
        self.assertTrue(json.loads((self.output / 'cleanup.json').read_text())['deleted'])

    def test_missing_key_fails_without_creating_session(self):
        with patch.dict(os.environ, {}, clear=True), patch.object(runner, 'Curl') as curl:
            self.assertEqual(runner.main(), 1)
        curl.assert_not_called()


if __name__ == '__main__':
    unittest.main()
