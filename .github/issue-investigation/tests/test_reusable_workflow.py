import hashlib
import json
import os
from pathlib import Path
import tempfile
from textwrap import dedent
import unittest
from unittest.mock import patch

import run as runner


WORKFLOW = Path(__file__).resolve().parents[2] / 'workflows' / 'investigate-issue-reusable.yml'
SOURCE = 'a' * 40
ENGINE = 'b' * 40
REVISION = 'c' * 64


def bootstrap():
    text = WORKFLOW.read_text()
    code = text.split('        run: |\n', 1)[1].split('\n  investigate:', 1)[0]
    namespace = {'__name__': 'fixture'}
    exec(compile(dedent(code), str(WORKFLOW), 'exec'), namespace)
    return namespace


def context(repository='BenCodez/AdvancedCore', event_name='issues'):
    repo_id, branch = {'BenCodez/AdvancedCore': (66120957, 'master'),
                       'BenCodez/SimpleAPI': (796949127, 'main')}[repository]
    event_repo = {'id': repo_id, 'full_name': repository, 'default_branch': branch,
                  'fork': False, 'private': False}
    actor = {'type': 'User', 'login': 'reporter'}
    env = {'GITHUB_REPOSITORY': repository, 'GITHUB_REPOSITORY_ID': str(repo_id),
           'GITHUB_SHA': SOURCE, 'GITHUB_RUN_ID': '123', 'GITHUB_RUN_ATTEMPT': '1',
           'GITHUB_EVENT_NAME': event_name, 'GITHUB_REF': 'refs/heads/' + branch,
           'GITHUB_WORKFLOW_REF': f'{repository}/.github/workflows/investigate-issue.yml@refs/heads/{branch}',
           'SELECTED_ISSUE': '42', 'SELECTED_TIMEOUT': '15', 'SELECTED_PROJECT': 'proj_fixture42',
           'POLICY_URL': 'https://example.com/checker', 'POLICY_SHA': 'd' * 64}
    event = {'repository': event_repo, 'action': 'opened', 'sender': actor,
             'issue': {'number': 42, 'user': actor}, 'inputs': {'issue_number': '42'}}
    run = {'id': 123, 'run_attempt': 1, 'head_sha': SOURCE, 'head_branch': branch,
           'event': event_name, 'repository': event_repo, 'head_repository': event_repo,
           'workflow_id': 48, 'referenced_workflows': [{
               'path': 'BenCodez/VotingPlugin/.github/workflows/investigate-issue-reusable.yml@' + ENGINE,
               'sha': ENGINE}]}
    return env, event, run


class ReusableTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bootstrap = bootstrap()

    def admit(self, env, event, run, count=1):
        calls = []

        def fetch(path):
            calls.append(path)
            return {'total_count': count} if '?' in path else run

        result = self.bootstrap['admission'](env, event, fetch, '2026-11-27')
        return result, calls

    def test_both_read_only_callers_and_both_events_use_distinct_engine_pin(self):
        self.assertEqual({name: str(data[0]) for name, data in self.bootstrap['CALLERS'].items()},
                         runner.ARTIFACT_CALLERS)
        for repository in runner.ARTIFACT_CALLERS:
            for kind in ('issues', 'workflow_dispatch'):
                args = context(repository, kind)
                result, calls = self.admit(*args)
                self.assertEqual(result, {'outcome': 'admitted', 'engine_sha': ENGINE, 'issue_number': '42'})
                self.assertIn('/attempts/1', calls[0])
                if kind == 'issues':
                    self.assertIn('/workflows/48/runs?event=issues&created=2026-11-27..2026-11-27', calls[1])
                else:
                    self.assertEqual(len(calls), 1)
                payload = runner.session_payload(repository, SOURCE, artifact_only=True)
                command = payload['environment']['setup_commands'][0]['command']
                self.assertIn(f'https://github.com/{repository}.git', command)
                self.assertIn(SOURCE, command)
                self.assertNotIn(ENGINE, command)
                with self.assertRaises(RuntimeError):
                    runner.session_payload(repository, SOURCE)

    def test_rejects_wrong_caller_source_inputs_and_attempt(self):
        env, event, run = context()
        cases = [
            ({**env, 'GITHUB_REPOSITORY_ID': '123'}, event, run),
            ({**env, 'GITHUB_REF': 'refs/heads/sandbox'}, event, run),
            ({**env, 'GITHUB_WORKFLOW_REF': 'wrong'}, event, run),
            ({**env, 'GITHUB_EVENT_NAME': 'pull_request'}, event, run),
            ({**env, 'SELECTED_ISSUE': '41'}, event, run),
            ({**env, 'SELECTED_PROJECT': ''}, event, run),
            ({**env, 'SELECTED_TIMEOUT': '31'}, event, run),
            (env, {**event, 'repository': {**event['repository'], 'fork': True}}, run),
            (env, {**event, 'issue': {**event['issue'], 'pull_request': {}}}, run),
            (env, {**event, 'sender': {'type': 'Bot', 'login': 'example[bot]'}}, run),
            (env, event, {**run, 'head_sha': ENGINE}),
            (env, event, {**run, 'run_attempt': 2}),
        ]
        for index, args in enumerate(cases):
            with self.subTest(index=index), self.assertRaises(ValueError):
                self.admit(*args)

    def test_rejects_missing_ambiguous_mutable_or_conflicting_engine(self):
        env, event, run = context()
        matched = run['referenced_workflows'][0]
        for references in (None, [], [matched, matched],
                           [{**matched, 'path': matched['path'].replace(ENGINE, 'main')}],
                           [{**matched, 'sha': SOURCE}]):
            with self.subTest(references=references), self.assertRaises(ValueError):
                self.admit(env, event, {**run, 'referenced_workflows': references})

    def test_budget_missing_policy_and_same_run_replay(self):
        env, event, run = context()
        self.assertEqual(self.admit(env, event, run, count=5)[0]['outcome'], 'admitted')
        self.assertEqual(self.admit(env, event, run, count=6)[0]['outcome'], 'skipped')
        for count in (0, None, '5', True):
            with self.assertRaises(ValueError):
                self.admit(env, event, run, count=count)
        for key in ('POLICY_URL', 'POLICY_SHA'):
            with self.assertRaisesRegex(ValueError, 'policy'):
                self.admit({**env, key: ''}, event, run)
        self.assertEqual(self.admit({**env, 'GITHUB_RUN_ATTEMPT': '2'}, event,
                                    {**run, 'run_attempt': 2})[0]['outcome'], 'skipped')

    def test_artifact_receipt_binds_exact_contents_producer_and_storage(self):
        env, _, _ = context()
        env.update(ISSUE_INVESTIGATION_ENGINE_SHA=ENGINE, INVESTIGATION_ARTIFACT_ID='456', ISSUE_NUMBER='42')
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            output = folder / 'output'
            env['GITHUB_OUTPUT'] = str(output)
            with patch.dict(os.environ, env, clear=True), patch.object(runner, 'RESULTS', folder), \
                 patch.object(runner, 'require_disclosure') as disclose:
                identity = runner.artifact_identity()
                body = b'Fictional unreviewed artifact\n'
                (folder / 'report.md').write_bytes(body)
                evidence = {'caller': identity, 'repository': identity['repository'],
                            'development_sha': SOURCE, 'number': '42', 'revision': REVISION,
                            'report_status': 'artifact-only'}
                receipt = {'caller': identity, 'issue': '42', 'revision': REVISION, 'run_id': '123',
                           'turn': 'e' * 64,
                           'report_sha256': hashlib.sha256(body).hexdigest()}
                runner.save(folder / 'evidence.json', evidence)
                runner.save(folder / 'completed.json', receipt)
                runner.save(folder / 'turn.json', {'status': 'completed', 'digest': receipt['turn']})
                runner.save(folder / 'usage.json', {'status': 'unavailable', 'usage': None})
                runner.save(folder / 'cleanup.json', {'deleted': False})
                with self.assertRaisesRegex(RuntimeError, 'cleanup'):
                    runner.confirm_hosted_upload()
                runner.save(folder / 'cleanup.json', {'deleted': True})

                with patch.dict(os.environ, {'INVESTIGATION_ARTIFACT_ID': ''}):
                    with self.assertRaisesRegex(RuntimeError, 'confirmed artifact'):
                        runner.confirm_hosted_upload()
                with patch.dict(os.environ, {'ISSUE_INVESTIGATION_ENGINE_SHA': SOURCE}):
                    with self.assertRaisesRegex(RuntimeError, 'scope'):
                        runner.confirm_hosted_upload()
                with patch.object(runner, 'require_disclosure', side_effect=RuntimeError('Refused')):
                    with self.assertRaisesRegex(RuntimeError, 'Refused'):
                        runner.confirm_hosted_upload()
                self.assertFalse(output.exists())

                with self.assertRaises(FileNotFoundError):
                    runner.confirm_hosted_upload()
                self.assertFalse(output.exists())

                for command in ('--prepare-reusable-upload', '--confirm-hosted-upload'):
                    for expected in ('43', '', '042', '42\n'):
                        with self.subTest(command=command, expected=expected), \
                             patch.dict(os.environ, {'ISSUE_NUMBER': expected}), \
                             patch.object(runner.sys, 'argv', ['run.py', command]):
                            if not expected:
                                del os.environ['ISSUE_NUMBER']
                            self.assertEqual(runner.main(), 1)
                            self.assertFalse(output.exists())
                        (folder / 'error.json').unlink()
                    with patch.dict(os.environ, {'ISSUE_NUMBER': '43'}), \
                         patch.object(runner.sys, 'argv', ['run.py', command]):
                        wrong_issue_scope = runner.artifact_identity()
                        runner.save(folder / 'evidence.json', {**evidence, 'caller': wrong_issue_scope})
                        runner.save(folder / 'completed.json', {**receipt, 'caller': wrong_issue_scope})
                        self.assertEqual(runner.main(), 1)
                        self.assertFalse(output.exists())
                    (folder / 'error.json').unlink()
                    runner.save(folder / 'evidence.json', evidence)
                    runner.save(folder / 'completed.json', receipt)
                with patch.object(runner.sys, 'argv', ['run.py', '--prepare-reusable-upload']):
                    self.assertEqual(runner.main(), 0)
                self.assertEqual(output.read_text(), 'report_ready=true\n')
                output.unlink()

                for name in ('error.json', 'issue-investigation-session-id-123.txt'):
                    blocker = folder / name
                    with patch.object(runner, 'session_state', return_value=folder / 'issue-investigation-session-id-123.txt'):
                        blocker.touch()
                        with self.assertRaisesRegex(RuntimeError, 'completed cleanup'):
                            runner.confirm_hosted_upload()
                        blocker.unlink()
                self.assertFalse(output.exists())
                runner.save(folder / 'usage.json', {'status': 'available', 'usage': {'input_tokens': 1}})
                with self.assertRaisesRegex(RuntimeError, 'prepared bytes'):
                    runner.confirm_hosted_upload()
                runner.save(folder / 'usage.json', {'status': 'unavailable', 'usage': None})
                with patch.object(runner.sys, 'argv', ['run.py', '--confirm-hosted-upload']):
                    self.assertEqual(runner.main(), 0)
                self.assertEqual(dict(line.split('=', 1) for line in output.read_text().splitlines()),
                                 {'outcome': 'artifact', 'artifact_id': '456', 'revision': REVISION,
                                  'source_sha': SOURCE, 'engine_sha': ENGINE})
                self.assertIn('github-artifact:cleanup.json', [x.args[1] for x in disclose.call_args_list])
                (folder / 'report.md').write_bytes(b'Changed')
                with self.assertRaisesRegex(RuntimeError, 'does not match'):
                    runner.confirm_hosted_upload()

    def test_reusable_runtime_fails_missing_key_or_policy_without_model_session(self):
        env, _, _ = context('BenCodez/SimpleAPI', 'workflow_dispatch')
        env.update(ISSUE_INVESTIGATION_ENGINE_SHA=ENGINE, ISSUE_INVESTIGATION_OWNER='github',
                   OPENAI_PROJECT_ID='proj_fixture42', ISSUE_NUMBER='42', GH_TOKEN='fixture')
        with tempfile.TemporaryDirectory() as directory:
            results = Path(directory) / 'results'
            with patch.dict(os.environ, env, clear=True), patch.object(runner, 'RESULTS', results), \
                 patch.object(runner.sys, 'argv', ['run.py', '--hosted-artifact']), \
                 patch.object(runner.signal, 'signal'), patch.object(runner, 'Curl') as client:
                self.assertEqual(runner.main(), 1)
                client.assert_not_called()
                (results / 'error.json').unlink()
                results.rmdir()
                with patch.dict(os.environ, {'OPENAI_API_KEY': 'fixture'}), \
                     patch.object(runner, 'issue_context', return_value={'title': 'Feature request'}):
                    # Real disclosure policy resolution: no provisioned checker.
                    self.assertEqual(runner.main(), 1)
                self.assertEqual(client.call_args_list[0].args[2], 'proj_fixture42')
                client.return_value.request.assert_not_called()
                self.assertFalse((results / 'completed.json').exists())

    def test_failed_cleanup_never_completes_or_releases_a_dispatched_claim(self):
        env, _, _ = context()
        env.update(ISSUE_INVESTIGATION_ENGINE_SHA=ENGINE, ISSUE_INVESTIGATION_OWNER='github',
                   OPENAI_PROJECT_ID='proj_fixture42', ISSUE_NUMBER='42', GH_TOKEN='fixture',
                   OPENAI_API_KEY='fixture')
        issue = {'title': 'Feature request'}
        repository = env['GITHUB_REPOSITORY']
        route = runner.route_revision(repository, '42', runner.evidence(
            issue, runner.profile_for(repository), SOURCE)['revision'], 'github')
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            results = folder / 'results'
            recovery = folder / 'session.txt'
            ledger = folder / 'claims.sqlite3'
            env['ISSUE_INVESTIGATION_LEDGER'] = str(ledger)

            completion_exists_at_delete = []

            def collect(*args, **kwargs):
                (results / 'report.md').write_text('Synthetic investigation report\n')
                runner.save(results / 'usage.json', {'status': 'unavailable', 'usage': None})
                return {}

            def respond(method, path, payload=None):
                if method == 'POST' and path == '/agents/sessions':
                    return {'id': 'sess_fixture'}
                if '/turns?' in path:
                    return {'data': [{'id': 'turn_fixture', 'status': 'completed'}]}
                if method == 'DELETE':
                    completion_exists_at_delete.append((results / 'completed.json').exists())
                    raise runner.ApiError(500)
                return {'status': 'idle'}

            with patch.dict(os.environ, env, clear=True), patch.object(runner, 'RESULTS', results), \
                 patch.object(runner, 'session_state', return_value=recovery), \
                 patch.object(runner, 'issue_context', return_value=issue), \
                 patch.object(runner, 'require_disclosure'), patch.object(runner.signal, 'signal'), \
                 patch.object(runner, 'collect', side_effect=collect), patch.object(runner, 'Curl') as client, \
                 patch.object(runner.sys, 'argv', ['run.py', '--hosted-artifact']):
                client.return_value.request.side_effect = respond
                self.assertEqual(runner.main(), 1)
            self.assertEqual(completion_exists_at_delete, [False, False])
            self.assertTrue((results / 'report.md').is_file())
            self.assertTrue((results / 'turn.json').is_file())
            self.assertTrue(recovery.exists())
            self.assertFalse((results / 'completed.json').exists())
            self.assertFalse((results / 'prepared-upload.json').exists())
            self.assertIsNone(runner.Claims(ledger).claim_ingress(
                route, 'github', retry_failed=True, force_completed=True))
            model_creates = [call for call in client.return_value.request.call_args_list
                             if call.args[:2] == ('POST', '/agents/sessions')]
            self.assertEqual(len(model_creates), 1)

    def test_cached_admission_cannot_launch_a_later_attempt_or_expose_outputs(self):
        workflow = WORKFLOW.read_text()
        env, event, run = context()
        cached, _ = self.admit(env, event, run)
        self.assertEqual(cached['outcome'], 'admitted')
        # This is the job-level guard, which is evaluated even when admission
        # is not rerun. All reusable outputs are independently gated.
        self.assertIn("if: ${{ github.run_attempt == '1' && needs.admit.outputs.outcome == 'admitted' }}", workflow)
        self.assertIn("value: ${{ github.run_attempt != '1' && 'skipped' ||", workflow)
        for output in ('artifact_id', 'revision', 'source_sha', 'engine_sha'):
            self.assertIn("github.run_attempt == '1' && jobs.investigate.outputs." + output + " || ''", workflow)
        rerun, _ = self.admit({**env, 'GITHUB_RUN_ATTEMPT': '2'}, event, {**run, 'run_attempt': 2})
        self.assertEqual(rerun, {'outcome': 'skipped'})

    def test_workflow_never_grants_posting_or_inherits_secrets(self):
        yaml = WORKFLOW.read_text()
        self.assertNotIn('issues: write', yaml)
        self.assertNotIn('post_comment', yaml)
        self.assertNotIn('secrets: inherit', yaml)
        self.assertIn('OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}', yaml)
        self.assertIn('ref: ${{ needs.admit.outputs.engine_sha }}', yaml)
        self.assertIn('repository: BenCodez/VotingPlugin', yaml)
        self.assertIn('ISSUE_NUMBER: ${{ needs.admit.outputs.issue_number }}', yaml)
        self.assertIn('--prepare-reusable-upload', yaml)
        self.assertIn('--confirm-hosted-upload', yaml)
