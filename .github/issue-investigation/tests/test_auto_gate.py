import importlib.util
from datetime import date
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
with patch.object(sys, 'path', [str(ROOT), *sys.path]):
    SPEC = importlib.util.spec_from_file_location('auto_gate', ROOT / 'auto_gate.py')
    gate = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(gate)


class Github:
    def __init__(self, count=1, response=None):
        self.calls = []
        self.count = count
        self.response = response
        self.environment_name = 'existing-environment'
        self.environment_response = None
        self.environment_error = None

    def request(self, method, path):
        self.calls.append((method, path))
        if '/environments/' in path:
            if self.environment_error is not None:
                raise gate.ApiError(self.environment_error)
            if self.environment_response is not None:
                return self.environment_response
            return {'name': self.environment_name}
        if self.response is not None:
            return self.response
        return {'total_count': self.count}


class AutomaticIssueTests(unittest.TestCase):
    def setUp(self):
        self.event = {'action': 'opened', 'issue': {
            'number': 1758, 'user': {'login': 'reporter', 'type': 'User'}}}
        self.github = Github()

    def eligible(self, environment='existing-environment'):
        return gate.eligible(self.event, self.github, 'BenCodez/VotingPlugin',
                             environment, date(2026, 11, 15), owner='github')

    def test_new_human_issue_is_admitted_below_limit(self):
        allowed, reason = self.eligible()
        self.assertTrue(allowed)
        self.assertIn('1758', reason)
        self.assertEqual(len(self.github.calls), 2)
        self.assertIn('/environments/existing-environment', self.github.calls[0][1])
        self.assertIn('event=issues', self.github.calls[1][1])
        self.assertIn('2026-11-15', self.github.calls[1][1])

    def test_limit_allows_fifth_run_but_skips_sixth(self):
        self.github.count = 5
        self.assertTrue(self.eligible()[0])
        self.github.count = 6
        self.assertFalse(self.eligible()[0])

    def test_missing_environment_skips_without_network(self):
        self.assertFalse(self.eligible('')[0])
        self.assertEqual(self.github.calls, [])

    def test_bot_and_non_opened_events_skip_without_network(self):
        self.event['issue']['user'] = {'login': 'dependabot[bot]', 'type': 'Bot'}
        self.assertFalse(self.eligible()[0])
        self.event['issue']['user'] = {'login': 'reporter', 'type': 'User'}
        self.event['action'] = 'edited'
        self.assertFalse(self.eligible()[0])
        self.assertEqual(self.github.calls, [])

    def test_pr_and_invalid_number_do_not_run(self):
        self.event['issue']['pull_request'] = {}
        self.assertFalse(self.eligible()[0])
        self.event['issue'].pop('pull_request')
        self.event['issue']['number'] = '1758; echo risky'
        with self.assertRaisesRegex(RuntimeError, 'Invalid issue number'):
            self.eligible()
        self.assertEqual(self.github.calls, [])

    def test_missing_environment_is_skipped_before_count_or_model(self):
        self.github.environment_error = 404
        admitted, reason = self.eligible()
        self.assertFalse(admitted)
        self.assertIn('does not exist', reason)
        self.assertEqual(len(self.github.calls), 1)
        self.assertIn('/environments/existing-environment', self.github.calls[0][1])

    def test_environment_permissions_error_fails_closed(self):
        self.github.environment_error = 403
        with self.assertRaises(gate.ApiError):
            self.eligible()
        self.assertEqual(len(self.github.calls), 1)

    def test_invalid_environment_response_fails_closed(self):
        for result in [{}, {'name': 'wrong-environment'}, None]:
            self.github.environment_response = result if result is not None else []
            with self.assertRaisesRegex(RuntimeError, 'Could not verify'):
                self.eligible()
        self.assertEqual(len(self.github.calls), 3)

    def test_environment_name_is_safely_url_encoded(self):
        self.github.environment_name = 'Prod / Review'
        self.assertTrue(self.eligible('Prod / Review')[0])
        self.assertIn('/environments/Prod%20%2F%20Review', self.github.calls[0][1])
        self.assertEqual(len(self.github.calls), 2)

    def test_unavailable_or_unexpected_count_fails_closed(self):
        self.github.response = {}
        with self.assertRaisesRegex(RuntimeError, 'run count'):
            self.eligible()
        self.github.response = {'total_count': '5'}
        with self.assertRaisesRegex(RuntimeError, 'run count'):
            self.eligible()

    def test_main_sets_boolean_output_without_creating_a_model_session(self):
        with tempfile.TemporaryDirectory() as folder:
            event_path = Path(folder) / 'event.json'
            output_path = Path(folder) / 'output'
            event_path.write_text(json.dumps(self.event), encoding='utf-8')
            env = {'GITHUB_EVENT_PATH': str(event_path), 'GITHUB_OUTPUT': str(output_path),
                   'GH_TOKEN': 'test', 'GITHUB_REPOSITORY': 'BenCodez/VotingPlugin',
                   'AUTO_ENVIRONMENT_NAME': 'existing-environment', 'ISSUE_INVESTIGATION_OWNER': 'github'}
            with patch.dict(os.environ, env, clear=True), patch.object(gate, 'Curl', return_value=self.github):
                self.assertEqual(gate.main(), 0)
            self.assertEqual(output_path.read_text(), 'allowed=true\n')
            self.assertEqual(len(self.github.calls), 2)

    def test_agent_uses_luna_without_billable_web_search(self):
        agent = json.loads((ROOT / 'agent.json').read_text(encoding='utf-8'))
        self.assertEqual(agent['model'], 'gpt-6-luna')
        self.assertEqual(agent['tools'], [])

    def test_workflow_is_manual_offline_without_issue_write_or_model_access(self):
        workflow = (ROOT.parent / 'workflows' / 'investigate-issue.yml').read_text(encoding='utf-8')
        self.assertIn('workflow_dispatch:', workflow)
        self.assertNotIn('  issues:', workflow)
        self.assertNotIn('  issue_comment:', workflow)
        self.assertNotIn('OPENAI_API_KEY', workflow)
        self.assertNotIn('issues: write', workflow)
        self.assertIn('unittest discover', workflow)


if __name__ == '__main__':
    unittest.main()
