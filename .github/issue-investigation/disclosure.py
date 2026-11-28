"""Require approval from a locally provisioned exact-payload validator."""
import hashlib
import json
import os
import re
from pathlib import Path
import subprocess
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, build_opener


def payload_bytes(value):
    return json.dumps(value).encode('ascii')


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


def install_checker():
    url = os.environ.get('ISSUE_INVESTIGATION_POLICY_URL', '')
    path = Path(os.environ.get('ISSUE_INVESTIGATION_DISCLOSURE_CHECKER', ''))
    expected = os.environ.get('ISSUE_INVESTIGATION_DISCLOSURE_SHA256', '')
    endpoint = urlsplit(url)
    if (endpoint.scheme != 'https' or not endpoint.hostname or endpoint.username is not None
            or endpoint.password is not None or endpoint.fragment or not path.is_absolute()
            or not re.fullmatch(r'[0-9a-f]{64}', expected)):
        raise RuntimeError('Trusted hosted policy is unavailable')
    bound = 256 * 1024
    with build_opener(NoRedirect).open(url, timeout=15) as response:
        policy = response.read(bound + 1)
    if len(policy) > bound or hashlib.sha256(policy).hexdigest() != expected:
        raise RuntimeError('Hosted policy does not match the approved digest')
    with path.open('xb') as installed:
        installed.write(policy)
    path.chmod(0o700)


def require_receipt(payload, destination, verdict):
    path = Path(os.environ.get('ISSUE_INVESTIGATION_DISCLOSURE_CHECKER', ''))
    expected = os.environ.get('ISSUE_INVESTIGATION_DISCLOSURE_SHA256')
    if not expected or not path.is_absolute() or not path.is_file():
        raise RuntimeError('Exact-payload disclosure checker is unavailable')
    if not payload or len(payload) > 256 * 1024:
        raise RuntimeError('Disclosure payload is empty or exceeds the review bound')
    try:
        if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
            raise RuntimeError('Disclosure checker does not match the approved digest')
        result = subprocess.run([str(path), destination], input=payload, capture_output=True,
                                timeout=20, check=False, env={'PATH': '/usr/bin:/bin', 'LANG': 'C.UTF-8'})
        receipt = json.loads(result.stdout)
    except (OSError, subprocess.TimeoutExpired, ValueError):
        raise RuntimeError('Exact-payload disclosure check failed') from None
    if result.returncode or receipt != {
            'verdict': verdict, 'destination': destination,
            'sha256': hashlib.sha256(payload).hexdigest()}:
        raise RuntimeError('Exact-payload disclosure check refused the content')


def require_disclosure(payload, destination):
    require_receipt(payload, destination, 'allow')


def require_substantive_approval(payload, destination):
    require_receipt(payload, destination, 'approve-substantive-report')
