"""Require approval from a locally provisioned exact-payload validator."""
import hashlib
import json
import os
from pathlib import Path
import subprocess


def payload_bytes(value):
    return json.dumps(value).encode('ascii')


def require_disclosure(payload, destination):
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
            'verdict': 'allow', 'destination': destination,
            'sha256': hashlib.sha256(payload).hexdigest()}:
        raise RuntimeError('Exact-payload disclosure check refused the content')
