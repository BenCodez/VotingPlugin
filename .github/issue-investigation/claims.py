"""Single-store claims and receipts for an explicitly routed evidence revision."""
from contextlib import closing
import re
import sqlite3

from engine import digest


class Claims:
    def __init__(self, path):
        self.path = path

    def claim_ingress(self, route, ingress, *, retry_failed=False, force_completed=False):
        if ingress not in {'github', 'cezar'} or route['owner'] != ingress:
            return None
        return self.claim(route, retry_failed=retry_failed, force_completed=force_completed)

    def claim(self, route, retry_failed=False, force_completed=False):
        key = digest({field: route[field] for field in ('repository', 'number', 'revision')})
        owner = route['owner']
        with closing(sqlite3.connect(self.path)) as db, db:
            db.execute('CREATE TABLE IF NOT EXISTS investigation_claims ('
                       'key TEXT PRIMARY KEY, owner TEXT NOT NULL, attempt INTEGER NOT NULL, '
                       'state TEXT NOT NULL, receipt TEXT)')
            db.execute('CREATE TABLE IF NOT EXISTS forced_investigations (key TEXT PRIMARY KEY)')
            db.execute('BEGIN IMMEDIATE')
            row = db.execute('SELECT owner, attempt, state FROM investigation_claims WHERE key=?',
                             (key,)).fetchone()
            if row:
                if row[0] != owner or row[2] == 'running':
                    return None
                if row[2] == 'completed':
                    if not force_completed or not db.execute(
                            'INSERT OR IGNORE INTO forced_investigations VALUES (?)', (key,)).rowcount:
                        return None
                elif row[2] != 'failed' or not retry_failed:
                    return None
                attempt = row[1] + 1
                db.execute("UPDATE investigation_claims SET attempt=?, state='running' WHERE key=?",
                           (attempt, key))
            else:
                attempt = 1
                # A prior published result may predate this store.
                if force_completed:
                    db.execute('INSERT INTO forced_investigations VALUES (?)', (key,))
                db.execute("INSERT INTO investigation_claims VALUES (?, ?, ?, 'running', NULL)",
                           (key, owner, attempt))
        return key, attempt

    def settle(self, claim, receipt=None):
        with closing(sqlite3.connect(self.path)) as db, db:
            return db.execute(
                "UPDATE investigation_claims SET state=?, receipt=? WHERE key=? AND attempt=? AND state='running'",
                ('completed' if receipt else 'failed', receipt, *claim)).rowcount == 1


    def record_artifact(self, claim, draft_digest, artifact_id):
        """Correlate storage confirmation with the current draft, never publish it."""
        if not re.fullmatch(r'[a-f0-9]{64}', draft_digest) or not re.fullmatch(
                r'[A-Za-z0-9_-]{1,100}', artifact_id):
            raise ValueError('Invalid draft or storage receipt identity')
        with closing(sqlite3.connect(self.path)) as db, db:
            db.execute('CREATE TABLE IF NOT EXISTS draft_artifacts ('
                       'key TEXT NOT NULL, attempt INTEGER NOT NULL, artifact_id TEXT NOT NULL, '
                       'PRIMARY KEY (key, attempt))')
            db.execute('BEGIN IMMEDIATE')
            state = db.execute('SELECT state, receipt FROM investigation_claims WHERE key=? AND attempt=?',
                               claim).fetchone()
            if state != ('completed', draft_digest):
                return False
            db.execute('INSERT OR IGNORE INTO draft_artifacts VALUES (?, ?, ?)', (*claim, artifact_id))
            return db.execute('SELECT artifact_id FROM draft_artifacts WHERE key=? AND attempt=?',
                              claim).fetchone() == (artifact_id,)
