"""Shared issue evidence and profile policy. No issue data becomes an executable command."""
import hashlib
import json
from pathlib import Path
import re
from urllib.parse import quote

PROFILES = json.loads(Path(__file__).with_name('profiles.json').read_text())
DEFAULT = 'VotingPlugin'
SHA = re.compile(r'[a-f0-9]{40}')
STABLE = re.compile(r'\d{1,3}\.\d{1,3}(?:\.\d{1,3})?')
REPORT_MARKER = re.compile(
    r'<!-- issue-investigation:v1:([1-9][0-9]*):([a-f0-9]{64}):([a-f0-9]{64}) -->\n')
LEGACY_MARKER = re.compile(r'<!-- votingplugin-issue-investigation:([1-9][0-9]*) -->\n')


def profile_for(repository, name=None):
    if name is None:
        name = repository.rsplit('/', 1)[-1]
    profile = PROFILES.get(name)
    if not profile or profile['repository'].casefold() != repository.casefold():
        raise RuntimeError('Repository does not match an investigator profile')
    return profile


def is_bot(user):
    return user.get('type') == 'Bot' or user.get('login', '').lower().endswith('[bot]')


def own_report(comment):
    return (comment.get('user') or {}).get('login') == 'github-actions[bot]'


def github_snapshot(issue, comment_rows, event_rows):
    """Normalize one full GitHub issue snapshot for either ingress."""
    if 'pull_request' in issue:
        raise RuntimeError('Choose an issue, not a pull request')
    comments, markers = [], []
    for row in comment_rows:
        user = row.get('user') or {}
        if own_report(row):
            body = row.get('body') or ''
            marker = REPORT_MARKER.match(body) or LEGACY_MARKER.match(body)
            if marker:
                markers.append(marker.groups())
        if not is_bot(user):
            comments.append({'user': user.get('login'), 'body': row.get('body'),
                             'url': row.get('html_url'), 'id': row.get('id'),
                             'association': row.get('author_association')})
    # GitHub IDs are the shared ordering key, independent of delivery order.
    comments.sort(key=lambda row: row['id'] or 0)
    reopened = max((row['id'] for row in event_rows if row.get('event') == 'reopened'), default=None)
    return {'url': issue.get('html_url'), 'title': issue.get('title'),
            'body': issue.get('body'), 'state': issue.get('state'),
            'author': (issue.get('user') or {}).get('login'),
            'type': (issue.get('type') or {}).get('name'),
            'labels': [label['name'] for label in issue.get('labels', [])],
            'comments': comments, 'reopened': reopened, 'report_markers': markers}


def has_accepted_revision(snapshot, revision):
    """The normalized snapshot supplies only authenticated Actions-bot markers."""
    return any(len(marker) == 3 and marker[1] == revision
               for marker in snapshot.get('report_markers', ()))


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, ensure_ascii=True,
                                     separators=(',', ':')).encode()).hexdigest()


def normalized(text):
    lines = (text or '').splitlines()
    return '\n'.join(line.rstrip() for line in lines).strip('\n')


def evidence_revision(issue):
    # IDs distinguish deleted, edited and replacement comments. Omit timestamps
    # so deliveries and whitespace-only edits do not invalidate the same evidence.
    return digest({
        'title': normalized(issue.get('title')), 'body': normalized(issue.get('body')),
        'state': issue.get('state'), 'reopened': issue.get('reopened'),
        'type': issue.get('type'), 'labels': sorted(issue.get('labels') or []),
        'comments': [(row.get('id'), row.get('user'), row.get('association'),
                      normalized(row.get('body'))) for row in issue.get('comments', [])
                     if normalized(row.get('body'))],
    })


def classify(issue, profile):
    if profile['modes'] == ['docs']:
        return 'docs'
    metadata = ' '.join([issue.get('type') or '', *(issue.get('labels') or [])]).lower()
    opening = ' '.join(issue.get(key) or '' for key in ('title', 'body')).lower()
    discussion = ' '.join(row.get('body') or '' for row in issue.get('comments', [])
                          if row.get('association') in {'OWNER', 'MEMBER', 'COLLABORATOR'}).lower()
    rules = (
        ('docs', r'\b(?:documentation|docs?)\b', r'\b(?:typo|broken link|documentation (?:error|update))\b'),
        ('feature', r'\b(?:feature|enhancement)\b', r'\b(?:feature request|add support for|would be handy)\b'),
        ('bug', r'\b(?:bug|regression)\b', r'\b(?:describe the bug|exception|stack trace|regression|crashes|fails|does not work)\b'),
        ('support', r'\b(?:support|question|configuration)\b', r'\b(?:how (?:do|can|to)|how should|configure|configuration help|where did you set)\b'),
    )
    for text, index in ((metadata, 1), (opening, 2), (discussion, 2)):
        for rule in rules:
            if re.search(rule[index], text):
                return rule[0]
    return 'insufficient'


def version_mentions(issue, name):
    pattern = re.compile(
        r'\b' + re.escape(name) + r'(?:\s+version)?\s*[:=]?\s*v?'
        r'(\d{1,3}\.\d{1,3}(?:\.\d{1,3})?'
        r'(?:[-+][A-Za-z0-9](?:[A-Za-z0-9._+-]{0,62}[A-Za-z0-9])?)?)'
        r'(?![A-Za-z0-9_+-]|\.[A-Za-z0-9._+-])',
        re.IGNORECASE)
    fields = [('body', issue.get('body'), False), ('title', issue.get('title'), False)]
    for row in issue.get('comments', []):
        trusted = (row.get('association') in {'OWNER', 'MEMBER', 'COLLABORATOR'}
                   or (issue.get('author') and row.get('user') == issue['author']))
        fields.append((row.get('url') or 'comment', row.get('body'), trusted))
    mentions = []
    for source, value, trusted in fields:
        if not isinstance(value, str):
            continue
        for match in pattern.finditer(value):
            line = value[value.rfind('\n', 0, match.start()) + 1:match.start()].strip()
            correction = bool(trusted and re.fullmatch(
                r'(?:version correction\s*[:=]?|(?:i am |i.m )?actually running)', line, re.I))
            mentions.append({'version': match.group(1), 'source': source, 'correction': correction})
    return mentions


def version_evidence(issue, name, resolve_ref):
    mentions = version_mentions(issue, name)
    corrections = [row for row in mentions if row['correction']]
    selected = corrections[-1:] or mentions
    values = list({row['version'].lower(): row['version'] for row in selected}.values())
    result = {'mentions': mentions, 'reported_version': values[0] if len(values) == 1 else None,
              'status': 'unverified', 'tag': None, 'commit': None,
              'artifact_sha256': None}
    if len(values) != 1 or not STABLE.fullmatch(values[0]):
        return result
    refs = [(tag, resolve_ref(tag)) for tag in (values[0], 'v' + values[0])]
    matches = [(tag, sha) for tag, sha in refs if sha and SHA.fullmatch(sha)]
    if matches and len({sha for _, sha in matches}) == 1:
        result.update(status='source-tag-verified', tag=matches[0][0], commit=matches[0][1])
    return result


def evidence(issue, profile, sha, resolve_ref=lambda tag: None):
    if not SHA.fullmatch(sha):
        raise RuntimeError('Invalid checkout SHA')
    name = profile['checkout']
    repository = profile['repository']
    return {
        'repository': repository, 'profile': name, 'issue': issue.get('url'),
        'mode': classify(issue, profile), 'revision': evidence_revision(issue),
        'development_sha': sha, 'reported': version_evidence(issue, name, resolve_ref),
        'dependencies': {peer: version_evidence(issue, peer, lambda tag: None)
                         for peer in profile['related'] if version_mentions(issue, peer)},
        'source_references': [], 'commands': [], 'artifacts': [],
        'resources': {'documentation': profile['docs'], 'releases': profile['releases'],
                      'development': profile['development'],
                      'related_issues': f'https://github.com/{repository}/issues?q=' +
                      quote('is:issue'),
                      'related_pull_requests': f'https://github.com/{repository}/pulls?q=' +
                      quote('is:pr')},
    }


GUIDANCE = {
    'feature': 'Review related proposals, current functionality, likely components, feasibility, and acceptance criteria. Consider later requests and maintainer confirmations. Ask for a version only if it changes the feature assessment.',
    'bug': 'Investigate the reported source first. A fix or feature availability claim requires pinned source evidence and explicit verification limits. Check related issues or PRs before crossing repository boundaries.',
    'support': 'Use the actual configuration keys, compatibility constraints, and documented checks. Ask for only the environment details that determine the answer.',
    'docs': 'Verify the affected documentation, page revision and linked source. Provide a concrete correction without running the software.',
    'insufficient': 'Ask only material questions that distinguish the possible causes. Avoid speculative fixes.',
}


def route_revision(repository, number, revision, owner):
    if owner not in {'github', 'cezar'} or not re.fullmatch(r'[a-f0-9]{64}', revision):
        raise RuntimeError('No explicit owner for the evidence revision')
    canonical = next((profile['repository'] for profile in PROFILES.values()
                      if profile['repository'].casefold() == repository.casefold()), None)
    number = str(number)
    if not canonical or not re.fullmatch(r'[1-9][0-9]{0,8}', number):
        raise RuntimeError('Invalid repository or issue identity')
    return {'owner': owner, 'repository': canonical, 'number': number, 'revision': revision}
