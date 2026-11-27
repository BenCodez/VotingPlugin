"""Validate report structure independently of model status."""
import re

from engine import digest

REPORT_SECTIONS = ('Finding', 'Evidence', 'Verification', 'Suggested next step', 'Limitations')
HEADING = re.compile(r'(?m)^(?:\*\*(' + '|'.join(REPORT_SECTIONS) +
                     r')\*\*:?|#{1,6} +(' + '|'.join(REPORT_SECTIONS) + r'):?(?=[ \t]*\r?$))[ \t]*')


def report_sections(report):
    sections = list(HEADING.finditer(report))
    if tuple(match.group(1) or match.group(2) for match in sections) != REPORT_SECTIONS:
        raise RuntimeError('Report is missing required evidence sections')
    contents = [report[match.end(): (sections[i + 1].start() if i + 1 < len(sections) else len(report))]
                for i, match in enumerate(sections)]
    if any(len(re.findall(r'[A-Za-z]{2,}', body)) < 3 or re.fullmatch(
            r'[\s*_<>{}\[\]().-]*(?:todo|tbd|placeholder|n/a|pending|none|same as above)[\s*_<>{}\[\]().-]*',
            body, re.I) for body in contents):
        raise RuntimeError('Report contains an incomplete evidence section')
    if report[:sections[0].start()].strip():
        raise RuntimeError('Report contains unexpected material before its sections')
    if len({re.sub(r'[^a-z0-9]+', ' ', body.casefold()).strip()
            for body in contents}) != len(contents):
        raise RuntimeError('Report repeats the same content across evidence sections')
    return dict(zip(REPORT_SECTIONS, contents))


def report_fingerprint(report):
    normalized = []
    for body in report_sections(report).values():
        # Ignore formatting, case and ephemeral output artifact link targets.
        body = re.sub(r'\]\((?:</workspace/outputs/[^>]+>|/workspace/outputs/[^)]+)\)', ']', body)
        normalized.append(' '.join(body.casefold().split()))
    return digest(normalized)


# An exact reference anchors the model's observation to runner-known inputs.
# It does not demonstrate that the cited file exists or that a test was run.
def report_completion(report, provenance, verify_source=None):
    sections = report_sections(report)
    repo = re.escape(provenance['repository'])
    root = rf'https://github\.com/{repo}'
    evidence = re.sub(r'(?ms)^ {0,3}(`{3,}|~{3,})[^\n]*\n.*?(?:^ {0,3}\1[`~]*[ \t]*(?:\n|$)|\Z)',
                      '', sections['Evidence'])
    evidence = re.sub(r'(?<!`)(`+)(?!`).*?(?<!`)\1(?!`)', '', evidence, flags=re.S)
    evidence = re.sub(r'(?m)^(?: {4}| {0,3}\t)[^\n]*', '', evidence)
    destinations = re.findall(r'(?<![!\\])\[[^\]\n]+\]\((https://[^\s()<>]+)\)', evidence)
    source = re.compile(root + r'/blob/([a-f0-9]{40})/([A-Za-z0-9_./-]+)(?:#L[0-9]+(?:-L[0-9]+)?)?', re.I)
    refs = {match.groups() for url in destinations if (match := source.fullmatch(url))}
    refs = sorted((sha, path) for sha, path in refs
                  if sha in {provenance['development_sha'], provenance.get('reported_commit')}
                  and all(part not in {'', '.', '..'} for part in path.split('/')))
    if len(refs) > 4:
        raise RuntimeError('Report exceeds the source reference limit')
    if verify_source and any(not verify_source(sha, path) for sha, path in refs):
        raise RuntimeError('Report cites a source file that could not be verified')
    commits = {sha for sha, _ in refs}
    number = re.escape(str(provenance['number']))
    issue = any(re.fullmatch(root + rf'/issues/{number}', url, re.I) for url in destinations)
    mode = provenance['mode']
    anchor = {'bug': bool(commits), 'docs': bool(commits),
              'feature': bool(commits) or issue, 'support': bool(commits) or issue,
              'insufficient': issue and bool(re.search(
                  r'\b(?:missing|unavailable|unknown|not provided|not reported|cannot verify)\b',
                  sections['Limitations'], re.I))}
    if not anchor.get(mode):
        raise RuntimeError('Report lacks corroborated evidence for its investigation mode')
    return {'mode': mode, 'source_commits_cited': sorted(commits), 'issue_cited': issue,
            'source_claim_provenance': 'model-reported', 'runtime_verified': False}
