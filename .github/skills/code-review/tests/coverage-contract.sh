#!/usr/bin/env bash
set -euo pipefail

skill=${1:-"$(cd "$(dirname "$0")/.." && pwd)/SKILL.md"}

grep -Fq 'Coverage describes the review scope, not finding completeness.' "$skill"
grep -Fq 'Preserve `Coverage: partial` for an explicitly scoped, focused review' "$skill"
grep -Fq 'findings do not upgrade partial coverage' "$skill"
if grep -Fq 'otherwise mark coverage complete.' "$skill"; then
	echo 'stale unconditional complete-coverage guidance remains' >&2
	exit 1
fi

echo 'coverage contract: ok'
