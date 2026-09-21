---
mex:
  id: mx_01M31068M0WJ4KB0ZD6C6BXHZK
  type: decision
  status: promoted
  revision: 1
  title: Defer end-to-end proxy vote delivery guarantee
  sources:
    - type: document
      ref: .mex/inbox/proposal_01M31040E5ECB5F57YHXYGX618.md
      note: "The user explicitly said guaranteed delivery will be added in the future
        while discussing new PR #1619 review comments. This separates current
        local queue reliability fixes from a future protocol design."
      capturedAt: 2026-09-21T03:29:40.432Z
      metadata:
        proposalId: proposal_01M31040E5ECB5F57YHXYGX618
        author:
          kind: git
          name: BenCodez
          email: 17074231+BenCodez@users.noreply.github.com
        approvedBy:
          kind: git
          name: BenCodez
          email: 17074231+BenCodez@users.noreply.github.com
    - type: manual
      note: "User message in this session: 'New comments, will add the guaranteed
        delivery in the future, please remember that'."
      capturedAt: 2026-09-21T03:29:40.432Z
      metadata:
        evidence:
          kind: manual
          note: "User message in this session: 'New comments, will add the guaranteed
            delivery in the future, please remember that'."
  provenance:
    createdBy:
      kind: human
      id: git:139ad6f2a86f1dbf589661a50db20426
    createdAt: 2026-09-21T03:29:40.432Z
---
# Defer end-to-end proxy vote delivery guarantee

For VotingPlugin PR #1619, retain the existing bounded backend queue work but defer an end-to-end guaranteed proxy-to-backend vote delivery protocol to future work. A future design may need proxy retention, backend durable acknowledgement, retry, and restart-safe duplicate handling. Do not treat the current overflow queue or plugin-message send result as a guarantee that every vote will be delivered or each reward will execute exactly once.
