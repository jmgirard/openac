# Lessons

Durable repo lessons — build quirks, testing tricks, gotchas worth
remembering next time — captured at milestone end and surfaced at plan time.
Not status, not decisions: a lesson is a reusable "how this repo actually
behaves" note. Cross-cutting *choices* still go to `DECISIONS.md`.

One line per lesson: `- YYYY-MM-DD (M<NN>): <lesson>`. One cap
(tracking-rules weight-caps): 50 lines, met by retiring or pruning entries.
Current knowledge: a lesson proven false is corrected in place (D-045).
Lessons also *leave*: one retires when a test fails on the mistake it warns
about, when another file's slot owns its content, or when a matured family
graduates whole into a doctrine module; pruning the stalest is the last resort
rather than the first (D-051, D-055).
