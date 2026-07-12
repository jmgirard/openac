# Design-interview banked-candidates ledger (transient)

_Scope: proto-principles banked during the /design-interview facts phase of
2026-07-11 (deepening pass; original principles pass is D-003). Consumed and
deleted by Phase 2 (principles). Owns nothing another cairn file owns._

## Banked from Phase 1 answers

1. **Two-layer testing contract** (GP candidate) — binary-dependent code is
   tested at two layers: command-string construction verified everywhere
   (mock/inspect the `system2` boundary; pairs with GP5 — an inspectable
   command is a testable command), plus real end-to-end integration tests
   gated by skip-if-binary-missing + `skip_on_cran`.
2. **Run-time version capture** (GP candidate; GP5 sibling) — extraction
   functions record the external tool's reported version and surface it
   (output attributes/log) for methods reporting and drift debugging.
   Capability also filed as a ROADMAP candidate.
3. **GP1 scope refinement** — post-read aggregation/derivation helpers are
   in scope but each earns its place as a ROADMAP candidate (mirrors the
   opportunistic tool roster); the reader is the guaranteed surface.
   Phase 2: decide whether GP1's wording needs tightening.

## Settled as facts at the seam (no Phase 2 action unless stress-tests bite)

- CRAN quality bar written into DESIGN Purpose & Scope (testing contract +
  audio.whisper resolution; not date- or 1.0-gated).
- Overwrite-`TRUE` family convention written into DESIGN Conventions.
- OpenFace posture (keep + scout) written into Known issues + candidate.
- Three new warts recorded (OneDrive URLs, audio.whisper volatility,
  global-only config).

## Phase 2 must still add its own candidates

- Mine git history for implicit principles (e.g., the media-validation fix
  streak: stream counting, audio-less files, video-vs-audio checks — is
  there a "validate media before compute" principle behind GP6?).
- Derive from the domain (reproducibility/methods-reporting; anything the
  IRB/consent context implies beyond IP3).
- Stress-test: two-layer testing vs. GP4 (lean deps — mocking helpers?);
  version capture vs. GP1 (thin wrappers); per-project config wart vs. IP1.
