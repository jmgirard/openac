# M21: A site that knows its version, and deploys that leave nothing behind

**Status:** done (2026-10-06, PR #23 https://github.com/jmgirard/openac/pull/23)

**Goal:** Give development documentation its own home under `/dev/`, and make
both deploy lanes replace what they publish instead of merging onto it.

**Outcome:** `development: mode: auto` in `_pkgdown.yml`. pkgdown prefixes
`dev/` for `devel` mode alone, so `0.1.0.9000` builds into `docs/dev` and
`0.1.0` into the root. `Pick the deploy lane` reads `[ -d docs/dev ]` into
`lane=dev` or `lane=release`. Dev deploys `docs/dev` to `dev/`, release
deploys `docs` to the root with `clean-exclude: dev`, both `clean: true`. The
version decides, not the event, so release prep publishes to the root on an
ordinary push. Two real runs of a throwaway copy measured both lanes on
`gh-pages`, each cleaning only within its target. `clean-exclude: dev` spares
a `dev` path at any depth. The shipped root target was never measured, and
the root keeps its development build until the first release build.

**Decisions:** D-021 (placement by version), D-022 (roxygen 8.1.0 re-pin, at
a user stop, no criterion rewording discharging the profile's own gate).

**Review:** Two passes, three fresh lenses each. 35 findings: 14 fixed, 7
follow-up, 12 rejected, 1 surfaced, 1 an amendment return. Main fix: NEWS
claimed a default-branch push no longer overwrites the root, false for
release prep. Four criteria amended. AC3 and AC4 hit the second-re-audit stop.
