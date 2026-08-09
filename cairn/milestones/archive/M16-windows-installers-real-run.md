# M16: The Windows installers, actually run

**Status:** done (2026-08-08, PR #16 https://github.com/jmgirard/openac/pull/16)

**Goal:** Run the three Windows installers against the live network on a real
Windows host, so their pinned URLs are known to work rather than assumed.

**Outcome:** Two of three installers were broken and neither said so. All four
OneDrive patch-expert links answered HTTP 200 with a 34 KB `login.live.com`
sign-in page while `install_openface_win()` returned `TRUE` onto four HTML
documents named `.dat`; `install_opensmile_win()` pinned an asset name the
v3.0.2 release never carried. Repointed to the Dropbox links OpenFace's own
`download_models` scripts try first — openac had copied only upstream's
fallback — and to `opensmile-3.0.2-windows-x86_64.zip`. New `download_model()`
refuses a model below a 40 MB floor or opening with markup (`raw_is_markup()`
is the rule's one home, `starts_with_markup()` its file view), warns through
`cli`, and names every missing model. Opt-in `test-installers-real.R` re-probes
all nine pinned URLs, off in CI.

**Decisions:** D-018 (`curl` to Suggests for the ranged-GET probe).

**Review:** Two passes. The first returned the milestone — the changelog gate
failed and the dependency gate had never been held — and actioned F3 (markup
sniff in three copies) and F10 (base `warning()` where DESIGN mandates `cli`).
The second found 15, none ≥80; N10 and N13 at 78 became candidate rows.
