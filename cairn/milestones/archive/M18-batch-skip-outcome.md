# M18: A skipped file is a skip, not a success

**Status:** done (2026-08-09, PR #19 https://github.com/jmgirard/openac/pull/19)

**Goal:** Give the batch outcome table a third state, so a file the batch
deliberately did not process is recorded as skipped rather than as a success.

**Outcome:** `dir_walk()` gains a `status` column — `"ok"`/`"skipped"`/
`"failed"` — with `success` now `status == "ok"` and `error` carrying a skip's
reason as it did a failure's. `skip_file()` signals a non-error
`openac_file_skipped`; `absorb_skip()` stops one raised by a nested prep call,
so the two `*_prep_audio_dir()` record `"skipped"` while `os_extract_dir()`/
`aw_transcribe_dir()` reuse the wav and still run their tool. `aw_transcribe()`
splits its branch: no audio stream skips, an unprobeable file aborts.

**Decisions:** Both cross-cutting choices promoted to D-019 — the non-error
condition class as the skip channel, and a nested skip stopping at the call
that raised it. None milestone-local.

**Review:** Two rounds. Round 1 actioned F1 (90)/F2 (88) — a nested
`overwrite = FALSE` skip unwound the entire per-file job, so openSMILE and
whisper never ran — and F3 (95), a literal `#'` leaked into all five rendered
`@return` blocks; F1 returned the milestone under the return floor. Round 2
actioned an `@param`/`@return` contradiction (85) and three vignettes calling
a failure a skip (80); 19 logged below, two clusters became candidates.
