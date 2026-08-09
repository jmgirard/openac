# M17: A tool that exited non-zero is a failed file

**Status:** done (2026-08-09, PR #18 https://github.com/jmgirard/openac/pull/18)

**Goal:** Make a non-zero exit from ffmpeg, openSMILE or OpenFace an error naming
the file, the tool and the status, so a batch records that file as a failed row.

**Outcome:** New internal `run_checked(program, arg, infile)` (`R/run_tool.R`)
reads the `status` attribute `system2(stdout = TRUE, stderr = TRUE)` sets and
aborts with class `openac_tool_failed`, naming file, program, status and the
tool's last three lines. Wired into `os_prep_audio()`, `aw_prep_audio()`,
`os_extract_wav()`, `of_extract()`; the eight passthrough bindings still return
`system2()`'s value raw, pinned by test. `os_extract_wav()` gained `source`, so
`os_extract()` names the user's file, not the temp wav. A successful run sets NO
`status` attribute — NULL, not 0 (MEASURED) — hence `!is.null()` first. The
KNOWN GAP's `os_prep_audio_dir()` half is now a contract; the rest is M18's.

**Decisions:** none milestone-local; the check-in-the-callers choice and its
rejected alternative (one check in `run_tool()`, which cannot name the file) are
work-log entries.

**Review:** Two rounds, seven actioned, all mutation-verified, 26 logged. Round 1
returned it on A (93) — the nesting inversion that swallowed the `set_program()`
hint — plus D (85), B (84), F (82). Round 2 read the fix delta: 1 (90) a test
claiming four wrappers and reaching two, 4 (90) and 5 (83) false DESIGN claims.
