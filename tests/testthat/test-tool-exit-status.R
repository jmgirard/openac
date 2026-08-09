# A tool that exited non-zero is a failed file (M17) ---------------------------
#
# `system2(stdout = TRUE, stderr = TRUE)` reports a non-zero exit in a `status`
# attribute rather than by erroring, so before M17 an ffmpeg, openSMILE or
# OpenFace failure was invisible to its caller: `run_tool()` returned the
# output verbatim and every per-file wrapper returned normally, which made
# `dir_walk()` record the file as a success. `ffp_count_streams()` was the only
# place in the package that read one.
#
# MEASURED 2026-08-08 (R 4.6.1, macOS 15, ffmpeg 8.0): a SUCCESSFUL run sets no
# `status` attribute at all -- `attr(out, "status")` is NULL, not 0 -- and a
# failing one sets 254 for a missing input. Both facts are load-bearing: a
# check written as `status != 0` errors on every successful call, and one
# written as `identical(status, 0L)` fails every successful call too.

local_media <- function(ext = ".wav", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

# A conforming openSMILE input, as os_check_audio() sees it (two ffprobe
# queries: the stream count, then the codec/rate/channel triple).
conforming <- function() list("audio", c("pcm_s16le", "44100", "1"))

# --- the low-level passthroughs are deliberately NOT checked ------------------

test_that("every passthrough and alias returns a non-zero exit rather than erroring", {
  # The check belongs in the callers that know which file is being processed,
  # not in `run_tool()`, which does not. These eight bindings are the
  # documented low-level escape hatch -- "a character vector containing the
  # output of X" -- and a user driving a tool by hand gets to see what it said
  # and decide for themselves.
  #
  # All eight, not the four canonical spellings: `ffm`, `ffp`, `of` and `os`
  # are separate bindings to the same closures (D-010 measured that rebinding
  # `ffmpeg` does not intercept `ffm`), so asserting four of them leaves half
  # the exported surface unpinned.
  calls <- list(
    ffmpeg, ffm, ffprobe, ffp, openface, of, opensmile, os
  )
  state <- local_fake_tools(
    results = rep(list(fake_nonzero_exit(status = 3L)), length(calls))
  )

  for (i in seq_along(calls)) {
    out <- suppressWarnings(calls[[i]]("-version"))
    expect_identical(attr(out, "status"), 3L)
  }
  expect_length(boundary_calls(state), length(calls))
})

test_that("run_checked() forwards the command it was given, quoted at the boundary", {
  # The command test D-010's computed closure demands of any function that can
  # reach an external tool. `run_checked()` builds no command of its own -- it
  # wraps `run_tool()` -- so what it owes is a demonstration that it passes one
  # through untouched, tokens and quoting intact (D-017, M13).
  infile <- local_media(".mp4")
  state <- local_fake_tools(results = list("ok"))

  out <- openac:::run_checked("ffmpeg", c("-i", "my clip.mp4"), infile)

  expect_identical(boundary_tools(state), "ffmpeg")
  expect_identical(boundary_argv(state)[[1]], shQuote(c("-i", "my clip.mp4")))
  expect_identical(out, "ok")
})

# --- the per-file wrappers abort, naming the file ----------------------------

# Every assertion below reads a whitespace-COLLAPSED message. `cli_abort()`
# bakes hard line breaks into `conditionMessage()` at the console width, so
# whether "ffmpeg exited with status 1" survives intact depends on how long the
# interpolated path happens to be -- the M14 review A3 trap, which passed
# locally only because macOS `tempdir()` is long.
collapsed_error <- function(expr) {
  gsub("\\s+", " ", conditionMessage(expect_error(expr, class = "openac_tool_failed")))
}

test_that("os_prep_audio() aborts when ffmpeg exits non-zero, naming the file", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  local_fake_tools(results = list(fake_nonzero_exit(status = 254L)))

  msg <- collapsed_error(os_prep_audio(infile, outfile))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "ffmpeg exited with status 254.", fixed = TRUE)
})

test_that("aw_prep_audio() aborts when ffmpeg exits non-zero, naming the file", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  # One audio stream from the probe, then a failing conversion.
  local_fake_tools(results = list("audio", fake_nonzero_exit(status = 254L)))

  msg <- collapsed_error(aw_prep_audio(infile, outfile))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "ffmpeg exited with status 254.", fixed = TRUE)
})

test_that("os_extract_wav() aborts when openSMILE exits non-zero, naming the file", {
  infile <- local_media()
  local_fake_tools(
    results = c(conforming(), list(fake_nonzero_exit(status = 1L)))
  )

  msg <- collapsed_error(openac:::os_extract_wav(infile))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "opensmile exited with status 1.", fixed = TRUE)
})

test_that("of_extract() aborts when OpenFace exits non-zero, naming the file", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  local_fake_tools(results = list(fake_nonzero_exit(status = 11L)))

  msg <- collapsed_error(of_extract(infile, outfile))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "openface exited with status 11.", fixed = TRUE)
})

test_that("os_extract() names the user's file, not the temp wav it converted to", {
  # The exported function NEWS actually names. When the input does not already
  # conform, `os_extract()` converts it to a `tempfile()` and hands THAT to
  # `os_extract_wav()`, so a message built from the wav names a path that no
  # longer exists and that the user never chose -- and it is this string that
  # lands in `os_extract_dir()`'s `error` column, which NEWS tells them to read
  # and re-run from. M17 shipped it that way and was returned for it.
  #
  # The AC2 sibling above drives `os_extract_wav()` directly, where the two
  # files are the same, so it cannot see this. This test is the exported path.
  infile <- local_media(".mp4")
  aggfile <- file.path(withr::local_tempdir(), "agg.csv")
  # An explicit `wavfile` rather than the `tempfile()` default, because the
  # mocked ffmpeg writes nothing and `os_extract_wav()`'s own existence guard
  # would trip before openSMILE is ever reached. It is still a DIFFERENT file
  # from `infile`, which is the whole discriminator: the message must name the
  # mp4 the user passed, not the wav openac derived.
  wavfile <- local_media(".wav")
  # Queue, in call order: os_check_audio() says no (a video stream is present)
  # in two ffprobe queries; ffmpeg converts; os_extract_wav()'s own
  # os_check_audio() says yes in two more; then openSMILE fails.
  local_fake_tools(results = c(
    list("video audio", c("aac", "44100", "2")),
    list("ok"),
    conforming(),
    list(fake_nonzero_exit(status = 1L))
  ))

  msg <- collapsed_error(
    os_extract(infile, wavfile = wavfile, aggfile = aggfile)
  )

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_no_match(msg, "\\.wav")
})

# --- the error path: run_tool() itself aborting ------------------------------

test_that("the set_program() hint survives an abort from the three wrappers that can reach it", {
  # `run_tool()` does not only return -- it ABORTS when `require_program()`
  # cannot resolve the tool, and `find_program()` warns with the
  # `set_program()` pointer on its way there, from inside the region where
  # warnings are held pending the exit status. Releasing them as the error
  # unwinds is the whole reason `run_checked()`'s `tryCatch` sits OUTSIDE the
  # `withCallingHandlers`.
  #
  # M17 shipped the nesting inverted, which made the release loop run while
  # the calling handler was still established: every released warning was
  # re-captured and muffled, and all four wrappers lost the hint. Every other
  # test in this file drives a RETURNED status and stayed green through it.
  # This is the counterpart to the sibling's pin at
  # test-commands-probe.R:258-273 ("It was, until this test"), which existed
  # for `ffp_count_streams()` and had none here.
  # Each case must actually REACH `run_checked()`, which is not automatic:
  # `aw_prep_audio()` counts streams first, so resolving nothing at all makes
  # it abort inside `ffp_count_streams()` -- re-testing M14's already-correct
  # handler and never touching this one. MEASURED at M17's round-2 review: a
  # first cut of this test resolved nothing and reached the code under test in
  # only two of its four cases while claiming all four. So the ffprobe-dependent
  # case resolves ffprobe and starves only its own tool, and carries the
  # `results` its preceding probe consumes.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  outcsv <- file.path(withr::local_tempdir(), "faces.csv")

  cases <- list(
    list(
      label = "os_prep_audio",
      resolve = character(),
      results = list(),
      run = function() os_prep_audio(infile, outfile)
    ),
    list(
      label = "aw_prep_audio",
      resolve = "ffprobe",
      results = list("audio"),
      run = function() aw_prep_audio(infile, outfile)
    ),
    # `os_extract_wav()` is deliberately absent, and cannot be added: it calls
    # `os_check_config()`, which resolves the config directory relative to the
    # openSMILE binary, so an unresolvable openSMILE aborts there -- before
    # `run_checked()` is reached and with a different message. `run_tool()`'s
    # not-found abort is unreachable from that wrapper, so three is the whole
    # domain here, not a sample of four. Its non-zero-exit path is covered by
    # its own test above.
    list(
      label = "of_extract",
      resolve = character(),
      results = list(),
      run = function() of_extract(infile, outcsv)
    )
  )

  for (case in cases) {
    local_fake_tools(resolve = case$resolve, results = case$results)
    # `info` names the wrapper: a bare loop over closures reports every case at
    # the same source line, so a red says nothing about which one broke.
    expect_warning(
      expect_error(case$run(), "could not be found", info = case$label),
      "set_program",
      info = case$label
    )
  }
})

# --- what the tool said reaches the message ----------------------------------

test_that("the tool's own last output lines reach the message", {
  # Without this the batch table says only that a file failed, which is a
  # shrug rather than a report: the caller still has to re-run the file by
  # hand to learn why. Capped at the last few lines so one bad file cannot
  # flood a data frame column.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  local_fake_tools(results = list(fake_nonzero_exit(
    status = 254L,
    output = c("line one", "line two", "Invalid data found when processing input")
  )))

  msg <- collapsed_error(os_prep_audio(infile, outfile))

  expect_match(msg, "Invalid data found when processing input", fixed = TRUE)
})

test_that("output containing brace characters is not interpolated as cli markup", {
  # The tool's output is untrusted text. Passed into a cli format string it
  # would be read as glue interpolation, so `{foo}` in an ffmpeg filter error
  # would abort inside the error handler -- a failure while reporting a
  # failure, which is how a batch loses the row it was trying to record.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  local_fake_tools(results = list(fake_nonzero_exit(
    output = "No such filter: '{nope}'"
  )))

  msg <- collapsed_error(os_prep_audio(infile, outfile))

  expect_match(msg, "{nope}", fixed = TRUE)
})

# --- the status comes from the attribute, never from R's warning text --------

test_that("a non-English status warning still produces the error", {
  # R TRANSLATES its own exit-status warning, so a check keyed on the English
  # "had status" is green on an English host and silently dead everywhere else
  # (M14 review A1). `fake_nonzero_exit()` defaults to the French wording
  # precisely so this test reds against that implementation and passes only
  # against one reading the `status` attribute.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  local_fake_tools(results = list(fake_nonzero_exit(status = 1L)))

  expect_error(os_prep_audio(infile, outfile), class = "openac_tool_failed")
})

test_that("R's own status warning is suppressed, and nothing else is", {
  # The error below REPLACES R's status report, so re-signalling it would show
  # the user the same failure twice. Everything else raised during the call is
  # a diagnostic the caller should still see -- an earlier cut of the sibling
  # code in ffp_count_streams() dropped all of them, which made a failed tool
  # the one path where a warning could vanish (M14 fix-delta review F1).
  #
  # Which warning is R's cannot be decided from its text -- that is the locale
  # trap above -- so it is decided by POSITION: R warns about the exit status
  # after the command has returned, so its warning is the LAST one raised.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  chatty <- function(command, args) {
    warning("deprecated pixel format")
    warning("l'exécution de la commande a renvoyé un statut 1")
    structure("boom", status = 1L)
  }
  local_fake_tools(results = list(chatty))

  seen <- collect_warnings(
    expect_error(os_prep_audio(infile, outfile), class = "openac_tool_failed")
  )

  expect_true(any(grepl("deprecated pixel format", seen, fixed = TRUE)))
  expect_false(any(grepl("statut", seen, fixed = TRUE)))
})

test_that("a successful run sets no status attribute and is not mistaken for a failure", {
  # The measured asymmetry at the top of this file: ffmpeg's own successful
  # exit leaves `attr(out, "status")` NULL. A check comparing a NULL status
  # against 0 would abort every successful conversion in the package.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  state <- local_fake_tools(results = list("frame= 100 fps=0.0"))

  expect_no_error(os_prep_audio(infile, outfile))
  expect_identical(boundary_tools(state), "ffmpeg")
})

test_that("a zero-length status is a failure, not a vacuous success", {
  # `all(integer(0) == 0)` is TRUE, so a zero-length status would read as a
  # clean exit under a bare `all()`. `system2()` sets no such attribute today;
  # this costs one comparison and removes the question -- the same vacuous
  # truth ffp_count_streams() guards against at R/use_ffprobe.R.
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "out.wav")
  empty <- function(command, args) structure("boom", status = integer(0))
  local_fake_tools(results = list(empty))

  expect_error(os_prep_audio(infile, outfile), class = "openac_tool_failed")
})
