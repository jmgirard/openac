# M18 -- the third outcome state: a file the batch deliberately did not process.
#
# Before M18, `dir_walk()` classified a row only by whether `.f` raised an
# error, so a single-file function that declined a file -- an `overwrite =
# FALSE` output that already exists, a file with no audio stream -- returned
# normally and its row read `success = TRUE, error = NA`. The batch reported
# work it had not done.
#
# The discriminating assertions below are on `status`, never on `success`
# alone: a skip and a failure both read `success = FALSE`, so `success` cannot
# tell "I chose not to" from "I tried and could not", which is the whole
# distinction this milestone adds.

# --- dir_walk(): the three states --------------------------------------------

test_that("dir_walk() records ok, skipped and failed as three distinct states", {
  # One `.f`, three inputs, three dispositions -- so the three rows are
  # produced by the same code path and differ only in what `.f` did.
  .l <- data.frame(
    infile = c("a.mp4", "b.mp4", "c.mp4"),
    stringsAsFactors = FALSE
  )
  .f <- function(infile) {
    if (infile == "b.mp4") openac:::skip_file("nothing to do here")
    if (infile == "c.mp4") stop("the tool exploded")
    invisible(TRUE)
  }

  suppressWarnings(suppressMessages(
    result <- openac:::dir_walk(.l, .f, parallel = FALSE)
  ))

  expect_identical(result$status, c("ok", "skipped", "failed"))
  expect_identical(result$success, c(TRUE, FALSE, FALSE))
  # `error` carries the reason for a skipped row as it already does for a
  # failed one, and stays NA for the row that actually did the work.
  expect_true(is.na(result$error[[1]]))
  expect_identical(result$error[[2]], "nothing to do here")
  expect_match(result$error[[3]], "the tool exploded")
})

test_that("a skipped file is announced but does not warn; a failed one warns", {
  .l <- data.frame(infile = c("a.mp4", "b.mp4"), stringsAsFactors = FALSE)
  .f <- function(infile) {
    if (infile == "a.mp4") openac:::skip_file("already done")
    stop("the tool exploded")
  }

  warnings <- collect_warnings(suppressMessages(
    result <- openac:::dir_walk(.l, .f, parallel = FALSE)
  ))

  # Exactly one warning, for the failure -- a re-run of a finished batch skips
  # every file, and 500 warnings for a batch behaving correctly is noise that
  # buries the one row a caller must act on.
  expect_length(warnings, 1L)
  expect_match(warnings, "b\\.mp4")
  expect_match(warnings, "exploded")
  # The skip still reached the console, naming its file and its reason.
  expect_message(
    suppressWarnings(openac:::dir_walk(.l[1, , drop = FALSE], .f, parallel = FALSE)),
    "a\\.mp4"
  )
  expect_identical(result$status, c("skipped", "failed"))
})

test_that("skip_file() leaves a direct call unaffected when nothing handles it", {
  # The contract that keeps this from being a breaking change: outside a batch
  # no handler is established, the condition is signalled to no one, and the
  # calling function goes on to return what it always returned.
  f <- function() {
    openac:::skip_file("no handler here")
    "Skipped"
  }

  expect_identical(f(), "Skipped")
  expect_no_error(f())
})

# --- the outcome table's shape ------------------------------------------------

test_that("the zero-row table carries the same columns as a populated one", {
  # `dir_walk()` builds the empty case in its own branch, so its column set can
  # drift from the one every other row is written with.
  empty <- openac:::dir_walk(
    data.frame(infile = character(0), stringsAsFactors = FALSE),
    function(infile) invisible(TRUE)
  )
  filled <- suppressMessages(openac:::dir_walk(
    data.frame(infile = "a.mp4", stringsAsFactors = FALSE),
    function(infile) invisible(TRUE),
    parallel = FALSE
  ))

  expect_identical(names(empty), names(filled))
  expect_identical(names(empty), c("infile", "status", "success", "error"))
  expect_identical(nrow(empty), 0L)
})

test_that("a batch where every file succeeds reports no not-processed row", {
  # The counterpart to the three-state test, and the reason the retired
  # `dir_walk_reports_failure()` helper is not missed. That helper treated ANY
  # column beyond the then-known set as evidence of a non-success outcome, so
  # adding `status` would have made it report EVERY table -- including this
  # one -- as carrying a failure. What it was really pinning is asserted here
  # directly: a clean batch is all-ok, and the outcome columns are exactly the
  # three.
  result <- suppressMessages(openac:::dir_walk(
    data.frame(infile = c("a.mp4", "b.mp4"), stringsAsFactors = FALSE),
    function(infile) invisible(TRUE),
    parallel = FALSE
  ))

  expect_identical(result$status, c("ok", "ok"))
  expect_true(all(result$success))
  expect_true(all(is.na(result$error)))
  expect_identical(names(result), c("infile", "status", "success", "error"))
})

test_that("every *_dir() wrapper returns a table carrying a status column", {
  # The wrapper list is COMPUTED, never hand-written: a seventh `*_dir()`
  # function lands in the namespace and reds this test until it is covered
  # here. The domain is `asNamespace()` rather than a grep over `R/*.R`,
  # because an installed package's `R/` holds only the lazy-load database --
  # a source grep matches nothing under `R CMD check` and the test would pass
  # vacuously in the run mode that gates the merge.
  wrappers <- grep("_dir$", ls(asNamespace("openac")), value = TRUE)
  covered <- c(
    "aw_prep_audio_dir",
    "aw_transcribe_dir",
    "of_extract_dir",
    "os_extract_dir",
    "os_prep_audio_dir"
  )
  expect_setequal(wrappers, covered)

  # Each wrapper, run over an empty directory: no tool is invoked, so this
  # exercises the return shape of all five without five tool fixtures.
  indir <- withr::local_tempdir()
  outdir <- file.path(withr::local_tempdir(), "out")
  local_fake_tools(results = list())
  results <- list(
    aw_prep_audio_dir(indir, "mp4", outdir),
    aw_transcribe_dir(
      indir, "mp4",
      model = structure(list(name = "tiny"), class = "whisper")
    ),
    of_extract_dir(indir, "mp4", outdir),
    os_extract_dir(indir, "mp4", aggdir = outdir),
    os_prep_audio_dir(indir, "mp4", outdir)
  )

  for (i in seq_along(results)) {
    expect_true("status" %in% names(results[[i]]), info = covered[[i]])
  }
})

# --- the deliberate-skip sites ------------------------------------------------

test_that("aw_prep_audio_dir() records an overwrite = FALSE skip as skipped", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "a.mp4"))
  outdir <- withr::local_tempdir()
  # The output this batch would produce is already sitting there.
  file.create(file.path(outdir, "a.wav"))

  state <- local_fake_tools(results = list())
  suppressMessages(
    result <- aw_prep_audio_dir(indir, "mp4", outdir, overwrite = FALSE)
  )

  expect_identical(result$status, "skipped")
  expect_identical(result$success, FALSE)
  expect_match(result$error[[1]], "overwrite")
  # It really declined: no probe, no conversion, nothing reached the boundary.
  expect_identical(boundary_tools(state), character(0))
})

test_that("os_prep_audio_dir() records an overwrite = FALSE skip as skipped", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "a.mp4"))
  outdir <- withr::local_tempdir()
  file.create(file.path(outdir, "a.wav"))

  state <- local_fake_tools(results = list())
  suppressMessages(
    result <- os_prep_audio_dir(indir, "mp4", outdir, overwrite = FALSE)
  )

  expect_identical(result$status, "skipped")
  expect_identical(result$success, FALSE)
  expect_match(result$error[[1]], "overwrite")
  expect_identical(boundary_tools(state), character(0))
})

test_that("aw_transcribe_dir() records a file with no audio stream as skipped", {
  # A file that PROBED CLEANLY and has zero audio streams. This is the half of
  # the old combined branch that is a genuine skip: the probe succeeded, the
  # answer is known, and there is simply nothing to transcribe.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "silent.mp4"))

  state <- local_fake_tools(results = list("video"))
  suppressMessages(
    result <- aw_transcribe_dir(
      indir, "mp4",
      model = structure(list(name = "tiny"), class = "whisper")
    )
  )

  expect_identical(result$status, "skipped")
  expect_identical(result$success, FALSE)
  expect_match(result$error[[1]], "audio")
  # One probe and nothing after it: the probe is what decided the skip.
  expect_identical(boundary_tools(state), "ffprobe")
})

# --- a NESTED skip is not the batch's skip (review round 1, F1/F2) -----------
#
# `overwrite = FALSE` means "reuse the audio you already prepared", and in a
# batch whose job is NOT the preparing -- extracting features, transcribing --
# reusing it is the fast path, not a reason to stop. The signal `os_prep_audio()`
# raises must therefore stop at the call that raised it, because `dir_walk()`'s
# handler is exiting and would otherwise unwind the whole per-file job: the tool
# would never run, no output would be written, and the row would report a
# deliberate skip of work the caller did want done.
#
# The discriminating assertions are on the TOOL and the OUTPUT, not on `status`
# alone: a row reading "ok" while openSMILE was never called is the same defect
# wearing a better label.

# A model object, as far as `aw_transcribe_wav()`'s class check is concerned.
skip_fixture_model <- function() structure(list(name = "tiny"), class = "whisper")

test_that("os_extract_dir() reuses an existing wav and still extracts features", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "a.mp4"))
  wavdir <- withr::local_tempdir()
  # The wav a previous run of this same batch already prepared.
  file.create(file.path(wavdir, "a.wav"))
  aggdir <- withr::local_tempdir()

  writer <- function(command, args) {
    for (path in boundary_value(args, "-csvoutput")) write_fake_os_output(path)
    "ok"
  }
  state <- local_fake_tools(results = c(
    # os_check_audio(infile): an mp4, so not already a conforming wav.
    list("audio", c("mp3", "44100", "2")),
    # os_check_audio(wavfile), inside os_extract_wav(): the reused wav is.
    list("audio", c("pcm_s16le", "44100", "1")),
    list(writer)
  ))
  suppressMessages(
    result <- os_extract_dir(
      indir, "mp4", wavdir = wavdir, aggdir = aggdir, overwrite = FALSE
    )
  )

  expect_identical(result$status, "ok")
  expect_identical(result$success, TRUE)
  # openSMILE ran and its output landed. Without these two the status assertion
  # above passes on a batch that did nothing.
  expect_true("opensmile" %in% boundary_tools(state))
  expect_identical(list.files(aggdir), "a.csv")
})

test_that("aw_transcribe_dir() reuses an existing wav and still transcribes", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "a.mp4"))
  wavdir <- withr::local_tempdir()
  file.create(file.path(wavdir, "a.wav"))
  rdsdir <- withr::local_tempdir()

  local_fake_tools(results = c(
    # aw_transcribe()'s own audio-stream check on the input.
    list("audio"),
    # aw_check_audio(infile): an mp4, so it is not already whisper-ready.
    list("audio", c("mp3", "44100", "2")),
    # aw_check_audio(wavfile), inside aw_transcribe_wav(): the reused wav is.
    list("audio", c("pcm_s16le", "16000", "1"))
  ))
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  # whisper is reached through `do.call(what = predict, ...)`, and the binding
  # resolves through openac's imports environment -- so the mock goes in
  # openac's namespace, not in stats.
  local_mocked_bindings(
    predict = function(...) {
      calls$n <- calls$n + 1L
      list(data = data.frame(
        segment = 1L, from = "00:00:00.000", to = "00:00:01.000",
        text = " Hello.", stringsAsFactors = FALSE
      ))
    },
    .package = "openac"
  )
  suppressMessages(
    result <- aw_transcribe_dir(
      indir, "mp4",
      wavdir = wavdir, rdsdir = rdsdir,
      model = skip_fixture_model(),
      audio_args = list(overwrite = FALSE)
    )
  )

  expect_identical(result$status, "ok")
  expect_identical(result$success, TRUE)
  expect_identical(calls$n, 1L)
  expect_identical(list.files(rdsdir), "a.rds")
})

test_that("os_prep_audio_dir() still skips: there the prep IS the job", {
  # The boundary the fix must not move. Same inputs, same `overwrite = FALSE`,
  # but the batch's own job is preparing the audio -- so there is genuinely
  # nothing to do and the row is a skip, not an "ok" for work never done.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "a.mp4"))
  outdir <- withr::local_tempdir()
  file.create(file.path(outdir, "a.wav"))

  state <- local_fake_tools(results = list())
  suppressMessages(
    result <- os_prep_audio_dir(indir, "mp4", outdir, overwrite = FALSE)
  )

  expect_identical(result$status, "skipped")
  expect_identical(boundary_tools(state), character(0))
})

test_that("aw_transcribe_dir() records an unprobeable file as failed", {
  # The other half of the old combined branch, and the reason it had to be
  # split: nothing was learned about this file, so recording it as
  # deliberately passed over would assert something false about it. The
  # discriminating assertion is `status == "failed"` -- both halves read
  # `success = FALSE`, so `success` cannot separate them.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "b.mp4"))

  local_fake_tools(results = list(fake_nonzero_exit()))
  suppressWarnings(suppressMessages(
    result <- aw_transcribe_dir(
      indir, "mp4",
      model = structure(list(name = "tiny"), class = "whisper")
    )
  ))

  expect_identical(result$status, "failed")
  expect_identical(result$success, FALSE)
  expect_match(gsub("\\s+", " ", result$error[[1]]), "could not be counted")
})
