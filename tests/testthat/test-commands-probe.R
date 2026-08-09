# AC3/AC5 -- the passthroughs, the alias family, and the ffprobe-backed checks.

# A file that exists, for the stopifnot(file.exists()) guards. Contents are
# irrelevant: the tool never runs.
local_media <- function(ext = ".wav", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

# --- passthroughs and aliases: pass-through identity -------------------------

test_that("each passthrough forwards its argument string unchanged", {
  state <- local_fake_tools(results = list("a", "b", "c", "d"))

  ffmpeg("-version")
  ffprobe("-h")
  openface("-help")
  openac:::opensmile("-L")

  expect_identical(
    boundary_tools(state),
    c("ffmpeg", "ffprobe", "openface", "opensmile")
  )
  expect_identical(
    boundary_argv(state),
    list("-version", "-h", "-help", "-L")
  )
})

test_that("each alias reaches the same tool as its primary name", {
  state <- local_fake_tools(results = list("a", "b", "c", "d"))

  ffm("-version")
  ffp("-h")
  of("-help")
  os("-L")

  expect_identical(
    boundary_tools(state),
    c("ffmpeg", "ffprobe", "openface", "opensmile")
  )
  expect_identical(
    boundary_argv(state),
    list("-version", "-h", "-help", "-L")
  )
})

test_that("each passthrough errors, and runs nothing, when its tool is absent", {
  # system2(NULL, args) hands `args` to the shell, so an unguarded passthrough
  # would EXECUTE its argument string when the tool is missing. The guard turns
  # that into an error; find_program()'s warning still carries the set_program()
  # hint.
  state <- local_fake_tools(results = list(), resolve = character())

  expect_warning(expect_error(ffmpeg("-version"), "Can't run"), "Failed to find")
  expect_warning(expect_error(ffprobe("-h"), "Can't run"), "Failed to find")
  expect_warning(expect_error(openface("-help"), "Can't run"), "Failed to find")
  expect_warning(
    expect_error(openac:::opensmile("-L"), "Can't run"), "Failed to find"
  )

  # Nothing reached the boundary: no shell command was run.
  expect_length(boundary_calls(state), 0)
})

test_that("passthroughs reject an argument that is not a character vector", {
  # `ffprobe(c("-a", "-b"))` was asserted here as an ERROR until M13; under
  # D-017 a vector is the token form and is valid, so the multi-element case
  # moved to the positive assertion below rather than being dropped.
  local_fake_tools()
  expect_error(ffmpeg(1), "character vector")
  expect_error(ffprobe(NULL), "character vector")
  expect_error(openface(list()), "character vector")
  expect_error(openac:::opensmile(character()), "at least one")
})

test_that("each passthrough takes the token form and quotes it per element", {
  state <- local_fake_tools(results = list("a", "b", "c", "d"))

  ffmpeg(c("-i", "a b.mp4"))
  ffprobe(c("-show_entries", "stream=codec_type"))
  openface(c("-f", "a b.mp4"))
  openac:::opensmile(c("-C", "a b.conf"))

  expect_identical(
    boundary_argv(state),
    list(
      shQuote(c("-i", "a b.mp4")),
      shQuote(c("-show_entries", "stream=codec_type")),
      shQuote(c("-f", "a b.mp4")),
      shQuote(c("-C", "a b.conf"))
    )
  )
})

# --- ffp_count_streams -------------------------------------------------------

test_that("ffp_count_streams() builds the documented ffprobe query", {
  infile <- local_media(".mp4")
  state <- local_fake_tools(results = list(c("video", "audio")))

  ffp_count_streams(infile)

  expect_identical(boundary_tools(state), "ffprobe")
  expect_identical(
    boundary_argv(state)[[1]],
    shQuote(c(
      "-v", "error",
      "-show_entries", "stream=codec_type",
      "-of", "csv=p=0",
      infile
    ))
  )
})

test_that("ffp_count_streams() sends a hostile path as one intact token", {
  # The regression M13 exists for, pinned to the failure it actually is.
  #
  # MEASURED before the fix: the old assembly interpolated `"` around the path,
  # and a `$` inside double quotes is expanded by the shell -- `/tmp/a $b.mp4`
  # reached the tool as `/tmp/a .mp4`. A space alone did NOT fail, so a test
  # using only a space would have passed against the broken form and pinned
  # nothing. Both characters are needed, and the `$` is the discriminating one.
  infile <- withr::local_tempfile(pattern = "has space and $dollar", fileext = ".mp4")
  file.create(infile)
  state <- local_fake_tools(results = list("video"))

  ffp_count_streams(infile)

  argv <- boundary_argv(state)[[1]]
  # The path is exactly one argument, and it is quoted -- not spliced into a
  # longer string, not split on its space.
  expect_identical(argv[[length(argv)]], shQuote(infile))
  expect_length(argv, 7L)
  # And the quoting is the kind that survives `$`: sh-style single quotes on
  # unix suppress expansion, which the double quotes it replaced did not.
  skip_on_os("windows")
  expect_true(startsWith(argv[[length(argv)]], "'"))
})

test_that("ffp_count_streams() counts each stream combination", {
  infile <- local_media(".mp4")

  # Integer counts, so expect_equal rather than expect_identical against
  # doubles -- the roxygen says "numeric".
  local_fake_tools(results = list(c("video", "audio")))
  expect_equal(ffp_count_streams(infile), c(Video = 1, Audio = 1))

  local_fake_tools(results = list("audio"))
  expect_equal(ffp_count_streams(infile), c(Video = 0, Audio = 1))

  local_fake_tools(results = list("video"))
  expect_equal(ffp_count_streams(infile), c(Video = 1, Audio = 0))

  local_fake_tools(results = list(character()))
  expect_equal(ffp_count_streams(infile), c(Video = 0, Audio = 0))
})

test_that("ffp_count_streams() reports a nonexistent file rather than aborting", {
  # Was `stopifnot(file.exists(infile))`, which killed the whole batch (GP6).
  local_fake_tools()
  absent <- file.path(tempdir(), "absent.mp4")

  # Messages are read through collect_warnings(), which collapses the hard line
  # breaks cli bakes in at the console width -- an assertion on the raw text
  # passes or fails on how long the temp path is (see the helper).
  warnings <- collect_warnings(streams <- ffp_count_streams(absent))

  expect_length(warnings, 1L)
  expect_match(warnings, "does not exist")
  expect_identical(streams, c(Video = NA_integer_, Audio = NA_integer_))
  # The warning names the file, so a batch report can be acted on.
  expect_match(warnings, "absent.mp4", fixed = TRUE)
})

test_that("ffp_count_streams() reports a failed probe rather than aborting", {
  infile <- local_media(".mp4")
  local_fake_tools(results = list(fake_nonzero_exit(status = 1L)))

  warnings <- collect_warnings(streams <- ffp_count_streams(infile))

  expect_match(warnings, "ffprobe exited with status 1")
  expect_identical(streams, c(Video = NA_integer_, Audio = NA_integer_))
})

test_that("a failed probe warns once whatever language R speaks", {
  # R's own status warning quotes the argv and never the file; ours does the
  # opposite, and only one of the two reaches the caller.
  #
  # The fake's message is the MEASURED French one, carrying no English at all
  # (see fake_nonzero_exit()). That is what gives this test teeth: suppression
  # is keyed on the exit status, so it cannot be satisfied by a handler grepping
  # for English text the way the first cut of this code did.
  infile <- local_media(".mp4")
  local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- collect_warnings(ffp_count_streams(infile))

  expect_length(warnings, 1L)
  expect_match(warnings, basename(infile), fixed = TRUE)
  # And what reached the caller is ours, not the tool's argv report.
  expect_no_match(warnings, "renvoie un statut", fixed = TRUE)
})

test_that("a diagnostic raised alongside a FAILED probe still reaches the caller", {
  # Suppression is aimed at R's exit-status report and nothing else. An earlier
  # cut dropped every warning held during a failed probe, which made this the
  # one path where a diagnostic could vanish silently (fix-delta review F1).
  infile <- local_media(".mp4")
  chatty <- function(command, args) {
    warning("ffprobe: this build is ancient")
    # R raises its status warning last, after the command has returned; the
    # fake reproduces that order because the suppression depends on it.
    warning("l'exécution de la commande 'ffprobe' renvoie un statut 1")
    structure("ffprobe: Invalid data", status = 1L)
  }
  local_fake_tools(results = list(chatty))

  warnings <- collect_warnings(streams <- ffp_count_streams(infile))

  expect_identical(streams, c(Video = NA_integer_, Audio = NA_integer_))
  expect_length(warnings, 2L)
  # The diagnostic survived; R's argv report did not.
  expect_match(warnings[[1]], "this build is ancient", fixed = TRUE)
  expect_match(warnings[[2]], "ffprobe exited with status 1")
  expect_false(any(grepl("renvoie un statut", warnings, fixed = TRUE)))
})

test_that("a warning from a probe that SUCCEEDS still reaches the caller", {
  # Suppression is scoped to the failure it replaces. A warning raised on a
  # successful probe is not ours to swallow, so it is re-signalled unchanged --
  # the case a blanket muffle would have silently eaten.
  infile <- local_media(".mp4")
  noisy <- function(command, args) {
    warning("ffprobe: deprecated pixel format")
    c("video", "audio")
  }
  local_fake_tools(results = list(noisy))

  warnings <- collect_warnings(streams <- ffp_count_streams(infile))

  expect_equal(streams, c(Video = 1, Audio = 1))
  expect_length(warnings, 1L)
  expect_match(warnings, "deprecated pixel format", fixed = TRUE)
})

test_that("ffp_count_streams() requires a single file path", {
  # Not a bad file but a bad call, so it aborts rather than returning NA. Both
  # shapes died on a raw base-R condition once the stopifnot() was removed.
  local_fake_tools()

  expect_error(ffp_count_streams(c("a.mp4", "b.mp4")), "single file path")
  expect_error(ffp_count_streams(character(0)), "single file path")
  expect_error(ffp_count_streams(42), "single file path")
})

test_that("ffp_count_streams() still aborts when ffprobe itself is unavailable", {
  # A missing tool fails every file in a batch identically, so it stays an
  # abort from require_program() rather than becoming a per-file NA.
  infile <- local_media(".mp4")
  local_fake_tools(resolve = character())

  # And the hint survives the abort. `find_program()` warns with the
  # `set_program()` pointer on its way to the error, from inside the region
  # where warnings are held pending the exit status -- so it has to be released
  # as the error unwinds, or the one message telling the user how to fix this
  # is lost. It was, until this test.
  expect_warning(
    expect_error(ffp_count_streams(infile), "could not be found"),
    "set_program"
  )
})

# --- ffp_run -----------------------------------------------------------------

test_that("ffp_run() forwards its tokens to ffprobe unchanged", {
  # The command test the contract gate asks of every boundary-reaching function.
  # `ffp_run()` builds no command of its own -- it exists for the exit status and
  # the warnings, and what it owes the argv is identity.
  state <- local_fake_tools(results = list("audio"))

  openac:::ffp_run(c("-v", "error", "-show_entries", "stream=codec_type"))

  expect_identical(boundary_tools(state), "ffprobe")
  expect_identical(
    boundary_argv(state),
    list(shQuote(c("-v", "error", "-show_entries", "stream=codec_type")))
  )
})

test_that("ffp_run() releases every warning when the probe succeeds", {
  local_fake_tools(results = list(function(command, args) {
    warning("a diagnostic the caller should still see")
    "audio"
  }))

  warnings <- collect_warnings(
    out <- openac:::ffp_run(c("-v", "error", "x.wav"))
  )

  expect_identical(as.character(out), "audio")
  expect_match(warnings, "a diagnostic the caller should still see")
})

test_that("ffp_run() drops only R's own status warning when the probe fails", {
  # Two warnings are raised inside the call: a diagnostic, then R's report of
  # the exit status (LAST, which is the position rule the release depends on --
  # its text is translated and cannot be matched). The first survives; the
  # second is the argv dump the caller's own message replaces.
  local_fake_tools(results = list(function(command, args) {
    warning("the set_program() hint, or any other diagnostic")
    warning("l'exécution de la commande 'ffprobe' renvoie un statut 1")
    structure("ffprobe: Invalid data", status = 1L)
  }))

  warnings <- collect_warnings(
    out <- openac:::ffp_run(c("-v", "error", "x.wav"))
  )

  expect_true(openac:::ffp_failed(out))
  expect_length(warnings, 1L)
  expect_match(warnings, "any other diagnostic")
})

# --- os_check_audio ----------------------------------------------------------

test_that("os_check_audio() issues both ffprobe queries in order", {
  infile <- local_media()
  state <- local_fake_tools(
    results = list("audio", c("pcm_s16le", "44100", "1"))
  )

  os_check_audio(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe"))
  expect_identical(
    boundary_argv(state),
    list(
      shQuote(c(
        "-v", "error",
        "-show_entries", "stream=codec_type",
        "-of", "csv=p=0",
        infile
      )),
      shQuote(c(
        "-v", "error",
        "-show_entries", "stream=codec_name,sample_rate,channels",
        "-of", "default=noprint_wrappers=1:nokey=1",
        infile
      ))
    )
  )
})

test_that("os_check_audio() accepts a conforming file and rejects others", {
  infile <- local_media()

  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "1")))
  expect_true(os_check_audio(infile))

  # Wrong codec.
  local_fake_tools(results = list("audio", c("mp3", "44100", "1")))
  expect_false(os_check_audio(infile))

  # Two channels.
  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "2")))
  expect_false(os_check_audio(infile))

  # A video stream present.
  local_fake_tools(results = list(c("video", "audio"), c("pcm_s16le", "44100", "1")))
  expect_false(os_check_audio(infile))
})

test_that("os_check_audio() returns FALSE on a file it cannot probe", {
  # The stream count is NA, so every test built on it would be NA and `all()`
  # would return NA rather than a logical. The queue holds ONE result: the
  # second ffprobe query must not be issued, because it would fail identically.
  infile <- local_media()
  state <- local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- collect_warnings(result <- os_check_audio(infile))

  expect_match(warnings, "ffprobe exited with status 1")
  expect_false(result)
  expect_identical(boundary_tools(state), "ffprobe")
})

test_that("os_check_audio(verbose = TRUE) names the file it could not probe", {
  infile <- local_media()
  local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- collect_warnings(
    expect_false(os_check_audio(infile, verbose = TRUE))
  )

  # Two messages, both naming the file: the probe's own, then the check's.
  expect_length(warnings, 2L)
  expect_true(all(grepl(basename(infile), warnings, fixed = TRUE)))
  expect_match(warnings[[2]], "Could not count the streams")
})

test_that("os_check_audio() reports a SECOND query that exited non-zero", {
  # The first query succeeds, so the `anyNA(streams)` branch above is not the
  # one under test: this is the file whose stream types ffprobe can read and
  # whose codec/rate/channels query it cannot. Before this fix the exit status
  # of that second call was never read, so its error text arrived looking like
  # ordinary output and was reported as "No audio stream found" -- the file
  # diagnosed as non-conforming when it was in fact unreadable.
  infile <- local_media()
  local_fake_tools(results = list("audio", fake_nonzero_exit()))

  # At DEFAULT verbosity: the warning is not verbose-gated, because a bare FALSE
  # for a file that could not be read reads as "not ready", which is the other
  # half of this bug. The file is named in full, as `ffp_count_streams()` names
  # it for the first query.
  warnings <- collect_warnings(result <- os_check_audio(infile))

  expect_false(result)
  expect_true(any(grepl(infile, warnings, fixed = TRUE)))
  expect_true(any(grepl("ffprobe exited with status 1", warnings, fixed = TRUE)))
  expect_false(any(grepl("No audio stream", warnings, fixed = TRUE)))
  # R's own warning quotes the whole command line back at a user who never
  # wrote it. The first query already suppresses it; the second must too.
  expect_false(any(grepl("renvoie un statut", warnings, fixed = TRUE)))
})

test_that("aw_check_audio() reports a SECOND query that exited non-zero", {
  infile <- local_media()
  local_fake_tools(results = list("audio", fake_nonzero_exit()))

  # Default verbosity, as above: this one returned a SILENT FALSE before the
  # fix, since its length branch is verbose-gated and nothing else spoke.
  warnings <- collect_warnings(result <- aw_check_audio(infile))

  expect_false(result)
  expect_true(any(grepl(infile, warnings, fixed = TRUE)))
  expect_true(any(grepl("ffprobe exited with status 1", warnings, fixed = TRUE)))
  expect_false(any(grepl("No audio stream", warnings, fixed = TRUE)))
  expect_false(any(grepl("renvoie un statut", warnings, fixed = TRUE)))
})

test_that("os_check_audio(verbose = TRUE) warns about a non-44.1kHz rate", {
  infile <- local_media()
  local_fake_tools(results = list("audio", c("pcm_s16le", "48000", "1")))

  expect_warning(
    expect_output(os_check_audio(infile, verbose = TRUE)),
    "44100 is recommended"
  )
})

# --- aw_check_audio ----------------------------------------------------------

test_that("aw_check_audio() issues both ffprobe queries in order", {
  infile <- local_media()
  state <- local_fake_tools(
    results = list("audio", c("pcm_s16le", "16000", "1"))
  )

  aw_check_audio(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe"))
  expect_identical(
    boundary_argv(state)[[2]],
    shQuote(c(
      "-v", "error",
      "-select_streams", "a",
      "-show_entries", "stream=codec_name,sample_rate,channels",
      "-of", "default=noprint_wrappers=1:nokey=1",
      infile
    ))
  )
})

test_that("aw_check_audio() accepts a conforming file and rejects others", {
  infile <- local_media()

  local_fake_tools(results = list("audio", c("pcm_s16le", "16000", "1")))
  expect_true(aw_check_audio(infile))

  # whisper wants 16kHz, so openSMILE's 44.1kHz file is non-conforming here.
  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "1")))
  expect_false(aw_check_audio(infile))

  local_fake_tools(results = list("audio", c("mp3", "16000", "1")))
  expect_false(aw_check_audio(infile))
})

test_that("aw_check_audio() returns FALSE on a file it cannot probe", {
  infile <- local_media()
  state <- local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- collect_warnings(result <- aw_check_audio(infile))

  expect_match(warnings, "ffprobe exited with status 1")
  expect_false(result)
  # One call, not two: the second query would fail on the same file.
  expect_identical(boundary_tools(state), "ffprobe")
})

test_that("aw_check_audio(verbose = TRUE) names the file it could not probe", {
  infile <- local_media()
  local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- collect_warnings(
    expect_false(aw_check_audio(infile, verbose = TRUE))
  )

  expect_length(warnings, 2L)
  expect_true(all(grepl(basename(infile), warnings, fixed = TRUE)))
  expect_match(warnings[[2]], "Could not count the streams")
})

test_that("aw_check_audio() returns FALSE when ffprobe reports under 3 fields", {
  infile <- local_media()

  # The audio-less case: aw_check_audio guards on length(dat) < 3 where
  # os_check_audio would error on dat[[3]].
  local_fake_tools(results = list("video", character()))
  expect_false(aw_check_audio(infile))

  local_fake_tools(results = list("video", character()))
  expect_warning(
    expect_false(aw_check_audio(infile, verbose = TRUE)),
    "No audio stream"
  )
})
