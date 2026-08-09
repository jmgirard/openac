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

# A queued result standing in for a tool that exits non-zero.
#
# MEASURED 2026-08-08 (R 4.6.1, macOS): `system2(stdout = TRUE, stderr = TRUE)`
# on a non-zero exit returns the output with a `status` attribute AND emits R's
# own warning quoting the whole command line --
# `running command ''ls' /nonexistent-zzz 2>&1' had status 1`. Both halves are
# reproduced here, because `ffp_count_streams()` reads the first and muffles the
# second.
fake_nonzero_exit <- function(status = 1L, output = "ffprobe: Invalid data") {
  force(status)
  force(output)
  function(command, args) {
    warning(sprintf("running command '%s' had status %d", basename(command), status))
    structure(output, status = status)
  }
}

test_that("ffp_count_streams() reports a nonexistent file rather than aborting", {
  # Was `stopifnot(file.exists(infile))`, which killed the whole batch (GP6).
  local_fake_tools()
  absent <- file.path(tempdir(), "absent.mp4")

  expect_warning(
    streams <- ffp_count_streams(absent),
    "does not exist"
  )
  expect_identical(streams, c(Video = NA_integer_, Audio = NA_integer_))
  # The warning names the file, so a batch report can be acted on.
  expect_warning(ffp_count_streams(absent), "absent\\.mp4")
})

test_that("ffp_count_streams() reports a failed probe rather than aborting", {
  infile <- local_media(".mp4")
  local_fake_tools(results = list(fake_nonzero_exit(status = 1L)))

  expect_warning(
    streams <- ffp_count_streams(infile),
    "status 1"
  )
  expect_identical(streams, c(Video = NA_integer_, Audio = NA_integer_))
})

test_that("a failed probe warns once, naming the file rather than the command", {
  # R's own status warning quotes the argv and never the file; ours does the
  # opposite, and only one of the two reaches the caller.
  infile <- local_media(".mp4")
  local_fake_tools(results = list(fake_nonzero_exit()))

  warnings <- character()
  withCallingHandlers(
    ffp_count_streams(infile),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, basename(infile), fixed = TRUE)
})

test_that("ffp_count_streams() still aborts when ffprobe itself is unavailable", {
  # A missing tool fails every file in a batch identically, so it stays an
  # abort from require_program() rather than becoming a per-file NA.
  infile <- local_media(".mp4")
  local_fake_tools(resolve = character())

  suppressWarnings(
    expect_error(ffp_count_streams(infile), "could not be found")
  )
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
