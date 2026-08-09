# AC3 -- feature extraction. of_extract() has eight independent booleans, each
# of which must appear in both states somewhere in this file; os_extract_wav()
# builds the whole openSMILE command and is reached only indirectly.

local_media <- function(ext = ".wav", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

# A conforming openSMILE input, as os_check_audio() sees it: audio only,
# pcm_s16le, one channel.
conforming <- function() list("audio", c("pcm_s16le", "44100", "1"))

# The argv of_extract() builds, quoted as run_tool() quotes it (M13). `flags` is
# the trailing run of feature switches, in source order.
of_cmd <- function(infile, outfile, flags = c("-2Dfp", "-3Dfp", "-pose", "-gaze", "-aus")) {
  shQuote(c("-f", infile, "-of", outfile, flags))
}

# The argv os_extract_wav() builds. `aggfile`/`lldfile` NULL means the flag is
# absent entirely -- `character()`, not an empty argument.
os_extract_cmd <- function(config, infile, aggfile = NULL, lldfile = NULL) {
  shQuote(c(
    "-C", config,
    "-I", infile,
    if (is.null(aggfile)) character() else c("-csvoutput", aggfile),
    if (is.null(lldfile)) character() else c("-lldcsvoutput", lldfile),
    "-instname", basename(infile)
  ))
}

# --- of_extract --------------------------------------------------------------

test_that("of_extract() builds the documented default command", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  state <- local_fake_tools(results = list("ok"))

  of_extract(infile, outfile)

  expect_identical(boundary_tools(state), "openface")
  expect_identical(
    boundary_argv(state)[[1]],
    of_cmd(infile, outfile)
  )
})

test_that("of_extract() emits each feature flag when enabled", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  state <- local_fake_tools(results = list("ok"))

  # Every boolean in its non-default state: the four defaulting to TRUE turned
  # off, and the three defaulting to FALSE turned on.
  of_extract(
    infile, outfile,
    fp2D = TRUE, fp3D = TRUE, pdm = TRUE, pose = TRUE,
    gaze = TRUE, aus = TRUE, wild = TRUE, multiview = TRUE
  )

  expect_identical(
    boundary_argv(state)[[1]],
    of_cmd(infile, outfile, c(
      "-2Dfp", "-3Dfp", "-pdmparams", "-pose", "-gaze", "-aus", "-wild",
      "-multi_view", "1"
    ))
  )
})

test_that("of_extract() omits every flag when all are disabled", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  state <- local_fake_tools(results = list("ok"))

  of_extract(
    infile, outfile,
    fp2D = FALSE, fp3D = FALSE, pdm = FALSE, pose = FALSE,
    gaze = FALSE, aus = FALSE, wild = FALSE, multiview = FALSE
  )

  expect_identical(
    boundary_argv(state)[[1]],
    of_cmd(infile, outfile, flags = character())
  )
})

test_that("of_extract() toggles each flag independently", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")

  flags <- list(
    fp2D = "-2Dfp", fp3D = "-3Dfp", pdm = "-pdmparams", pose = "-pose",
    gaze = "-gaze", aus = "-aus", wild = "-wild",
    multiview = c("-multi_view", "1")
  )

  for (nm in names(flags)) {
    off <- stats::setNames(as.list(rep(FALSE, length(flags))), names(flags))
    state <- local_fake_tools(results = list("ok"))
    do.call(of_extract, c(list(infile, outfile), replace(off, nm, TRUE)))
    # Token containment, not substring: `-pose` must be its own argument, not
    # a fragment of a longer one. And for the one flag that carries a VALUE,
    # adjacency as well -- `%in%` alone asserted that `-multi_view` and `1`
    # each appear somewhere, which `c("-multi_view", "-2Dfp", "1")` satisfies
    # (M13 review B7).
    argv <- boundary_argv(state)[[1]]
    expect_true(all(shQuote(flags[[nm]]) %in% argv), info = nm)
    if (length(flags[[nm]]) > 1L) {
      expect_identical(
        boundary_value(argv, flags[[nm]][[1]]), flags[[nm]][[2]],
        info = nm
      )
    }
  }
})

# `of_extract()`'s ten argument guards moved to `test-guard-messages.R` at M19.
# They pinned the `stopifnot()` deparses -- "file.exists", "is_string",
# "is_bool" -- which is precisely the text M19 removed, and all ten are now
# asserted there against messages that name the file and the defect (the eight
# feature flags one by one, where this test sampled `aus` alone).

# --- os_extract_wav ----------------------------------------------------------

test_that("os_extract_wav() builds the openSMILE command with no outputs", {
  infile <- local_media()
  state <- local_fake_tools(results = c(conforming(), list("ok")))

  openac:::os_extract_wav(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe", "opensmile"))
  expect_identical(
    boundary_argv(state)[[3]],
    os_extract_cmd(fake_config_path(state), infile)
  )
})

test_that("os_extract_wav() adds -csvoutput and -lldcsvoutput when asked", {
  infile <- local_media()
  outdir <- withr::local_tempdir()
  aggfile <- file.path(outdir, "agg.csv")
  lldfile <- file.path(outdir, "lld.csv")
  state <- local_fake_tools(results = c(conforming(), list("ok")))

  # The mocked tool writes nothing, but os_fix_csv() reads both back.
  write_fake_os_output(aggfile)
  write_fake_os_output(lldfile)

  openac:::os_extract_wav(infile, aggfile = aggfile, lldfile = lldfile)

  expect_identical(
    boundary_argv(state)[[3]],
    os_extract_cmd(fake_config_path(state), infile, aggfile, lldfile)
  )
  # os_fix_csv() rewrote the semicolon-delimited stand-in as comma-delimited.
  expect_match(readLines(aggfile)[[1]], ",", fixed = TRUE)
})

test_that("os_extract_wav() honours a non-default config", {
  infile <- local_media()
  state <- local_fake_tools(results = c(conforming(), list("ok")))

  openac:::os_extract_wav(infile, config = "egemaps/v02/eGeMAPSv02")

  expect_identical(
    boundary_value(boundary_argv(state)[[3]], "-C"),
    fake_config_path(state, "egemaps/v02/eGeMAPSv02")
  )
})

test_that("os_extract_wav() rejects an unknown config and non-csv outputs", {
  infile <- local_media()

  local_fake_tools(results = conforming())
  expect_error(
    openac:::os_extract_wav(infile, config = "nope/missing"),
    "nope/missing"
  )

  local_fake_tools(results = conforming())
  expect_error(
    openac:::os_extract_wav(infile, aggfile = "agg.txt"),
    "aggfile"
  )
})

# --- os_extract --------------------------------------------------------------

test_that("os_extract() skips preparation when the input already conforms", {
  infile <- local_media()
  # os_check_audio runs twice: once in os_extract, once inside os_extract_wav.
  state <- local_fake_tools(results = c(conforming(), conforming(), list("ok")))

  os_extract(infile)

  expect_identical(
    boundary_tools(state),
    c("ffprobe", "ffprobe", "ffprobe", "ffprobe", "opensmile")
  )
  # The input went to openSMILE untouched -- no ffmpeg conversion.
  expect_identical(boundary_value(boundary_argv(state)[[5]], "-I"), infile)
})

test_that("os_extract() uses and then discards a temp wav when wavfile is NULL", {
  infile <- local_media(".mp4")

  # The real ffmpeg writes the file os_extract_wav() then requires; the mock
  # writes nothing, so this stand-in creates the output path it was handed.
  written <- NULL
  fake_ffmpeg <- function(command, args) {
    written <<- boundary_outfile(args)
    file.create(written)
    "ok"
  }

  state <- local_fake_tools(
    results = c(
      list("audio", c("mp3", "44100", "2")),  # os_extract's check: fails
      list(fake_ffmpeg),                      # os_prep_audio writes the temp wav
      conforming(),                           # os_extract_wav's check: passes
      list("ok")                              # opensmile
    )
  )

  os_extract(infile)

  # The temp file was created by tempfile(), passed to openSMILE...
  expect_true(startsWith(written, tempdir()))
  expect_identical(tools::file_ext(written), "wav")
  expect_identical(boundary_value(boundary_argv(state)[[6]], "-I"), written)
  # ...and unlinked once os_extract() returned.
  expect_false(file.exists(written))
})

test_that("os_extract() prepares a non-conforming input first", {
  infile <- local_media(".mp4")
  wavfile <- file.path(withr::local_tempdir(), "prepped.wav")
  # The mocked ffmpeg writes nothing, but os_extract_wav() requires the file
  # os_prep_audio() would have produced.
  file.create(wavfile)
  state <- local_fake_tools(
    results = c(
      list("audio", c("mp3", "44100", "2")),  # os_extract's check: fails
      list("ok"),                              # os_prep_audio's ffmpeg
      conforming(),                            # os_extract_wav's check: passes
      list("ok")                               # opensmile
    )
  )

  os_extract(infile, wavfile = wavfile)

  expect_identical(
    boundary_tools(state),
    c("ffprobe", "ffprobe", "ffmpeg", "ffprobe", "ffprobe", "opensmile")
  )
  # openSMILE reads the prepared wav, not the original input.
  expect_identical(boundary_value(boundary_argv(state)[[6]], "-I"), wavfile)
})
