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

# --- of_extract --------------------------------------------------------------

test_that("of_extract() builds the documented default command", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  state <- local_fake_tools(results = list("ok"))

  of_extract(infile, outfile)

  expect_identical(boundary_tools(state), "openface")
  expect_identical(
    boundary_args(state),
    paste0('-f "', infile, '" -of "', outfile, '" -2Dfp -3Dfp -pose -gaze -aus')
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
    boundary_args(state),
    paste0(
      '-f "', infile, '" -of "', outfile, '"',
      ' -2Dfp -3Dfp -pdmparams -pose -gaze -aus -wild -multi_view 1'
    )
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
    boundary_args(state),
    paste0('-f "', infile, '" -of "', outfile, '"')
  )
})

test_that("of_extract() toggles each flag independently", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")

  flags <- list(
    fp2D = "-2Dfp", fp3D = "-3Dfp", pdm = "-pdmparams", pose = "-pose",
    gaze = "-gaze", aus = "-aus", wild = "-wild", multiview = "-multi_view 1"
  )

  for (nm in names(flags)) {
    off <- stats::setNames(as.list(rep(FALSE, length(flags))), names(flags))
    state <- local_fake_tools(results = list("ok"))
    do.call(of_extract, c(list(infile, outfile), replace(off, nm, TRUE)))
    expect_match(boundary_args(state), flags[[nm]], fixed = TRUE, info = nm)
  }
})

test_that("of_extract() validates its arguments", {
  infile <- local_media(".mp4")
  outfile <- file.path(withr::local_tempdir(), "faces.csv")
  local_fake_tools(results = list())

  expect_error(of_extract(file.path(tempdir(), "gone.mp4"), outfile), "file.exists")
  expect_error(of_extract(infile, 1), "is_string")
  expect_error(of_extract(infile, outfile, aus = "yes"), "is_bool")
})

# --- os_extract_wav ----------------------------------------------------------

test_that("os_extract_wav() builds the openSMILE command with no outputs", {
  infile <- local_media()
  state <- local_fake_tools(results = c(conforming(), list("ok")))

  openac:::os_extract_wav(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe", "opensmile"))
  expect_identical(
    boundary_args(state)[[3]],
    paste0(
      '-C "', fake_config_path(state), '"',
      ' -I "', infile, '"',
      ' -instname "', basename(infile), '"'
    )
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
    boundary_args(state)[[3]],
    paste0(
      '-C "', fake_config_path(state), '"',
      ' -I "', infile, '"',
      ' -csvoutput "', aggfile, '"',
      ' -lldcsvoutput "', lldfile, '"',
      ' -instname "', basename(infile), '"'
    )
  )
  # os_fix_csv() rewrote the semicolon-delimited stand-in as comma-delimited.
  expect_match(readLines(aggfile)[[1]], ",", fixed = TRUE)
})

test_that("os_extract_wav() honours a non-default config", {
  infile <- local_media()
  state <- local_fake_tools(results = c(conforming(), list("ok")))

  openac:::os_extract_wav(infile, config = "egemaps/v02/eGeMAPSv02")

  expect_match(
    boundary_args(state)[[3]],
    paste0('-C "', fake_config_path(state, "egemaps/v02/eGeMAPSv02"), '"'),
    fixed = TRUE
  )
})

test_that("os_extract_wav() rejects an unknown config and non-csv outputs", {
  infile <- local_media()

  local_fake_tools(results = conforming())
  expect_error(
    openac:::os_extract_wav(infile, config = "nope/missing"),
    "Config file not found"
  )

  local_fake_tools(results = conforming())
  expect_error(
    openac:::os_extract_wav(infile, aggfile = "agg.txt"),
    "file_ext"
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
  expect_match(boundary_args(state)[[5]], paste0(' -I "', infile, '"'), fixed = TRUE)
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
  expect_match(boundary_args(state)[[6]], paste0(' -I "', written, '"'), fixed = TRUE)
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
  expect_match(boundary_args(state)[[6]], paste0(' -I "', wavfile, '"'), fixed = TRUE)
})
