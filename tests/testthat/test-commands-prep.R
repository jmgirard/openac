# AC3 -- audio preparation. Every parameter that alters the constructed command
# is exercised in each distinct shape it produces.

local_media <- function(ext = ".mp4", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

local_outpath <- function(.env = parent.frame()) {
  file.path(withr::local_tempdir(.local_envir = .env), "out.wav")
}

# The exact argv os_prep_audio() builds, quoted as run_tool() quotes it (M13).
# These returned one glued string until M13; the doubled space they used to pin
# ('" ' followed by ' -map') was an artifact of string concatenation and is gone
# with it, which is the test change that cleanup was meant to show up as.
os_prep_cmd <- function(infile, outfile, stream = 0) {
  shQuote(c(
    "-y",
    "-i", infile,
    "-map", paste0("0:a:", stream),
    "-ar", "44100",
    "-ac", "1",
    "-c:a", "pcm_s16le",
    outfile
  ))
}

# The whole filter chain aw_prep_audio(afilters = TRUE) inserts, in order, as
# the two tokens it now is: the flag, and the chain as ONE value. Pinned entire
# rather than by fragment: a chain checked piecewise let afftdn, compand and
# dynaudnorm go unasserted.
aw_filter_chain <- c("-af", paste0(
  'loudnorm=I=-24:LRA=7:tp=-2,',
  'highpass=f=70,',
  'lowpass=f=14000,',
  'afftdn=nf=-20,',
  'compand=attacks=0:points=-80/-80|-50/-50|-20/-5|-5/-3:soft-knee=6,',
  'dynaudnorm=p=0.7,',
  'areverse,',
  'asubboost,',
  'areverse'
))

aw_prep_cmd <- function(infile, outfile, stream = 0, afilters = character()) {
  shQuote(c(
    "-y",
    "-i", infile,
    "-map", paste0("0:a:", stream),
    afilters,
    "-ar", "16000",
    "-ac", "1",
    "-c:a", "pcm_s16le",
    outfile
  ))
}

# --- os_prep_audio -----------------------------------------------------------

test_that("os_prep_audio() builds the documented ffmpeg command", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("ok"))

  os_prep_audio(infile, outfile)

  expect_identical(boundary_tools(state), "ffmpeg")
  expect_identical(boundary_argv(state)[[1]], os_prep_cmd(infile, outfile))
})

test_that("os_prep_audio() maps the requested audio stream", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("ok"))

  os_prep_audio(infile, outfile, stream = 2)

  expect_identical(boundary_argv(state)[[1]], os_prep_cmd(infile, outfile, stream = 2))
})

test_that("os_prep_audio(overwrite = FALSE) skips an existing output", {
  infile <- local_media()
  outfile <- local_outpath()
  file.create(outfile)
  state <- local_fake_tools(results = list())

  expect_identical(os_prep_audio(infile, outfile, overwrite = FALSE), "Skipped")
  expect_length(boundary_calls(state), 0)  # the tool is never reached
})

test_that("os_prep_audio(overwrite = FALSE) still runs when output is absent", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("ok"))

  os_prep_audio(infile, outfile, overwrite = FALSE)

  expect_identical(boundary_argv(state)[[1]], os_prep_cmd(infile, outfile))
})

test_that("os_prep_audio() creates a missing output directory", {
  infile <- local_media()
  nested <- file.path(withr::local_tempdir(), "a", "b", "out.wav")
  local_fake_tools(results = list("ok"))

  os_prep_audio(infile, nested)

  expect_true(dir.exists(dirname(nested)))
})

test_that("os_prep_audio() validates its arguments", {
  infile <- local_media()
  outfile <- local_outpath()
  local_fake_tools(results = list())

  expect_error(os_prep_audio(file.path(tempdir(), "gone.mp4"), outfile), "file.exists")
  expect_error(os_prep_audio(infile, 1), "is_string")
  expect_error(os_prep_audio(infile, outfile, stream = -1), "stream >= 0")
  expect_error(os_prep_audio(infile, outfile, overwrite = "yes"), "is_bool")
})

# --- aw_prep_audio -----------------------------------------------------------

test_that("aw_prep_audio() counts streams before building its ffmpeg command", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("audio", "ok"))

  aw_prep_audio(infile, outfile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffmpeg"))
  expect_identical(boundary_argv(state)[[2]], aw_prep_cmd(infile, outfile))
  # Both calls belong to aw_prep_audio, not to the inner ffp_count_streams.
  expect_identical(boundary_owners(state), c("aw_prep_audio", "aw_prep_audio"))
})

test_that("aw_prep_audio() maps the requested audio stream", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list(c("audio", "audio"), "ok"))

  aw_prep_audio(infile, outfile, stream = 1)

  expect_identical(boundary_argv(state)[[2]], aw_prep_cmd(infile, outfile, stream = 1))
})

test_that("aw_prep_audio() rejects a stream the file does not have", {
  infile <- local_media()
  outfile <- local_outpath()
  local_fake_tools(results = list("audio"))

  expect_error(aw_prep_audio(infile, outfile, stream = 3), "Audio")
})

test_that("aw_prep_audio() aborts on a file it cannot probe, naming it", {
  # An abort rather than a warn-and-skip, deliberately: dir_walk() records a row
  # as FAILED only on an error, so a skip here would report the bad file as a
  # success in the batch table.
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list(fake_nonzero_exit()))

  suppressWarnings(
    expect_error(aw_prep_audio(infile, outfile), basename(infile), fixed = TRUE)
  )

  # ffmpeg was never reached: the count is the gate.
  expect_identical(boundary_tools(state), "ffprobe")
})

test_that("aw_prep_audio(afilters = TRUE) inserts the filter chain", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("audio", "ok"))

  aw_prep_audio(infile, outfile, afilters = TRUE)

  # The whole command, so every filter in the chain -- and its position between
  # the stream map and the output format flags -- is asserted.
  expect_identical(
    boundary_argv(state)[[2]],
    aw_prep_cmd(infile, outfile, afilters = aw_filter_chain)
  )
})

test_that("aw_prep_audio(afilters = FALSE) inserts no filter chain", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("audio", "ok"))

  aw_prep_audio(infile, outfile, afilters = FALSE)

  # Both halves: the flag is not its own token, AND no token merely CONTAINS
  # it. `%in%` alone passed a wrapper that glued flag to chain as one token
  # (`"-af loudnorm=..."`), which the substring match this replaced had caught
  # (M13 review B8).
  argv <- boundary_argv(state)[[2]]
  expect_false(shQuote("-af") %in% argv)
  expect_false(any(grepl("-af", argv, fixed = TRUE)))
})

test_that("aw_prep_audio(overwrite = FALSE) skips an existing output", {
  infile <- local_media()
  outfile <- local_outpath()
  file.create(outfile)
  state <- local_fake_tools(results = list())

  expect_identical(aw_prep_audio(infile, outfile, overwrite = FALSE), "Skipped")
  expect_length(boundary_calls(state), 0)
})

test_that("aw_prep_audio() validates its arguments", {
  infile <- local_media()
  outfile <- local_outpath()
  local_fake_tools(results = list())

  expect_error(aw_prep_audio(file.path(tempdir(), "gone.mp4"), outfile), "file.exists")
  expect_error(aw_prep_audio(infile, outfile, afilters = "yes"), "is_bool")
  expect_error(aw_prep_audio(infile, outfile, stream = 1.5), "is_integerish")
})
