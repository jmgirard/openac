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

# The exact string os_prep_audio() builds. The doubled space after the input
# path is what the source produces ('" ' followed by ' -map'); it is pinned
# here deliberately so a future cleanup shows up as a test change.
os_prep_cmd <- function(infile, outfile, stream = 0) {
  paste0(
    '-y -i "', infile, '" ',
    ' -map 0:a:', stream,
    ' -ar 44100 -ac 1 -c:a pcm_s16le "', outfile, '"'
  )
}

aw_prep_cmd <- function(infile, outfile, stream = 0, afilters = "") {
  paste0(
    '-y -i "', infile, '"',
    ' -map 0:a:', stream,
    afilters,
    ' -ar 16000 -ac 1 -c:a pcm_s16le "', outfile, '"'
  )
}

# --- os_prep_audio -----------------------------------------------------------

test_that("os_prep_audio() builds the documented ffmpeg command", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("ok"))

  os_prep_audio(infile, outfile)

  expect_identical(boundary_tools(state), "ffmpeg")
  expect_identical(boundary_args(state), os_prep_cmd(infile, outfile))
})

test_that("os_prep_audio() maps the requested audio stream", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("ok"))

  os_prep_audio(infile, outfile, stream = 2)

  expect_identical(boundary_args(state), os_prep_cmd(infile, outfile, stream = 2))
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

  expect_identical(boundary_args(state), os_prep_cmd(infile, outfile))
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
  expect_identical(boundary_args(state)[[2]], aw_prep_cmd(infile, outfile))
  # Both calls belong to aw_prep_audio, not to the inner ffp_count_streams.
  expect_identical(boundary_owners(state), c("aw_prep_audio", "aw_prep_audio"))
})

test_that("aw_prep_audio() maps the requested audio stream", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list(c("audio", "audio"), "ok"))

  aw_prep_audio(infile, outfile, stream = 1)

  expect_identical(boundary_args(state)[[2]], aw_prep_cmd(infile, outfile, stream = 1))
})

test_that("aw_prep_audio() rejects a stream the file does not have", {
  infile <- local_media()
  outfile <- local_outpath()
  local_fake_tools(results = list("audio"))

  expect_error(aw_prep_audio(infile, outfile, stream = 3), "Audio")
})

test_that("aw_prep_audio(afilters = TRUE) inserts the filter chain", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("audio", "ok"))

  aw_prep_audio(infile, outfile, afilters = TRUE)

  args <- boundary_args(state)[[2]]
  expect_match(args, ' -af "loudnorm=I=-24:LRA=7:tp=-2,', fixed = TRUE)
  expect_match(args, "highpass=f=70,lowpass=f=14000,", fixed = TRUE)
  expect_match(args, 'areverse,asubboost,areverse"', fixed = TRUE)
  # The chain sits between the stream map and the output format flags.
  expect_match(args, '-map 0:a:0 -af "', fixed = TRUE)
  expect_match(args, 'areverse" -ar 16000', fixed = TRUE)
})

test_that("aw_prep_audio(afilters = FALSE) inserts no filter chain", {
  infile <- local_media()
  outfile <- local_outpath()
  state <- local_fake_tools(results = list("audio", "ok"))

  aw_prep_audio(infile, outfile, afilters = FALSE)

  expect_no_match(boundary_args(state)[[2]], "-af", fixed = TRUE)
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
