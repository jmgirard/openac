# AC1 -- the harness itself. If these fail, every other command test is
# meaningless, so the helper is tested before it is trusted.

test_that("the boundary mock intercepts primary names, aliases and internals", {
  state <- local_fake_tools(results = list("A", "B", "C"))

  expect_identical(ffmpeg("-one"), "A")
  expect_identical(ffm("-two"), "B")            # alias: separate binding
  expect_identical(openac:::opensmile("-three"), "C")  # internal call form

  expect_identical(boundary_tools(state), c("ffmpeg", "ffmpeg", "opensmile"))
  expect_identical(boundary_args(state), c("-one", "-two", "-three"))
})

test_that("coverage is attributed to the outermost openac frame", {
  # os_check_audio() reaches ffprobe twice, via ffp_count_streams() and
  # directly. Both calls must be owned by os_check_audio(), not by the inner
  # helpers -- otherwise testing one function would mark three as covered.
  state <- local_fake_tools(
    results = list(c("video", "audio"), c("pcm_s16le", "44100", "1"))
  )
  infile <- withr::local_tempfile(fileext = ".wav")
  file.create(infile)

  os_check_audio(infile)

  expect_identical(boundary_owners(state), c("os_check_audio", "os_check_audio"))
})

test_that("a directly tested passthrough owns its own call", {
  state <- local_fake_tools(results = list("x"))
  ffprobe("-version")
  expect_identical(boundary_owners(state), "ffprobe")
})

test_that("a do.call()-dispatched frame is attributed to the outer function", {
  # os_extract_dir() and aw_transcribe_dir() reach their tools through
  # do.call(what = <function value>, ...), where the call head is a function
  # rather than a name. A dropped frame there would credit the call to the
  # inner passthrough -- marking openface covered by a test of of_extract.
  state <- local_fake_tools(results = list("ok"))
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  outfile <- file.path(withr::local_tempdir(), "faces.csv")

  do.call(of_extract, list(infile, outfile))

  expect_identical(boundary_owners(state), "of_extract")
})

test_that("an exhausted result queue errors instead of recycling", {
  local_fake_tools(results = list("only one"))
  expect_identical(ffmpeg("-first"), "only one")
  expect_error(ffmpeg("-second"), "queue exhausted")
})

test_that("resolution is deterministic and independent of the real machine", {
  state <- local_fake_tools(results = list("ok"))
  ffmpeg("-x")
  # Resolved to the fake tree, never to a binary that happens to be installed.
  expect_identical(basename(state$calls[[1]]$command), "ffmpeg")
  expect_false(startsWith(state$calls[[1]]$command, "/opt"))
  expect_false(startsWith(state$calls[[1]]$command, "/usr"))
})

test_that("a program left out of `resolve` is not found", {
  local_fake_tools(results = list(), resolve = character())
  expect_warning(res <- find_program("ffmpeg"))
  expect_null(res)
})
