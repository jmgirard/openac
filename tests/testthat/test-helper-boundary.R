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

test_that("the install-time mock intercepts the network and the extractor", {
  # AC2's no-real-network claim rests entirely on these two bindings being the
  # ones the installers reach. Asserted here rather than assumed, because a
  # miss would let an install test hit gyan.dev or GitHub for real.
  dest <- withr::local_tempfile()
  target <- withr::local_tempdir()
  state <- local_fake_downloads(extract_creates = "bin/tool")

  expect_identical(
    utils::download.file(url = "https://example.invalid/x.zip", destfile = dest),
    0L
  )
  archive::archive_extract("ignored", dir = target)

  expect_identical(download_urls(state), "https://example.invalid/x.zip")
  expect_identical(download_dests(state), dest)
  expect_identical(extract_dirs(state), target)
  # The fake writes the placeholder the real extractor would have unpacked.
  expect_true(file.exists(file.path(target, "bin", "tool")))
})

test_that("local_fake_tools() redirects every rappdirs dir openac's code reads", {
  # The domain is not a remembered list of two functions: it is whatever `R/`
  # actually calls, read off `R/` here. A future call site reaching a third
  # rappdirs dir fails this test instead of quietly reading the real one --
  # which is how the original leak survived (find_program() falls through to
  # user_config_dir() whenever Sys.which() reports "", and a maintainer who has
  # run set_program() has a file sitting there).
  # Read off the loaded namespace, not off `R/`: under `R CMD check` the source
  # tree is gone but the namespace is exactly what will run.
  ns <- asNamespace("openac")
  code <- unlist(lapply(ls(ns, all.names = TRUE), function(n) {
    obj <- get(n, envir = ns)
    if (is.function(obj)) deparse(body(obj)) else character()
  }))
  hits <- unlist(regmatches(code, gregexpr("rappdirs::user_[a-z_]+dir", code)))
  used <- sort(unique(sub("^rappdirs::", "", hits)))
  expect_gt(length(used), 0)  # the walk itself must not silently find nothing

  real <- vapply(used, function(fn) {
    getExportedValue("rappdirs", fn)("openac", "R")
  }, character(1))

  local_fake_tools()

  redirected <- vapply(used, function(fn) {
    getExportedValue("rappdirs", fn)("openac", "R")
  }, character(1))

  # Every one of them, named, so a failure says which dir leaked.
  for (fn in used) {
    expect_false(identical(redirected[[fn]], real[[fn]]), label = fn)
  }
})

test_that("a program left out of `resolve` is not found", {
  local_fake_tools(results = list(), resolve = character())
  expect_warning(res <- find_program("ffmpeg"))
  expect_null(res)
})
