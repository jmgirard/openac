# M13 T1 -- the boundary runner (AC1).
#
# `run_tool()` is the single place openac quotes for the shell. It is internal,
# but its logic is independent of any caller -- the length rule decides between
# two different contracts -- so it is tested directly rather than only through
# the four passthroughs (tracking-rules "What gets a test").

# --- the length rule ---------------------------------------------------------

test_that("a length-1 argument reaches system2 untouched", {
  state <- local_fake_tools(results = list("out"))

  openac:::run_tool("ffprobe", "-v error -i in.mp4")

  # Byte-identical: this is the legacy raw-string form, and D-017 keeps it raw
  # so that no call working today changes behavior. Quoting it would collapse
  # the whole string into one argument.
  expect_identical(boundary_argv(state)[[1]], "-v error -i in.mp4")
})

test_that("a longer argument is quoted one element per token", {
  state <- local_fake_tools(results = list("out"))
  tokens <- c("-i", "a b.mp4", "-c", "copy")

  openac:::run_tool("ffprobe", tokens)

  argv <- boundary_argv(state)[[1]]
  # Token boundaries are preserved: as many arguments out as tokens in. This is
  # what the collapsed accessor cannot see, and what a glued string destroys.
  expect_length(argv, length(tokens))
  expect_identical(argv, shQuote(tokens))
  # And the quoting is real, not a no-op, exactly where it is needed.
  expect_false(identical(argv[[2]], tokens[[2]]))
})

test_that("quoting uses shQuote's platform default rather than a fixed style", {
  # Asserted against `shQuote()` itself rather than a literal `'a b'`, because a
  # literal encodes the unix style and would have to be wrong on one platform.
  # What is pinned is that run_tool adds no type of its own: sh-style quoting on
  # unix, cmd-style on Windows, decided by the same rule `shQuote()` uses.
  state <- local_fake_tools(results = list("out"))

  openac:::run_tool("ffprobe", c("-i", "a b.mp4"))

  expect_identical(boundary_argv(state)[[1]][[2]], shQuote("a b.mp4"))
  expect_identical(
    boundary_argv(state)[[1]][[2]],
    shQuote("a b.mp4", type = if (.Platform$OS.type == "windows") "cmd" else "sh")
  )
})

# --- validation --------------------------------------------------------------

test_that("run_tool rejects an argument that is not a character vector", {
  local_fake_tools()

  expect_error(openac:::run_tool("ffprobe", 1), class = "rlang_error")
  expect_error(openac:::run_tool("ffprobe", 1), "character vector")
  expect_error(openac:::run_tool("ffprobe", NULL), "character vector")
  expect_error(openac:::run_tool("ffprobe", list("-a")), "character vector")
})

test_that("run_tool rejects an empty argument vector and a missing token", {
  local_fake_tools()

  # `character(0)` would reach system2 as no arguments at all -- a bare tool
  # invocation the caller never asked for.
  expect_error(openac:::run_tool("ffprobe", character()), "at least one")
  # NA is not a token; `shQuote()` renders it as the literal string "NA".
  expect_error(openac:::run_tool("ffprobe", c("-i", NA)), "missing value")
})

test_that("run_tool stops when the program cannot be found", {
  # The guard that stops `system2(NULL, args)` executing `args` as a shell
  # command (M06) lives in require_program(); run_tool must keep going through
  # it rather than resolving the path itself.
  state <- local_fake_tools(results = list(), resolve = character())

  expect_warning(
    expect_error(openac:::run_tool("ffprobe", c("-i", "a.mp4")), "Can't run"),
    "Failed to find"
  )
  expect_length(boundary_calls(state), 0)
})

# --- the oracle: what the shell actually delivers ----------------------------

test_that("the quoted argv survives the real shell as one argument per token", {
  # Every other test in this file inspects what `system2()` was HANDED. This one
  # asserts what the tool RECEIVES, which is the claim that actually matters and
  # which no mock can make: system2 pastes args into a command string that the
  # shell then re-splits, so the round trip is the only honest oracle.
  #
  # Nothing is mocked here except program discovery -- the real `system2()` runs
  # a real script that echoes its own argv. The measured failure this pins is
  # openac's pre-M13 form: `paste0('-i "', path, '"')` with a `$` in the path
  # delivered `/tmp/a .mp4`, because `$b` expands inside double quotes.
  skip_on_os("windows") # the script is /bin/sh; the cmd.exe path is unpinned here
  skip_on_cran()

  dir <- withr::local_tempdir()
  echo <- file.path(dir, "ffprobe")
  writeLines(c("#!/bin/sh", 'for a in "$@"; do echo "[$a]"; done'), echo)
  Sys.chmod(echo, "0755")

  # Only discovery is faked: find_program() resolves "ffprobe" to the script.
  local_fake_config()
  testthat::local_mocked_bindings(
    Sys.which = function(names) {
      stats::setNames(ifelse(names == "ffprobe", echo, ""), names)
    },
    .package = "base"
  )

  tokens <- c("-i", "/tmp/a $b.mp4", "-show_entries", "stream=codec_type")
  out <- openac:::run_tool("ffprobe", tokens)

  expect_identical(out, paste0("[", tokens, "]"))
})
