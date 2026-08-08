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
  # What is pinned is that run_tool adds no type of its own.
  #
  # The second assertion here used to repeat the first against shQuote's own
  # documented default, which could only fail if base R changed and covered
  # nothing in run_tool (M13 review B6). Replaced by the property that actually
  # distinguishes "quoted by the platform rule" from "quoted some other way":
  # the token round-trips back to its input.
  state <- local_fake_tools(results = list("out"))

  openac:::run_tool("ffprobe", c("-i", "a b.mp4"))

  quoted <- boundary_argv(state)[[1]][[2]]
  expect_identical(quoted, shQuote("a b.mp4"))
  expect_identical(boundary_unquote(quoted), "a b.mp4")
})

test_that("a path containing an apostrophe is quoted, and read back, correctly", {
  # M13 review B1/B2. `shQuote(type = "sh")` does NOT always wrap in a single
  # quote: given a string containing an apostrophe it switches to the
  # double-quote branch -- MEASURED, `shQuote("Jeff's clip.mp4")` is
  # `"Jeff's clip.mp4"`. The harness assumed one quote character, so this call
  # aborted with the guard blaming its own (correct) call site, and the
  # unquoting accessors returned a path with the quotes still on it.
  state <- local_fake_tools(results = list("out"))
  path <- "/tmp/Jeff's clip.mp4"

  expect_no_error(openac:::run_tool("ffprobe", c("-i", path)))

  argv <- boundary_argv(state)[[1]]
  expect_identical(argv[[2]], shQuote(path))
  # The double-quote branch really is what shQuote chose here, so this test
  # would pass vacuously if it only ever exercised the single-quote branch.
  skip_on_os("windows")
  expect_identical(substr(argv[[2]], 1L, 1L), "\"")
  # And the accessors recover the original path rather than a quoted one.
  expect_identical(boundary_unquote(argv[[2]]), path)
  expect_identical(boundary_value(argv, "-i"), path)
})

# --- the Windows rule, asserted from any host (M15) ---------------------------

test_that("quote_type() names the style base shQuote() would have chosen", {
  # The extraction must be behavior-preserving: `run_tool()` used to call
  # `shQuote()` bare. If the named style ever diverged from the platform default,
  # every command openac builds would change under it.
  expect_identical(
    openac:::quote_type(),
    if (.Platform$OS.type == "windows") "cmd" else "sh"
  )
  expect_identical(
    shQuote(c("-i", "a b.mp4"), type = openac:::quote_type()),
    shQuote(c("-i", "a b.mp4"))
  )
})

test_that("the Windows rule quotes every hostile name to a known literal", {
  # AC4. `run_tool()` reaches the Windows style only when RUNNING on Windows, so
  # a bare `run_tool()` test can never redden on macOS or Linux for a Windows
  # quoting regression -- and this repository's Windows evidence is one manual
  # run (AC2), not CI. Naming the style makes the rule assertable everywhere.
  #
  # Literals, not `shQuote(..., type = "cmd")` recomputed: an assertion against
  # the function under test's own implementation would pass whatever base R did.
  # These strings were MEASURED on the Windows host of AC2, 2026-08-08.
  expected <- c(
    space      = "\"a space.wav\"",
    dollar     = "\"a $dollar.wav\"",
    percent    = "\"a %TEMP% token.wav\"",
    caret      = "\"a ^caret.wav\"",
    ampersand  = "\"a &ampersand.wav\"",
    bang       = "\"a !bang.wav\"",
    apostrophe = "\"a Jeff's clip.wav\"",
    backtick   = "\"a `backtick`.wav\""
  )
  # The table is the oracle's, so an entry added there without an expected
  # literal here is a failure rather than a silently untested name.
  expect_setequal(names(expected), names(hostile_names()))

  for (case in names(expected)) {
    quoted <- openac:::quote_tokens(
      c("-i", hostile_names()[[case]]),
      type = "cmd"
    )
    expect_identical(quoted[[2]], expected[[case]], info = case)
    # `cmd` style is quoting only: nothing inside the name is rewritten. This is
    # the property AC2 measured sufficient, and what a switch to `cmd2` -- which
    # would emit `a ^%TEMP^% token.wav` -- would break here first.
    expect_identical(
      substr(quoted[[2]], 2L, nchar(quoted[[2]]) - 1L),
      hostile_names()[[case]],
      info = case
    )
  }
})

test_that("the length rule survives the extraction under either style", {
  # D-017's contract is the internal's now, so it is pinned on the internal.
  for (type in c("sh", "cmd")) {
    expect_identical(
      openac:::quote_tokens("-v error -i in.mp4", type = type),
      "-v error -i in.mp4"
    )
    expect_identical(
      openac:::quote_tokens(c("-i", "a b.mp4"), type = type),
      shQuote(c("-i", "a b.mp4"), type = type)
    )
  }
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
