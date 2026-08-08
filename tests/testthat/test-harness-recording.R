# AC1/AC2 -- the two halves of the completeness precondition, each held to a
# BEHAVIORAL guarantee over a fixture suite generated at runtime.
#
# The expected set must be content-blind and the recorded set must be
# execution-driven. Both are asserted against directories built here rather than
# against the real suite, because the interesting cases -- a file that does not
# parse, a file whose only test skips -- cannot exist in a suite that has to
# stay green. The fixtures are generated into `withr::local_tempdir()` and never
# committed, so no fixture provenance is owed for them.

# Call heads occurring anywhere in a language object. Used for the hygiene check
# below, which is deliberately NOT the guarantee: a token check over a body is
# the proxy shape this milestone exists to retire, and one level of delegation
# beats it. It is here to keep the body obviously boring, nothing more.
call_heads <- function(x) {
  if (!is.call(x)) return(character())
  head <- x[[1L]]
  name <- if (is.symbol(head)) as.character(head) else character()
  c(name, unlist(lapply(as.list(x)[-1L], call_heads), use.names = FALSE))
}

# A directory of test files with the given contents, named by list names.
write_fixture_dir <- function(contents, .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  for (name in names(contents)) {
    writeLines(contents[[name]], file.path(dir, name))
  }
  dir
}

# --- the expected set is content-blind -------------------------------------

test_that("expected_test_files() returns the test files on disk, whatever they contain", {
  # A CONTENT directory: never executed, and holding a member that does not
  # parse. Any implementation that reads file contents dies on `test-garbage.R`;
  # one that lists names does not notice it.
  dir <- write_fixture_dir(list(
    "test-alpha.R" = 'test_that("a", { expect_true(TRUE) })',
    "test-beta.r" = 'test_that("b", { expect_true(TRUE) })',
    "test-empty.R" = character(),
    "test-garbage.R" = 'test_that("unclosed", { expect_true(TRUE',
    # testthat discovers `^test.*\\.[rR]$`, so these two run and must be
    # expected; the `^test-` pattern this used until M10's review missed both.
    "test_underscore.R" = 'test_that("u", { expect_true(TRUE) })',
    "testbare.R" = 'test_that("v", { expect_true(TRUE) })',
    "helper-ignored.R" = "x <- 1",
    "notes.txt" = "not a test file"
  ))

  ground_truth <- sort(list.files(dir, pattern = "^test.*\\.[rR]$"))
  expect_identical(expected_test_files(dir), ground_truth)
  # Membership, not order: `sort()` collates by locale, so pinning the ORDER
  # here would assert this machine's collation on every CI platform.
  expect_setequal(
    expected_test_files(dir),
    c("test-alpha.R", "test-beta.r", "test-empty.R", "test-garbage.R",
      "test_underscore.R", "testbare.R")
  )

  # And it is invariant under arbitrary mutation of EVERY member's contents.
  mutate <- list(
    append = function(p) cat("\n# appended\n", file = p, append = TRUE),
    truncate = function(p) writeLines(character(), p),
    garbage = function(p) writeLines("({[ not R at all", p)
  )
  for (how in names(mutate)) {
    for (f in list.files(dir, full.names = TRUE)) mutate[[how]](f)
    expect_identical(expected_test_files(dir), ground_truth, label = how)
  }
})

test_that("a directory holding an unparseable member cannot be executed", {
  # Measured, not assumed -- this is WHY the content fixture is a separate
  # directory that is never run, rather than one suite doing both jobs.
  dir <- write_fixture_dir(list(
    "test-garbage.R" = 'test_that("unclosed", { expect_true(TRUE'
  ))
  expect_error(
    testthat::test_dir(dir, reporter = "silent", stop_on_failure = FALSE)
  )
})

test_that("expected_test_files() reads no file content", {
  # HYGIENE, not the guarantee. The guarantee is the invariance measured above;
  # this only keeps the body from quietly growing a reader.
  heads <- setdiff(call_heads(body(expected_test_files)), "{")
  expect_setequal(heads, c("sort", "list.files"))
})

# --- the recorded set is execution-driven ----------------------------------

# Run an EXECUTABLE fixture suite and return the registry it recorded into.
#
# The fixture's helper is this suite's own `helper-openac.R`, copied verbatim
# with only the registry binding overridden (plus any `helper_extra` lines the
# case needs) -- so the recorder under test is the recorder that ships, not a
# re-implementation of it that could agree with a broken original. The private
# registry is what keeps fixture file names out of the real `ran`.
run_fixture_suite <- function(contents, helper_extra = character(),
                              .env = parent.frame()) {
  registry <- new.env(parent = emptyenv())
  registry$owners <- character()
  registry$ran <- character()
  withr::local_options(openac_fixture_registry = registry)

  dir <- write_fixture_dir(contents, .env = .env)
  writeLines(
    c(readLines(test_path("helper-openac.R")),
      'openac_registry <- getOption("openac_fixture_registry")',
      helper_extra),
    file.path(dir, "helper-openac.R")
  )
  testthat::test_dir(dir, reporter = "silent", stop_on_failure = FALSE)
  attr(registry, "dir") <- dir
  registry
}

test_that("a file whose only test skips still joins the recorded set", {
  before <- openac_registry$ran
  registry <- run_fixture_suite(list(
    "test-runs.R" = 'test_that("runs", { expect_true(TRUE) })',
    "test-skips.R" = 'test_that("skips", { skip("nothing to do"); expect_true(FALSE) })',
    "test-fails.R" = 'test_that("fails", { expect_true(FALSE) })'
  ))

  # Every file that executed a test is recorded, regardless of its outcome --
  # the skip-only file is the case the two content proxies both got wrong.
  expect_setequal(
    sort(unique(registry$ran)),
    c("test-fails.R", "test-runs.R", "test-skips.R")
  )
  # And the nested run left the real registry alone.
  expect_identical(openac_registry$ran, before)
})

test_that("the files testthat discovers are exactly the files expected of it", {
  # The other half of AC1's guarantee, measured rather than assumed: the
  # expected set and the set testthat actually EXECUTES agree, over names the
  # narrower `^test-` pattern used to miss. An underscore- or bare-prefixed
  # file ran and was required by nothing until M10's review.
  contents <- list(
    "test-hyphen.R" = 'test_that("h", { expect_true(TRUE) })',
    "test_underscore.R" = 'test_that("u", { expect_true(TRUE) })',
    "testbare.R" = 'test_that("b", { expect_true(TRUE) })',
    "helper-ignored.R" = "x <- 1"
  )
  registry <- run_fixture_suite(contents)
  dir <- attr(registry, "dir")

  ran <- sort(unique(registry$ran))
  expect_setequal(ran, c("test-hyphen.R", "test_underscore.R", "testbare.R"))
  # The point of the pattern: what testthat ran and what the gate expects are
  # the same set, so nothing testthat executes is exempt from the gate.
  expect_setequal(expected_test_files(dir), ran)
})

test_that("installing the tool fakes records nothing about which files ran", {
  # The recording this replaces happened at the harness INSTALL site: a file was
  # counted because it called `local_fake_tools()`, which made the expected set
  # a proxy for a coding habit rather than a fact about execution (D-013). The
  # names it used are gone, but a rename would smuggle the same shape back in --
  # so this asserts the BEHAVIOR: installing the fakes must not touch `ran`.
  before <- openac_registry$ran
  local_fake_tools()
  expect_identical(openac_registry$ran, before)
  local_fake_downloads()
  expect_identical(openac_registry$ran, before)
})
