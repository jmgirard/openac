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
    "helper-ignored.R" = "x <- 1",
    "notes.txt" = "not a test file"
  ))

  ground_truth <- sort(list.files(dir, pattern = "^test-.*\\.[Rr]$"))
  expect_identical(expected_test_files(dir), ground_truth)
  expect_identical(
    expected_test_files(dir),
    c("test-alpha.R", "test-beta.r", "test-empty.R", "test-garbage.R")
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

test_that("a file whose only test skips still joins the recorded set", {
  # An EXECUTABLE fixture suite, run on a registry of its own so its file names
  # cannot leak into the real `ran`. Its helper is this suite's own
  # `helper-openac.R`, copied verbatim with only the registry binding
  # overridden -- so the recorder under test is the recorder that ships, not a
  # re-implementation of it that could agree with a broken original.
  registry <- new.env(parent = emptyenv())
  registry$owners <- character()
  registry$ran <- character()
  withr::local_options(openac_fixture_registry = registry)

  dir <- write_fixture_dir(list(
    "test-runs.R" = 'test_that("runs", { expect_true(TRUE) })',
    "test-skips.R" = 'test_that("skips", { skip("nothing to do"); expect_true(FALSE) })',
    "test-fails.R" = 'test_that("fails", { expect_true(FALSE) })'
  ))
  writeLines(
    c(readLines(test_path("helper-openac.R")),
      'openac_registry <- getOption("openac_fixture_registry")'),
    file.path(dir, "helper-openac.R")
  )

  before <- openac_registry$ran
  testthat::test_dir(dir, reporter = "silent", stop_on_failure = FALSE)

  # Every file that executed a test is recorded, regardless of its outcome --
  # the skip-only file is the case the two content proxies both got wrong.
  expect_setequal(
    sort(unique(registry$ran)),
    c("test-fails.R", "test-runs.R", "test-skips.R")
  )
  # And the nested run left the real registry alone.
  expect_identical(openac_registry$ran, before)
})
