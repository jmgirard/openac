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

# --- no test file may skip before its tests run -----------------------------

# The recorded set is execution-driven, which leaves one hole: a file that skips
# at TOP LEVEL executes no `test_that()` at all, so nothing records it and a
# declared-full run reads it as a file that never ran (M10 review D20). M11
# closes the hole at the door rather than widening the observation -- such a file
# may not exist here, and `top_level_skips()` is what says so.
#
# Asserted over a CONTENT directory: static, never executed, so it can hold both
# a member that does not parse and members that would abort a real run.
#
# The fixture set is chosen to kill the wrong implementations, not merely to
# exercise the right one. A column-anchored text scan (`^skip`) passes a naive
# fixture set and is exactly the proxy shape D-013 retired, so `test-top-indented.R`
# (a top-level skip that is NOT at column 0) and `test-skipname.R` (a column-0
# line beginning `skip` that is not a skip) are both here. A scanner whose domain
# is `^test-` rather than testthat's own pattern is the hole M10's review closed,
# so `test_underscore.R` and `testbare.R` carry top-level skips too.
skip_fixture_contents <- function() {
  list(
    # Reported: the top level skips, so the file never reaches its tests.
    "test-top-bare.R" = c('skip("nope")',
                          'test_that("a", { expect_true(TRUE) })'),
    "test-top-qualified.R" = c('testthat::skip_on_cran()',
                               'test_that("b", { expect_true(TRUE) })'),
    "test-top-indented.R" = c('  skip("indented but still top level")',
                              'test_that("c", { expect_true(TRUE) })'),
    "test-top-guarded.R" = c('if (TRUE) skip("conditional")',
                             'test_that("d", { expect_true(TRUE) })'),
    "test-top-local.R" = c('local({ skip("wrapped") })',
                           'test_that("e", { expect_true(TRUE) })'),
    "test_underscore.R" = c('skip("nope")',
                            'test_that("u", { expect_true(TRUE) })'),
    "testbare.R" = c('skip("nope")',
                     'test_that("v", { expect_true(TRUE) })'),
    # Not reported: each file still runs its tests.
    "test-inner.R" = 'test_that("f", { skip("fine here"); expect_true(TRUE) })',
    "test-clean.R" = 'test_that("g", { expect_true(TRUE) })',
    # A column-0 line starting `skip` that is not a skip call.
    "test-skipname.R" = c('skipper <- function() NULL',
                          'test_that("h", { expect_true(TRUE) })'),
    # DEFINING a skip is not skipping -- the body never runs at top level.
    "test-fn-def.R" = c('gate <- function() skip("only if called")',
                        'test_that("i", { expect_true(TRUE) })'),
    # Not a test file, so not in the domain however it skips.
    "helper-top-skip.R" = 'skip("helpers are not the gate\'s business")'
  )
}

# Sorted here rather than written out sorted: `sort()` collates by locale, and
# hyphen-vs-underscore ordering differs between them, so a hand-sorted literal
# would assert this machine's collation on every CI platform.
skip_fixture_expected <- function() {
  sort(c("test-top-bare.R", "test-top-qualified.R", "test-top-indented.R",
         "test-top-guarded.R", "test-top-local.R", "test_underscore.R",
         "testbare.R"))
}

test_that("top_level_skips() reports the files that skip before their tests run", {
  dir <- write_fixture_dir(skip_fixture_contents())
  expect_identical(top_level_skips(dir), skip_fixture_expected())
})

test_that("top_level_skips() survives a member that does not parse", {
  # A non-parsing member aborts a real run outright (measured above), so it can
  # never hide a top-level skip from a suite that executes. What it must not do
  # is take the scanner down with it -- the scanner runs over the same directory
  # the parse error lives in.
  contents <- skip_fixture_contents()
  contents[["test-garbage.R"]] <- 'test_that("unclosed", { expect_true(TRUE'
  dir <- write_fixture_dir(contents)

  expect_no_error(result <- top_level_skips(dir))
  expect_no_warning(top_level_skips(dir))
  expect_identical(result, skip_fixture_expected())
})

# Review findings F1/F3/F4, each in its own directory rather than added to the
# set above: that set is fixed at twelve members by AC1, and growing it would
# change an assertion the criterion states literally.
test_that("top_level_skips() sees an applied function and an indirect call", {
  # F1: an immediately-invoked function expression RUNS its body, so the
  # `function` exclusion must not swallow it -- the head-position test is what
  # separates it from `gate <- function() skip()` two lines down.
  # F3: `do.call` holds its callee as a symbol or a string, so no call to
  # `skip` exists in the tree for the walk to find.
  # F4: `skipper()` is a call whose name merely begins "skip"; reporting it
  # tells its author to move a skip that is not there.
  dir <- write_fixture_dir(list(
    "test-iife.R" = c('(function() skip("invoked here"))()',
                      'test_that("a", { expect_true(TRUE) })'),
    "test-iife-lambda.R" = c('(\\() skip("invoked here"))()',
                             'test_that("b", { expect_true(TRUE) })'),
    "test-docall-symbol.R" = c('do.call(skip, list("x"))',
                               'test_that("c", { expect_true(TRUE) })'),
    "test-docall-string.R" = c('do.call("skip_on_cran", list())',
                               'test_that("d", { expect_true(TRUE) })'),
    "test-docall-named.R" = c('do.call(what = skip, args = list("x"))',
                              'test_that("e", { expect_true(TRUE) })'),
    # Not reported: a call whose name only begins "skip", a definition that is
    # never applied, and an unrelated `do.call`.
    "test-skipper-called.R" = c('skipper <- function() NULL', 'skipper()',
                                'test_that("f", { expect_true(TRUE) })'),
    "test-def-only.R" = c('gate <- function() skip("only if called")',
                          'test_that("g", { expect_true(TRUE) })'),
    "test-docall-other.R" = c('do.call(sum, list(1, 2))',
                              'test_that("h", { expect_true(TRUE) })')
  ))

  expect_identical(
    top_level_skips(dir),
    sort(c("test-iife.R", "test-iife-lambda.R", "test-docall-symbol.R",
           "test-docall-string.R", "test-docall-named.R"))
  )
})

# --- the runner's full-run declaration --------------------------------------

# `declaration_present()` answers "does this runner start the suite with the
# declaration on", which is not "does a declaration appear somewhere in it".
# Every case below returned the wrong answer under the first cut (review
# F11/F12/F14), and each is a plausible edit to a nine-line file.
test_that("declaration_present() reads the state the run actually starts with", {
  runner <- function(lines) {
    path <- withr::local_tempfile(fileext = ".R", .local_envir = parent.frame(2))
    writeLines(lines, path)
    path
  }
  declares <- 'Sys.setenv(OPENAC_FULL_SUITE = "true")'
  runs <- 'test_check("openac")'

  expect_true(declaration_present(runner(c(declares, runs))))

  # Declared only AFTER the suite has already run.
  expect_false(declaration_present(runner(c(runs, declares))))
  # Turned back off, or unset, before the run.
  expect_false(declaration_present(runner(c(
    declares, 'Sys.setenv(OPENAC_FULL_SUITE = "false")', runs
  ))))
  expect_false(declaration_present(runner(c(
    declares, 'Sys.unsetenv("OPENAC_FULL_SUITE")', runs
  ))))
  # Re-declared after being unset is on again -- last write before the run.
  expect_true(declaration_present(runner(c(
    'Sys.unsetenv("OPENAC_FULL_SUITE")', declares, runs
  ))))
  # A value no static read can resolve is not a declaration, and must not be an
  # error either: the check has to be able to SAY the runner stopped declaring.
  expect_false(
    declaration_present(runner(c('flag <- "true"',
                                 "Sys.setenv(OPENAC_FULL_SUITE = flag)", runs)))
  )
  # No declaration at all, and a file that does not parse.
  expect_false(declaration_present(runner(runs)))
  expect_false(declaration_present(runner('test_check("openac"')))
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

test_that("a test a helper generates is recorded against the file that ran it", {
  # M10's review found the recorder answering with the file a test body was
  # WRITTEN in: a generator living in `helper-*.R` put the helper into `ran` and
  # left the test file that called it out, so a file that really ran read as
  # missing. The fixture reproduces exactly that shape.
  registry <- run_fixture_suite(
    list(
      "test-direct.R" = 'test_that("direct", { expect_true(TRUE) })',
      "test-viahelper.R" = 'generate_test("via helper")'
    ),
    helper_extra =
      'generate_test <- function(desc) test_that(desc, { expect_true(TRUE) })'
  )

  ran <- sort(unique(registry$ran))
  expect_setequal(ran, c("test-direct.R", "test-viahelper.R"))
  # And the helper it was written in is NOT what got credited.
  expect_false("helper-openac.R" %in% ran)
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
