# AC2 -- the coverage gate (D-010).
#
# Every openac function that can reach an external tool must have a command
# test. The domain is COMPUTED from the call graph rather than listed by hand,
# so a new wrapper cannot be added without either a test or an explicit
# deferral. Named test-zzz-* so it runs after the files whose calls it counts.

# Symbols occurring anywhere in a language object. Deliberately an
# over-approximation over call heads: os_extract_dir() and aw_transcribe_dir()
# reach their tools through do.call(what = os_extract, ...), where the function
# is a value, and a call-head walk cannot see them (D-010).
language_symbols <- function(x) {
  if (is.symbol(x)) return(as.character(x))
  if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
    return(unlist(lapply(as.list(x), language_symbols), use.names = FALSE))
  }
  character()
}

# Every openac function from which base::system2 is transitively reachable.
system2_closure <- function() {
  ns <- asNamespace("openac")
  fns <- Filter(
    function(n) is.function(get(n, envir = ns)),
    ls(ns, all.names = TRUE)
  )
  deps <- lapply(fns, function(n) {
    f <- get(n, envir = ns)
    unique(c(language_symbols(body(f)), language_symbols(as.list(formals(f)))))
  })
  names(deps) <- fns

  reach <- names(deps)[vapply(deps, function(d) "system2" %in% d, logical(1))]
  repeat {
    grown <- names(deps)[vapply(deps, function(d) any(d %in% reach), logical(1))]
    fresh <- setdiff(grown, reach)
    if (!length(fresh)) break
    reach <- c(reach, fresh)
  }
  sort(reach)
}

# Literal function names, never globs: a pattern like "*_dir" would be a
# permanent escape hatch that any future batch wrapper slips through. Each entry
# is annotated with the milestone that will cover it.
#
# Empty as of M07: every function that can reach an external tool has a command
# test. A new wrapper fails the suite until it has one, or until it is deferred
# here to a named milestone. Written with names so the empty vector has the same
# shape a populated one does -- a bare `character()` carries `names() == NULL`,
# and the staleness check below compares against `character()`.
deferred <- stats::setNames(character(), character())

test_that("the computed domain is non-empty and includes the passthroughs", {
  domain <- system2_closure()
  expect_gt(length(domain), 0)
  # A sanity anchor: if the graph walk silently broke, these would vanish.
  expect_true(all(c("ffmpeg", "ffprobe", "openface", "opensmile") %in% domain))
  # And the indirect case the over-approximation exists to catch.
  expect_true("os_extract_dir" %in% domain)
})

test_that("no deferral has gone stale", {
  domain <- system2_closure()
  stale <- setdiff(names(deferred), domain)
  expect_identical(
    stale, character(),
    info = paste0(
      "These names are deferred but no longer reach system2, so the deferral ",
      "is dead and should be removed: ", paste(stale, collapse = ", ")
    )
  )
})

# The test files that drive the boundary harness, read off the test directory
# rather than listed by hand -- the same rule the domain above follows. A new
# command-test file joins the expected set the moment it exists, so it cannot be
# forgotten, and the gate below cannot be quietly narrowed by deleting a name.
harness_test_files <- function() {
  # Assembled rather than written out, so THIS file does not match its own
  # search and count itself among the files that drive the harness.
  needle <- paste0("local_fake_", "tools(")
  dir <- testthat::test_path(".")
  files <- list.files(dir, pattern = "^test-.*\\.[Rr]$")
  drives <- vapply(
    files,
    function(f) {
      any(grepl(needle, readLines(file.path(dir, f), warn = FALSE), fixed = TRUE))
    },
    logical(1)
  )
  sort(files[drives])
}

test_that("every tool-calling function has a command test", {
  covered <- registered_owners()
  expected <- harness_test_files()
  ran <- harness_files()

  # The walk itself must not silently find nothing: an empty `expected` would
  # make every run below look complete.
  expect_gt(length(expected), 0)

  # Which run is this? Three states, and the gate needs all three apart.
  #
  #   nothing installed          -- this file alone. Nothing to count; skip.
  #   some harness files ran     -- a filtered run. The invariant is about the
  #                                 whole suite, so a partial answer is not a
  #                                 violation; skip. Counting INSTALLS could not
  #                                 see this case and reported every unrun
  #                                 file's functions as uncovered (M09 review).
  #   all of them ran            -- enforce, below.
  #
  # What this must never do is skip because nothing was RECORDED: that is the
  # broken-attribution case, and gating on an empty `covered` (as this did
  # until M09) made the gate skip itself exactly when it needed to fail.
  skip_if(harness_runs() == 0L, "command contract needs the full test suite")

  # Installs happened, so the files that made them must have been recorded.
  # None means `harness_caller_file()` stopped identifying test files, and the
  # completeness check below would then skip every run.
  expect_gt(
    length(ran), 0,
    label = sprintf("test files recorded across %d harness installs",
                    harness_runs())
  )

  missing <- setdiff(expected, ran)
  skip_if(
    length(missing) > 0,
    sprintf(
      "command contract needs the full test suite (%d of %d harness files ran; missing %s)",
      length(expected) - length(missing), length(expected),
      paste(missing, collapse = ", ")
    )
  )

  # The whole suite ran, so something must have been attributed. Nothing means
  # openac_stack() stopped identifying frames, and every assertion below would
  # then pass over an empty set.
  expect_gt(
    length(covered), 0,
    label = sprintf(
      "owners recorded across %d harness installs", harness_runs()
    )
  )

  domain <- system2_closure()
  enforced <- setdiff(domain, names(deferred))
  uncovered <- setdiff(enforced, covered)

  expect_identical(
    uncovered, character(),
    info = paste0(
      "These functions can reach an external tool but no test asserts the ",
      "command they build: ", paste(uncovered, collapse = ", "),
      ". Add a command test, or defer them explicitly with the milestone that ",
      "will cover them."
    )
  )
})
