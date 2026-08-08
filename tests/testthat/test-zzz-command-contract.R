# AC2 -- the coverage gate (D-010).
#
# Every openac function that can reach an external tool must have a command
# test. The domain is COMPUTED from the call graph rather than listed by hand,
# so a new wrapper cannot be added without either a test or an explicit
# deferral. Named test-zzz-* so it runs after the files whose calls it counts.

# The canary, deliberately FIRST in the file (D-013).
#
# It asserts this file's own execution was recorded, through the same shadow
# every other file is recorded by -- there is no self-registration path, and
# adding one would satisfy this assertion while the recorder was dead, which is
# the exact failure it exists to catch. Being first means a later top-level
# error in this file cannot stop it running.
#
# It RUNS, never skips, under every invocation: a full run, a filtered run, and
# `test_file()` on this file alone. So a broken recorder fails the next run of
# any scope, rather than turning the gate into a permanent silent skip.
test_that("the contract file's own execution is recorded", {
  expect_true("test-zzz-command-contract.R" %in% recorded_test_files())
})

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

# Test files using a form the recorder cannot see.
#
# DIAGNOSTIC ONLY -- it turns "these files did not run" into "and here is the
# likely reason", and the guarantee remains `setdiff(expected, ran)`, which
# needs no help from this. Bounded to what it can honestly claim: a file
# executing at least one BARE `test_that()` is recorded whatever else it also
# contains, so this reports usage, never non-execution.
#
# Comments are stripped before matching, and the literals are assembled rather
# than written out: a file that merely NAMES these forms -- this one does, in
# the prose above -- is not using them, and a scan that cannot tell the
# difference reports its own documentation as a violation.
bypassing_forms <- function(dir) {
  pattern <- paste0(
    "testthat::", "test_that\\(", "|", "\\bdescribe\\(", "|", "\\bit\\("
  )
  files <- expected_test_files(dir)
  hits <- vapply(files, function(f) {
    code <- sub("#.*$", "", readLines(file.path(dir, f), warn = FALSE))
    any(grepl(pattern, code))
  }, logical(1))
  files[hits]
}

test_that("no test file reaches past the recording shadow", {
  # `test_that()` is shadowed from the helper, so a qualified call, `describe()`
  # or `it()` runs the test without recording its file -- which would make the
  # suite look incomplete forever, or, worse, hide a file from the gate
  # (D-013). Asserted rather than documented, because documentation does not
  # fail.
  expect_identical(bypassing_forms(test_path(".")), character())
})

test_that("every tool-calling function has a command test", {
  # The decision is made by CALLING the pure function -- this test carries no
  # skip or failure path of its own for completeness, so there is nowhere for a
  # second, divergent rule to grow (D-013).
  decision <- contract_decision(
    expected = expected_test_files(test_path(".")),
    ran = recorded_test_files(),
    covered = registered_owners(),
    domain = system2_closure(),
    deferred = names(deferred),
    declared_full = declared_full_run()
  )

  if (identical(decision$action, "skip_partial")) {
    skip(paste0(
      "the command contract needs a complete run; these test files did not ",
      "run: ", paste(decision$files, collapse = ", "),
      ". Run the full suite (devtools::test()) to enforce it."
    ))
  }

  expect_identical(
    decision$action, "enforce_pass",
    info = switch(
      decision$action,
      fail_incomplete = paste0(
        "OPENAC_FULL_SUITE declared a complete run, but these test files did ",
        "not run: ", paste(decision$files, collapse = ", "),
        if (length(bypassing_forms(test_path(".")))) paste0(
          ". These files use a form the recorder cannot see: ",
          paste(bypassing_forms(test_path(".")), collapse = ", ")
        ) else ""
      ),
      fail_broken_attribution = paste0(
        "every test file ran, yet no boundary call was attributed to any ",
        "openac function. The coverage recorder in local_fake_tools() is ",
        "broken -- this is not an empty domain."
      ),
      enforce_fail = paste0(
        "These functions can reach an external tool but no test asserts the ",
        "command they build: ", paste(decision$uncovered, collapse = ", "),
        ". Add a command test, or defer them explicitly with the milestone ",
        "that will cover them."
      ),
      decision$action
    )
  )
})
