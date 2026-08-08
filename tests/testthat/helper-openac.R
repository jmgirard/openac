# Test harness for the external-tool boundary (M06, GP7 layer 1).
#
# openac reaches every wrapped CLI through a single chokepoint: `system2()`,
# called from the four passthroughs. Mocking the passthroughs themselves is not
# enough -- the exported aliases `ffm`/`ffp`/`of`/`os` are separate bindings to
# the same closures, so rebinding `ffmpeg` does not intercept `ffm`, and a real
# binary runs (D-010). So we mock `base::system2` and, for determinism,
# `base::Sys.which`.

# Suite-wide record of which openac function drove each boundary call, and of
# which test files actually executed. Accumulates across test files within one
# run; the command-contract test reads `owners` to decide which members of the
# computed domain the suite covers (D-010), and `ran` to decide whether the run
# was complete enough for that question to mean anything (D-013).
openac_registry <- new.env(parent = emptyenv())
openac_registry$owners <- character()
openac_registry$ran <- character()

registered_owners <- function() sort(unique(openac_registry$owners))

# The test files observed to have executed at least one test.
recorded_test_files <- function() sort(unique(openac_registry$ran))

# The test files a complete run of `dir` is expected to execute.
#
# Content-free BY CONSTRUCTION: it lists names and never opens a file, so
# nothing any contributor writes INSIDE a test file can add to or remove from
# this set. That is the guarantee, and it is behavioral -- the set is identical
# to `sort(list.files(dir, pattern = "^test.*\\.[rR]$"))` under arbitrary
# mutation of every member's contents. The two proxies this replaces derived
# the expected set from what the files SAID (an install count, then a text
# search), and both diverged from what the suite DID, each time leaving the
# coverage gate silently disarmed (D-013).
#
# The pattern is testthat's OWN, copied from `find_test_scripts()`
# (`dir(path, "^test.*\\.[rR]$")`, testthat 3.3.2), so the expected set is
# exactly what the runner executes. It read `^test-.*\\.[Rr]$` until M10's
# review: a narrower expectation than the runner's discovery is a hole, not a
# convention -- a `test_foo.R` ran, sorted after this file, and was required by
# nothing.
#
# Parameterized by directory so the fixture suites can hold it to that
# guarantee over deliberately hostile contents -- including a member that does
# not parse, which no content-reading implementation survives.
expected_test_files <- function(dir) {
  sort(list.files(dir, pattern = "^test.*\\.[rR]$"))
}

# Does this expression skip, in a position that aborts the file it sits in?
#
# TRUE for a call to anything named `skip*` -- `skip()`, `skip_on_cran()`,
# `skip_if_not_installed()`, qualified or bare -- occurring anywhere in the
# expression, EXCEPT inside the two subtrees where a skip cannot abort a file:
#
#   test_that(...)   a skip there ends that one test, which is the whole point
#                    of a skip and the placement GP7 asks for.
#   function(...)    defining a skip is not performing one; the body runs only
#                    if something calls it.
#
# Matching the call ANYWHERE rather than at the top-level call head is
# deliberate: `if (cond) skip()`, `local({ skip() })` and
# `suppressWarnings(skip_on_cran())` each abort a file exactly as a bare
# `skip()` does, and a head-only match reports none of them. The residual hole
# is the mirror of the `function` exclusion -- a top-level call to a locally
# defined wrapper that itself skips -- and is disclosed rather than closed,
# because closing it means resolving values, which a static scan cannot do.
skip_call_present <- function(x) {
  if (!is.call(x)) return(FALSE)
  head <- x[[1L]]
  head_is <- function(h, what) {
    is.call(h) && as.character(h[[1L]])[[1L]] %in% what
  }
  name <- if (is.symbol(head)) {
    as.character(head)
  } else if (head_is(head, c("::", ":::"))) {
    as.character(head[[3L]])
  } else {
    ""
  }
  if (name %in% c("test_that", "function")) return(FALSE)
  # `skip` and `skip_*`, never every name merely BEGINNING "skip": a bare
  # `^skip` prefix reports a call to `skipper()` or `skips_expected()` and tells
  # its author to move a skip that is not there (review F4). testthat's whole
  # skip surface -- `skip`, `skip_if`, `skip_if_not`, `skip_if_not_installed`,
  # `skip_on_cran`, `skip_on_os`, `skip_on_ci`, ... -- matches this.
  if (grepl("^skip($|_)", name)) return(TRUE)
  # `do.call(skip, ...)` and `do.call("skip", ...)` hold the callee as a SYMBOL
  # or a string, so no call to it exists in the tree and the walk below cannot
  # see it (review F3). `language_symbols()` in `test-zzz-command-contract.R`
  # exists for this same defeat (D-010); this is its narrow instance.
  if (identical(name, "do.call")) {
    what <- as.list(x)[-1L]
    what <- if (!is.null(what[["what"]])) what[["what"]] else what[[1L]]
    if ((is.symbol(what) || is.character(what)) &&
        grepl("^skip($|_)", as.character(what))) {
      return(TRUE)
    }
  }
  # An immediately-invoked function expression RUNS its body right here, so the
  # `function` exclusion above must not reach it (review F1). The exclusion is
  # meant for a definition -- `gate <- function() skip()` describes a skip and
  # performs none -- and the discriminator is application, which is visible:
  # the definition sits in the CALL HEAD rather than in an argument.
  # The parentheses an IIFE needs survive in the AST as a `(` call wrapping the
  # definition, so the head is `(function() ...)` rather than `function() ...`
  # -- measured, after the first cut of this branch reported neither IIFE form.
  applied <- head
  while (head_is(applied, "(")) applied <- applied[[2L]]
  if (head_is(applied, "function") &&
      any(vapply(as.list(applied)[-1L], skip_call_present, logical(1)))) {
    return(TRUE)
  }
  any(vapply(as.list(x)[-1L], skip_call_present, logical(1)))
}

# The test files that skip before their tests can run.
#
# A file whose top level skips executes no `test_that()`, so the execution-time
# recorder never sees it and a declared-full run reports it as a file that never
# ran -- the coverage gate's one blind spot (M10 review D20). Rather than widen
# what counts as "ran", M11 forbids the shape: `test-zzz-command-contract.R`
# fails while this returns anything.
#
# The domain is `expected_test_files()`, not a narrower name pattern -- the same
# set testthat itself discovers, for the reason recorded above it. A member that
# does not parse is NOT reported: it cannot hide a top-level skip from a suite
# that executes, because a parse error aborts the whole run first (measured in
# `test-harness-recording.R`), and reporting it here would blame this rule for a
# different defect.
top_level_skips <- function(dir) {
  files <- expected_test_files(dir)
  hits <- vapply(files, function(f) {
    exprs <- tryCatch(
      parse(file.path(dir, f), keep.source = FALSE),
      error = function(e) NULL
    )
    if (is.null(exprs)) return(FALSE)
    any(vapply(as.list(exprs), skip_call_present, logical(1)))
  }, logical(1))
  unname(files[hits])
}

# Did the runner DECLARE this an unfiltered run of the whole suite?
#
# `tests/testthat.R` sets the variable and is the only thing that can honestly
# know -- it is the entry point `R CMD check` and CI take, and it runs
# `test_check()` unfiltered. A local `devtools::test()` never sources that file,
# so an interactive run is undeclared and a partial one merely skips.
declared_full_run <- function() {
  isTRUE(as.logical(Sys.getenv("OPENAC_FULL_SUITE", "false")))
}

# Does the runner file at `path` still DECLARE a full run?
#
# `declared_full_run()` above reads an environment variable, and exactly one
# line in the package sets it: `Sys.setenv(OPENAC_FULL_SUITE = "true")` in
# `tests/testthat.R`. Delete that line and every incompleteness downgrades from
# a failure to a skip -- silently, because the suite then PASSES: the files that
# would have been missing are missing only under conditions the declaration is
# what makes fatal.
#
# Checked by parsing the runner rather than by reading the environment, because
# an environment check can only ask "are we under R CMD check?", and every
# answer to that is an undocumented internal that fails OPEN when it changes
# (the failure mode D-013 rejected internals for). The file either contains the
# declaration or it does not, in every run mode, with nothing to go stale.
declaration_present <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(FALSE)

  call_name <- function(x) {
    if (!is.call(x)) return("")
    head <- x[[1L]]
    if (is.symbol(head)) {
      as.character(head)
    } else if (is.call(head) &&
               as.character(head[[1L]])[[1L]] %in% c("::", ":::")) {
      as.character(head[[3L]])
    } else {
      ""
    }
  }

  # Walk top level IN ORDER and keep the state the run actually starts with,
  # rather than asking whether a declaration appears anywhere. `any()` was the
  # first cut and it read TRUE for three runners that do not declare a full run
  # (review F11/F12): a `Sys.setenv()` placed AFTER `test_check()`, a later
  # `Sys.unsetenv()`, and a later re-set to "false". The last write before the
  # run is the only one that matters, so that is what this reads.
  declared <- FALSE
  for (x in as.list(exprs)) {
    name <- call_name(x)
    if (identical(name, "test_check")) break
    if (identical(name, "Sys.setenv")) {
      value <- as.list(x)[-1L][["OPENAC_FULL_SUITE"]]
      # A non-literal (`Sys.setenv(OPENAC_FULL_SUITE = flag)`) cannot be read
      # statically; it errored out of `as.logical()` before review F14. Unknown
      # is not declared -- fail closed and let the check say so.
      if (!is.null(value)) {
        declared <- (is.character(value) || is.logical(value)) &&
          isTRUE(as.logical(value))
      }
    } else if (identical(name, "Sys.unsetenv")) {
      if (any(vapply(as.list(x)[-1L],
                     function(a) identical(as.character(a), "OPENAC_FULL_SUITE"),
                     logical(1)))) {
        declared <- FALSE
      }
    }
  }
  declared
}

# The skip/fail/enforce decision, as a PURE function of the six facts the
# contract test can observe. Pure so that every branch -- including the ones a
# healthy suite must never take -- is reachable from a unit test with ordinary
# arguments, rather than only by breaking the real suite (D-013).
#
# The five returns, in the order they are decided:
#
#   fail_incomplete          files are missing and the runner declared a full
#                            run: the declaration and the observation disagree,
#                            which is the case CI and `R CMD check` must fail on
#   skip_partial             files are missing and nothing declared a full run:
#                            an ordinary filtered local run, skipped with the
#                            missing files named
#   fail_broken_attribution  every file ran, yet no boundary call was attributed
#                            to anything -- the coverage recorder is dead, and
#                            an empty `covered` would otherwise read as "the
#                            domain is uncovered" or, worse, pass vacuously
#   enforce_fail             every file ran and some enforced function has no
#                            command test
#   enforce_pass             every file ran and every enforced function is
#                            covered
contract_decision <- function(expected, ran, covered, domain, deferred,
                              declared_full) {
  missing <- setdiff(expected, ran)
  if (length(missing)) {
    action <- if (isTRUE(declared_full)) "fail_incomplete" else "skip_partial"
    return(list(action = action, files = missing))
  }
  if (!length(covered)) return(list(action = "fail_broken_attribution"))
  uncovered <- setdiff(setdiff(domain, deferred), covered)
  if (length(uncovered)) {
    return(list(action = "enforce_fail", uncovered = uncovered))
  }
  list(action = "enforce_pass")
}

# The test file currently EXECUTING, or NA.
#
# Read from the innermost `source_file()` frame testthat runs each file inside,
# whose `path` is the file being sourced right now. That is the file the gate
# needs: `expected` is a directory listing, so a recorded name only cancels a
# missing one when it names the file testthat is running.
#
# The srcref of the test body is the FALLBACK, not the primary, and M10's review
# is why. `getSrcFilename(substitute(code))` names the file the expression was
# WRITTEN in, which differs from the file executing whenever a helper generates
# tests: a `helper-*.R` generator called from `test-x.R` put `helper-openac.R`
# into `ran` and left `test-x.R` out of it -- a real test file the gate then
# reported as never run. It stays as the fallback because a run with source
# references intact but no `source_file()` frame -- `test_that()` called outside
# testthat's own sourcing -- has nothing else to answer with.
#
# Both routes FAIL CLOSED. Whatever breaks them, `ran` comes back short and the
# contract file's canary -- which asserts its OWN name is in `ran`, recorded
# through this same path -- fails on the next run of any scope. That is the
# whole reason the canary exists (D-013): a recorder that silently stops
# recording must not read as a suite that silently stopped running.
harness_caller_file <- function(expr = NULL) {
  frames <- sys.frames()
  calls <- sys.calls()
  for (i in rev(seq_along(calls))) {
    head <- calls[[i]][[1L]]
    name <- if (is.symbol(head)) {
      as.character(head)
    } else if (is.call(head) &&
               as.character(head[[1L]])[[1L]] %in% c("::", ":::")) {
      as.character(head[[3L]])
    } else {
      next
    }
    if (!identical(name, "source_file")) next
    path <- tryCatch(get("path", envir = frames[[i]], inherits = FALSE),
                     error = function(e) NULL)
    if (is.character(path) && length(path) == 1L && nzchar(path)) {
      return(basename(path))
    }
  }

  file <- tryCatch(utils::getSrcFilename(expr), error = function(e) character())
  if (length(file) == 1L && nzchar(file)) return(basename(file))
  NA_character_
}

# Shadow `test_that()` so that running a test RECORDS its file.
#
# Completeness used to be inferred from the content of test files -- an install
# count, then a text search -- and both proxies diverged from the thing proxied,
# leaving the coverage gate silently disarmed (D-013). This records the fact
# itself, at execution time: a file joins `ran` because one of its tests ran,
# not because something about its text suggested one would.
#
# testthat sources helper files into the environment test files are evaluated
# in, so this binding shadows `testthat::test_that` for every test file in the
# suite -- which is why this suite may only call `test_that()` bare. A qualified
# `testthat::test_that()`, and `describe()`/`it()`, reach past the shadow and
# are forbidden here (D-013); `test-zzz-command-contract.R` asserts their
# absence.
#
# The call is forwarded UNEVALUATED to the real `test_that()`: `code` is never
# forced here, so the test body runs exactly once, inside testthat's own
# handlers, and a test that fails or skips is recorded exactly as one that
# passes. Recording before the forward is deliberate -- a file whose only test
# begins with `skip()` still ran.
test_that <- function(desc, code) {
  file <- harness_caller_file(substitute(code))
  if (!is.na(file)) openac_registry$ran <- c(openac_registry$ran, file)
  call <- match.call()
  call[[1L]] <- quote(testthat::test_that)
  eval.parent(call)
}

# Programs `find_program()` knows about; the fake resolver serves these.
fake_programs <- function() c("ffmpeg", "ffprobe", "openface", "opensmile")

# The file name a fixture binary must carry for `Sys.which()` to resolve it on
# the platform being SIMULATED. Windows needs an extension (see
# `fake_is_executable()`); an extensionless fixture there is what a real Windows
# install would never have, and testing against one asserts a resolution the
# platform refuses.
#
# `os` reads the simulated platform, the same source `fake_is_executable()`
# reads, because the two must agree: this function decides what the fake tree
# CONTAINS and the predicate decides what resolves out of it, so one reading the
# host while the other reads `local_fake_os()`'s value would build a tree its
# own resolver refuses.
fake_program_file <- function(name, os = Sys.info()[["sysname"]]) {
  paste0(name, if (identical(os, "Windows")) ".exe" else "")
}

# Drop that extension again, so assertions read the same on every platform. The
# set is `fake_win_exts()`, not a second list beside it: two lists drift, and
# one that strips less than the fixture namer adds leaves a `.exe` in an
# assertion that reads bare everywhere else.
fake_program_name <- function(file) {
  pattern <- paste0("(", paste(gsub(".", "\\.", fake_win_exts(), fixed = TRUE),
                               collapse = "|"), ")$")
  sub(pattern, "", file, ignore.case = TRUE)
}

# Config names the fake openSMILE install ships, relative to its config/ dir
# and without the .conf extension.
fake_configs <- function() c("misc/emo_large", "egemaps/v02/eGeMAPSv02")

# Absolute path of a fake config, as os_check_config() would resolve it.
fake_config_path <- function(state, config = "misc/emo_large") {
  tools::file_path_as_absolute(
    file.path(state$bindir, "..", "config", paste0(config, ".conf"))
  )
}

# A semicolon-delimited stand-in for an openSMILE output CSV. The mocked tool
# writes nothing, but os_extract_wav() hands its outputs to os_fix_csv(), which
# reads them -- so any test exercising aggfile/lldfile must pre-create them.
write_fake_os_output <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("name;frameTime;F0", "'x';0.00;120.5"), path)
  path
}

# The openac name a function value is bound to, or NA.
#
# Aliases share one closure (`ffm` and `ffmpeg` are the same object), so a hit
# can be ambiguous; the longest name wins, which is the primary name in every
# alias pair openac exports (ffmpeg/ffm, ffprobe/ffp, openface/of, opensmile/os).
openac_name_of <- function(f, ns) {
  hits <- Filter(
    function(n) {
      obj <- get(n, envir = ns)
      is.function(obj) && identical(obj, f)
    },
    ls(ns, all.names = TRUE)
  )
  if (!length(hits)) return(NA_character_)
  hits[order(-nchar(hits), hits)][[1]]
}

# Names of the openac functions on the current call stack, outermost first.
#
# A frame belongs to openac when its environment's top-level environment is the
# package namespace. Coverage is attributed to the OUTERMOST such frame: testing
# `os_check_audio()` must not mark `ffp_count_streams()`, `ffprobe()` and `ffp()`
# as covered, since none of them was tested directly (D-010).
openac_stack <- function() {
  ns <- asNamespace("openac")
  frames <- sys.frames()
  calls <- sys.calls()
  out <- character()
  for (i in seq_along(frames)) {
    if (!identical(topenv(frames[[i]]), ns)) next
    head <- calls[[i]][[1]]
    name <- if (is.symbol(head)) {
      as.character(head)
    } else if (is.call(head) && as.character(head[[1]])[[1]] %in% c("::", ":::")) {
      as.character(head[[3]])
    } else {
      # `do.call(what = <function value>, ...)` -- the form os_extract_dir() and
      # aw_transcribe_dir() dispatch through -- leaves a function, not a name,
      # in the call head. Dropping the frame would attribute the call to the
      # inner passthrough and mark it covered by a test of the outer function.
      openac_name_of(sys.function(i), ns)
    }
    if (!is.na(name)) out <- c(out, name)
  }
  out
}

# Would a real `Sys.which()` resolve this path? MEASURED on GitHub runners
# (R 4.6.1, M09 probe workflow), not assumed -- guessing this is how M08's CI
# went red, and the guess it was replaced with was wrong too:
#
#   Unix     resolves an existing file iff `file.access(path, 1L) == 0`.
#            The extension is irrelevant: `tool.txt` at 0755 resolved.
#   Windows  resolves an existing file iff it carries ANY extension -- `.exe`,
#            `.bat`, `.cmd`, `.com` and `.txt` all resolved, and `.txt` did so
#            at 0755 while `file.access()` reported -1, so the mode is
#            irrelevant there -- or, for an extensionless path, iff a sibling
#            carrying an EXECUTABLE extension exists. That last case is how a
#            recorded `SMILExtract` resolves to `SMILExtract.exe`.
#
# The sibling set is measured, not assumed, and it is not just `.exe`: a second
# probe gave each case its own directory holding exactly ONE file and always
# asked for the extensionless name. `.exe`, `.bat`, `.cmd` and `.com` siblings
# all resolved; a `.txt` sibling did not, and neither did an extensionless file
# with no sibling. (The first probe created `tool` and `tool.exe` together, so
# it could not see any of this -- it was the extensionless file answering.)
# Which sibling wins when several exist was NOT measured, since no case
# presented two; `fake_sys_which_path()` takes them in PATHEXT's documented
# default order, and the harness only ever creates `.exe` anyway.
#
# `os` is the platform being SIMULATED and defaults to what `Sys.info()`
# reports, so `local_fake_os()` drives it and a macOS run still exercises the
# Windows rule. Simulating unix on a Windows HOST is the one case the host
# cannot answer -- a Windows filesystem has no mode bit to read, and the probe
# measured `file.access(<0755 extensionless>, 1L)` as -1 there -- so the unix
# branch degrades to existence when the host is Windows. Directories are
# excluded outright: `file.exists()` is TRUE for one and `file.access(dir, 1L)`
# is 0 for a searchable one (M07 hit this), and no tool path is ever a
# directory. That exclusion is a deliberate tightening and a MEASURED
# divergence: the real Windows `Sys.which()` returned a directory named
# `tool.exe` when asked for it. openac would then hand that directory to
# `system2()`, so the fake refuses what the platform would allow.

# The sibling extensions an extensionless Windows path resolves through, in
# PATHEXT's documented default order. `.txt` is deliberately absent: it resolves
# when named directly and does NOT resolve as a sibling, and the probe measured
# both halves of that.
fake_win_exts <- function() c(".com", ".exe", ".bat", ".cmd")

fake_is_executable <- function(path, os = Sys.info()[["sysname"]]) {
  !identical(fake_sys_which_path(path, os), "")
}

# What a real `Sys.which()` RETURNS for this path -- the resolved file, or "" --
# rather than merely whether it resolves. On Windows an extensionless path
# resolves TO its sibling, and handing back the name that was asked for instead
# hands back a path that does not exist, which `find_program()`'s
# `file_path_as_absolute()` then errors on.
fake_sys_which_path <- function(path, os = Sys.info()[["sysname"]]) {
  if (!nzchar(path)) return("")
  is_file <- function(p) file.exists(p) && !dir.exists(p)
  if (identical(os, "Windows")) {
    if (nzchar(tools::file_ext(path))) {
      return(if (is_file(path)) path else "")
    }
    # The sibling search is NOT gated on the extensionless path existing. It
    # was, until M09's review: an `!file.exists(path)` guard ran first and made
    # this whole branch dead code, so a recorded `SMILExtract` sitting beside a
    # real `SMILExtract.exe` -- the only arrangement a Windows install ever has
    # -- came back unresolved.
    for (ext in fake_win_exts()) {
      sibling <- paste0(path, ext)
      if (is_file(sibling)) return(sibling)
    }
    return("")
  }
  if (!is_file(path)) return("")
  if (.Platform$OS.type == "windows") return(path)
  if (file.access(path, 1L) == 0L) path else ""
}

# Is this an absolute path? NOT `identical(p, normalizePath(p, mustWork =
# FALSE))`, which was tried and is silently wrong: normalizePath() returns a
# path it cannot resolve unchanged, so every relative path that does not exist
# -- i.e. exactly the regression this guards against -- compared equal and
# passed. Matched instead against the three absolute forms: POSIX `/x`, UNC
# `\\\\server\\share`, and a Windows drive `C:/x` or `C:\\x`.
is_absolute_path <- function(path) {
  grepl("^(/|\\\\\\\\|[A-Za-z]:[/\\\\])", path)
}

# The one `Sys.which` fake both scoped helpers install. `resolve` names the
# programs that appear installed, served from `bindir`; anything else is
# decided by the predicate above, so the two helpers can no longer drift apart
# (they carried separate, disagreeing copies until M09).
#
# `os` defaults to NULL and is resolved on EVERY call rather than defaulting to
# `Sys.info()[["sysname"]]` in the signature. A default argument is a promise
# forced once, on first use, and cached: a fake built before `local_fake_os()`
# runs would then pin whichever platform happened to be current at its first
# `Sys.which()` call and silently ignore the faked one from there on. Callers
# that know the platform (`local_fake_tools()`) still pass it explicitly.
fake_sys_which <- function(resolve = character(), bindir = NULL, os = NULL) {
  function(names) {
    platform <- if (is.null(os)) Sys.info()[["sysname"]] else os
    out <- vapply(
      names,
      function(n) {
        if (n %in% resolve) {
          file.path(bindir, fake_program_file(n, platform))
        } else {
          fake_sys_which_path(n, platform)
        }
      },
      character(1)
    )
    stats::setNames(out, names)
  }
}

# Install fakes for the tool boundary, scoped to the calling test.
#
# `results` is a queue of return values, one per `system2()` call, consumed in
# order. Exhausting it is an ERROR, not a recycle or a NULL: an unexpected extra
# boundary call must fail loudly rather than silently eat the next fake.
#
# `resolve` names the programs that appear installed; anything else resolves to
# "" so the not-found paths of `find_program()` are reachable.
# `os` is the platform to build the fake tree for, captured ONCE here and handed
# to both the fixture namer and the resolver so they cannot disagree. It reads
# the simulated platform, so `local_fake_os("Windows")` before this call gives a
# Windows-shaped tree on any host; calling `local_fake_os()` after this one
# leaves the tree built for the earlier platform, which is why the argument
# exists as an override.
local_fake_tools <- function(results = list(),
                             resolve = fake_programs(),
                             os = Sys.info()[["sysname"]],
                             check_quoting = TRUE,
                             .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)

  # The rappdirs redirect belongs here rather than at each call site. Whenever
  # `Sys.which()` reports "", `find_program()` falls through to
  # `<user_config_dir>/<program>_location.txt` (R/programs_find.R:26) -- so any
  # test passing `resolve = character()` reads the real config dir unless it
  # remembered to redirect, and a maintainer who has ever run `set_program()`
  # has a file sitting there. Owning both dirs makes the leak unreachable
  # instead of a convention, and `state$config` / `state$data` are the single
  # source of truth for where they went.
  config_dir <- local_fake_config(.env = .env)
  data_dir <- local_fake_data_dir(.env = .env)

  # A tool tree shaped like a real openSMILE install: the binary sits in bin/,
  # so `os_check_config()` resolves `dirname(find_opensmile())/../config/` to
  # the config/ sibling below.
  bindir <- file.path(dir, "bin")
  dir.create(bindir, recursive = TRUE)

  # Real, executable files: `find_program()` calls `tools::file_path_as_absolute()`
  # on what it resolves, which errors on a path that does not exist, and
  # `Sys.which()` reports "" for a file that exists but is not executable.
  for (p in resolve) {
    bin <- file.path(bindir, fake_program_file(p, os))
    file.create(bin)
    Sys.chmod(bin, "0755")
  }

  # Config files openSMILE would ship. os_check_config() only needs them to
  # exist; the tool never reads them here.
  for (conf in fake_configs()) {
    path <- file.path(dir, "config", paste0(conf, ".conf"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("// placeholder openSMILE config", path)
  }

  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$i <- 0L
  state$dir <- dir
  state$bindir <- bindir
  state$os <- os
  state$config <- config_dir
  state$data <- data_dir

  fake_system2 <- function(command, args = character(), ...) {
    cmd <- as.character(command)[[1]]
    # IP1 says a tool location is always discovered or user-configured and
    # comes back absolute (`find_program()` ends in file_path_as_absolute()).
    # Checked HERE rather than in a few chosen tests, so it holds for every
    # call any test routes through the harness: a regression handing system2()
    # a bare name would otherwise pass every command assertion, since those
    # compare basenames and args. Decided by `is_absolute_path()`, which counts
    # a Windows `C:\...` and a UNC `\\server\share` as absolute too.
    if (!is_absolute_path(cmd)) {
      stop(
        sprintf("fake system2: command is not an absolute path: %s", cmd),
        call. = FALSE
      )
    }
    # D-017 says a multi-element `args` is the token form, and run_tool()
    # shQuote()s every element of it. So an element carrying whitespace that is
    # NOT enclosed by the platform's quoting is a token the shell will split --
    # the exact defect M13 removed, reappearing at some call site.
    #
    # Checked HERE rather than in a few chosen tests, for the same reason the
    # absolute-path check above is: it then holds for every call any test routes
    # through the harness, and the command-contract gate guarantees each function
    # that can reach a tool routes at least one. A per-test assertion is skipped
    # by omission, which is how the next assembler would slip through.
    #
    # The quote character is DERIVED from shQuote() rather than written out, so
    # this stays strict per platform: a hand-written `"..."` on unix would pass a
    # permissive both-characters test while still expanding `$`, which is the bug
    # measured at M13 T1, not a variant of it.
    if (isTRUE(check_quoting) && length(args) > 1L) {
      qc <- substr(shQuote("x"), 1L, 1L)
      bare <- vapply(as.character(args), function(el) {
        if (!grepl("[[:space:]]", el)) return(FALSE)
        !(nchar(el) >= 2L && startsWith(el, qc) && endsWith(el, qc))
      }, logical(1), USE.NAMES = FALSE)
      if (any(bare)) {
        stop(
          sprintf(
            paste0(
              "fake system2: unquoted whitespace in a token-form argument: %s. ",
              "Build the command as a token vector and let run_tool() quote it; ",
              "do not interpolate quotes at the call site."
            ),
            paste(sQuote(as.character(args)[bare]), collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    stack <- openac_stack()
    state$i <- state$i + 1L
    state$calls[[state$i]] <- list(
      tool = fake_program_name(basename(cmd)),
      command = cmd,
      args = args,
      stack = stack
    )
    # Suite-wide coverage record, read by the command-contract test.
    if (length(stack)) {
      openac_registry$owners <- c(openac_registry$owners, stack[[1]])
    }
    if (state$i > length(results)) {
      stop(
        sprintf(
          "fake system2: result queue exhausted on call %d (tool %s)",
          state$i, fake_program_name(basename(cmd))
        ),
        call. = FALSE
      )
    }
    res <- results[[state$i]]
    # A queued function stands in for a tool with a side effect: it is called
    # with the recorded (command, args) and its value is the tool's output. The
    # real tools write files their callers then read, and a plain value cannot
    # express that.
    if (is.function(res)) res(command, args) else res
  }

  testthat::local_mocked_bindings(
    system2 = fake_system2,
    Sys.which = fake_sys_which(resolve = resolve, bindir = bindir, os = os),
    .package = "base",
    .env = .env
  )

  invisible(state)
}

# Redirect openac's rappdirs config dir to a temp dir for the calling test.
local_fake_config <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  testthat::local_mocked_bindings(
    user_config_dir = function(...) dir,
    .package = "rappdirs",
    .env = .env
  )
  dir
}

# Redirect openac's rappdirs data dir -- where install_* places tools by
# default -- to a temp dir for the calling test.
local_fake_data_dir <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  testthat::local_mocked_bindings(
    user_data_dir = function(...) dir,
    .package = "rappdirs",
    .env = .env
  )
  dir
}

# Pretend the session runs on `sysname` -- a `Sys.info()[["sysname"]]` value such
# as "Windows", "Darwin" or "Linux" -- for the calling test. The rest of
# Sys.info() is left as this machine reports it, so only the platform varies.
local_fake_os <- function(sysname, .env = parent.frame()) {
  info <- Sys.info()
  info[["sysname"]] <- sysname
  testthat::local_mocked_bindings(
    Sys.info = function() info,
    .package = "base",
    .env = .env
  )
  invisible(sysname)
}

# Install fakes for the install-time boundary, scoped to the calling test.
#
# The install_* family is the only one that reaches the network, so its tests
# never let a real fetch or extraction run: both fakes record their arguments
# instead. `status` is what the download reports (0 is success, as
# `utils::download.file()` defines it); `extract_creates` names paths, relative
# to the extraction directory, that the fake archive materializes -- the
# installers hand those to `set_*()`, which refuses a location that is not
# there.
#
# `Sys.which()` is faked with the SAME shared resolver `local_fake_tools()`
# installs -- it once resolved any existing file, which is a rule no platform
# implements and which made the install tests assert against a fake nothing
# could reproduce (M07 B1/P1). It carries no `resolve` list here: the installers
# look for the files the fake extractor just wrote, so the predicate answers for
# them, reading the platform `local_fake_os()` names rather than the host's.
local_fake_downloads <- function(status = 0L,
                                 extract_creates = character(),
                                 .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$downloads <- list()
  state$extracts <- list()

  fake_download <- function(url, destfile, ...) {
    state$downloads[[length(state$downloads) + 1L]] <-
      list(url = url, destfile = destfile, args = list(...))
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
    writeLines("fake archive", destfile)
    status
  }

  fake_extract <- function(archive, dir = ".", ...) {
    state$extracts[[length(state$extracts) + 1L]] <-
      list(archive = archive, dir = dir, args = list(...))
    for (rel in extract_creates) {
      path <- file.path(dir, rel)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      file.create(path)
      Sys.chmod(path, "0755")
    }
    invisible(character())
  }

  testthat::local_mocked_bindings(
    download.file = fake_download, .package = "utils", .env = .env
  )
  testthat::local_mocked_bindings(
    archive_extract = fake_extract, .package = "archive", .env = .env
  )
  testthat::local_mocked_bindings(
    Sys.which = fake_sys_which(), .package = "base", .env = .env
  )
  invisible(state)
}

# --- accessors over a recorder returned by local_fake_downloads() ------------

download_urls <- function(state) {
  vapply(state$downloads, function(x) x$url, character(1))
}

download_dests <- function(state) {
  vapply(state$downloads, function(x) x$destfile, character(1))
}

extract_dirs <- function(state) {
  vapply(state$extracts, function(x) as.character(x$dir), character(1))
}

# --- accessors over a recorder returned by local_fake_tools() ----------------

# The ordered (tool, args) pairs seen at the boundary.
boundary_calls <- function(state) {
  lapply(state$calls, function(x) list(tool = x$tool, args = x$args))
}

# Just the tools, in call order.
boundary_tools <- function(state) {
  vapply(state$calls, function(x) x$tool, character(1))
}

# The raw `args` of each call, exactly as `system2()` received it.
#
# `boundary_args()` below collapses each call's args to one string, which is
# lossless only while every wrapper passes a single space-separated string --
# the shape openac uses today. The moment one passes a vector, the collapse
# erases the difference between `c("-i", "a b")` and `"-i a b"`, which are not
# the same command. Assertions that care about argument boundaries read this.
boundary_argv <- function(state) {
  lapply(state$calls, function(x) as.character(x$args))
}

# Just the argument strings, in call order.
boundary_args <- function(state) {
  vapply(boundary_argv(state), paste, character(1), collapse = " ")
}

# The outermost openac function responsible for each boundary call.
boundary_owners <- function(state) {
  vapply(
    state$calls,
    function(x) if (length(x$stack)) x$stack[[1]] else NA_character_,
    character(1)
  )
}
