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

test_that("a relative command is refused at the boundary", {
  # IP1: a tool location is discovered or user-configured and comes back
  # absolute. Every command assertion in the suite compares basenames and
  # args, so a regression handing system2() a bare name would pass all of
  # them -- the check lives in the recorder instead, where it sees every call.
  local_fake_tools(results = list())
  expect_error(system2("ffmpeg", "-x"), "not an absolute path")
  expect_error(system2(file.path("rel", "ffmpeg"), "-x"), "not an absolute path")
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

# AC2 -- the predicate the two Sys.which fakes share. Its rules were MEASURED
# on GitHub runners (R 4.6.1, M09 probe workflow), and the assertions below
# restate those measurements; see the comment on fake_is_executable(). The
# platform is an argument, so a macOS run still exercises the Windows rule.

test_that("the Windows rule resolves any extension, and mode is irrelevant", {
  dir <- withr::local_tempdir()
  make <- function(name, mode = "0755") {
    p <- file.path(dir, name)
    file.create(p)
    Sys.chmod(p, mode)
    p
  }

  # Measured: .exe, .bat, .cmd, .com AND .txt all resolved on Windows.
  for (ext in c(".exe", ".bat", ".cmd", ".com", ".txt")) {
    expect_true(
      fake_is_executable(make(paste0("tool", ext)), os = "Windows"),
      label = ext
    )
  }
  # Measured: a 0644 file with an extension still resolved -- the mode plays
  # no part on Windows, which is why file.access() is not consulted there.
  expect_true(fake_is_executable(make("mode.exe", "0644"), os = "Windows"))
})

test_that("the Windows rule refuses an extensionless path unless .exe exists", {
  dir <- withr::local_tempdir()
  bare <- file.path(dir, "tool")
  file.create(bare)
  Sys.chmod(bare, "0755")

  # No sibling: measured as <empty> on the runner, mode notwithstanding.
  expect_false(fake_is_executable(bare, os = "Windows"))

  # With the sibling: measured as resolving TO the sibling. This is how a
  # recorded `SMILExtract` finds `SMILExtract.exe`.
  file.create(paste0(bare, ".exe"))
  expect_true(fake_is_executable(bare, os = "Windows"))
})

test_that("the Unix rule is the execute bit, whatever the extension", {
  # file.access(path, 1L) reports 0 for root regardless of mode, and a Windows
  # host has no mode bit to read at all (the probe measured -1 for a 0755
  # extensionless file there), so the mode distinction is only observable on a
  # non-root Unix host.
  skip_on_os("windows")
  skip_if(
    identical(Sys.info()[["effective_user"]], "root"),
    "file.access() ignores mode for root"
  )
  dir <- withr::local_tempdir()
  make <- function(name, mode) {
    p <- file.path(dir, name)
    file.create(p)
    Sys.chmod(p, mode)
    p
  }

  expect_true(fake_is_executable(make("tool", "0755"), os = "Linux"))
  expect_false(fake_is_executable(make("nonexec", "0644"), os = "Linux"))
  # The extension plays no part here -- measured: tool.txt at 0755 resolved.
  expect_true(fake_is_executable(make("tool.txt", "0755"), os = "Darwin"))
  expect_false(fake_is_executable(make("nonexec.exe", "0644"), os = "Darwin"))
})

test_that("the predicate refuses what no Sys.which() would return", {
  dir <- withr::local_tempdir()
  for (os in c("Windows", "Linux")) {
    expect_false(fake_is_executable("", os = os), label = os)
    expect_false(fake_is_executable(file.path(dir, "missing"), os = os), label = os)
    expect_false(fake_is_executable(file.path(dir, "missing.exe"), os = os), label = os)
    # A directory: file.exists() is TRUE for one and file.access(dir, 1L) is 0
    # for a searchable one, so both branches would otherwise let it through.
    expect_false(fake_is_executable(dir, os = os), label = os)
  }
})

test_that("both scoped helpers resolve by the same rule", {
  # local_fake_downloads() used to carry its own Sys.which fake that resolved
  # ANY existing file -- so it disagreed with local_fake_tools() about a
  # non-executable one, and the install tests were asserting against a resolver
  # no platform implements (M07 B1/P1). Observed here rather than asserted
  # structurally: a file that exists but cannot run must resolve to "" under
  # BOTH helpers.
  skip_on_os("windows")  # where the mode bit carries no meaning
  skip_if(identical(Sys.info()[["effective_user"]], "root"))
  dir <- withr::local_tempdir()
  dud <- file.path(dir, "dud")
  file.create(dud)
  Sys.chmod(dud, "0644")

  under_downloads <- local({
    local_fake_downloads()
    Sys.which(dud)
  })
  under_tools <- local({
    local_fake_tools()
    Sys.which(dud)
  })

  expect_identical(unname(under_downloads), "")
  expect_identical(unname(under_tools), "")
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

test_that("boundary_argv() preserves argument boundaries that collapse erases", {
  # Two different commands: one passes two arguments, the other one argument
  # containing a space. `boundary_args()` renders them identically, so an
  # assertion built on it cannot tell a correctly quoted path from a wrapper
  # that split on whitespace.
  state <- local_fake_tools(results = list("a", "b"))
  bin <- file.path(state$bindir, fake_program_file("ffmpeg"))

  system2(bin, c("-i", "a b"))
  system2(bin, "-i a b")

  expect_identical(boundary_argv(state), list(c("-i", "a b"), "-i a b"))
  expect_identical(boundary_args(state)[[1]], boundary_args(state)[[2]])
})

test_that("openac_name_of() picks the primary name for every alias class", {
  # The attribution rule is "longest name wins", which is correct only while
  # every alias is shorter than its primary. The classes are COMPUTED from the
  # namespace rather than listed, so a newly added alias appears here and fails
  # this test until its primary is recorded -- rather than silently changing
  # which function a do.call()-dispatched boundary call is credited to.
  ns <- asNamespace("openac")
  fns <- Filter(function(n) is.function(get(n, envir = ns)),
                ls(ns, all.names = TRUE))
  # Group names by the closure object they are bound to.
  classes <- list()
  for (n in fns) {
    f <- get(n, envir = ns)
    hit <- NA_integer_
    for (i in seq_along(classes)) {
      if (identical(get(classes[[i]][[1]], envir = ns), f)) {
        hit <- i
        break
      }
    }
    if (is.na(hit)) classes[[length(classes) + 1L]] <- n else
      classes[[hit]] <- c(classes[[hit]], n)
  }
  aliased <- Filter(function(x) length(x) > 1L, classes)

  # The recorded answer for each class, keyed by the class's sorted names.
  primary <- c(
    "ffm,ffmpeg" = "ffmpeg",
    "ffp,ffprobe" = "ffprobe",
    "of,openface" = "openface",
    "opensmile,os" = "opensmile"
  )

  found <- vapply(aliased, function(x) paste(sort(x), collapse = ","),
                  character(1))
  expect_setequal(found, names(primary))

  for (cls in aliased) {
    key <- paste(sort(cls), collapse = ",")
    # Asked via every binding in the class: attribution must not depend on
    # which name the caller happened to use.
    for (n in cls) {
      expect_identical(
        openac_name_of(get(n, envir = ns), ns), unname(primary[[key]]),
        label = paste0(n, " (class ", key, ")")
      )
    }
  }
})
