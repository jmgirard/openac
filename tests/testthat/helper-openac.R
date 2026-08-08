# Test harness for the external-tool boundary (M06, GP7 layer 1).
#
# openac reaches every wrapped CLI through a single chokepoint: `system2()`,
# called from the four passthroughs. Mocking the passthroughs themselves is not
# enough -- the exported aliases `ffm`/`ffp`/`of`/`os` are separate bindings to
# the same closures, so rebinding `ffmpeg` does not intercept `ffm`, and a real
# binary runs (D-010). So we mock `base::system2` and, for determinism,
# `base::Sys.which`.

# Suite-wide record of which openac function drove each boundary call.
# Accumulates across test files within one run; the command-contract test reads
# it to decide which members of the computed domain the suite actually covers,
# so coverage is never a hand-maintained list of names (D-010).
openac_registry <- new.env(parent = emptyenv())
openac_registry$owners <- character()
# How many times the harness was INSTALLED, tracked separately from what it
# attributed. The two answer different questions, and conflating them is what
# let the coverage gate skip itself: an empty `owners` means either "no test
# file that uses the harness ran" (a single-file run -- skip) or "they ran and
# attribution recorded nothing" (broken -- must fail), and `owners` alone
# cannot tell those apart.
openac_registry$runs <- 0L

registered_owners <- function() sort(unique(openac_registry$owners))

# Whether any test in this run installed the boundary harness at all.
harness_runs <- function() openac_registry$runs

# Programs `find_program()` knows about; the fake resolver serves these.
fake_programs <- function() c("ffmpeg", "ffprobe", "openface", "opensmile")

# The file name a fixture binary must carry for the HOST's real `Sys.which()`
# to resolve it. Windows needs an extension (see `fake_is_executable()`); an
# extensionless fixture there is what a real Windows install would never have,
# and testing against one asserts a resolution the platform refuses.
fake_program_file <- function(name) {
  paste0(name, if (.Platform$OS.type == "windows") ".exe" else "")
}

# Drop that extension again, so assertions read the same on every platform.
fake_program_name <- function(file) {
  sub("\\.(exe|bat|cmd|com)$", "", file, ignore.case = TRUE)
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
#            irrelevant there -- or, for an extensionless path, iff a
#            `<path>.exe` sibling exists. That last case is how a recorded
#            `SMILExtract` resolves to `SMILExtract.exe`.
#
# `os` is the platform being SIMULATED and defaults to what `Sys.info()`
# reports, so `local_fake_os()` drives it and a macOS run still exercises the
# Windows rule. Simulating unix on a Windows HOST is the one case the host
# cannot answer -- a Windows filesystem has no mode bit to read, and the probe
# measured `file.access(<0755 extensionless>, 1L)` as -1 there -- so the unix
# branch degrades to existence when the host is Windows. Directories are
# excluded outright: `file.exists()` is TRUE for one and `file.access(dir, 1L)`
# is 0 for a searchable one (M07 hit this), and no tool path is ever a
# directory. That exclusion is a deliberate tightening, not a measured claim
# about `Sys.which()`.
fake_is_executable <- function(path, os = Sys.info()[["sysname"]]) {
  if (!nzchar(path) || !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  if (identical(os, "Windows")) {
    nzchar(tools::file_ext(path)) || file.exists(paste0(path, ".exe"))
  } else if (.Platform$OS.type == "windows") {
    TRUE
  } else {
    file.access(path, 1L) == 0L
  }
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
fake_sys_which <- function(resolve = character(), bindir = NULL) {
  function(names) {
    out <- vapply(
      names,
      function(n) {
        if (n %in% resolve) {
          file.path(bindir, fake_program_file(n))
        } else if (fake_is_executable(n)) {
          n
        } else {
          ""
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
local_fake_tools <- function(results = list(),
                             resolve = fake_programs(),
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
    bin <- file.path(bindir, fake_program_file(p))
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

  openac_registry$runs <- openac_registry$runs + 1L

  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$i <- 0L
  state$dir <- dir
  state$bindir <- bindir
  state$config <- config_dir
  state$data <- data_dir

  fake_system2 <- function(command, args = character(), ...) {
    cmd <- as.character(command)[[1]]
    # IP1 says a tool location is always discovered or user-configured and
    # comes back absolute (`find_program()` ends in file_path_as_absolute()).
    # Checked HERE rather than in a few chosen tests, so it holds for every
    # call any test routes through the harness: a regression handing system2()
    # a bare name would otherwise pass every command assertion, since those
    # compare basenames and args. normalizePath, not a "/" prefix, so a
    # Windows `C:\...` counts as absolute too.
    if (!is_absolute_path(cmd)) {
      stop(
        sprintf("fake system2: command is not an absolute path: %s", cmd),
        call. = FALSE
      )
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
          state$i, basename(cmd)
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
    Sys.which = fake_sys_which(resolve = resolve, bindir = bindir),
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
# `Sys.which()` is faked to resolve any existing file so an installer under a
# mocked OS behaves the same on every host: a real `Sys.which()` resolves
# `SMILExtract` on Unix and `SMILExtract.exe` on Windows, which would otherwise
# make the macOS installer's test fail on Windows CI and vice versa (M08).
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
