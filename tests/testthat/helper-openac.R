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

registered_owners <- function() sort(unique(openac_registry$owners))

# Programs `find_program()` knows about; the fake resolver serves these.
fake_programs <- function() c("ffmpeg", "ffprobe", "openface", "opensmile")

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

# Install fakes for the tool boundary, scoped to the calling test.
#
# `results` is a queue of return values, one per `system2()` call, consumed in
# order. Exhausting it is an ERROR, not a recycle or a NULL: an unexpected extra
# boundary call must fail loudly rather than silently eat the next fake.
#
# `resolve` names the programs that appear installed; anything else resolves to
# "" so the not-found paths of `find_program()` are reachable.
# Would a real `Sys.which()` resolve this path? It reports "" for a file that
# exists but is not executable, which on Unix is `file.access(mode = 1)`.
# Windows has no execute bit: executability there comes from the extension
# (.exe/.bat/.cmd), and `file.access(mode = 1)` is documented as not really
# meaningful, returning -1 for an extensionless file whatever `Sys.chmod()`
# did. So the fixture binaries this harness creates -- extensionless, chmod
# 0755 -- resolve on Unix and never on Windows unless executability degrades
# to existence there (M08: two CI failures in test-programs-resolve.R).
fake_is_executable <- function(path) {
  if (.Platform$OS.type == "windows") {
    file.exists(path)
  } else {
    file.access(path, 1L) == 0L
  }
}

local_fake_tools <- function(results = list(),
                             resolve = fake_programs(),
                             .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)

  # A tool tree shaped like a real openSMILE install: the binary sits in bin/,
  # so `os_check_config()` resolves `dirname(find_opensmile())/../config/` to
  # the config/ sibling below.
  bindir <- file.path(dir, "bin")
  dir.create(bindir, recursive = TRUE)

  # Real, executable files: `find_program()` calls `tools::file_path_as_absolute()`
  # on what it resolves, which errors on a path that does not exist, and
  # `Sys.which()` reports "" for a file that exists but is not executable.
  for (p in resolve) {
    bin <- file.path(bindir, p)
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

  fake_system2 <- function(command, args = character(), ...) {
    stack <- openac_stack()
    state$i <- state$i + 1L
    state$calls[[state$i]] <- list(
      tool = basename(as.character(command)[[1]]),
      command = as.character(command)[[1]],
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
          state$i, basename(as.character(command)[[1]])
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

  fake_sys_which <- function(names) {
    out <- vapply(
      names,
      function(n) {
        if (n %in% resolve) {
          file.path(bindir, n)
        } else if (nzchar(n) && file.exists(n) && fake_is_executable(n)) {
          n
        } else {
          ""
        }
      },
      character(1)
    )
    stats::setNames(out, names)
  }

  testthat::local_mocked_bindings(
    system2 = fake_system2,
    Sys.which = fake_sys_which,
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

# --- accessors over a recorder returned by local_fake_tools() ----------------

# The ordered (tool, args) pairs seen at the boundary.
boundary_calls <- function(state) {
  lapply(state$calls, function(x) list(tool = x$tool, args = x$args))
}

# Just the tools, in call order.
boundary_tools <- function(state) {
  vapply(state$calls, function(x) x$tool, character(1))
}

# Just the argument strings, in call order.
boundary_args <- function(state) {
  vapply(state$calls, function(x) paste(x$args, collapse = " "), character(1))
}

# The outermost openac function responsible for each boundary call.
boundary_owners <- function(state) {
  vapply(
    state$calls,
    function(x) if (length(x$stack)) x$stack[[1]] else NA_character_,
    character(1)
  )
}
