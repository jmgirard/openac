# A guard that names no file (M19) ---------------------------------------------
#
# `dir_walk()` copies `conditionMessage()` straight into the batch table's
# `error` column, so whatever a per-file guard says is what a user reads when
# they go to find out why file 412 of 500 failed. Before M19 exactly one guard
# in that path named its file; every other one was a bare `stopifnot()`, whose
# message is the deparsed condition -- `file.exists(infile) is not TRUE` --
# naming neither the file nor the defect. The file name reached the user at all
# only because `dir_walk()` prepends a basename to its own warning, which the
# `error` column does not carry.
#
# Every guard the M19 work-log enumeration classifies as batch-reachable is
# asserted below, one case per guard, each naming itself through `info` so a red
# says which guard broke.
#
# Every assertion reads a whitespace-COLLAPSED message. `cli_abort()` bakes hard
# line breaks into `conditionMessage()` at the console width, so whether a
# phrase survives intact depends on how long the interpolated path happens to be
# -- the M14 review A3 trap.

local_media <- function(ext = ".wav", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

local_outpath <- function(name = "out.wav", .env = parent.frame()) {
  file.path(withr::local_tempdir(.local_envir = .env), name)
}

# A path nothing has ever written.
gone <- function(name = "gone.mp4") file.path(tempdir(), name)

# What os_check_audio() / aw_check_audio() see for an input each accepts: the
# stream count, then the codec/rate/channel triple.
conforming <- function() list("audio", c("pcm_s16le", "44100", "1"))
conforming_wav <- function() list("audio", c("pcm_s16le", "16000", "1"))
nonconforming <- function() list("audio", c("mp3", "44100", "2"))

fake_model <- function() structure(list(name = "tiny"), class = "whisper")

collapsed_guard <- function(expr) {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  if (is.null(cnd)) {
    return(NA_character_)
  }
  gsub("\\s+", " ", conditionMessage(cnd))
}

# --- one case per batch-reachable guard --------------------------------------
#
# `file` is the file whose basename the message must name, which is not always
# the file the guard tested: `os_extract_wav()` may be handed a temporary wav
# derived from the user's input, and it is the user's input the batch row is
# about (M17 review, finding B). `says` are further fixed substrings the message
# must carry -- the defect, without which naming the file is only half of it.

guard_cases <- function() {
  list(
    # os_check_audio ----------------------------------------------------------
    list(
      label = "os_check_audio(): infile does not exist",
      file = gone("gone.wav"),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_check_audio(file)
      }
    ),
    list(
      label = "os_check_audio(): verbose is not TRUE/FALSE",
      file = NULL,
      says = "verbose",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_check_audio(file, verbose = "yes")
      }
    ),
    # os_prep_audio -----------------------------------------------------------
    list(
      label = "os_prep_audio(): infile does not exist",
      file = gone(),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_prep_audio(file, local_outpath(.env = parent.frame()))
      }
    ),
    list(
      label = "os_prep_audio(): outfile is not a string",
      file = NULL,
      says = "outfile",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_prep_audio(file, 1)
      }
    ),
    list(
      label = "os_prep_audio(): stream is negative",
      file = NULL,
      says = "stream",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_prep_audio(file, local_outpath(.env = parent.frame()), stream = -1)
      }
    ),
    list(
      label = "os_prep_audio(): overwrite is not TRUE/FALSE",
      file = NULL,
      says = "overwrite",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        os_prep_audio(file, local_outpath(.env = parent.frame()), overwrite = "yes")
      }
    ),
    # os_extract_wav ----------------------------------------------------------
    list(
      label = "os_extract_wav(): infile does not exist",
      file = gone("gone.wav"),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        openac:::os_extract_wav(file)
      }
    ),
    list(
      label = "os_extract_wav(): infile is not audio openSMILE can read",
      file = NULL,
      says = "16-bit",
      run = function(file) {
        local_fake_tools(results = nonconforming(), .env = parent.frame())
        openac:::os_extract_wav(file)
      }
    ),
    list(
      label = "os_extract_wav(): aggfile is not a .csv path",
      file = NULL,
      says = "aggfile",
      run = function(file) {
        local_fake_tools(results = conforming(), .env = parent.frame())
        openac:::os_extract_wav(file, aggfile = "agg.txt")
      }
    ),
    list(
      label = "os_extract_wav(): lldfile is not a .csv path",
      file = NULL,
      says = "lldfile",
      run = function(file) {
        local_fake_tools(results = conforming(), .env = parent.frame())
        openac:::os_extract_wav(file, lldfile = "lld.txt")
      }
    ),
    # os_fix_csv --------------------------------------------------------------
    list(
      label = "os_fix_csv(): openSMILE wrote no output",
      file = gone("agg.csv"),
      says = "openSMILE wrote no output",
      run = function(file) openac:::os_fix_csv(file)
    ),
    # of_extract --------------------------------------------------------------
    list(
      label = "of_extract(): infile does not exist",
      file = gone(),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        of_extract(file, local_outpath("faces.csv", .env = parent.frame()))
      }
    ),
    list(
      label = "of_extract(): outfile is not a string",
      file = NULL,
      says = "outfile",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        of_extract(file, 1)
      }
    ),
    # aw_check_audio ----------------------------------------------------------
    list(
      label = "aw_check_audio(): infile does not exist",
      file = gone("gone.wav"),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_check_audio(file)
      }
    ),
    list(
      label = "aw_check_audio(): verbose is not TRUE/FALSE",
      file = NULL,
      says = "verbose",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_check_audio(file, verbose = "yes")
      }
    ),
    # aw_prep_audio -----------------------------------------------------------
    list(
      label = "aw_prep_audio(): infile does not exist",
      file = gone(),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_prep_audio(file, local_outpath(.env = parent.frame()))
      }
    ),
    list(
      label = "aw_prep_audio(): outfile is not a string",
      file = NULL,
      says = "outfile",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_prep_audio(file, 1)
      }
    ),
    list(
      label = "aw_prep_audio(): stream is not a whole number",
      file = NULL,
      says = "stream",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_prep_audio(file, local_outpath(.env = parent.frame()), stream = 1.5)
      }
    ),
    list(
      label = "aw_prep_audio(): afilters is not TRUE/FALSE",
      file = NULL,
      says = "afilters",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_prep_audio(file, local_outpath(.env = parent.frame()), afilters = "yes")
      }
    ),
    list(
      label = "aw_prep_audio(): overwrite is not TRUE/FALSE",
      file = NULL,
      says = "overwrite",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        aw_prep_audio(file, local_outpath(.env = parent.frame()), overwrite = "yes")
      }
    ),
    list(
      label = "aw_prep_audio(): the streams could not be counted",
      file = NULL,
      says = "streams could not be counted",
      run = function(file) {
        local_fake_tools(results = list(fake_nonzero_exit()), .env = parent.frame())
        suppressWarnings(aw_prep_audio(file, local_outpath(.env = parent.frame())))
      }
    ),
    list(
      label = "aw_prep_audio(): the file has no such audio stream",
      file = NULL,
      says = "audio stream",
      run = function(file) {
        local_fake_tools(results = list("audio"), .env = parent.frame())
        aw_prep_audio(file, local_outpath(.env = parent.frame()), stream = 3)
      }
    ),
    # aw_transcribe_wav -------------------------------------------------------
    list(
      label = "aw_transcribe_wav(): infile does not exist",
      file = gone("gone.wav"),
      says = "No file exists at",
      run = function(file) {
        local_fake_tools(results = list(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model())
      }
    ),
    list(
      label = "aw_transcribe_wav(): infile is not audio whisper can read",
      file = NULL,
      says = "16-bit",
      run = function(file) {
        local_fake_tools(results = nonconforming(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model())
      }
    ),
    list(
      label = "aw_transcribe_wav(): model is not a whisper model",
      file = NULL,
      says = "model",
      run = function(file) {
        local_fake_tools(results = conforming_wav(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = list())
      }
    ),
    list(
      label = "aw_transcribe_wav(): language is not a string",
      file = NULL,
      says = "language",
      run = function(file) {
        local_fake_tools(results = conforming_wav(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model(), language = 1)
      }
    ),
    list(
      label = "aw_transcribe_wav(): rdsfile is not a .rds path",
      file = NULL,
      says = "rdsfile",
      run = function(file) {
        local_fake_tools(results = conforming_wav(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model(), rdsfile = "out.txt")
      }
    ),
    list(
      label = "aw_transcribe_wav(): csvfile is not a .csv path",
      file = NULL,
      says = "csvfile",
      run = function(file) {
        local_fake_tools(results = conforming_wav(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model(), csvfile = "out.txt")
      }
    ),
    list(
      label = "aw_transcribe_wav(): whisper_args is not a list",
      file = NULL,
      says = "whisper_args",
      run = function(file) {
        local_fake_tools(results = conforming_wav(), .env = parent.frame())
        openac:::aw_transcribe_wav(file, model = fake_model(), whisper_args = "fast")
      }
    ),
    # aw_transcribe -----------------------------------------------------------
    list(
      label = "aw_transcribe(): the streams could not be counted",
      file = NULL,
      says = "streams could not be counted",
      run = function(file) {
        local_fake_tools(results = list(fake_nonzero_exit()), .env = parent.frame())
        suppressWarnings(aw_transcribe(file, model = fake_model()))
      }
    )
  )
}

for (case in guard_cases()) {
  test_that(paste0("names the file and the defect -- ", case$label), {
    # A `file` of NULL means the guard fires on a file that DOES exist, so the
    # case gets a real one; the named cases carry a path nothing wrote.
    infile <- if (is.null(case$file)) local_media(".wav") else case$file

    msg <- collapsed_guard(case$run(infile))

    expect_false(is.na(msg), info = case$label)
    expect_match(msg, basename(infile), fixed = TRUE, info = case$label)
    expect_match(msg, case$says, fixed = TRUE, info = case$label)
  })
}

# --- review round 2: the error column is plain data, in EVERY guard -----------
#
# Round 1's F14 fix lives inside `abort_file()`, so a guard building its own
# `cli_abort()` never received it: `os_fix_csv()` was still shipping a hard line
# break and a bullet glyph into the `error` column (round 2, F1). The eager
# `format_inline()` that fixed the others has its own leak -- it bakes ANSI
# colour codes in whenever colours are on (F3). Neither was catchable, because
# every assertion above reads a whitespace-COLLAPSED message and
# `gsub("\\s+", " ", ...)` deletes the newline under test (F2).
#
# So this loop reads the RAW `conditionMessage()`, over the same case table, and
# under conditions chosen to make a console-formatted message misbehave: a
# 40-column width, so anything that wraps wraps, and 256 colours, so anything
# that colourizes does.

raw_guard <- function(expr) {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  if (is.null(cnd)) {
    return(NA_character_)
  }
  conditionMessage(cnd)
}

for (case in guard_cases()) {
  test_that(paste0("the message is one plain line -- ", case$label), {
    withr::local_options(cli.width = 40, cli.num_colors = 256)
    infile <- if (is.null(case$file)) local_media(".wav") else case$file

    msg <- raw_guard(case$run(infile))

    expect_false(is.na(msg), info = case$label)
    expect_false(grepl("\n", msg, fixed = TRUE), info = case$label)
    expect_false(grepl("✖", msg, fixed = TRUE), info = case$label)
    expect_identical(msg, cli::ansi_strip(msg), info = case$label)
  })
}

# --- the eight of_extract() flags, each its own guard -------------------------

for (flag in c("fp2D", "fp3D", "pdm", "pose", "gaze", "aus", "wild", "multiview")) {
  test_that(paste0("of_extract() names the file and the defect -- ", flag), {
    infile <- local_media(".mp4")
    outfile <- local_outpath("faces.csv")
    local_fake_tools(results = list())

    msg <- collapsed_guard(
      do.call(of_extract, c(list(infile, outfile), stats::setNames(list("yes"), flag)))
    )

    expect_false(is.na(msg), info = flag)
    expect_match(msg, basename(infile), fixed = TRUE, info = flag)
    expect_match(msg, flag, fixed = TRUE, info = flag)
  })
}

# --- AC2: the intermediate wav ffmpeg never wrote -----------------------------

test_that("os_extract_dir() attributes a missing intermediate wav to ffmpeg", {
  # The defect DESIGN's Known issues measured on 2026-08-08: a row whose `error`
  # read `file.exists(infile) is not TRUE`, about a temporary wav that was never
  # written. M17 fixed the case where ffmpeg EXITS non-zero; this is the other
  # one -- ffmpeg exits 0 and writes nothing, so the next thing to notice is
  # `os_extract_wav()`'s own existence guard, which knows the file it was handed
  # is a wav openac derived rather than anything the user chose.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  wavdir <- withr::local_tempdir()
  aggdir <- withr::local_tempdir()

  # In call order: os_extract()'s own os_check_audio() says no (a video stream
  # is present), then ffmpeg "succeeds" while writing nothing at all.
  local_fake_tools(results = c(
    list("video audio", c("aac", "44100", "2")),
    list("ok")
  ))

  out <- suppressWarnings(
    os_extract_dir(indir, "mp4", wavdir = wavdir, aggdir = aggdir)
  )

  expect_identical(out$status, "failed")
  msg <- gsub("\\s+", " ", out$error)
  # Through `fs`, as `dir_outputs()` derived it: `tempdir()` can carry a doubled
  # separator, which the derived path does not.
  expect_match(
    msg,
    as.character(fs::path_abs(file.path(wavdir, "clip.wav"))),
    fixed = TRUE
  )
  expect_match(msg, "ffmpeg wrote no output", fixed = TRUE)
})

# --- AC3: the config is resolved once, before the loop ------------------------

# Strip the configs `local_fake_tools()` planted, leaving an openSMILE install
# that resolves but carries no config at all.
empty_config_dir <- function(state) {
  unlink(file.path(state$dir, "config"), recursive = TRUE)
  dir.create(file.path(state$dir, "config"))
}

test_that("os_check_config() names the config it could not resolve", {
  local_fake_tools(results = list())

  msg <- collapsed_guard(os_check_config("egemaps/v99/nope"))

  expect_match(msg, "egemaps/v99/nope", fixed = TRUE)
  # The way out, not only the complaint: a user who mistyped one of several
  # dozen config names needs the list, and NEWS says the message points at it.
  expect_match(msg, "os_list_configs", fixed = TRUE)
})

test_that("os_extract_dir() rejects an unresolvable config before running anything", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  state <- local_fake_tools(results = list())

  msg <- collapsed_guard(
    os_extract_dir(indir, "mp4", aggdir = aggdir, config = "egemaps/v99/nope")
  )

  expect_match(msg, "egemaps/v99/nope", fixed = TRUE)
  # No call AT ALL, not merely no openSMILE call: the per-file path reaches
  # ffprobe long before it reaches openSMILE, so asserting the absence of
  # "opensmile" alone would pass over a batch that had already probed N files.
  expect_identical(boundary_tools(state), character(0))
})

test_that("os_extract_dir() validates the default config too", {
  # The default is carried by `os_extract()`'s signature, not passed through
  # `...`, so a pre-flight check reading only what the caller supplied would
  # skip validation entirely on the commonest call of all.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  state <- local_fake_tools(results = list())
  empty_config_dir(state)

  msg <- collapsed_guard(os_extract_dir(indir, "mp4", aggdir = aggdir))

  expect_match(msg, "misc/emo_large", fixed = TRUE)
  expect_identical(boundary_tools(state), character(0))
})

# --- AC4: os_fix_csv() attributes its missing input ---------------------------

test_that("os_fix_csv() names the path it looked for and says who should have written it", {
  # Honest for both of its callers: `os_extract_wav()` calls it only on an
  # `aggfile`/`lldfile` it has just handed openSMILE as `-csvoutput` /
  # `-lldcsvoutput`, so the file's absence IS openSMILE having written nothing.
  missing <- gone("agg.csv")

  msg <- collapsed_guard(openac:::os_fix_csv(missing))

  expect_match(msg, missing, fixed = TRUE)
  expect_match(msg, "openSMILE wrote no output", fixed = TRUE)
})

# --- review round 1 (2026-08-09): guards that raised a raw R condition --------
#
# Three shapes escaped `abort_file()` entirely and reached `dir_walk()`'s
# `error` column as base R's own text -- `missing value where TRUE/FALSE
# needed`, `the condition has length > 1`, `argument is of length zero` --
# naming neither the file nor the defect, which is the whole of what AC1
# removes. Each is asserted below as the openac condition it must be, never as
# "some error occurred".

# F2: a non-scalar or non-character `infile` reaches `if (!file.exists(infile))`,
# whose condition is then length 2 or length 0. This guard names the ARGUMENT
# and not a file, and cannot do otherwise: `basename()` of a length-2 path names
# two files and of `character(0)` names none, so there is no one file it stopped
# on. It is outside AC1's batch-reachable domain for the reason T2 gave
# `ffp_count_streams()`'s identical guard -- `dir_walk()`'s `infile` column is
# always a length-1 character from `fs::path_abs()`.

scalar_infile_cases <- function() {
  list(
    list(label = "os_check_audio()", run = function(x) os_check_audio(x)),
    list(
      label = "os_prep_audio()",
      run = function(x) os_prep_audio(x, local_outpath(.env = parent.frame()))
    ),
    list(label = "os_extract_wav()", run = function(x) openac:::os_extract_wav(x)),
    list(label = "os_extract()", run = function(x) os_extract(x, aggfile = "a.csv")),
    list(
      label = "of_extract()",
      run = function(x) of_extract(x, local_outpath("faces.csv", .env = parent.frame()))
    ),
    list(label = "aw_check_audio()", run = function(x) aw_check_audio(x)),
    list(
      label = "aw_prep_audio()",
      run = function(x) aw_prep_audio(x, local_outpath(.env = parent.frame()))
    ),
    list(
      label = "aw_transcribe()",
      run = function(x) aw_transcribe(x, model = fake_model())
    ),
    list(
      label = "aw_transcribe_wav()",
      run = function(x) openac:::aw_transcribe_wav(x, model = fake_model())
    )
  )
}

for (case in scalar_infile_cases()) {
  test_that(paste0("a non-scalar infile is openac's error, not R's -- ", case$label), {
    local_fake_tools(results = list())
    # Length 2, length 0, and the wrong type: the first two are what base R's
    # `if` dies on, the third is what `file.exists()` coerces silently.
    for (bad in list(c("a.wav", "b.wav"), character(0), 1L)) {
      cnd <- rlang::catch_cnd(case$run(bad), classes = "error")
      expect_s3_class(cnd, "openac_bad_argument")
      expect_match(
        gsub("\\s+", " ", conditionMessage(cnd)), "infile",
        fixed = TRUE, info = paste(case$label, class(bad), length(bad))
      )
    }
  })
}

# F3: `os_check_audio()` / `aw_check_audio()` returned `NA` when the SECOND
# ffprobe query answered with a missing field, and `if (!os_check_audio(x))` in
# `os_extract_wav()` then died on `missing value where TRUE/FALSE needed`. The
# early `anyNA(streams)` return covers only the stream COUNT, which is the first
# query. The contract is a single logical, so an unanswerable check is FALSE --
# the same disposition the count branch already takes.

test_that("os_check_audio() answers FALSE, never NA, when ffprobe leaves a field blank", {
  infile <- local_media(".wav")
  local_fake_tools(results = list("audio", c(NA_character_, "44100", "1")))

  expect_identical(os_check_audio(infile), FALSE)
})

test_that("aw_check_audio() answers FALSE, never NA, when ffprobe leaves a field blank", {
  infile <- local_media(".wav")
  local_fake_tools(results = list("audio", c(NA_character_, "16000", "1")))

  expect_identical(aw_check_audio(infile), FALSE)
})

test_that("os_check_audio() answers FALSE when the second query returns nothing", {
  # `aw_check_audio()` already guards this; `os_check_audio()` indexed `dat[[3]]`
  # straight into a subscript-out-of-bounds.
  infile <- local_media(".wav")
  local_fake_tools(results = list("audio", character(0)))

  expect_identical(os_check_audio(infile), FALSE)
})

test_that("os_extract_wav() names the file when ffprobe leaves a field blank", {
  infile <- local_media(".wav")
  local_fake_tools(results = list("audio", c(NA_character_, "44100", "1")))

  msg <- collapsed_guard(openac:::os_extract_wav(infile))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "16-bit", fixed = TRUE)
})

test_that("aw_transcribe_wav() names the file when ffprobe leaves a field blank", {
  infile <- local_media(".wav")
  local_fake_tools(results = list("audio", c(NA_character_, "16000", "1")))

  msg <- collapsed_guard(openac:::aw_transcribe_wav(infile, model = fake_model()))

  expect_match(msg, basename(infile), fixed = TRUE)
  expect_match(msg, "16-bit", fixed = TRUE)
})

# --- review round 1: AC3's pre-flight, as written -----------------------------

test_that("os_extract_dir() pre-flights a config named by an abbreviation", {
  # F5: `...` is forwarded through `do.call()`, which matches names PARTIALLY --
  # so `conf =` reaches `os_extract()`'s `config` while an exact read of
  # `list(...)$config` sees nothing and validates the default instead. The batch
  # then probed every input before failing each one.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  state <- local_fake_tools(results = list())

  msg <- collapsed_guard(
    os_extract_dir(indir, "mp4", aggdir = aggdir, conf = "egemaps/v99/nope")
  )

  expect_match(msg, "egemaps/v99/nope", fixed = TRUE)
  expect_identical(boundary_tools(state), character(0))
})

# --- review round 2: resolving a prefix may not lose an argument --------------

test_that("two abbreviations of one argument are rejected, not silently merged", {
  # F6: `pmatch()` is greedy, so `conf` and `confi` both resolve to `config` and
  # the second was renamed onto the first and dropped by `names<-`. Plain R
  # raises `formal argument "config" matched by multiple actual arguments` for
  # that call; the helper turned that error into silence, and the batch ran with
  # one of the two values the user supplied.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  state <- local_fake_tools(results = list())

  msg <- collapsed_guard(
    os_extract_dir(
      indir, "mp4", aggdir = aggdir,
      conf = "egemaps/v02", confi = "misc/emo_large"
    )
  )

  # Backticked, because `conf` and `confi` are both substrings of `config`: a
  # bare `expect_match(msg, "confi")` passes on any message naming the formal
  # and so discriminates nothing.
  expect_match(msg, "`config`", fixed = TRUE)
  expect_match(msg, "`conf`", fixed = TRUE)
  expect_match(msg, "`confi`", fixed = TRUE)
  # Batch-wide, like the config check beside it: nothing runs, no rows.
  expect_identical(boundary_tools(state), character(0))
})

test_that("match_formals() leaves a name alone when it matches no formal", {
  # The complement of the case above: an abbreviation that is ambiguous between
  # two DIFFERENT formals, and a name bound for `fn`'s own `...`, are both
  # passed through untouched for `do.call()` to accept or reject as it would.
  fn <- function(alpha, alphabet, ...) NULL

  expect_identical(
    names(openac:::match_formals(list(al = 1), fn)),
    "al"
  )
  expect_identical(
    names(openac:::match_formals(list(zeta = 1), fn)),
    "zeta"
  )
})

test_that("match_formals() does not abbreviate a formal that follows ...", {
  # F5 (round 2, below the bar): R stops partial matching at `...` -- an
  # argument after it must be named in full. The helper matched against every
  # formal, so it would have renamed `ver` onto `verbose` where R would have
  # left it in `...`.
  fn <- function(config, ..., verbose = FALSE) NULL

  expect_identical(
    names(openac:::match_formals(list(conf = 1, ver = 2), fn)),
    c("config", "ver")
  )
})

test_that("os_extract_dir() pre-flights an explicit NULL config", {
  # F12: `config = NULL` was indistinguishable from `config` absent, so the
  # pre-flight validated the DEFAULT and the batch then failed per file with a
  # message naming no file. Supplied means checked, whatever the value.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  state <- local_fake_tools(results = list())

  msg <- collapsed_guard(
    os_extract_dir(indir, "mp4", aggdir = aggdir, config = NULL)
  )

  expect_match(msg, "config", fixed = TRUE)
  expect_identical(boundary_tools(state), character(0))
})

test_that("an unresolved openSMILE is named, not a dirname() failure", {
  # F6: `os_list_configs()` called `dirname(find_opensmile())`, and with
  # openSMILE unresolved that is `dirname(NULL)` -- `a character vector argument
  # expected`, raised before `os_check_config()` could say anything. Pre-flight
  # made it the whole batch's death rather than one file's.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  local_fake_tools(results = list(), resolve = c("ffmpeg", "ffprobe"))

  msg <- suppressWarnings(
    collapsed_guard(os_extract_dir(indir, "mp4", aggdir = aggdir))
  )

  expect_match(msg, "opensmile", fixed = TRUE)
  expect_no_match(msg, "character vector argument expected", fixed = TRUE)
})

# --- review round 1: the error column is a data column, not a console ---------

test_that("a failed row's error is one line, unglyphed, and names the file once", {
  # F14: `cli_abort()` formats for a terminal -- it hard-wraps at the console
  # width and prefixes each bullet with a glyph -- and both survive
  # `conditionMessage()` into a character column a user prints in a data frame
  # and writes to CSV. `dir_walk()`'s own warning then prepended the basename a
  # second time.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "clip.mp4"))
  aggdir <- withr::local_tempdir()
  local_fake_tools(results = list("video audio", c("aac", "44100", "2"), "ok"))

  warnings <- collect_warnings(
    out <- os_extract_dir(indir, "mp4", aggdir = aggdir)
  )

  expect_identical(out$status, "failed")
  expect_false(grepl("\n", out$error, fixed = TRUE))
  expect_false(grepl("✖", out$error, fixed = TRUE))
  expect_identical(out$error, cli::ansi_strip(out$error))
  # The row's own message still names the file -- that is AC1 -- but the
  # warning, which already leads with the basename, must not say it twice.
  expect_match(out$error, "clip.mp4", fixed = TRUE)
  failure <- grep("clip.mp4", warnings, value = TRUE)
  expect_length(failure, 1L)
  expect_identical(lengths(regmatches(failure, gregexpr("clip.mp4", failure))), 1L)
})

# F1: `!is_integerish(stream, n = 1) || stream < 0` is NA for a TYPED
# `NA_integer_` -- `is_integerish(NA_integer_, n = 1)` is TRUE, so the guard
# falls through to `NA < 0` and `if (NA)` dies naming no file. A bare `NA` is
# logical and fails the first test, which is why only the typed one got through.

stream_na_cases <- function() {
  list(
    list(
      label = "os_prep_audio()",
      run = function(file, stream) {
        os_prep_audio(file, local_outpath(.env = parent.frame()), stream = stream)
      }
    ),
    list(
      label = "aw_prep_audio()",
      run = function(file, stream) {
        aw_prep_audio(file, local_outpath(.env = parent.frame()), stream = stream)
      }
    )
  )
}

for (case in stream_na_cases()) {
  test_that(paste0("a typed NA stream names the file and the defect -- ", case$label), {
    infile <- local_media(".mp4")
    local_fake_tools(results = list())

    for (bad in list(NA_integer_, NA_real_)) {
      msg <- collapsed_guard(case$run(infile, bad))
      expect_match(msg, basename(infile), fixed = TRUE, info = case$label)
      expect_match(msg, "stream", fixed = TRUE, info = case$label)
    }
  })
}
