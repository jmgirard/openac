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
