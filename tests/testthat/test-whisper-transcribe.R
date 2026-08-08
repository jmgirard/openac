# AC5 -- aw_transcribe() and aw_transcribe_wav() against a mocked whisper.
#
# `aw_transcribe_wav()` reaches whisper through `do.call(what = predict, ...)`,
# where `predict` is the `stats` generic openac imports and dispatch would land
# on `audio.whisper:::predict.whisper`. The mock is installed in openac's own
# namespace, not in `stats`: an imported binding is resolved through openac's
# imports environment, so rebinding `stats::predict` leaves the real generic in
# play and dispatch fails with "no applicable method". Nothing below downloads a
# model or runs whisper, and audio.whisper need not be installed to run these.

# A model object, as far as aw_transcribe_wav()'s `class(model) == "whisper"`
# check is concerned.
fake_model <- function() structure(list(name = "tiny"), class = "whisper")

# A whisper transcription, in the shape aw_read() and the CSV writer expect.
fake_transcription <- function() {
  list(
    data = data.frame(
      segment = 1:2,
      from = c("00:00:00.000", "00:00:02.500"),
      to = c("00:00:02.500", "00:00:05.000"),
      text = c(" Hello there.", " General Kenobi."),
      stringsAsFactors = FALSE
    )
  )
}

# Intercept whisper. Records the arguments each call received and returns a
# fixed transcription; the recorder is what the parameter assertions read.
local_fake_whisper <- function(value = fake_transcription(), .env = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  testthat::local_mocked_bindings(
    predict = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      value
    },
    .env = .env
  )
  state
}

# ffprobe output for a file aw_check_audio() accepts: one audio stream, no
# video, pcm_s16le at 16 kHz mono.
conforming_wav <- function() list("audio", c("pcm_s16le", "16000", "1"))

# ...and for one it rejects: stereo mp3 at 44.1 kHz.
nonconforming <- function() list("audio", c("mp3", "44100", "2"))

local_wav <- function(.env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".wav", .local_envir = .env)
  file.create(path)
  path
}

# --- aw_transcribe_wav -------------------------------------------------------

test_that("aw_transcribe_wav() passes the documented parameters to whisper", {
  infile <- local_wav()
  local_fake_tools(results = conforming_wav())
  whisper <- local_fake_whisper()
  model <- fake_model()

  out <- openac:::aw_transcribe_wav(infile, model = model)

  expect_length(whisper$calls, 1)
  args <- whisper$calls[[1]]
  expect_identical(args$object, model)
  expect_identical(args$newdata, infile)
  expect_identical(args$type, "transcribe")
  expect_identical(args$language, "auto")
  expect_false(args$trace)
  # The whisper output is returned unchanged.
  expect_identical(out, fake_transcription())
})

test_that("aw_transcribe_wav() forwards whisper_args and honours language", {
  infile <- local_wav()
  local_fake_tools(results = conforming_wav())
  whisper <- local_fake_whisper()

  openac:::aw_transcribe_wav(
    infile,
    model = fake_model(),
    language = "es",
    whisper_args = list(n_threads = 4, offset = 1000)
  )

  args <- whisper$calls[[1]]
  expect_identical(args$language, "es")
  expect_identical(args$n_threads, 4)
  expect_identical(args$offset, 1000)
})

test_that("aw_transcribe_wav() writes the rds and csv outputs it is given", {
  infile <- local_wav()
  outdir <- withr::local_tempdir()
  # Nested, so the directory-creation branch is exercised rather than assumed.
  rdsfile <- file.path(outdir, "nested", "out.rds")
  csvfile <- file.path(outdir, "nested", "out.csv")
  local_fake_tools(results = conforming_wav())
  local_fake_whisper()

  openac:::aw_transcribe_wav(
    infile, model = fake_model(), rdsfile = rdsfile, csvfile = csvfile
  )

  expect_true(file.exists(rdsfile))
  expect_true(file.exists(csvfile))
  # The rds holds the whole object; the csv holds only $data.
  expect_identical(readRDS(rdsfile), fake_transcription())
  expect_identical(
    utils::read.csv(csvfile, stringsAsFactors = FALSE)$segment,
    fake_transcription()$data$segment
  )
})

test_that("aw_transcribe_wav() writes nothing when no output path is given", {
  # The input lives alone in its own directory, and the assertion is made on
  # THAT directory: an unrequested `.rds` or `.csv` written beside the input is
  # the IP1 "no surprise writes" violation this guards, and it can only be seen
  # where the input actually is.
  dir <- withr::local_tempdir()
  infile <- file.path(dir, "clip.wav")
  file.create(infile)
  local_fake_tools(results = conforming_wav())
  local_fake_whisper()

  openac:::aw_transcribe_wav(infile, model = fake_model())

  expect_identical(list.files(dir), "clip.wav")
})

test_that("aw_transcribe_wav() validates its arguments before running whisper", {
  infile <- local_wav()
  model <- fake_model()

  local_fake_tools(results = conforming_wav())
  whisper <- local_fake_whisper()
  expect_error(
    openac:::aw_transcribe_wav(infile, model = model, rdsfile = "out.txt"),
    "file_ext"
  )

  local_fake_tools(results = conforming_wav())
  expect_error(
    openac:::aw_transcribe_wav(infile, model = model, csvfile = "out.txt"),
    "file_ext"
  )

  local_fake_tools(results = conforming_wav())
  expect_error(
    openac:::aw_transcribe_wav(infile, model = list()),
    "whisper"
  )

  # Whisper is never reached on any of the three rejections.
  expect_length(whisper$calls, 0)
})

test_that("aw_transcribe_wav() refuses an input whisper cannot read", {
  infile <- local_wav()
  local_fake_tools(results = nonconforming())
  whisper <- local_fake_whisper()

  expect_error(
    openac:::aw_transcribe_wav(infile, model = fake_model()),
    "aw_check_audio"
  )
  expect_length(whisper$calls, 0)
})

# --- aw_transcribe -----------------------------------------------------------

test_that("aw_transcribe() transcribes a conforming input in place", {
  infile <- local_wav()
  # Two aw_check_audio rounds: one in aw_transcribe, one in aw_transcribe_wav,
  # plus the leading stream count aw_transcribe does itself.
  state <- local_fake_tools(
    results = c(list("audio"), conforming_wav(), conforming_wav())
  )
  whisper <- local_fake_whisper()

  aw_transcribe(infile, model = fake_model())

  # No ffmpeg: the input was already in whisper's format.
  expect_false("ffmpeg" %in% boundary_tools(state))
  expect_identical(whisper$calls[[1]]$newdata, infile)
})

test_that("aw_transcribe() prepares a non-conforming input and keeps wavfile", {
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  wavfile <- file.path(withr::local_tempdir(), "prepped.wav")
  # The mocked ffmpeg writes nothing; aw_transcribe_wav() requires the file
  # aw_prep_audio() would have produced.
  writer <- function(command, args) {
    file.create(wavfile)
    "ok"
  }
  state <- local_fake_tools(
    results = c(
      list("audio"),          # aw_transcribe's stream count
      nonconforming(),        # aw_transcribe's check: fails
      list("audio"),          # aw_prep_audio's stream count
      list(writer),           # aw_prep_audio's ffmpeg
      conforming_wav()        # aw_transcribe_wav's check: passes
    )
  )
  whisper <- local_fake_whisper()

  aw_transcribe(infile, model = fake_model(), wavfile = wavfile)

  expect_true("ffmpeg" %in% boundary_tools(state))
  # Whisper read the prepared wav, not the original input...
  expect_identical(whisper$calls[[1]]$newdata, wavfile)
  # ...and an explicitly named wavfile is kept, not cleaned up.
  expect_true(file.exists(wavfile))
})

test_that("aw_transcribe() discards the temp wav it creates when wavfile is NULL", {
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  written <- NULL
  writer <- function(command, args) {
    written <<- boundary_outfile(args)
    file.create(written)
    "ok"
  }
  local_fake_tools(
    results = c(
      list("audio"), nonconforming(), list("audio"), list(writer), conforming_wav()
    )
  )
  whisper <- local_fake_whisper()

  aw_transcribe(infile, model = fake_model())

  expect_identical(tools::file_ext(written), "wav")
  expect_identical(whisper$calls[[1]]$newdata, written)
  expect_false(file.exists(written))
})

test_that("aw_transcribe() forwards audio_args to the preparation step", {
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  writer <- function(command, args) {
    file.create(boundary_outfile(args))
    "ok"
  }
  state <- local_fake_tools(
    results = c(
      list("audio"), nonconforming(), list("audio"), list(writer), conforming_wav()
    )
  )
  local_fake_whisper()

  aw_transcribe(infile, model = fake_model(), audio_args = list(afilters = TRUE))

  # The filter chain appears in the ffmpeg command, so audio_args reached it.
  ffmpeg_args <- boundary_args(state)[boundary_tools(state) == "ffmpeg"]
  expect_length(ffmpeg_args, 1)
  expect_match(ffmpeg_args, "loudnorm", fixed = TRUE)
})

test_that("aw_transcribe() skips a file with no audio rather than transcribing it", {
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  local_fake_tools(results = list("video"))
  whisper <- local_fake_whisper()

  expect_message(out <- aw_transcribe(infile, model = fake_model()), "No audio")

  expect_null(out)
  expect_length(whisper$calls, 0)
})

# --- aw_transcribe_dir -------------------------------------------------------

test_that("aw_transcribe_dir() transcribes each file and reports per file", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, c("a.wav", "b.wav")))
  rdsdir <- file.path(withr::local_tempdir(), "rds")
  local_fake_tools(
    results = rep(c(list("audio"), conforming_wav(), conforming_wav()), 2)
  )
  whisper <- local_fake_whisper()

  result <- aw_transcribe_dir(indir, "wav", rdsdir = rdsdir, model = fake_model())

  expect_length(whisper$calls, 2)
  expect_identical(nrow(result), 2L)
  expect_true(all(result$success))
  expect_identical(sort(basename(result$rdsfile)), c("a.rds", "b.rds"))
  expect_true(all(file.exists(result$rdsfile)))
})

test_that("aw_transcribe_dir() skips a failing file and keeps going", {
  indir <- withr::local_tempdir()
  file.create(file.path(indir, c("a.wav", "b.wav")))
  boom <- function(command, args) stop("ffprobe exploded")
  # The failure is placed on aw_check_audio's probe, not on the leading stream
  # count: aw_transcribe() catches an error there and reports "no audio" instead,
  # which is a skip rather than the failure this test is about.
  local_fake_tools(
    results = c(
      list("audio"), list(boom),                       # file a: check explodes
      list("audio"), conforming_wav(), conforming_wav() # file b: transcribes
    )
  )
  whisper <- local_fake_whisper()

  expect_warning(
    result <- aw_transcribe_dir(indir, "wav", model = fake_model()),
    "ffprobe exploded"
  )

  # The first file failed before whisper; the second still ran.
  expect_identical(result$success, c(FALSE, TRUE))
  expect_length(whisper$calls, 1)
})
