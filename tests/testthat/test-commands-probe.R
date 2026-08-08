# AC3/AC5 -- the passthroughs, the alias family, and the ffprobe-backed checks.

# A file that exists, for the stopifnot(file.exists()) guards. Contents are
# irrelevant: the tool never runs.
local_media <- function(ext = ".wav", .env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = .env)
  file.create(path)
  path
}

# --- passthroughs and aliases: pass-through identity -------------------------

test_that("each passthrough forwards its argument string unchanged", {
  state <- local_fake_tools(results = list("a", "b", "c", "d"))

  ffmpeg("-version")
  ffprobe("-h")
  openface("-help")
  openac:::opensmile("-L")

  expect_identical(
    boundary_tools(state),
    c("ffmpeg", "ffprobe", "openface", "opensmile")
  )
  expect_identical(boundary_args(state), c("-version", "-h", "-help", "-L"))
})

test_that("each alias reaches the same tool as its primary name", {
  state <- local_fake_tools(results = list("a", "b", "c", "d"))

  ffm("-version")
  ffp("-h")
  of("-help")
  os("-L")

  expect_identical(
    boundary_tools(state),
    c("ffmpeg", "ffprobe", "openface", "opensmile")
  )
  expect_identical(boundary_args(state), c("-version", "-h", "-help", "-L"))
})

test_that("each passthrough errors, and runs nothing, when its tool is absent", {
  # system2(NULL, args) hands `args` to the shell, so an unguarded passthrough
  # would EXECUTE its argument string when the tool is missing. The guard turns
  # that into an error; find_program()'s warning still carries the set_program()
  # hint.
  state <- local_fake_tools(results = list(), resolve = character())

  expect_warning(expect_error(ffmpeg("-version"), "Can't run"), "Failed to find")
  expect_warning(expect_error(ffprobe("-h"), "Can't run"), "Failed to find")
  expect_warning(expect_error(openface("-help"), "Can't run"), "Failed to find")
  expect_warning(
    expect_error(openac:::opensmile("-L"), "Can't run"), "Failed to find"
  )

  # Nothing reached the boundary: no shell command was run.
  expect_length(boundary_calls(state), 0)
})

test_that("passthroughs reject a non-string argument", {
  local_fake_tools()
  expect_error(ffmpeg(1), "is_string")
  expect_error(ffprobe(c("-a", "-b")), "is_string")
  expect_error(openface(NULL), "is_string")
  expect_error(openac:::opensmile(list()), "is_string")
})

# --- ffp_count_streams -------------------------------------------------------

test_that("ffp_count_streams() builds the documented ffprobe query", {
  infile <- local_media(".mp4")
  state <- local_fake_tools(results = list(c("video", "audio")))

  ffp_count_streams(infile)

  expect_identical(boundary_tools(state), "ffprobe")
  expect_identical(
    boundary_args(state),
    paste0(
      '-v error -show_entries stream=codec_type -of csv=p=0 "', infile, '"'
    )
  )
})

test_that("ffp_count_streams() counts each stream combination", {
  infile <- local_media(".mp4")

  # Integer counts, so expect_equal rather than expect_identical against
  # doubles -- the roxygen says "numeric".
  local_fake_tools(results = list(c("video", "audio")))
  expect_equal(ffp_count_streams(infile), c(Video = 1, Audio = 1))

  local_fake_tools(results = list("audio"))
  expect_equal(ffp_count_streams(infile), c(Video = 0, Audio = 1))

  local_fake_tools(results = list("video"))
  expect_equal(ffp_count_streams(infile), c(Video = 1, Audio = 0))

  local_fake_tools(results = list(character()))
  expect_equal(ffp_count_streams(infile), c(Video = 0, Audio = 0))
})

test_that("ffp_count_streams() requires an existing file", {
  local_fake_tools()
  expect_error(ffp_count_streams(file.path(tempdir(), "absent.mp4")), "file.exists")
})

# --- os_check_audio ----------------------------------------------------------

test_that("os_check_audio() issues both ffprobe queries in order", {
  infile <- local_media()
  state <- local_fake_tools(
    results = list("audio", c("pcm_s16le", "44100", "1"))
  )

  os_check_audio(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe"))
  expect_identical(
    boundary_args(state),
    c(
      paste0('-v error -show_entries stream=codec_type -of csv=p=0 "', infile, '"'),
      paste0(
        '-v error -show_entries stream=codec_name,sample_rate,channels',
        ' -of default=noprint_wrappers=1:nokey=1 "', infile, '"'
      )
    )
  )
})

test_that("os_check_audio() accepts a conforming file and rejects others", {
  infile <- local_media()

  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "1")))
  expect_true(os_check_audio(infile))

  # Wrong codec.
  local_fake_tools(results = list("audio", c("mp3", "44100", "1")))
  expect_false(os_check_audio(infile))

  # Two channels.
  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "2")))
  expect_false(os_check_audio(infile))

  # A video stream present.
  local_fake_tools(results = list(c("video", "audio"), c("pcm_s16le", "44100", "1")))
  expect_false(os_check_audio(infile))
})

test_that("os_check_audio(verbose = TRUE) warns about a non-44.1kHz rate", {
  infile <- local_media()
  local_fake_tools(results = list("audio", c("pcm_s16le", "48000", "1")))

  expect_warning(
    expect_output(os_check_audio(infile, verbose = TRUE)),
    "44100 is recommended"
  )
})

# --- aw_check_audio ----------------------------------------------------------

test_that("aw_check_audio() issues both ffprobe queries in order", {
  infile <- local_media()
  state <- local_fake_tools(
    results = list("audio", c("pcm_s16le", "16000", "1"))
  )

  aw_check_audio(infile)

  expect_identical(boundary_tools(state), c("ffprobe", "ffprobe"))
  expect_identical(
    boundary_args(state)[[2]],
    paste0(
      '-v error -select_streams a',
      ' -show_entries stream=codec_name,sample_rate,channels',
      ' -of default=noprint_wrappers=1:nokey=1 "', infile, '"'
    )
  )
})

test_that("aw_check_audio() accepts a conforming file and rejects others", {
  infile <- local_media()

  local_fake_tools(results = list("audio", c("pcm_s16le", "16000", "1")))
  expect_true(aw_check_audio(infile))

  # whisper wants 16kHz, so openSMILE's 44.1kHz file is non-conforming here.
  local_fake_tools(results = list("audio", c("pcm_s16le", "44100", "1")))
  expect_false(aw_check_audio(infile))

  local_fake_tools(results = list("audio", c("mp3", "16000", "1")))
  expect_false(aw_check_audio(infile))
})

test_that("aw_check_audio() returns FALSE when ffprobe reports under 3 fields", {
  infile <- local_media()

  # The audio-less case: aw_check_audio guards on length(dat) < 3 where
  # os_check_audio would error on dat[[3]].
  local_fake_tools(results = list("video", character()))
  expect_false(aw_check_audio(infile))

  local_fake_tools(results = list("video", character()))
  expect_warning(
    expect_false(aw_check_audio(infile, verbose = TRUE)),
    "No audio stream"
  )
})
