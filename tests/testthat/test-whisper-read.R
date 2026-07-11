# Tests for aw_read() — reshaping an audio.whisper transcription into a tidy
# tibble. The fixture mirrors audio.whisper::predict.whisper()'s return value
# (see cairn/references/audiowhisper.md): class "whisper_transcription" with a
# $data data.frame carrying an extra `segment_offset` column that aw_read drops.
# Timestamp oracles are hand-computed: H*3600 + M*60 + S.

# One source of truth for all three input forms (object / .rds / .csv), so the
# parity test cannot drift. The .csv is written exactly as aw_transcribe() does
# it: write.csv(out$data, row.names = FALSE).
make_aw_result <- function(empty = FALSE) {
  if (empty) {
    dat <- data.frame(
      segment        = integer(0),
      segment_offset = integer(0),
      text           = character(0),
      from           = character(0),
      to             = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    dat <- data.frame(
      segment        = 1:3,
      segment_offset = c(0L, 1500L, 4250L),
      text           = c(" Hello world.", " This is a test.", " Minute mark."),
      from           = c("00:00:00.000", "00:00:01.500", "00:01:00.000"),
      to             = c("00:00:01.500", "00:00:04.250", "00:01:02.750"),
      stringsAsFactors = FALSE
    )
  }
  structure(
    list(
      n_segments = nrow(dat),
      data       = dat,
      tokens     = data.frame(),
      params     = list(diarize = FALSE),
      timing     = list()
    ),
    class = "whisper_transcription"
  )
}

test_that("aw_read() returns a tidy tibble, one row per segment", {
  out <- aw_read(make_aw_result())
  expect_true(tibble::is_tibble(out))
  expect_identical(names(out), c("segment", "from", "to", "text"))
  expect_identical(nrow(out), 3L)
  expect_type(out$segment, "integer")
  expect_type(out$from, "double")
  expect_type(out$to, "double")
  expect_type(out$text, "character")
})

test_that("aw_read() parses HH:MM:SS.mmm timestamps to numeric seconds", {
  out <- aw_read(make_aw_result())
  # 00:00:00.000, 00:00:01.500, 00:01:00.000
  expect_equal(out$from, c(0, 1.5, 60))
  # 00:00:01.500 = 1.5; 00:00:04.250 = 4.25; 00:01:02.750 = 62*1 + 2.75 = 62.75
  expect_equal(out$to, c(1.5, 4.25, 62.75))
})

test_that("aw_read() preserves text verbatim, including leading spaces", {
  out <- aw_read(make_aw_result())
  expect_identical(out$text, c(" Hello world.", " This is a test.", " Minute mark."))
})

test_that("aw_read() drops non-target columns such as segment_offset", {
  res <- make_aw_result()
  expect_true("segment_offset" %in% names(res$data))
  expect_false("segment_offset" %in% names(aw_read(res)))
})

test_that("aw_read() gives identical output from object, .rds, and .csv", {
  res <- make_aw_result()
  rds <- tempfile(fileext = ".rds")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(rds, csv)), add = TRUE)
  saveRDS(res, rds)
  write.csv(res$data, csv, row.names = FALSE)   # exactly as aw_transcribe() writes it

  from_obj <- aw_read(res)
  expect_identical(from_obj, aw_read(rds))
  expect_identical(from_obj, aw_read(csv))
})

test_that("aw_read() handles an empty transcript as a 0-row tibble", {
  out <- aw_read(make_aw_result(empty = TRUE))
  expect_true(tibble::is_tibble(out))
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("segment", "from", "to", "text"))
  expect_type(out$from, "double")
})

test_that("aw_read() empty-transcript output is identical across forms", {
  res <- make_aw_result(empty = TRUE)
  rds <- tempfile(fileext = ".rds")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(rds, csv)), add = TRUE)
  saveRDS(res, rds)
  write.csv(res$data, csv, row.names = FALSE)
  expect_identical(aw_read(res), aw_read(rds))
  expect_identical(aw_read(res), aw_read(csv))
})

test_that("aw_read() errors on wrong-type input", {
  expect_error(aw_read(42), "transcription")            # not an object or path
  expect_error(aw_read(list(foo = 1)), "transcription") # list without $data
  expect_error(aw_read(c("a.rds", "b.rds")), "single")  # length > 1
})

test_that("aw_read() errors on a missing file path", {
  expect_error(aw_read(tempfile(fileext = ".rds")), "find")
})

test_that("aw_read() errors on an unsupported file extension", {
  f <- tempfile(fileext = ".txt")
  file.create(f)
  on.exit(unlink(f), add = TRUE)
  expect_error(aw_read(f), "\\.rds")
})
