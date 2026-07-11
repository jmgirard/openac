# Tests for of_read() — parsing OpenFace output CSV into a tidy tibble.
# Oracle is a hand-built fixture (fixtures/openface-sample.csv) whose header
# uses OpenFace's ", " (comma-space) separator, so every column name after the
# first carries a leading space that of_read() must trim.

test_that("of_read() returns a tibble", {
  out <- of_read(test_path("fixtures", "openface-sample.csv"))
  expect_true(tibble::is_tibble(out))
})

test_that("of_read() yields one row per frame with correct metadata types", {
  # fixtures/openface-sample.csv: 3 frames
  #   frame      = 1, 2, 3
  #   timestamp  = 0.000, 0.033, 0.067
  #   confidence = 0.98, 0.97, 0.20
  #   success    = 1, 1, 0
  out <- of_read(test_path("fixtures", "openface-sample.csv"))

  expect_equal(nrow(out), 3L)
  expect_type(out$frame, "integer")
  expect_equal(out$frame, c(1L, 2L, 3L))
  expect_type(out$timestamp, "double")
  expect_equal(out$timestamp, c(0.000, 0.033, 0.067))
  expect_equal(out$confidence, c(0.98, 0.97, 0.20))
  expect_true(all(out$success %in% c(0L, 1L)))
  expect_equal(out$success, c(1L, 1L, 0L))
})

test_that("of_read() strips the leading whitespace OpenFace puts in headers", {
  out <- of_read(test_path("fixtures", "openface-sample.csv"))
  # Trimmed names present; space-padded originals absent.
  expect_true(all(c("confidence", "success", "face_id", "AU01_r", "AU01_c") %in%
                    names(out)))
  expect_false(any(grepl("^\\s|\\s$", names(out))))
  expect_false(" confidence" %in% names(out))
})

test_that("of_read() parses AU intensity and presence columns as numeric", {
  # AU01_r (intensity) = 1.25, 0.50, 0.00 ; AU01_c (presence) = 1, 0, 0
  out <- of_read(test_path("fixtures", "openface-sample.csv"))
  expect_type(out$AU01_r, "double")
  expect_type(out$AU01_c, "double")
  expect_equal(out$AU01_r, c(1.25, 0.50, 0.00))
  expect_equal(out$AU01_c, c(1.00, 0.00, 0.00))
})

test_that("of_read() value-checks the gaze/pose/landmark feature blocks", {
  # gaze_0_x = -0.12, -0.20, 0.00 ; pose_Tx = 12.50, 12.60, 0.00 ;
  # x_0 (2D landmark) = 320.5, 321.0, 0.00
  out <- of_read(test_path("fixtures", "openface-sample.csv"))
  expect_equal(out$gaze_0_x, c(-0.12, -0.20, 0.00))
  expect_equal(out$pose_Tx, c(12.50, 12.60, 0.00))
  expect_equal(out$x_0, c(320.5, 321.0, 0.00))
})

test_that("of_read() keeps one row per detected face per frame (multi-face)", {
  # OpenFace multi-face output: two faces in frame 1 share `frame` but differ
  # in `face_id`.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("frame, face_id, timestamp, confidence, success, AU01_r",
               "1, 0, 0.000, 0.98, 1, 1.25",
               "1, 1, 0.000, 0.91, 1, 0.40"), tmp)
  out <- of_read(tmp)
  expect_equal(nrow(out), 2L)
  expect_equal(out$frame, c(1L, 1L))
  expect_equal(out$face_id, c(0L, 1L))
})

test_that("of_read() returns a 0-row tibble for a header-only file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("frame, face_id, timestamp, confidence, success", tmp)
  out <- of_read(tmp)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("frame", "face_id", "timestamp", "confidence", "success"))
})

test_that("of_read() coerces blank cells to NA within a numeric column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  # confidence blank in row 2 (two commas in a row) -> NA
  writeLines(c("frame, face_id, timestamp, confidence, success",
               "1, 0, 0.000, 0.98, 1",
               "2, 0, 0.033, , 1"), tmp)
  out <- of_read(tmp)
  expect_type(out$confidence, "double")
  expect_equal(out$confidence, c(0.98, NA))
})

test_that("of_read() errors on bad input", {
  expect_error(of_read(123), "single string")
  expect_error(of_read(c("a.csv", "b.csv")), "single string")
  expect_error(of_read(tempfile(fileext = ".csv")), "find")   # missing file

  empty <- tempfile(fileext = ".csv")
  on.exit(unlink(empty), add = TRUE)
  file.create(empty)
  expect_error(of_read(empty), "empty")
})
