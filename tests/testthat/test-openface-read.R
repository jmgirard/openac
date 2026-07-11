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

test_that("of_read() errors on bad input", {
  expect_error(of_read(123), "single string")
  expect_error(of_read(c("a.csv", "b.csv")), "single string")
  expect_error(of_read(tempfile(fileext = ".csv")), "find")   # missing file

  empty <- tempfile(fileext = ".csv")
  on.exit(unlink(empty), add = TRUE)
  file.create(empty)
  expect_error(of_read(empty), "empty")
})
