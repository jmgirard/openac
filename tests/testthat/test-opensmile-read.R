# Tests for os_read() — parsing openSMILE CSV output into a tidy tibble.
# Oracles are hand-built fixtures with known values (see fixtures/*.csv);
# expected values below are read directly off those files.

test_that("os_read() returns a tibble", {
  agg <- os_read(test_path("fixtures", "opensmile-agg.csv"))
  expect_true(tibble::is_tibble(agg))
})

test_that("os_read() parses an aggregate (functionals) file", {
  # fixtures/opensmile-agg.csv: header `name;<3 features>`, one data row
  #   name                        = audio1.wav
  #   pcm_RMSenergy_sma_amean      = 0.0234560
  #   pcm_RMSenergy_sma_stddev     = 0.0112340
  #   F0final_sma_amean            = 145.6789
  agg <- os_read(test_path("fixtures", "opensmile-agg.csv"))

  expect_equal(nrow(agg), 1L)
  expect_equal(ncol(agg), 4L)
  expect_equal(agg$name, "audio1.wav")
  expect_type(agg$pcm_RMSenergy_sma_amean, "double")
  expect_equal(agg$pcm_RMSenergy_sma_amean, 0.023456)
  expect_equal(agg$pcm_RMSenergy_sma_stddev, 0.011234)
  expect_equal(agg$F0final_sma_amean, 145.6789)
})

test_that("os_read() parses an LLD (per-frame) file", {
  # fixtures/opensmile-lld.csv: header `name;frameTime;<2 llds>`, 4 frames
  #   frameTime           = 0.00, 0.01, 0.02, 0.03   (non-decreasing)
  #   pcm_RMSenergy_sma   = 0.010, 0.020, 0.030, 0.025
  #   F0final_sma         = 0.00, 110.50, 112.25, 0.00
  lld <- os_read(test_path("fixtures", "opensmile-lld.csv"))

  expect_equal(nrow(lld), 4L)
  expect_equal(names(lld), c("name", "frameTime", "pcm_RMSenergy_sma", "F0final_sma"))
  expect_type(lld$frameTime, "double")
  expect_equal(lld$frameTime, c(0, 0.01, 0.02, 0.03))
  expect_false(is.unsorted(lld$frameTime))
  expect_equal(lld$pcm_RMSenergy_sma, c(0.010, 0.020, 0.030, 0.025))
  expect_equal(lld$F0final_sma, c(0, 110.50, 112.25, 0))
})

test_that("os_read() gives the same result for native ';' and comma-normalized files", {
  # os_fix_csv() rewrites a native ';'-delimited openSMILE file to a
  # comma-delimited, quote-wrapped CSV in place. os_read() must handle both.
  native <- os_read(test_path("fixtures", "opensmile-agg.csv"))

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(test_path("fixtures", "opensmile-agg.csv"), tmp, overwrite = TRUE)
  os_fix_csv(tmp)                    # ; -> , (in place)
  header <- readLines(tmp, n = 1L)
  expect_false(grepl(";", header, fixed = TRUE))   # confirm it was normalized

  fixed <- os_read(tmp)
  expect_equal(fixed, native)
})

test_that("os_read() preserves non-syntactic feature names (e.g. mfcc[1])", {
  # openSMILE emits bracketed feature names; check.names must not mangle them.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("name;frameTime;pcm_fftMag_mfcc[1]",
               "audio1.wav;0.000000;-42.5"), tmp)
  out <- os_read(tmp)
  expect_true("pcm_fftMag_mfcc[1]" %in% names(out))
  expect_equal(out[["pcm_fftMag_mfcc[1]"]], -42.5)
})

test_that("os_read() strips a quoted instance name", {
  # Some openSMILE builds single-quote the instance name.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("name;frameTime;pcm_RMSenergy_sma",
               "'audio1.wav';0.000000;0.01"), tmp)
  out <- os_read(tmp)
  expect_equal(out$name, "audio1.wav")
})

test_that("os_read() errors on bad input", {
  expect_error(os_read(123), "single string")
  expect_error(os_read(c("a.csv", "b.csv")), "single string")
  expect_error(os_read(tempfile(fileext = ".csv")), "find")   # missing file

  empty <- tempfile(fileext = ".csv")
  on.exit(unlink(empty), add = TRUE)
  file.create(empty)
  expect_error(os_read(empty), "empty")
})
