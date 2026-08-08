# AC3 -- the skip/fail/enforce decision, unit-tested by name.
#
# The decision is a pure function precisely so that this file can reach every
# branch with ordinary arguments. Three of the five returns are states a healthy
# suite must never be in, and the only other way to observe them is to break the
# real suite -- which is how the previous two attempts ended up with branches
# nobody had ever seen taken (D-013).

# The healthy case every other case perturbs one fact of.
healthy <- list(
  expected = c("test-a.R", "test-b.R"),
  ran = c("test-a.R", "test-b.R"),
  covered = c("ffmpeg", "os_extract"),
  domain = c("ffmpeg", "os_extract"),
  deferred = character(),
  declared_full = FALSE
)

decide <- function(...) do.call(contract_decision, utils::modifyList(healthy, list(...)))

test_that("enforce_pass: a complete run with every enforced function covered", {
  expect_identical(decide()$action, "enforce_pass")
  # A declared full run reaches the same verdict -- the declaration only ever
  # changes what an INCOMPLETE run does.
  expect_identical(decide(declared_full = TRUE)$action, "enforce_pass")
  # A function that is in the domain but deferred is not enforced.
  expect_identical(
    decide(domain = c("ffmpeg", "os_extract", "later"),
           deferred = "later")$action,
    "enforce_pass"
  )
})

test_that("enforce_fail names the functions with no command test", {
  d <- decide(domain = c("ffmpeg", "os_extract", "of_extract", "aw_transcribe"))
  expect_identical(d$action, "enforce_fail")
  expect_setequal(d$uncovered, c("of_extract", "aw_transcribe"))
})

test_that("fail_incomplete names the files a declared-full run did not execute", {
  d <- decide(ran = "test-a.R", declared_full = TRUE)
  expect_identical(d$action, "fail_incomplete")
  expect_identical(d$files, "test-b.R")
  # The declaration is what makes this a failure rather than a skip, and it is
  # the ONLY difference between this case and the next one.
  expect_identical(decide(ran = "test-a.R", declared_full = FALSE)$action,
                   "skip_partial")
})

test_that("skip_partial names the files an undeclared partial run did not execute", {
  d <- decide(ran = character())
  expect_identical(d$action, "skip_partial")
  expect_identical(d$files, c("test-a.R", "test-b.R"))
  # Incompleteness is decided before coverage: a partial run says which files
  # are missing rather than reporting every uncovered function as a gap.
  d2 <- decide(ran = "test-a.R", covered = character())
  expect_identical(d2$action, "skip_partial")
})

test_that("fail_broken_attribution fires on a complete run that attributed nothing", {
  d <- decide(covered = character())
  expect_identical(d$action, "fail_broken_attribution")
  # Not a skip and not a vacuous pass -- an empty domain must not launder a
  # dead recorder into "nothing to enforce".
  expect_identical(decide(covered = character(), domain = character())$action,
                   "fail_broken_attribution")
  expect_identical(
    decide(covered = character(), declared_full = TRUE)$action,
    "fail_broken_attribution"
  )
})

test_that("declared_full_run() reads OPENAC_FULL_SUITE", {
  withr::local_envvar(OPENAC_FULL_SUITE = "true")
  expect_true(declared_full_run())
  withr::local_envvar(OPENAC_FULL_SUITE = "false")
  expect_false(declared_full_run())
  withr::local_envvar(OPENAC_FULL_SUITE = NA)
  expect_false(declared_full_run())
  # A value that is not a logical is not a declaration.
  withr::local_envvar(OPENAC_FULL_SUITE = "yes please")
  expect_false(declared_full_run())
})
