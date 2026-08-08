# AC4 -- program resolution and the check_* contract.
#
# All config I/O is redirected to a temp dir; no test reads or writes the real
# rappdirs config location (IP1).

# A real, executable file standing in for an installed binary. Returns the
# canonical path, since find_program() promises an absolute one and
# tools::file_path_as_absolute() resolves symlinks (on macOS /var is a link
# into /private/var, so a bare tempdir() path is not what comes back).
fake_binary <- function(name = "tool") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, name)
  file.create(path)
  Sys.chmod(path, "0755")
  tools::file_path_as_absolute(path)
}

test_that("find_program() resolves a program found on PATH", {
  local_fake_tools()

  expect_identical(basename(find_program("ffmpeg")), "ffmpeg")
  expect_identical(basename(find_ffprobe()), "ffprobe")
  expect_identical(basename(find_openface()), "openface")
  expect_identical(basename(find_opensmile()), "opensmile")
})

test_that("find_program() resolves a program recorded in the config file", {
  # Nothing on PATH, so resolution must come from the config file.
  config_dir <- local_fake_tools(resolve = character())$config
  recorded <- fake_binary("opensmile")
  writeLines(recorded, file.path(config_dir, "opensmile_location.txt"))

  expect_identical(find_program("opensmile"), recorded)
})

test_that("find_program() resolves a config entry naming a bare program name", {
  # set_program()'s guard is Sys.which(location) != "", which a bare name on
  # the PATH passes -- so the config can legitimately hold "ffmpeg" rather than
  # a path. Before this fix find_program() handed that raw string to
  # tools::file_path_as_absolute(), which errored, and check_*() propagated it.
  state <- local_fake_tools(results = list("v"), resolve = "ffmpeg")
  config_dir <- state$config
  writeLines("ffmpeg", file.path(config_dir, "opensmile_location.txt"))

  expect_identical(
    find_program("opensmile"),
    tools::file_path_as_absolute(file.path(state$bindir, "ffmpeg"))
  )
  # And the check_* contract holds on that path instead of erroring.
  expect_true(check_opensmile())
})

test_that("set_program() writes a location find_program() reads back", {
  local_fake_tools(resolve = character())
  recorded <- fake_binary("ffmpeg")

  set_ffmpeg(recorded)
  expect_identical(find_ffmpeg(), recorded)
})

test_that("find_program() warns and returns NULL when the tool is absent", {
  local_fake_tools(resolve = character())

  expect_warning(res <- find_program("ffmpeg"), "Failed to find")
  expect_null(res)
})

test_that("find_program() warns and returns NULL for a stale config entry", {
  config_dir <- local_fake_tools(resolve = character())$config
  writeLines(
    file.path(tempdir(), "gone-for-good"),
    file.path(config_dir, "openface_location.txt")
  )

  expect_warning(res <- find_program("openface"), "no longer resolves")
  expect_null(res)
})

test_that("find_program() treats an empty config file as unresolved", {
  config_dir <- local_fake_tools(resolve = character())$config
  writeLines(c("", "   "), file.path(config_dir, "ffprobe_location.txt"))

  expect_warning(res <- find_program("ffprobe"), "no longer resolves")
  expect_null(res)
})

test_that("find_program() rejects an unknown or non-string program", {
  expect_error(find_program("mplayer"), "must be one of")
  expect_error(find_program(1), "must be one of")
  expect_error(find_program(c("ffmpeg", "ffprobe")), "must be one of")
})

test_that("check_*() return FALSE, not an error, when the tool is absent", {
  # The regression this milestone exists to prevent: before the find_program()
  # fix these propagated an error out of file_path_as_absolute(NULL).
  local_fake_tools(resolve = character())

  expect_warning(expect_false(check_ffmpeg()), "Failed to find")
  expect_warning(expect_false(check_ffprobe()), "Failed to find")
  expect_warning(expect_false(check_openface()), "Failed to find")
  expect_warning(expect_false(check_opensmile()), "Failed to find")
})

test_that("check_*() return TRUE when the tool resolves and runs", {
  state <- local_fake_tools(results = list("v1", "v2", "v3", "v4"))

  expect_true(check_ffmpeg())
  expect_true(check_ffprobe())
  expect_true(check_openface())
  expect_true(check_opensmile())

  # Each check_* probes its own tool with the version/help flag that tool takes.
  expect_identical(
    boundary_tools(state),
    c("ffmpeg", "ffprobe", "openface", "opensmile")
  )
  expect_identical(boundary_args(state), c("-version", "-version", "-h", "-h"))
})
