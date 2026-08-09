# AC3 -- the platform guards on the suffixed installers.
#
# The domain is COMPUTED from the package's own exports rather than listed here,
# so a future `install_<tool>_<os>()` is held to the same contract without an
# edit to this file. The completeness test below is what makes that true: a new
# suffixed installer with no fixture fails rather than going unexercised.

suffixed_installers <- function() {
  sort(grep(
    "^install_[a-z.]+_(win|mac)$",
    getNamespaceExports("openac"),
    value = TRUE
  ))
}

# Per-installer facts a generic test cannot derive: which tool it installs, the
# platform its suffix declares, and the files a successful extraction leaves
# behind -- the installers hand those to `set_*()`, which refuses a location
# that is not there.
installer_fixtures <- list(
  install_ffmpeg_win = list(
    tool = "ffmpeg",
    target = "Windows",
    creates = c("bin/ffmpeg.exe", "bin/ffprobe.exe")
  ),
  install_openface_win = list(
    tool = "openface",
    target = "Windows",
    creates = "FaceLandmarkVidMulti.exe"
  ),
  install_opensmile_win = list(
    tool = "opensmile",
    target = "Windows",
    creates = "bin/SMILExtract.exe"
  ),
  install_opensmile_mac = list(
    tool = "opensmile",
    target = "Darwin",
    creates = "bin/SMILExtract"
  )
)

# A platform that is neither the installer's target nor one openac has any
# installer for -- the branch that must say so rather than fail silently.
unsupported_sysname <- "Linux"

# Message text with cli's line wrapping normalized away, so an assertion does
# not depend on the console width the test happens to run at.
wrong_os_message <- function(expr) {
  cnd <- rlang::catch_cnd(expr, classes = "openac_wrong_os")
  if (is.null(cnd)) {
    return(NA_character_)
  }
  gsub("\\s+", " ", paste(conditionMessage(cnd), collapse = " "))
}

test_that("the suffixed-installer set is computed and every member has a fixture", {
  fns <- suffixed_installers()
  expect_gt(length(fns), 0)
  # A sanity anchor: if the export scan silently broke, these would vanish.
  expect_true(all(
    c("install_ffmpeg_win", "install_opensmile_win", "install_opensmile_mac")
      %in% fns
  ))
  expect_identical(setdiff(fns, names(installer_fixtures)), character())
  expect_identical(setdiff(names(installer_fixtures), fns), character())
})

# --- wrong platform ----------------------------------------------------------

for (fn in suffixed_installers()) {
  fixture <- installer_fixtures[[fn]]
  other <- if (fixture$target == "Windows") "Darwin" else "Windows"

  test_that(paste0(fn, "() aborts on a platform it is not for"), {
    local_fake_os(other)
    local_fake_config()
    local_fake_data_dir()
    state <- local_fake_downloads()

    expect_error(do.call(fn, list()), class = "openac_wrong_os")
    # The guard runs before anything is fetched: a wrong-platform call must not
    # reach the network at all, not merely fail after it has.
    expect_length(state$downloads, 0)
    expect_length(state$extracts, 0)
  })

  test_that(paste0(fn, "() names the tool when no installer exists here"), {
    local_fake_os(unsupported_sysname)
    local_fake_config()
    local_fake_data_dir()

    msg <- wrong_os_message(do.call(fn, list()))
    expect_match(msg, "no automated")
    expect_match(msg, "Linux")
    expect_match(msg, paste0("set_", fixture$tool))
  })
}

test_that("a Windows installer points at its macOS sibling when one exists", {
  local_fake_os("Darwin")
  local_fake_config()
  local_fake_data_dir()

  msg <- wrong_os_message(install_opensmile_win())
  expect_match(msg, "install_opensmile_mac")
  expect_match(msg, "macOS")
  # The sibling branch and the no-installer branch are mutually exclusive.
  expect_no_match(msg, "no automated")
})

test_that("an installer with no sibling says so rather than suggesting one", {
  local_fake_os("Darwin")
  local_fake_config()
  local_fake_data_dir()

  msg <- wrong_os_message(install_ffmpeg_win())
  expect_match(msg, "no automated")
  expect_match(msg, "FFmpeg")
  expect_no_match(msg, "install_ffmpeg_mac")
})

# --- right platform ----------------------------------------------------------

for (fn in suffixed_installers()) {
  fixture <- installer_fixtures[[fn]]

  test_that(paste0(fn, "() proceeds on the platform it is for"), {
    local_fake_os(fixture$target)
    local_fake_config()
    local_fake_data_dir()
    state <- local_fake_downloads(extract_creates = fixture$creates)
    # This test is about the platform guard, not the model-integrity guard, so
    # the 40 MB floor the fake's 13-byte file cannot clear is lowered rather
    # than left to warn through every installer in the loop.
    testthat::local_mocked_bindings(model_byte_floor = function() 0)

    expect_no_error(do.call(fn, list()))
    # Proceeding means reaching the download, which is what the wrong-platform
    # test above asserts never happens.
    expect_gt(length(state$downloads), 0)
  })
}

# AC2 -- the URL each installer fetches and the path it installs to.
#
# Every test below runs under local_fake_downloads(), so `utils::download.file`
# and `archive::archive_extract` are recorders: nothing here reaches the network,
# and every path written is under a per-test temp dir (local_fake_config() and
# local_fake_data_dir() redirect the two rappdirs locations).

# The location an installer recorded for `program`, read back from the config
# file `set_program()` writes -- the install path as openac will later resolve
# it, not as the test recomputed it.
recorded_location <- function(config_dir, program) {
  path <- file.path(config_dir, paste0(program, "_location.txt"))
  if (!file.exists(path)) {
    return(NA_character_)
  }
  readLines(path, warn = FALSE)[[1]]
}

local_install_env <- function(target, .env = parent.frame()) {
  local_fake_os(target, .env = .env)
  list(
    config = local_fake_config(.env = .env),
    data = local_fake_data_dir(.env = .env)
  )
}

# --- install_ffmpeg_win ------------------------------------------------------

test_that("install_ffmpeg_win() fetches the pinned gyan.dev build", {
  env <- local_install_env("Windows")
  state <- local_fake_downloads(
    extract_creates = c("bin/ffmpeg.exe", "bin/ffprobe.exe")
  )
  install_dir <- withr::local_tempdir()

  expect_true(install_ffmpeg_win(install_dir = install_dir))

  expect_identical(
    download_urls(state),
    "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"
  )
  expect_identical(extract_dirs(state), install_dir)
  # Both binaries this installer promises are registered, at the paths it built.
  expect_identical(
    recorded_location(env$config, "ffmpeg"),
    file.path(install_dir, "bin", "ffmpeg.exe")
  )
  expect_identical(
    recorded_location(env$config, "ffprobe"),
    file.path(install_dir, "bin", "ffprobe.exe")
  )
})

test_that("install_ffmpeg_win() honours a supplied URL and default directory", {
  env <- local_install_env("Windows")
  state <- local_fake_downloads(
    extract_creates = c("bin/ffmpeg.exe", "bin/ffprobe.exe")
  )

  install_ffmpeg_win(download_url = "https://example.invalid/custom.7z")

  expect_identical(download_urls(state), "https://example.invalid/custom.7z")
  # install_dir = NULL means the rappdirs data dir, under a `ffmpeg` subfolder.
  expect_identical(extract_dirs(state), file.path(env$data, "ffmpeg"))
})

# --- install_openface_win ----------------------------------------------------

test_that("install_openface_win() fetches the release and its patch experts", {
  env <- local_install_env("Windows")
  withr::local_options(timeout = getOption("timeout"))
  state <- local_fake_downloads(extract_creates = "FaceLandmarkVidMulti.exe")
  # The real floor is 40 MB and this fake writes 13 bytes. Lowering it here
  # keeps this test about WHICH urls are fetched and WHERE they land; the floor
  # itself is asserted by the two failure tests below.
  testthat::local_mocked_bindings(model_byte_floor = function() 0)
  install_dir <- withr::local_tempdir()

  expect_true(install_openface_win(install_dir = install_dir))

  urls <- download_urls(state)
  expect_identical(
    urls[[1]],
    paste0(
      "https://github.com/TadasBaltrusaitis/OpenFace/releases/download/",
      "OpenFace_2.2.0/OpenFace_2.2.0_win_x64.zip"
    )
  )
  expect_identical(extract_dirs(state), install_dir)
  expect_identical(
    recorded_location(env$config, "openface"),
    file.path(install_dir, "FaceLandmarkVidMulti.exe")
  )
  # The four patch-expert models, at the scales OpenFace looks for them under.
  expect_length(urls, 5)
  # Dropbox, not OneDrive. M16 MEASURED all four OneDrive links answering 200
  # with a login.live.com page on 2026-08-08; these are the primary links
  # OpenFace's own download_models scripts try first, which openac had skipped
  # in favour of upstream's fallback.
  expect_true(all(grepl("^https://www\\.dropbox\\.com/s/", urls[-1])))
  expect_false(any(grepl("onedrive", urls, fixed = TRUE)))
  expect_identical(
    download_dests(state)[-1],
    file.path(
      install_dir, "model", "patch_experts",
      paste0("cen_patches_", c("0.25", "0.35", "0.50", "1.00"), "_of.dat")
    )
  )
})

test_that("install_openface_win() refuses a model URL serving a sign-in page", {
  # The failure M16 exists to catch, reproduced at the boundary. `download.file`
  # reports success (status 0) and writes a file that exists and is non-empty --
  # every check the installer used to make. What distinguishes it from a model
  # is its content, so that is what is checked.
  local_install_env("Windows")
  withr::local_options(timeout = getOption("timeout"))
  local_fake_downloads(
    extract_creates = "FaceLandmarkVidMulti.exe",
    content = "<!-- Copyright (C) Microsoft Corporation. --><html><body>Sign in"
  )
  testthat::local_mocked_bindings(model_byte_floor = function() 0)
  install_dir <- withr::local_tempdir()

  warnings <- testthat::capture_warnings(
    result <- install_openface_win(install_dir = install_dir)
  )
  expect_false(result)
  expect_true(any(grepl("markup document", warnings)))
})

test_that("install_openface_win() refuses a model file below the byte floor", {
  # The other half. A truncated or error-body download can be perfectly valid
  # binary and still not be a 60 MB model, so the sniff alone is not enough.
  local_install_env("Windows")
  withr::local_options(timeout = getOption("timeout"))
  local_fake_downloads(extract_creates = "FaceLandmarkVidMulti.exe")
  install_dir <- withr::local_tempdir()

  # No mocked floor here: the real 40 MB one against the fake's 13 bytes.
  warnings <- testthat::capture_warnings(
    result <- install_openface_win(install_dir = install_dir)
  )
  expect_false(result)
  expect_true(any(grepl("below the", warnings)))
})

test_that("install_openface_win() tries all four models and names every failure", {
  # The OneDrive set died as a SET (M16's measurement), so an installer that
  # returns at the first bad model tells the user about one dead link per run --
  # and each run re-downloads the 130 MB release archive to get there. The
  # assertion is the count of download attempts, which is what distinguishes
  # "tried all four" from "stopped at the first"; the fake's 13-byte files fail
  # the real 40 MB floor, so all four models fail.
  local_install_env("Windows")
  withr::local_options(timeout = getOption("timeout"))
  state <- local_fake_downloads(extract_creates = "FaceLandmarkVidMulti.exe")
  install_dir <- withr::local_tempdir()

  warnings <- testthat::capture_warnings(
    result <- install_openface_win(install_dir = install_dir)
  )
  expect_false(result)

  # The release archive plus all four patch experts: five attempts, not two.
  expect_length(download_urls(state), 5L)
  expect_true(all(grepl(
    "^https://www\\.dropbox\\.com/s/", download_urls(state)[-1]
  )))

  # Each failure reported on its own, then one line naming the whole set.
  expect_length(grep("below the", warnings), 4L)
  summary <- warnings[grepl("did not download", warnings)]
  expect_length(summary, 1L)
  for (scale in c("0.25", "0.35", "0.50", "1.00")) {
    expect_true(grepl(paste0("cen_patches_", scale, "_of.dat"), summary), info = scale)
  }
})

test_that("install_openface_win() reports a download that fails outright", {
  local_install_env("Windows")
  withr::local_options(timeout = getOption("timeout"))
  # Status 1 on the FIRST download aborts before the models are reached, so the
  # release archive's own failure path is what this covers; the models' is the
  # two tests above.
  local_fake_downloads(status = 1L, extract_creates = "FaceLandmarkVidMulti.exe")
  install_dir <- withr::local_tempdir()

  expect_warning(
    expect_false(install_openface_win(install_dir = install_dir)),
    "download failed"
  )
})

# --- install_opensmile_win ---------------------------------------------------

test_that("install_opensmile_win() fetches the pinned 3.0.2 Windows archive", {
  env <- local_install_env("Windows")
  state <- local_fake_downloads(extract_creates = "bin/SMILExtract.exe")
  install_dir <- withr::local_tempdir()

  expect_true(install_opensmile_win(install_dir = install_dir))

  expect_identical(
    download_urls(state),
    paste0(
      "https://github.com/audeering/opensmile/releases/download/",
      # `win-x64` was pinned until 2026-08-08 and MEASURED 404 (M16): the v3.0.2
      # release has no asset by that name. This one it does have.
      "v3.0.2/opensmile-3.0.2-windows-x86_64.zip"
    )
  )
  expect_identical(extract_dirs(state), install_dir)
  expect_identical(
    recorded_location(env$config, "opensmile"),
    file.path(install_dir, "bin", "SMILExtract.exe")
  )
})

# --- install_opensmile_mac ---------------------------------------------------

test_that("install_opensmile_mac() fetches the archive for the requested arch", {
  env <- local_install_env("Darwin")
  state <- local_fake_downloads(extract_creates = "bin/SMILExtract")
  install_dir <- withr::local_tempdir()

  expect_true(install_opensmile_mac(install_dir = install_dir))

  # armv8 is the default arch.
  expect_identical(
    download_urls(state),
    paste0(
      "https://github.com/audeering/opensmile/releases/download/v3.0.2/",
      "opensmile-3.0.2-macos-armv8.zip"
    )
  )
  expect_identical(extract_dirs(state), install_dir)
  # This installer records an absolutized path, unlike its Windows sibling.
  expect_identical(
    recorded_location(env$config, "opensmile"),
    tools::file_path_as_absolute(file.path(install_dir, "bin", "SMILExtract"))
  )
})

test_that("install_opensmile_mac(arch = 'x86_64') fetches the Intel archive", {
  local_install_env("Darwin")
  state <- local_fake_downloads(extract_creates = "bin/SMILExtract")
  install_dir <- withr::local_tempdir()

  install_opensmile_mac(install_dir = install_dir, arch = "x86_64")

  expect_identical(
    download_urls(state),
    paste0(
      "https://github.com/audeering/opensmile/releases/download/v3.0.2/",
      "opensmile-3.0.2-macos-x86_64.zip"
    )
  )
})

test_that("install_opensmile_mac() rejects an unknown arch", {
  local_install_env("Darwin")
  state <- local_fake_downloads()

  expect_error(
    install_opensmile_mac(install_dir = withr::local_tempdir(), arch = "riscv"),
    "arg"
  )
  expect_length(state$downloads, 0)
})

# --- a failed download -------------------------------------------------------

for (fn in suffixed_installers()) {
  fixture <- installer_fixtures[[fn]]

  test_that(paste0(fn, "() reports a failed download and installs nothing"), {
    env <- local_install_env(fixture$target)
    # A non-zero status is what utils::download.file() reports on failure.
    state <- local_fake_downloads(status = 1L, extract_creates = fixture$creates)

    expect_warning(
      result <- do.call(fn, list(install_dir = withr::local_tempdir())),
      "download failed"
    )
    expect_false(result)
    # Nothing is extracted and no tool location is recorded from a failed fetch.
    expect_length(state$extracts, 0)
    expect_true(is.na(recorded_location(env$config, fixture$tool)))
  })
}

# --- install_whisper ---------------------------------------------------------

test_that("install_whisper() delegates to rlang::check_installed()", {
  # It downloads nothing itself -- rlang prompts and installs from Remotes.
  asked <- NULL
  testthat::local_mocked_bindings(
    check_installed = function(pkg, ...) {
      asked <<- pkg
      invisible(TRUE)
    },
    .package = "rlang"
  )
  state <- local_fake_downloads()

  install_whisper()

  expect_identical(asked, "audio.whisper")
  expect_length(state$downloads, 0)
})
