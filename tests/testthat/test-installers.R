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
  expect_true(all(grepl("^https://onedrive\\.live\\.com/download\\?", urls[-1])))
  expect_identical(
    download_dests(state)[-1],
    file.path(
      install_dir, "model", "patch_experts",
      paste0("cen_patches_", c("0.25", "0.35", "0.50", "1.00"), "_of.dat")
    )
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
      "v3.0.2/opensmile-3.0.2-win-x64.zip"
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
