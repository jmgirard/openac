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
