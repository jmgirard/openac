# M16 T1 -- the Windows installers, actually run (AC1, AC2, AC4).
#
# `test-installers.R` mocks `download.file()` and `archive::archive_extract()`.
# It proves the installers call the right things with the right paths, and it
# is structurally incapable of noticing that a URL is dead: a mocked download
# succeeds whatever it was pointed at. Every green run of that file is
# compatible with all seven pinned URLs having rotted years ago.
#
# This file is the other half. It downloads from the live network, for real,
# and asserts the tools it installs actually work. That costs roughly 600 MB
# and several minutes, so it is OPT-IN -- `OPENAC_INSTALLER_RUN=true` -- and
# unset everywhere by default, CI included. A skip here is the expected
# outcome; the opt-in run is a maintenance act, done deliberately.
#
# Gates are per-test rather than at file top level (M11), so no future edit can
# take the whole file down with one line, and the platform gate is per-test
# because the three installers target different platforms.

# --- gates -------------------------------------------------------------------

# The opt-in. Read through `as.logical()` so "TRUE", "true" and "T" all work and
# anything else -- including the unset default -- is `NA`, hence not `TRUE`.
installer_run_requested <- function() {
  isTRUE(as.logical(Sys.getenv("OPENAC_INSTALLER_RUN", "false")))
}

# `platform` is a `Sys.info()[["sysname"]]` value. Checked against the REAL one,
# never a faked one: the point of this file is what happens on this machine.
skip_unless_installer_run <- function(platform) {
  testthat::skip_on_cran()
  testthat::skip_if(
    !installer_run_requested(),
    "set OPENAC_INSTALLER_RUN=true to run the real installers (~600 MB)"
  )
  testthat::skip_if_not_installed("curl")
  running <- unname(Sys.info()[["sysname"]])
  testthat::skip_if(
    !identical(running, platform),
    paste0("this installer targets ", platform, "; running on ", running)
  )
}

# --- the measurement record --------------------------------------------------
#
# AC2 wants a per-URL record transcribed from the run, not retyped from memory.
# testthat swallows `cat()`, so the record goes to a file the runner names --
# `OPENAC_INSTALLER_LOG` -- and nowhere at all when that is unset. Appending
# rather than truncating keeps every test's rows in one file across the run.
record_measurement <- function(...) {
  path <- Sys.getenv("OPENAC_INSTALLER_LOG", "")
  if (!nzchar(path)) return(invisible(NULL))
  cat(paste0(..., collapse = ""), "\n", sep = "", file = path, append = TRUE)
}

# --- what "delivered" means --------------------------------------------------
#
# The failure this file exists to catch is not a 404. It is a URL that answers
# 200 with a sign-in page: `download.file()` reports success, writes 34 KB of
# HTML where a 60 MB model belongs, and `install_openface_win()` returns TRUE.
# So a URL counts as delivered only if it clears BOTH bars -- a byte floor and
# a content sniff -- and neither alone is sufficient.

# Floors are deliberately well under the sizes MEASURED on 2026-08-08 (recorded
# in the milestone), so an upstream re-release does not redden the suite, while
# a sign-in page (~34 KB) or an error body (~29 bytes) is nowhere near them.
installer_floors <- function() {
  c(
    ffmpeg_tree      = 30e6,
    opensmile_tree   = 5e6,
    openface_tree    = 80e6,
    patch_expert     = 40e6
  )
}

# The markup sniff is NOT re-implemented here. `openac:::starts_with_markup()`
# and `openac:::raw_is_markup()` are the shipped guard, and asserting a local
# copy of the rule would leave this file green over a production sniff that had
# stopped matching -- which is the whole failure mode the guard exists for.
# `raw_is_markup()` takes bytes, so the probe below can use it on a response
# body without writing the body to disk first.

# The size of a whole extracted tree, which is what an archive URL delivers --
# the downloaded archive itself is `unlink()`ed by the installer before it
# returns, so it cannot be measured afterwards.
tree_size <- function(dir) {
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  sum(file.size(files[!dir.exists(files)]), na.rm = TRUE)
}

# --- AC2: what every pinned URL answers --------------------------------------

test_that("every URL pinned in programs_install.R delivers a real file", {
  # Runs on any platform: a URL's health has nothing to do with the host, and
  # the macOS openSMILE URLs are in `programs_install.R` too, so a Windows-only
  # gate here would leave two of the seven permanently unmeasured.
  testthat::skip_on_cran()
  testthat::skip_if(
    !installer_run_requested(),
    "set OPENAC_INSTALLER_RUN=true to probe the pinned URLs"
  )
  testthat::skip_if_not_installed("curl")

  # Every `https://` in `R/programs_install.R` outside roxygen, spelled out as
  # the whole URL each installer builds. Assembled by hand from the source
  # because three of them are `paste0()`d from pieces; the milestone's AC2 list
  # is the check that none went missing.
  urls <- c(
    ffmpeg = "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z",
    openface = paste0(
      "https://github.com/TadasBaltrusaitis/OpenFace/releases/download/",
      "OpenFace_2.2.0/OpenFace_2.2.0_win_x64.zip"
    ),
    opensmile_win = paste0(
      "https://github.com/audeering/opensmile/releases/download/",
      "v3.0.2/opensmile-3.0.2-windows-x86_64.zip"
    ),
    opensmile_mac_armv8 = paste0(
      "https://github.com/audeering/opensmile/releases/download/v3.0.2/",
      "opensmile-3.0.2-macos-armv8.zip"
    ),
    opensmile_mac_x86_64 = paste0(
      "https://github.com/audeering/opensmile/releases/download/v3.0.2/",
      "opensmile-3.0.2-macos-x86_64.zip"
    ),
    patch_expert_0.25 = "https://www.dropbox.com/s/7na5qsjzz8yfoer/cen_patches_0.25_of.dat?dl=1",
    patch_expert_0.35 = "https://www.dropbox.com/s/k7bj804cyiu474t/cen_patches_0.35_of.dat?dl=1",
    patch_expert_0.50 = "https://www.dropbox.com/s/ixt4vkbmxgab1iu/cen_patches_0.50_of.dat?dl=1",
    patch_expert_1.00 = "https://www.dropbox.com/s/2t5t1sdpshzfhpj/cen_patches_1.00_of.dat?dl=1"
  )

  for (name in names(urls)) {
    # A ranged GET rather than a HEAD: HEAD is answered by CDNs that then serve
    # something else, and the first bytes are what the content sniff needs. The
    # `Content-Range` total is the full size without downloading it.
    handle <- curl::new_handle()
    curl::handle_setopt(handle, followlocation = TRUE, range = "0-2047")
    response <- curl::curl_fetch_memory(urls[[name]], handle = handle)
    headers <- curl::parse_headers_list(response$headers)

    content_range <- headers[["content-range"]]
    if (is.null(content_range)) content_range <- ""
    total <- sub("^bytes [0-9]+-[0-9]+/", "", content_range)
    # The shipped rule, on the response body -- not a copy of it.
    markup <- openac:::raw_is_markup(utils::head(response$content, 512))

    record_measurement(
      "URL ", name,
      " | status=", response$status_code,
      " | type=", if (is.null(headers[["content-type"]])) "-" else headers[["content-type"]],
      " | total-bytes=", if (nzchar(total)) total else "-",
      " | markup=", markup,
      " | final=", substr(response$url, 1L, 120L)
    )

    # 206 because the request is ranged; a server that ignores `Range` answers
    # 200 with the whole body, which is equally fine.
    expect_true(
      response$status_code %in% c(200L, 206L),
      info = paste0(name, ": HTTP ", response$status_code)
    )
    # The bar a sign-in page fails. `install_openface_win()` shipped four URLs
    # that answered 200 for years while delivering login.live.com's HTML.
    expect_false(markup, info = paste0(name, ": served markup, not a file"))
  }
})

# --- AC1/AC4: the installers, run for real -----------------------------------

test_that("install_ffmpeg_win() really installs a working ffmpeg and ffprobe", {
  skip_unless_installer_run("Windows")

  # Both dirs redirected: this runs on the maintainer's working machine, and
  # `set_ffmpeg()` writing the real rappdirs config would overwrite the tool
  # locations openac is actually used with here.
  local_fake_config()
  local_fake_data_dir()
  install_dir <- withr::local_tempdir()

  expect_true(install_ffmpeg_win(install_dir = install_dir))

  size <- tree_size(install_dir)
  record_measurement("INSTALL ffmpeg | extracted-tree-bytes=", size)
  expect_gt(size, installer_floors()[["ffmpeg_tree"]])

  # The tools, not the installer's own report: `TRUE` is what a sign-in page
  # gets you too.
  expect_true(file.exists(file.path(install_dir, "bin", "ffmpeg.exe")))
  expect_true(file.exists(file.path(install_dir, "bin", "ffprobe.exe")))
  expect_true(suppressWarnings(check_ffmpeg()))
  expect_true(suppressWarnings(check_ffprobe()))
})

test_that("install_opensmile_win() really installs a working openSMILE", {
  skip_unless_installer_run("Windows")

  local_fake_config()
  local_fake_data_dir()
  install_dir <- withr::local_tempdir()

  expect_true(install_opensmile_win(install_dir = install_dir))

  size <- tree_size(install_dir)
  record_measurement("INSTALL opensmile | extracted-tree-bytes=", size)
  expect_gt(size, installer_floors()[["opensmile_tree"]])

  expect_true(file.exists(file.path(install_dir, "bin", "SMILExtract.exe")))
  expect_true(suppressWarnings(check_opensmile()))
})

test_that("install_openface_win() really installs OpenFace and its patch experts", {
  skip_unless_installer_run("Windows")

  local_fake_config()
  local_fake_data_dir()
  install_dir <- withr::local_tempdir()

  expect_true(install_openface_win(install_dir = install_dir))

  size <- tree_size(install_dir)
  record_measurement("INSTALL openface | extracted-tree-bytes=", size)
  expect_gt(size, installer_floors()[["openface_tree"]])
  expect_true(file.exists(file.path(install_dir, "FaceLandmarkVidMulti.exe")))

  # AC4. The four models are the whole reason this installer is more than an
  # unzip, and they are where the silent failure lived: `download.file()`
  # returned 0, the file existed, and it held a login page. Existence is not
  # the assertion -- a floor and a content sniff are.
  for (scale in c("0.25", "0.35", "0.50", "1.00")) {
    path <- file.path(
      install_dir, "model", "patch_experts",
      paste0("cen_patches_", scale, "_of.dat")
    )
    expect_true(file.exists(path), info = scale)
    if (!file.exists(path)) next

    bytes <- file.size(path)
    markup <- openac:::starts_with_markup(path)
    record_measurement(
      "INSTALL patch_expert ", scale, " | bytes=", bytes, " | markup=", markup
    )
    expect_gt(bytes, installer_floors()[["patch_expert"]])
    expect_false(markup, info = paste0(scale, ": a markup document, not a model"))
  }

  # And the tool runs with what was installed. `check_openface()` shells the
  # binary, which loads every model file above -- so it fails if any of them is
  # a login page that cleared the floor some other way.
  expect_true(suppressWarnings(check_openface()))
})
