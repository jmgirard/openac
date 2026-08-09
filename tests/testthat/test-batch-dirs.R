# AC4 -- the `*_dir()` batch wrappers: which files they enumerate, where they
# derive outputs to, and what they do when one file of several fails.
#
# The enumeration and derivation tests are regression tests: before M07 the
# wrappers matched `paste0(inext, "$")` and derived outputs with
# `gsub(indir, outdir, .)`, so each case below produced a wrong path or a wrong
# match. See `dir_inputs()`/`dir_outputs()` in R/utils.R.

# An input tree holding every case extension matching gets wrong:
#   clip.mp4             the ordinary one
#   clip.mp4.backup.mp4  the extension occurring twice in one name
#   notes.notmp4         a name merely *ending* in those letters
#   mp4/nested.mp4       a DIRECTORY named like the extension, holding an input
#   scenes.mp4/          a DIRECTORY whose own name MATCHES the pattern
#
# The last one is the case that actually bites: `list.files(recursive = FALSE)`
# returns directories too, so `scenes.mp4` is enumerated as an input unless
# something filters it out. A directory named plain `mp4` cannot show it,
# because it does not match `\.mp4$` in the first place.
local_input_tree <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  dir.create(file.path(dir, "mp4"))
  dir.create(file.path(dir, "scenes.mp4"))
  files <- c("clip.mp4", "clip.mp4.backup.mp4", "notes.notmp4", "mp4/nested.mp4")
  for (f in files) file.create(file.path(dir, f))
  dir
}

# The top-level `.mp4` inputs, sorted as list.files() returns them.
top_level_mp4 <- function(dir) {
  sort(file.path(dir, c("clip.mp4", "clip.mp4.backup.mp4")))
}

# --- enumeration -------------------------------------------------------------

test_that("extension matching is anchored on the dot and the end of the name", {
  indir <- local_input_tree()

  expect_identical(
    sort(openac:::dir_inputs(indir, "mp4")),
    top_level_mp4(indir)
  )
  # `notes.notmp4` ends in "mp4" but its extension is not mp4.
  expect_false(any(grepl("notmp4", openac:::dir_inputs(indir, "mp4"))))
  # A leading dot on `inext` is accepted and means the same thing.
  expect_identical(
    sort(openac:::dir_inputs(indir, ".mp4")),
    top_level_mp4(indir)
  )
})

test_that("recursive = TRUE reaches subdirectories and FALSE does not", {
  indir <- local_input_tree()

  flat <- openac:::dir_inputs(indir, "mp4", recursive = FALSE)
  deep <- openac:::dir_inputs(indir, "mp4", recursive = TRUE)

  expect_length(flat, 2)
  expect_length(deep, 3)
  # A directory named like the extension contributes its files, never itself.
  expect_true(file.path(indir, "mp4", "nested.mp4") %in% deep)
  expect_false(file.path(indir, "mp4") %in% deep)
})

test_that("a directory whose own name matches the extension is not an input", {
  indir <- local_input_tree()

  # Asserted on the NON-recursive listing, which is the only one that returns
  # directories at all -- the recursive form omits them for free, so an
  # assertion made there cannot fail and proves nothing.
  flat <- openac:::dir_inputs(indir, "mp4", recursive = FALSE)

  expect_false(file.path(indir, "scenes.mp4") %in% flat)
  expect_true(all(!dir.exists(flat)))
  # And the batch wrapper never hands a directory to the tool: `file.exists()`
  # is TRUE for a directory, so the wrapper's own input check would not catch it.
  state <- local_fake_tools(results = list("ok", "ok"))
  result <- of_extract_dir(indir, "mp4", file.path(withr::local_tempdir(), "out"))
  expect_identical(nrow(result), 2L)
  expect_length(boundary_tools(state), 2L)
  expect_false(any(basename(result$infile) == "scenes.mp4"))
})

test_that("an extension carrying regex metacharacters is matched literally", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("a.c++", "axcyy")))

  expect_identical(
    openac:::dir_inputs(dir, "c++"),
    file.path(dir, "a.c++")
  )
})

# --- output-path derivation --------------------------------------------------

test_that("output paths mirror the input tree without treating indir as a regex", {
  root <- withr::local_tempdir()
  # Every character the old gsub(indir, outdir, .) would have read as a regex.
  indir <- file.path(root, "study(1)+raw.data")
  outdir <- file.path(root, "out")
  dir.create(file.path(indir, "sub"), recursive = TRUE)
  file.create(file.path(indir, c("a.mp4", "b.mp4.backup.mp4")))
  file.create(file.path(indir, "sub", "c.mp4"))

  infiles <- openac:::dir_inputs(indir, "mp4", recursive = TRUE)
  outfiles <- openac:::dir_outputs(infiles, indir, outdir, "wav")

  expect_identical(
    sort(basename(outfiles)),
    c("a.wav", "b.mp4.backup.wav", "c.wav")
  )
  # Only the trailing extension changes: the doubled `.mp4` inside the stem
  # survives, where the old unanchored gsub rewrote every occurrence.
  expect_true(any(endsWith(outfiles, file.path("out", "b.mp4.backup.wav"))))
  # The subdirectory is mirrored under outdir, and every path lands there.
  expect_true(any(endsWith(outfiles, file.path("out", "sub", "c.wav"))))
  expect_true(all(startsWith(outfiles, as.character(fs::path_abs(outdir)))))
  # Nothing was derived back into the input tree.
  expect_false(any(grepl("study(1)+raw.data", outfiles, fixed = TRUE)))
})

test_that("inputs differing only in extension case are refused, not overwritten", {
  # `dir_inputs()` matches the extension case-insensitively, so on a
  # case-sensitive filesystem `clip.mp4` and `clip.MP4` are both inputs and both
  # derive `clip.wav`. Deriving them silently means the batch writes one output
  # twice and one input's result is lost with no warning.
  # The two inputs are handed to `dir_outputs()` directly rather than created on
  # disk: it derives paths lexically, and a case-INsensitive filesystem (APFS,
  # NTFS) cannot hold both names at once, which would make this skip on the
  # majority of developer machines and only ever run on Linux CI.
  root <- withr::local_tempdir()
  indir <- file.path(root, "in")
  dir.create(indir)
  infiles <- file.path(indir, c("clip.mp4", "clip.MP4"))

  expect_error(
    openac:::dir_outputs(infiles, indir, file.path(root, "out"), "wav"),
    class = "openac_output_collision"
  )
  # The message names both colliding inputs, so the caller can rename one.
  expect_error(
    openac:::dir_outputs(infiles, indir, file.path(root, "out"), "wav"),
    "clip\\.MP4"
  )
})

test_that("distinct inputs sharing a stem across subdirectories do not collide", {
  # The guard must key on the derived output path, not the stem: `a/clip.mp4`
  # and `b/clip.mp4` both derive `clip.wav` but under mirrored subdirectories,
  # so they are distinct outputs and must be allowed through.
  root <- withr::local_tempdir()
  indir <- file.path(root, "in")
  dir.create(file.path(indir, "a"), recursive = TRUE)
  dir.create(file.path(indir, "b"), recursive = TRUE)
  file.create(file.path(indir, c("a/clip.mp4", "b/clip.mp4")))

  infiles <- openac:::dir_inputs(indir, "mp4", recursive = TRUE)
  outfiles <- openac:::dir_outputs(infiles, indir, file.path(root, "out"), "wav")

  expect_length(unique(outfiles), 2L)
})

test_that("an input outside indir is refused rather than derived wrongly", {
  root <- withr::local_tempdir()
  indir <- file.path(root, "in")
  dir.create(indir)
  stray <- file.path(root, "stray.mp4")
  file.create(stray)

  expect_error(
    openac:::dir_outputs(stray, indir, file.path(root, "out"), "wav"),
    "under"
  )
})

# --- the wrappers end to end -------------------------------------------------

test_that("of_extract_dir() runs one openface call per matched file", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "faces")
  state <- local_fake_tools(results = list("ok", "ok"))

  result <- of_extract_dir(indir, "mp4", outdir)

  expect_identical(boundary_tools(state), c("openface", "openface"))
  # Two calls, not three: `notes.notmp4` is not an input.
  expect_identical(nrow(result), 2L)
  expect_true(all(result$success))
  expect_identical(
    sort(basename(result$outfile)),
    c("clip.csv", "clip.mp4.backup.csv")
  )
  # Each command names the input and its derived output.
  expect_true(all(file.exists(dirname(result$outfile))))
})

test_that("os_prep_audio_dir() derives .wav outputs under outdir", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "wavs")
  local_fake_tools(results = list("ok", "ok"))

  result <- os_prep_audio_dir(indir, "mp4", outdir)

  expect_identical(
    sort(basename(result$outfile)),
    c("clip.mp4.backup.wav", "clip.wav")
  )
  expect_true(all(startsWith(result$outfile, as.character(fs::path_abs(outdir)))))
})

test_that("aw_prep_audio_dir() mirrors subdirectories under outdir", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "wavs")
  # aw_prep_audio counts streams before converting: two calls per file.
  local_fake_tools(results = rep(list("audio", "ok"), 3))

  result <- aw_prep_audio_dir(indir, "mp4", outdir, recursive = TRUE)

  expect_identical(nrow(result), 3L)
  expect_true(all(result$success))
  expect_true(any(endsWith(result$outfile, file.path("wavs", "mp4", "nested.wav"))))
})

test_that("os_extract_dir() derives a path per requested output kind", {
  indir <- local_input_tree()
  root <- withr::local_tempdir()
  aggdir <- file.path(root, "agg")
  llddir <- file.path(root, "lld")
  # os_extract on a conforming input: two os_check_audio rounds then openSMILE.
  conforming <- list("audio", c("pcm_s16le", "44100", "1"))
  writer <- function(command, args) {
    # Reads the token after each output flag rather than regexing the glued
    # argument string, which no longer exists (M13). `-csvoutput` is matched
    # exactly, so it no longer also matches `-lldcsvoutput` by suffix.
    for (flag in c("-csvoutput", "-lldcsvoutput")) {
      for (path in boundary_value(args, flag)) write_fake_os_output(path)
    }
    "ok"
  }
  local_fake_tools(
    results = rep(c(conforming, conforming, list(writer)), 2)
  )

  result <- os_extract_dir(indir, "mp4", aggdir = aggdir, llddir = llddir)

  expect_identical(nrow(result), 2L)
  expect_true(all(result$success))
  expect_identical(sort(basename(result$aggfile)), c("clip.csv", "clip.mp4.backup.csv"))
  expect_identical(sort(basename(result$lldfile)), c("clip.csv", "clip.mp4.backup.csv"))
  expect_true(all(startsWith(result$aggfile, as.character(fs::path_abs(aggdir)))))
  expect_true(all(startsWith(result$lldfile, as.character(fs::path_abs(llddir)))))
})

test_that("a directory with no matching files yields an empty result", {
  indir <- local_input_tree()
  local_fake_tools(results = list())

  result <- of_extract_dir(indir, "avi", file.path(withr::local_tempdir(), "out"))

  expect_identical(nrow(result), 0L)
  expect_identical(names(result), c("infile", "outfile", "success", "error"))
})

# --- GP6: skip and report ----------------------------------------------------

test_that("of_extract_dir() survives one failing file and reports it", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "faces")
  # list.files() returns sorted names, so `clip.mp4` runs before
  # `clip.mp4.backup.mp4`; the first call is the one made to fail.
  boom <- function(command, args) stop("openface exploded")
  state <- local_fake_tools(results = list(boom, "ok"))

  expect_warning(
    result <- of_extract_dir(indir, "mp4", outdir),
    "openface exploded"
  )

  # The batch ran to the end: the second file was still attempted.
  expect_identical(boundary_tools(state), c("openface", "openface"))
  expect_identical(result$success, c(FALSE, TRUE))
  expect_match(result$error[[1]], "openface exploded")
  expect_true(is.na(result$error[[2]]))
  # The report names the file, so a caller can re-run exactly the failures.
  expect_identical(basename(result$infile[!result$success]), "clip.mp4")
})

test_that("the failure warning names the file that was skipped", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "wavs")
  boom <- function(command, args) stop("ffmpeg exploded")
  local_fake_tools(results = list(boom, "ok"))

  expect_warning(os_prep_audio_dir(indir, "mp4", outdir), "clip\\.mp4")
})

test_that("one unprobeable file among three is a row, not the end of the batch", {
  # The milestone case, pinned to the failure it actually is. MEASURED against
  # the pre-M14 sources: the batch already survived this file, because
  # dir_walk() caught aw_prep_audio()'s abort and recorded the row. What it
  # recorded was the defect --
  #   x | (stream + 1) <= ffp_count_streams(infile)[["Audio"]] is not TRUE
  # -- because a probe that FAILED was parsed as a file with zero audio streams.
  # So the report named neither the file nor the reason, and asserted something
  # false about the input: nothing was ever learned about its streams. The
  # message assertion below is the discriminating one; the success column passes
  # against the old code too.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, c("a.mp4", "b.mp4", "c.mp4")))
  outdir <- file.path(withr::local_tempdir(), "wavs")
  # list.files() sorts, so the queue runs a, b, c. `b` fails its probe and never
  # reaches ffmpeg; the other two spend a probe and a conversion each.
  state <- local_fake_tools(
    results = list("audio", "ok", fake_nonzero_exit(), "audio", "ok")
  )

  warnings <- collect_warnings(
    result <- aw_prep_audio_dir(indir, "mp4", outdir)
  )

  expect_identical(basename(result$infile), c("a.mp4", "b.mp4", "c.mp4"))
  expect_identical(result$success, c(TRUE, FALSE, TRUE))
  expect_match(result$error[[2]], "could not be counted")
  expect_true(all(is.na(result$error[c(1, 3)])))
  # The batch really ran the other two: two conversions, not three and not one.
  expect_identical(
    boundary_tools(state),
    c("ffprobe", "ffmpeg", "ffprobe", "ffprobe", "ffmpeg")
  )
  # And it said so at the time, naming the file it skipped.
  expect_true(any(grepl("b.mp4", warnings, fixed = TRUE)))
})

test_that("KNOWN GAP: two batch tables record a skipped file as a success", {
  # Pinning a wart, not a contract. `aw_prep_audio_dir()` above reports an
  # unprobeable file as a failed row; these two do not, because dir_walk()
  # records a row as failed only on an ERROR -- `aw_transcribe()` skips such a
  # file with a message and returns NULL, and `os_prep_audio()` never counts
  # streams at all and never inspects ffmpeg's exit status.
  #
  # The test exists because NEWS names this limitation to users, and a claim in
  # the changelog needs something that fails when it stops being true. When the
  # ROADMAP candidate for it lands, this test SHOULD red -- update it and the
  # NEWS entry together.
  #
  # BOTH functions NEWS names are exercised, and each is asserted through
  # `dir_walk_reports_failure()` rather than on `success` alone: the candidate
  # offers two routes -- abort, or a third outcome column -- and an assertion on
  # `success` alone stays green under the second while NEWS goes stale.
  indir <- withr::local_tempdir()
  file.create(file.path(indir, "b.mp4"))
  outdir <- file.path(withr::local_tempdir(), "wavs")

  local_fake_tools(results = list(fake_nonzero_exit()))
  suppressWarnings(prep <- os_prep_audio_dir(indir, "mp4", outdir))
  expect_false(dir_walk_reports_failure(prep))

  local_fake_tools(results = list(fake_nonzero_exit()))
  suppressWarnings(suppressMessages(
    transcribed <- aw_transcribe_dir(
      indir, "mp4",
      model = structure(list(name = "tiny"), class = "whisper")
    )
  ))
  expect_false(dir_walk_reports_failure(transcribed))
})

test_that("every file failing still returns a full report rather than erroring", {
  indir <- local_input_tree()
  outdir <- file.path(withr::local_tempdir(), "faces")
  boom <- function(command, args) stop("openface exploded")
  local_fake_tools(results = list(boom, boom))

  suppressWarnings(result <- of_extract_dir(indir, "mp4", outdir))

  expect_identical(nrow(result), 2L)
  expect_false(any(result$success))
  expect_false(any(is.na(result$error)))
})
