# opensmile --------------------------------------------------------------------

#' Low-level access to the opensmile command line interface
#'
#' Attempt to find and run opensmile with the specified arguments.
#'
#' @param arg (character) The arguments to append to the SMILEextract
#'   command line call, in either of two forms. Give a **character vector**
#'   with one CLI token per element and each element is quoted for you at the
#'   process boundary, so a file path may contain spaces or a `$` --- and, on
#'   Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`. None of those are
#'   expanded, because openac starts the tool directly rather than through a
#'   command interpreter. Give a **single string** and
#'   it is passed through exactly as written, quoting and all, which leaves any
#'   quoting up to you. Prefer the vector form.
#' @return A character vector containing the output of opensmile. Errors if
#'   opensmile cannot be found.
#' @references https://audeering.github.io/opensmile/
#' @aliases os
#' @export
#' @examples
#' \dontrun{
#' opensmile('-h')
#' opensmile(c("-C", "my config.conf", "-I", "in.wav"))
#' }
#'
opensmile <- function(arg) {
  run_tool("opensmile", arg)
}


# os ---------------------------------------------------------------------------

#' @rdname opensmile
#' @export
os <- opensmile


# os_list_configs --------------------------------------------------------------

#' List openSMILE configuration files
#'
#' Return a list of all configuration (.config) files found in the openSMILE
#' installation folder.
#'
#' @return A character vector containing the configuration files found.
#' @export
#' @examples
#' \dontrun{
#' os_list_configs()
#' }
#'
os_list_configs <- function() {
  # Find opensmile install directory. `require_program()` rather than
  # `find_opensmile()`: the latter WARNS and returns NULL when openSMILE cannot
  # be resolved, and `dirname(NULL)` then dies on base R's "a character vector
  # argument expected" -- raised before `os_check_config()`, whose message this
  # feeds, could say anything. Since M19 that abort happens pre-flight, so it
  # was the whole batch's death with a message about a character vector
  # (M19 review round 1, F6).
  fd <- dirname(require_program("opensmile"))
  # Find all config files
  configs <- list.files(
    path = file.path(fd, "..", "config"),
    pattern = ".conf$",
    full.names = FALSE,
    recursive = TRUE
  )
  # Strip away file extensions
  tools::file_path_sans_ext(configs)
}


# os_check_config --------------------------------------------------------------

#' Resolve an openSMILE config name to an absolute path
#'
#' Check that a named openSMILE configuration exists in the installed openSMILE
#' `config/` directory and return its absolute path. Errors if the config is
#' not found.
#'
#' @param config A required string naming an openSMILE config, with or without
#'   the `.conf` extension (e.g. `"egemaps/v02/eGeMAPSv02"`).
#' @return A string giving the absolute path to the matching `.conf` file.
#' @examples
#' \dontrun{
#' os_check_config("egemaps/v02/eGeMAPSv02")
#' }
#' @export
os_check_config <- function(config) {
  # Validate input. This guard names no file, and cannot: `config` is a
  # batch-wide argument rather than an input, and since M19 `os_extract_dir()`
  # resolves it BEFORE `dir_walk()` is entered, so an unresolvable config aborts
  # the batch with no row to carry a message. What it owes instead is the value
  # it could not resolve -- the old message named neither the config nor where
  # to look, so a typo in one of `os_list_configs()`'s several dozen names cost
  # a batch and told the user nothing about which name was wrong.
  if (!rlang::is_string(config)) {
    cli::cli_abort(
      "{.arg config} must be a single string, not {.obj_type_friendly {config}}."
    )
  }
  # Strip away file extensions
  config_sans <- tools::file_path_sans_ext(config)
  configs_sans <- tools::file_path_sans_ext(os_list_configs())
  if (config_sans %in% configs_sans == FALSE) {
    cli::cli_abort(
      c(
        "Can't find the openSMILE config {.val {config}}.",
        "i" = "{.run openac::os_list_configs()} lists the configs this
               openSMILE installation carries."
      ),
      class = "openac_config_not_found"
    )
  }
  # Get absolute path to config
  fd <- dirname(require_program("opensmile"))
  config <- file.path(fd, "..", "config", paste0(config_sans, ".conf"))
  tools::file_path_as_absolute(config)
}


# os_check_audio ---------------------------------------------------------------

#' Check if an audio file is ready for analysis by openSMILE
#'
#' Check if an audio file has the proper format for openSMILE, i.e., the
#' pcm_s16le audio codec and 1 audio channel.
#'
#' @param infile A required string indicating the filepath of the audio file to
#'   check.
#' @param verbose An optional logical indicating whether to print warnings.
#' @return A logical indicating whether `infile` is ready for openSMILE
#' @export
os_check_audio <- function(infile, verbose = FALSE) {
  # Validate input
  check_file_arg(infile)
  if (!file.exists(infile)) {
    abort_file(infile, "No file exists at {.file {guarded_path}}.")
  }
  if (!rlang::is_bool(verbose)) {
    abort_file(infile, "{.arg verbose} must be {.code TRUE} or {.code FALSE},
                        not {.obj_type_friendly {verbose}}.")
  }
  # Count streams
  streams <- ffp_count_streams(infile)
  # A file ffprobe could not read cannot be checked, and every test below would
  # be NA rather than a logical. Return before the second query, which would
  # fail on that same file. ffp_count_streams() has already warned naming it, so
  # this second message is verbose-gated like the sibling warning below.
  if (anyNA(streams)) {
    if (verbose) {
      cli::cli_warn(c(
        "!" = "Could not count the streams in {.file {basename(infile)}}",
        "i" = "Returning FALSE."
      ))
    }
    return(FALSE)
  }
  # Create ffprobe command
  arg <- c(
    "-v", "error",
    "-show_entries", "stream=codec_name,sample_rate,channels",
    "-of", "default=noprint_wrappers=1:nokey=1",
    infile
  )
  # Run ffprobe command
  dat <- ffprobe(arg)
  # Validate ffprobe output. The sibling `aw_check_audio()` has always had this
  # branch; without it `dat[[3]]` below indexed a short answer straight into
  # base R's "subscript out of bounds" (M19 review round 1, F3).
  if (length(dat) < 3) {
    if (verbose) {
      cli::cli_warn(c(
        "!" = "No audio stream found in {.file {basename(infile)}}",
        "i" = "Returning FALSE."
      ))
    }
    return(FALSE)
  }
  # Check ffprobe output
  tests <- c(
    No_Video = streams["Video"] == 0,
    One_Stream = streams["Audio"] == 1,
    Right_Codec = dat[[1]] == "pcm_s16le",
    One_Channel = dat[[3]] == "1"
  )
  # If verbose, state the result
  if (verbose) {
    print(tests)
    if (!isTRUE(dat[[2]] == "44100")) {
      cli::cli_warn("A sampling rate of 44100 is recommended.")
    }
  }
  # Return single logical -- `isTRUE()`, never a bare `all()`. A field ffprobe
  # left blank makes its test NA and `all()` NA with it, and this function's
  # callers ask `if (!os_check_audio(x))`, which dies on `missing value where
  # TRUE/FALSE needed` naming no file. The contract is one logical, and a check
  # that could not be answered is not a pass -- the same disposition the
  # unreadable-streams branch above already takes (M19 review round 1, F3).
  isTRUE(all(tests))
}


# os_prep_audio ----------------------------------------------------------------

#' Prepare an audio stream for analysis by opensmile
#'
#' Import an audio or video file and export an audio file for acoustic analysis.
#' Extract the audio stream specified by `stream` and then transcode it to a
#' mono (i.e., single channel) 16-bit PCM .wav file at 44.1kHz sampling rate.
#'
#' @param infile (character) What is the filepath of the audio or video file
#'   to import?
#' @param outfile (character) What is the filepath of the .wav file to create?
#' @param stream (numeric, default=0) The index of the audio stream to extract
#' (ffmpeg uses zero-indexing so 0 is the first stream).
#' @param overwrite Should outfile be overwritten if it already exists? It will
#'   be skipped otherwise, silently for a direct call. In a batch the row
#'   depends on whose job the preparing is: under `os_prep_audio_dir()` it is
#'   the whole job, so the row reads `"skipped"`; under `os_extract_dir()` the
#'   existing file is reused and openSMILE still runs, so the row reads
#'   `"ok"`. Defaults to TRUE.
#' @return A character vector containing the output of ffmpeg. Errors, naming
#'   the file, if ffmpeg exits non-zero.
#' @export
#'
os_prep_audio <- function(infile, outfile, stream = 0, overwrite = TRUE) {
  # Validate input
  check_file_arg(infile)
  if (!file.exists(infile)) {
    abort_file(infile, "No file exists at {.file {guarded_path}}.")
  }
  if (!rlang::is_string(outfile)) {
    abort_file(infile, "{.arg outfile} must be a single file path,
                        not {.obj_type_friendly {outfile}}.")
  }
  # `is.na()` before the comparison: `is_integerish(NA_integer_, n = 1)` is
  # TRUE, so a TYPED missing value passed the first test and left `NA < 0` for
  # `if` to die on, naming no file (M19 review round 1, F1). A bare `NA` is
  # logical and never got that far, which is why only the typed one showed.
  if (!rlang::is_integerish(stream, n = 1) || is.na(stream) || stream < 0) {
    abort_file(infile, "{.arg stream} must be a single whole number
                        {.code >= 0}, not {.val {stream}}.")
  }
  if (!rlang::is_bool(overwrite)) {
    abort_file(infile, "{.arg overwrite} must be {.code TRUE} or {.code FALSE},
                        not {.obj_type_friendly {overwrite}}.")
  }
  # Return early if overwrite is TRUE and outfile exists. The skip is SIGNALLED
  # as well as returned (M18): a direct caller sees the same "Skipped" it always
  # did, while a `*_dir()` batch records the row as skipped rather than as work
  # it did not do.
  if (overwrite == FALSE && file.exists(outfile)) {
    skip_file(paste0(
      basename(outfile), " already exists and overwrite = FALSE."
    ))
    return("Skipped")
  }
  # Create outfile directory if needed
  if (!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }
  # Construct ffmpeg command
  arg <- c(
    "-y",
    "-i", infile,
    "-map", paste0("0:a:", stream), # one token: ffmpeg takes the value joined
    "-ar", "44100", # set sample rate to 44.1kHz
    "-ac", "1", # set to mono audio (1 channel)
    "-c:a", "pcm_s16le", # set to 16-bit PCM Little-Endian codec
    outfile
  )
  # Run ffmpeg command, failing the file if ffmpeg does (M17)
  run_checked("ffmpeg", arg, infile)
}


# os_prep_audio_dir ------------------------------------------------------------

#' Run os_prep_audio() on multiple files in a directory
#'
#' Find all media files with a specified extension in a specified directory and
#' then extract an audio file for acoustic analysis from each.
#'
#' Can be optionally run in parallel by running \code{\link[future]{plan}()}
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#'
#' Can optionally output a progress bar by using
#' \code{\link[progressr]{handlers}()} beforehand, e.g., by calling
#' `handlers("cli"); handlers(global = TRUE)`.
#'
#' @param indir (string) What directory contains the input files?
#' @param inext (string) What file extension should be looked for in `indir`
#'   (e.g., "mp4" or "mp3")? Matched regardless of case, so "mp4" also takes
#'   `.MP4` files; if that leaves two inputs deriving the same output file, the
#'   batch is refused rather than one silently overwriting the other.
#' @param outdir (string) What directory should the audio files be output to?
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @inheritDotParams os_prep_audio stream overwrite
#' @return (Invisibly) a data frame with one row per input file, giving the
#'   `infile` and `outfile` it was called with, its `status`, whether it
#'   `success`ed, and the `error` message if it did not. `status` is one of
#'   `"ok"` (the operation completed), `"skipped"` (the file was deliberately
#'   not processed) or `"failed"` (the operation raised an error); `success` is
#'   `status == "ok"`, so a skipped file reads `FALSE`, and `error` carries the
#'   reason for a skipped file as well as for a failed one. A file that fails
#'   does not abort the batch: it is warned about, recorded as `"failed"`, and
#'   the remaining files still run.
#' @export
#'
os_prep_audio_dir <- function(
  indir,
  inext,
  outdir,
  recursive = FALSE,
  ...
) {
  # Validate input
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(rlang::is_string(outdir))
  stopifnot(rlang::is_bool(recursive))
  # Find input filenames and derive matching output paths
  infiles <- dir_inputs(indir, inext, recursive)
  df <- data.frame(
    infile = as.character(fs::path_abs(infiles)),
    outfile = dir_outputs(infiles, indir, outdir, "wav"),
    stringsAsFactors = FALSE
  )
  # Iterate os_prep_audio() over infiles, surviving per-file failures
  invisible(dir_walk(df, function(infile, outfile) {
    os_prep_audio(infile, outfile, ...)
  }))
}


# os_extract -------------------------------------------------------------------

#' Extract opensmile features
#'
#' Extract openSMILE acoustic features from an audio file based on a config
#' file. Lower level descriptors (LLDs) will be calculated per frame and then
#' summarized into an aggregate (AGG) file.
#'
#' @param infile (character) What is the filepath for the input file to be
#' analyzed? The proper format can be created by `os_prep_audio()`.
#' @param wavfile (character, default=NULL) Either NULL or a string indicating
#' the path to save the prepared version of `infile` to (must end with '.wav').
#' If NULL, a temporary file will be created and later discarded.
#' @param aggfile (character, default=NULL) What is the filepath to write the
#' AGG output to? If `NULL`, the AGG output will not be saved. Note that either
#' `aggfile` or `lldfile` (or both) must be non-NULL.
#' @param lldfile (character, default=NULL) What is the filepath to write the
#' LLD output to? If `NULL`, the LLD output will not be saved. Note that either
#' `aggfile` or `lldfile` (or both) must be non-NULL.
#' @param config (character, default="misc/emo_large") Which configuration file
#' should be used to analyze `infile`? A list of available config files can be
#' generated using `os_list_configs()`.
#' @inheritDotParams os_prep_audio stream overwrite
#' @return A character vector including opensmile output. Errors, naming the
#'   file, if openSMILE exits non-zero.
#' @export
#'
os_extract <- function(
  infile,
  wavfile = NULL,
  aggfile = NULL,
  lldfile = NULL,
  config = "misc/emo_large",
  ...
) {
  # Input validation will be handled by subfunctions
  # Preallocate temp
  temp <- FALSE
  if (os_check_audio(infile) == FALSE) {
    # If no wavfile provided, create tempfile
    if (is.null(wavfile)) {
      temp <- TRUE
      wavfile <- tempfile(fileext = ".wav")
    }
    # Prepare audio stream as wavfile/tempfile. A skip here -- an
    # `overwrite = FALSE` wav this batch already prepared -- is the fast path
    # into the extraction below, never a reason to abandon the file, so it is
    # absorbed rather than left to reach `dir_walk()`'s exiting handler.
    x <- absorb_skip(os_prep_audio(
      infile = infile,
      outfile = wavfile,
      ...
    ))
  } else {
    wavfile <- infile
  }
  # Extract features from prepared audio file
  out <- os_extract_wav(
    infile = wavfile,
    aggfile = aggfile,
    lldfile = lldfile,
    config = config,
    source = infile
  )
  # Clean up temporary file if created
  if (temp) unlink(wavfile)
  # Return the output from opensmile
  out
}


# os_extract_wav ------------------------------------------------------------

# `source` is the file to NAME in a failure message, which is not always the
# file openSMILE is handed. `os_extract()` converts a non-conforming input to a
# `tempfile()` and passes that as `infile`, so a message built from `infile`
# names a temp path that no longer exists and that the user never chose -- and
# it is `os_extract_dir()`'s `error` column the NEWS entry tells them to read
# and re-run from. Defaults to `infile` for a direct call, where the two are the
# same file (M17 review, finding B).
os_extract_wav <- function(
  infile,
  aggfile = NULL,
  lldfile = NULL,
  config = "misc/emo_large",
  source = infile
) {
  # Validate inputs. The missing-`infile` branch splits on whether openac
  # derived `infile` itself, because the two are different failures with the
  # same symptom. When `source` differs, `infile` is the wav `os_prep_audio()`
  # was asked to write and ffmpeg returned success without writing -- an
  # absence openac caused, and the batch row says so rather than reporting a
  # missing file about a temp path the user never chose. When they are the
  # same file, the user handed openSMILE a path with nothing at it.
  check_file_arg(infile)
  check_file_arg(source)
  if (!file.exists(infile)) {
    if (identical(infile, source)) {
      abort_file(source, "No file exists at {.file {guarded_path}}.")
    }
    abort_file(source, "ffmpeg wrote no output at {.file {infile}}.")
  }
  if (!os_check_audio(infile)) {
    abort_file(source, "It is not the mono 16-bit PCM audio openSMILE reads;
                        {.fn os_prep_audio} converts it.")
  }
  if (!is.null(aggfile) &&
      !(rlang::is_string(aggfile) && tools::file_ext(aggfile) == "csv")) {
    abort_file(source, "{.arg aggfile} must be {.code NULL} or a single
                        {.field .csv} path, not {.val {aggfile}}.")
  }
  if (!is.null(lldfile) &&
      !(rlang::is_string(lldfile) && tools::file_ext(lldfile) == "csv")) {
    abort_file(source, "{.arg lldfile} must be {.code NULL} or a single
                        {.field .csv} path, not {.val {lldfile}}.")
  }
  config <- os_check_config(config)
  # Create output directories if necessary
  if (!is.null(aggfile) && !dir.exists(dirname(aggfile))) {
    dir.create(dirname(aggfile), recursive = TRUE)
  }
  if (!is.null(lldfile) && !dir.exists(dirname(lldfile))) {
    dir.create(dirname(lldfile), recursive = TRUE)
  }
  # Construct opensmile command
  arg <- c(
    "-C", config,
    "-I", infile,
    opt_arg(!is.null(aggfile), "-csvoutput", aggfile),
    opt_arg(!is.null(lldfile), "-lldcsvoutput", lldfile),
    "-instname", basename(infile)
  )
  # Run opensmile command, failing the file if openSMILE does (M17)
  out <- run_checked("opensmile", arg, source)
  # Fix the output CSV files
  if (!is.null(aggfile)) {
    os_fix_csv(aggfile)
  }
  if (!is.null(lldfile)) {
    os_fix_csv(lldfile)
  }
  # Return opensmile output
  out
}


# os_extract_dir ---------------------------------------------------------------

#' Run os_extract() on multiple files in a directory
#'
#' Find all .wav files in a specified directory and then extract opensmile
#' features from each (according to `config`).
#'
#' Can be optionally run in parallel by running \code{\link[future]{plan}()}
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#'
#' Can optionally output a progress bar by using
#' \code{\link[progressr]{handlers}()} beforehand, e.g., by calling
#' `handlers("cli"); handlers(global = TRUE)`.
#'
#' @param indir (character) What directory contains the input .wav files?
#' @param inext (character) What file extension to look for in `indir`? Matched
#'   regardless of case, so "mp4" also takes `.MP4` files; if that leaves two
#'   inputs deriving the same output file, the batch is refused rather than one
#'   silently overwriting the other.
#' @param wavdir (character, default=NULL) What directory should the prepared
#' WAV audio files be saved to? If `NULL`, temporary WAV files will be created
#' and then discarded (if needed).
#' @param aggdir (character, default=NULL) What directory should the AGG output
#' files be saved to? If `NULL`, AGG files will not be output. Note that
#' `aggdir` or `llddir` (or both) must be non-NULL.
#' @param llddir (character, default=NULL) What directory should the LLD output
#' files be saved to? If `NULL`, LLD files will not be output. Note that
#' `aggdir` or `llddir` (or both) must be non-NULL.
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @inheritDotParams os_extract config
#' @inheritDotParams os_prep_audio stream overwrite
#' @return (Invisibly) a data frame with one row per input file, giving the
#'   paths it was called with, its `status`, whether it
#'   `success`ed, and the `error` message if it did not. `status` is one of
#'   `"ok"` (the operation completed), `"skipped"` (the file was deliberately
#'   not processed) or `"failed"` (the operation raised an error); `success` is
#'   `status == "ok"`, so a skipped file reads `FALSE`, and `error` carries the
#'   reason for a skipped file as well as for a failed one. A file that fails
#'   does not abort the batch: it is warned about, recorded as `"failed"`, and
#'   the remaining files still run. A `config` that cannot be resolved is the
#'   exception, and is not a per-file outcome: it is wrong for every input, so
#'   it errors before any file is touched, naming the config, and no table is
#'   returned.
#' @export
#'
os_extract_dir <- function(
  indir,
  inext,
  wavdir = NULL,
  aggdir = NULL,
  llddir = NULL,
  recursive = FALSE,
  ...
) {
  # Validate inputs
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(is.null(wavdir) || rlang::is_string(wavdir))
  stopifnot(is.null(aggdir) || rlang::is_string(aggdir))
  stopifnot(is.null(llddir) || rlang::is_string(llddir))
  stopifnot(!is.null(aggdir) || !is.null(llddir))
  stopifnot(rlang::is_bool(recursive))
  extra_args <- list(...)
  # Resolve `config` ONCE, here, rather than once per file inside the loop.
  # `config` is batch-wide: a typo in it is wrong for every input, so the
  # per-file form spent a full `os_check_audio()` round -- two ffprobe calls --
  # on each of N files before failing each of them identically, and returned a
  # table of N failed rows where the truth is one bad argument. Pre-flight it
  # aborts before `dir_walk()` is entered, with nothing run.
  #
  # The default is read from `os_extract()`'s own signature rather than repeated
  # here, because the commonest call of all supplies no `config` at all: a check
  # reading only `...` would validate nothing precisely when the caller relied
  # on the default, and a second copy of the literal could drift from it.
  # `...` is forwarded through `do.call()`, which matches names PARTIALLY: a
  # caller writing `conf =` reaches `os_extract()`'s `config`, while an exact
  # read of `extra_args$config` saw nothing and pre-flighted the default. The
  # abbreviation is resolved here the same way the call below will resolve it,
  # so the check and the call read one argument (M19 review round 1, F5).
  extra_args <- match_formals(extra_args, os_extract)
  # SUPPLIED, not non-NULL. `config = NULL` is a value the caller chose and it
  # is wrong; testing `is.null()` made it indistinguishable from `config` absent,
  # so the pre-flight validated the default and every file then failed inside
  # the loop with a message naming no file (M19 review round 1, F12).
  config <- if ("config" %in% names(extra_args)) {
    extra_args$config
  } else {
    eval(formals(os_extract)$config)
  }
  os_check_config(config)
  # Find input filepaths
  infiles <- dir_inputs(indir, inext, recursive)
  # Construct iteration data frame
  df <- data.frame(
    infile = as.character(fs::path_abs(infiles)),
    stringsAsFactors = FALSE
  )
  # If saving prepared WAV files...
  if (!is.null(wavdir)) {
    df$wavfile <- dir_outputs(infiles, indir, wavdir, "wav")
  }
  # If exporting AGG output...
  if (!is.null(aggdir)) {
    df$aggfile <- dir_outputs(infiles, indir, aggdir, "csv")
  }
  # If exporting LLD output...
  if (!is.null(llddir)) {
    df$lldfile <- dir_outputs(infiles, indir, llddir, "csv")
  }
  # Iterate os_extract() over infiles, surviving per-file failures
  invisible(dir_walk(df, function(...) {
    do.call(what = os_extract, args = c(list(...), extra_args))
  }))
}


# os_fix_csv -------------------------------------------------------------------

# Rewrite an openSMILE output CSV from its native ';' delimiter to ','.
#
# The missing-input guard ATTRIBUTES the absence, which it can only do because
# of who calls it: `os_extract_wav()` calls it on an `aggfile`/`lldfile` it has
# just handed openSMILE as `-csvoutput` / `-lldcsvoutput`, and on nothing else.
# So a file that is not there is openSMILE having written nothing there, and
# saying so is what turns a batch row from "a path does not exist" into
# something the user can act on. Adding a caller that does not have openSMILE
# write the file first makes that attribution false.
#
# The guard goes through `abort_file()` like every other one in the batch path.
# It used to build its own `cli::cli_abort()`, which is how it kept the hard
# line break and the bullet glyph the shared helper had already removed
# everywhere else -- a guard that opts out of the helper opts out of its fixes
# (M19 review round 2, F1).
os_fix_csv <- function(infile) {
  # Validate input
  check_file_arg(infile)
  if (!file.exists(infile)) {
    abort_file(
      infile,
      "openSMILE wrote no output at {.file {guarded_path}}."
    )
  }
  # Read in opensmile output in original format
  df <- read.csv(file = infile, sep = ";", dec = ".")
  # Write out opensmile output in traditional format
  write.csv(df, file = infile, row.names = FALSE)
}


# os_read ----------------------------------------------------------------------

#' Read openSMILE output into a tidy tibble
#'
#' Read an openSMILE output CSV --- either an aggregate/functionals file
#' (from `-csvoutput`, one row) or a low-level descriptor file (from
#' `-lldcsvoutput`, one row per frame) --- into a wide
#' [tibble][tibble::tibble]. There is one row per observation and one column
#' per feature, alongside the openSMILE metadata columns (`name`, and
#' `frameTime` for LLD output).
#'
#' The delimiter is detected automatically, so both the native
#' semicolon-delimited output openSMILE writes and the comma-delimited form
#' produced by `os_fix_csv()` are accepted. Feature names are preserved
#' verbatim, including non-syntactic names such as `pcm_fftMag_mfcc[1]`.
#'
#' @param file (character) Path to an openSMILE output CSV, as written by
#' [os_extract()] (its `aggfile` or `lldfile`).
#' @return A [tibble][tibble::tibble] with one row per observation and one
#' column per openSMILE metadata field and feature.
#' @seealso [os_extract()], which produces the output files.
#' @examples
#' \dontrun{
#' os_extract("audio.wav", aggfile = "agg.csv", lldfile = "lld.csv")
#' agg <- os_read("agg.csv")
#' lld <- os_read("lld.csv")
#' }
#' @export
os_read <- function(file) {
  # Validate input
  if (!rlang::is_string(file)) {
    cli::cli_abort(
      "{.arg file} must be a single string, not {.obj_type_friendly {file}}."
    )
  }
  if (!file.exists(file)) {
    cli::cli_abort("Can't find the file {.file {file}}.")
  }
  header <- readLines(file, n = 1L, warn = FALSE)
  if (length(header) == 0L || !nzchar(header)) {
    cli::cli_abort("The file {.file {file}} is empty.")
  }
  # openSMILE writes ';'-delimited output; os_fix_csv() rewrites it to ','.
  sep <- if (grepl(";", header, fixed = TRUE)) ";" else ","
  df <- read.csv(
    file = file,
    sep = sep,
    dec = ".",
    quote = "\"",          # double-quote only (read.csv default); adding the
                           # single quote would make an unquoted instance name
                           # containing an apostrophe swallow the rest of the file
    check.names = FALSE,   # keep feature names like pcm_fftMag_mfcc[1]
    stringsAsFactors = FALSE
  )
  tibble::as_tibble(df)
}
