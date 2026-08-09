# openac (development version)

* `install_openface_win()` was silently installing an OpenFace that cannot
  track faces. The four model files it downloads separately from the main
  release came from links that had stopped serving the models and started
  serving a sign-in page instead — and because that page arrives as a
  successful download, the installer wrote four web pages where the models
  belong and reported success. **If you installed OpenFace through openac,
  re-run `install_openface_win()`.** The models now come from the same
  location the OpenFace project itself uses first, and a download that is too
  small or is a web page is refused with a message naming the link, instead of
  being accepted. A failed install now reports every missing model at once
  rather than stopping at the first.

* `install_opensmile_win()` could never have worked: it asked for a file name
  the openSMILE 3.0.2 release has never carried, and the download failed with a
  "not found" error. It now asks for the file that release does carry.

* File paths containing a `$` are now handled correctly. Previously a file such
  as `my $clip.mp4` was passed to ffmpeg, ffprobe, OpenFace or openSMILE with
  the `$clip` part removed, so the tool was asked for a file that did not
  exist. Paths containing spaces were already handled and are unaffected. An
  earlier version of this note warned that a Windows path containing a token
  such as `%TEMP%` could still be expanded; that turned out not to be so. Such
  paths — and ones containing `&`, `^` or `!` — were run against the real tools
  on Windows and reached them unchanged, because openac starts a tool directly
  rather than through a command interpreter.

* `ffmpeg()`, `ffprobe()`, `openface()` and `opensmile()` now also accept a
  character vector giving one command-line argument per element, and quote each
  element for you. Passing a single string still works exactly as before, with
  quoting left to you; the vector form is preferred and is what the rest of
  openac now uses internally.

* The batch functions match `inext` regardless of case, so `inext = "mp4"` also
  takes `.MP4` files. This is now documented, and it can no longer cost you a
  result: where two inputs differing only in extension case would have derived
  the same output file, the batch previously wrote that file twice and reported
  success for both. It now stops before any tool runs, naming the files
  involved.

* The batch functions `os_prep_audio_dir()`, `os_extract_dir()`,
  `of_extract_dir()`, `aw_prep_audio_dir()` and `aw_transcribe_dir()` no longer
  abort the whole run when one file fails. The failing file is skipped with a
  warning naming it and the reason, and the batch continues.

* Those functions now return (invisibly) a data frame with one row per input
  file — the paths it was called with, whether it succeeded, and the error
  message if it did not — so a failed file can be found and re-run. They
  previously returned `NULL` or the vector of input paths.

* `os_prep_audio_dir()`, `os_extract_dir()` and `of_extract_dir()` now derive
  output paths correctly. An input directory whose name contains a regular
  expression character (such as `(`, `+` or `.`) previously produced output
  paths inside the *input* directory rather than under the output directory,
  and a file such as `clip.mp4.backup.mp4` had every occurrence of the
  extension replaced, yielding `clip.wav.backup.wav`.

* Those same functions no longer treat a file merely ending in the extension's
  letters (`notes.notmp4` for `inext = "mp4"`) as an input.

* The platform-specific installers now check which platform they are running on
  before doing anything. `install_ffmpeg_win()`, `install_openface_win()` and
  `install_opensmile_win()` fail with an error unless they are run on Windows,
  and `install_opensmile_mac()` unless it is run on macOS; previously none of
  them checked, and the download and extraction ran regardless. The error points
  at the installer for the platform you are on, or reports that openac has no
  automated installer for that tool there.

* The low-level wrappers `ffmpeg()`, `ffprobe()`, `openface()` and `opensmile()`
  (and their aliases `ffm()`, `ffp()`, `of()` and `os()`) now fail with an error
  naming the program when it cannot be found, instead of the low-level error
  that previously surfaced from path resolution.

* `find_program()` (and `find_ffmpeg()`, `find_ffprobe()`, `find_openface()` and
  `find_opensmile()`) now warn and return `NULL` when a program cannot be found,
  instead of failing with an error. `check_ffmpeg()`, `check_ffprobe()`,
  `check_openface()` and
  `check_opensmile()` correspondingly return `FALSE` in that case.

* `find_program()` now resolves a location recorded by `set_program()` even when
  that location is a bare program name found on the `PATH`.

* The documented return value of `set_program()` now matches what it returns.

# openac 0.1.0

First public release. openac wraps open-source affective-computing tools —
ffmpeg/ffprobe, OpenFace, openSMILE, and Whisper (via audio.whisper) — behind
one consistent R interface: tool discovery/installation, audio/video
preparation, single-file and batch (`_dir`) extraction with parallelism
(future/furrr) and progress (progressr), and tidy readers for the tool outputs.

Reading tool outputs into tidy tibbles:

* `os_read()` reads openSMILE CSV output — either an aggregate (functionals)
  file or a per-frame low-level descriptor (LLD) file — into a tidy tibble,
  auto-detecting the delimiter and preserving openSMILE's feature names.
* `of_read()` reads an OpenFace output CSV into a tidy tibble (one row per
  detected face per frame), trimming the whitespace OpenFace pads its column
  headers with.
* `aw_read()` turns a Whisper transcription — the object from `aw_transcribe()`,
  or its `.rds`/`.csv` output — into a tidy tibble with one row per segment,
  parsing the `HH:MM:SS.mmm` timestamps to numeric seconds and keeping a
  `speaker` column for diarized transcripts.
