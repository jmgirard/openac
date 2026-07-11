# openac 0.0.0.9000

* New `os_read()` reads openSMILE CSV output — either an aggregate
  (functionals) file or a per-frame low-level descriptor (LLD) file — into a
  tidy tibble, auto-detecting the delimiter and preserving openSMILE's
  feature names.
* New `of_read()` reads an OpenFace output CSV into a tidy tibble (one row per
  frame), trimming the whitespace OpenFace pads its column headers with.
