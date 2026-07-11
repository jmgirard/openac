# openac (development version)

* New `os_read()` reads openSMILE CSV output — either an aggregate
  (functionals) file or a per-frame low-level descriptor (LLD) file — into a
  tidy tibble, auto-detecting the delimiter and preserving openSMILE's
  feature names.
