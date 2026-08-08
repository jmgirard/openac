# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# It IS modified, deliberately and in one place: the Sys.setenv() below. The
# command-contract gate can only decide coverage over a COMPLETE run, and this
# file is the only thing in the package that can honestly say a run is complete
# -- it is the entry point `R CMD check` and CI take, and the `test_check()`
# call below is unfiltered. A local `devtools::test()` never sources this file,
# so an interactive filtered run stays undeclared and the gate skips instead of
# failing. Removing the line does not disarm the gate: an undeclared full run
# still enforces. It only turns an INCOMPLETE run under `R CMD check` from a
# failure back into a skip, which is the silent-disarm this exists to prevent.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(openac)

Sys.setenv(OPENAC_FULL_SUITE = "true")

test_check("openac")
