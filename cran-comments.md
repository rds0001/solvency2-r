## Local release preparation

Local check on 2026-10-06: R 4.1.2, x86_64 Linux, UTF-8. Source build,
installation, all runnable help examples and the vignette passed. R CMD check
with --as-cran --no-manual --no-tests: 0 errors, 0 warnings, 2 notes (installed
size and inability to verify the current time in the offline environment).
Tests were executed separately to avoid duplicate full-suite work: 12,382
calculation cases, 227 integration portfolios and reporting fixture/interface
tests, with focused reruns after the packaging and native-helper corrections.
Additional cross-language trace sample: 712 passed; reporting component audit:
203 passed. Native-helper correction: 29 reference cases passed and an
independent recursive analytic case passed. Integration/input rerun: 1,752
expectations, no failures or errors. This is not a current-R or cross-platform
CRAN submission result.

The built archive was also installed into an isolated local library and checked
outside the checkout: all 432 catalogue exports and 12 documentation topics
available; recursive analytic regression passed; a multi-template reporting
specimen with 106 facts passed all 28 independent expected rule checks.

This package is prepared for local source-package and GitHub testing. No CRAN
submission has been made. The repository workflow covers R release, oldrel-1
and devel on Linux, and release on Windows and macOS once pushed and run.

The compressed regulatory resources are necessary for complete offline access
to the reference's parameter, catastrophe and reporting corpus. They preserve
source hashes and independent synthetic reference cases. The installed data
footprint exceeds the usual 5 MB recommendation for this reason.

Before a CRAN submission: assign the authorised natural-person maintainer,
record results on then-current R release/devel and Win-builder or R-hub, check
reverse dependencies, and replace this preparation note with the actual results.
The company remains author and copyright holder; contact riskdatascience@web.de.
