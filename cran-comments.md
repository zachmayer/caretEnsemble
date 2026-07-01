## Test environments
* local M2 mac, R 4.6.0
* R CMD CHECK github actions: mac, windows (R-release)
* R CMD CHECK github actions: linux (R-release, R-devel, R-oldrelease)
* win-builder (R-release, R-devel, R-oldrelease)
* rhub (R-release, linux)
* reverse dependencies via revdepcheck

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## revdepcheck results
We checked 8 reverse dependencies (6 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Changes
This release fixes a test that errored on a CRAN check machine where the suggested package 'klaR' was unavailable; the test now skips when its suggested dependencies are absent.

New features:
* Keep per-fold resamples in `caretStack`/`caretList` instead of aggregating them.
* Include original raw-data features in the stack.
* `plot_variable_importance()` for `caretStack`/`caretEnsemble` models.
