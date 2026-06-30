## Test environments
* local M2 mac, R 4.6.0
* R CMD CHECK github actions: mac, windows (R-release)
* R CMD CHECK github actions: linux (R-release, R-devel, R-oldrelease)
* win-builder (R-release, R-devel, R-oldrelease)
* rhub (R-release, linux)
* reverse dependencies via revdepcheck

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Changes
This release fixes a test that errored on a CRAN check machine where the suggested package 'klaR' was unavailable; the test now skips when its suggested dependencies are absent. New features are listed in NEWS.md.
