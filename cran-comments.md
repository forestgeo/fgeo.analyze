## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

IGNORING
* New submission
* Unknown, possibly mis-spelled, fields in DESCRIPTION: 'Remotes'
* Strong dependencies not in mainstream repositories: fgeo.tool, fgeo.x
* Authors@R field gives persons with non-standard roles:
  Gabriel Arellano <gabriel.arellano.torres@gmail.com> [aut, rev]: rev
  Suzanne Lao <laoz@si.edu> [aut, rev]: rev
  KangMin Ngo <ngokangmin@gmail.com> [rev]: rev
  Haley Overstreet <OverstreetH@si.edu> [rev]: rev
  Sabrina Russo <srusso2@unl.edu> [aut, rev]: rev
  Daniel Zuleta <dfzuleta@gmail.com> [aut, rev]: rev
  
WARNINGS:
* Version contains large components (0.0.0.9003)

ERRORS:
* checking tests ...
  Running ‘spelling.R’
  Running ‘testthat.R’ [21s/21s]
 ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  18: data.frame(result, stringsAsFactors = FALSE)
  19: stop("row names contain missing values")
  
  ── 2. Failure: tt_test can handle data with rows full of NA (@test-tt_test.R#21)
  `tt_test(census, habitat = fgeo.x::habitat)` threw an error.
  Message: row names contain missing values
  Class:   simpleError/error/condition
  
  ══ testthat results  ═══════════════════════════════════════════════════════════
  OK: 306 SKIPPED: 15 FAILED: 2
  1. Error: fgeo_habitat: outputs identical with elevation list or dataframe (@test-fgeo_habitat.R#68) 
  2. Failure: tt_test can handle data with rows full of NA (@test-tt_test.R#21) 

## Downstream dependencies

The package fgeo depends on this package. fgeo is on GitHub -- not on CRAN.
