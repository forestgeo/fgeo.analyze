## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

1.1.1.9000

WARNING
New submission
Version contains large components (1.1.1.9000)
Strong dependencies not in mainstream repositories:
  fgeo.tool
Suggests or Enhances not in mainstream repositories:
  fgeo.x
Availability using Additional_repositories specification:
  fgeo.tool   yes   https://forestgeo.github.io/drat/
  fgeo.x      yes   https://forestgeo.github.io/drat/

NOTE
Examples with CPU or elapsed time > 5s
              user system elapsed
tt_test      2.000  0.004   5.274
fgeo_habitat 1.896  0.060   5.320
## Downstream dependencies

I checked 1 reverse dependencies (0 from CRAN + 1 from <https://forestgeo.github.io/drat/>; summary at <https://github.com/forestgeo/fgeo.analyze/tree/master/revdep>):

 * I saw 0 new problems
 * I failed to check 0 packages
