## Test environments

* local: windows 10 x64 (R 3.5.2)
* travis: 
  * mac os x 10.13.3 (R 3.5.2)
  * ubuntu 14.04 (R 3.2, 3.3, oldrel, release, devel)
* win-builder (R devel and release)

## R CMD check results

* using R version 3.4.4 (2018-03-15)
* using platform: x86_64-pc-linux-gnu (64-bit)
* this is package ‘fgeo.analyze’ version ‘1.1.3’

Status: 2 NOTEs
* NOTE 1 raises if the package is built using R v3.5. (<https://ropensci.github.io/dev_guide/building.html#authorship>).
* NOTE 2 is safe to ignore.

NOTE 1
* Authors@R field gives persons with non-standard roles:
* Gabriel Arellano [aut, rev]: rev
* Suzanne Lao [aut, rev]: rev
* KangMin Ngo [rev]: rev
* Haley Overstreet [rev]: rev
* Sabrina Russo [aut, rev]: rev
* Daniel Zuleta [aut, rev]: rev

NOTE 2
* New submission
* Strong dependencies not in mainstream repositories:
* fgeo.tool
* Suggests or Enhances not in mainstream repositories:
* fgeo.x
* Availability using Additional_repositories specification:
* fgeo.tool yes https://forestgeo.github.io/drat/
* fgeo.x yes https://forestgeo.github.io/drat/

## Downstream dependencies

I checked 1 reverse dependencies (0 from CRAN + 1 from <https://forestgeo.github.io/drat/>; summary at <https://github.com/forestgeo/fgeo.analyze/tree/master/revdep>):

 * I saw 0 new problems
 * I failed to check 0 packages
