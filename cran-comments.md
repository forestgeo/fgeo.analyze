## Test environments

* local windows 10 x64, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

1.1.2

WARNING
New submission
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


ERROR
Running examples in 'fgeo.analyze-Ex.R' failed
The error most likely occurred in:
```R
> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: abundance
> ### Title: Abundance and basal area, optionally by groups.
> ### Aliases: abundance basal_area
> 
> ### ** Examples
> 
> library(fgeo.tool)

Attaching package: 'fgeo.tool'

The following object is masked from 'package:stats':

    filter

> 
> # abundance() -------------------------------------------------------------
> 
> abundance(data.frame(1))
Error in n() : could not find function "n"
Calls: abundance ... .summary -> summarize -> summarise.tbl_df -> summarise_impl
Execution halted
** running examples for arch 'x64' ... ERROR
Running examples in 'fgeo.analyze-Ex.R' failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: abundance
> ### Title: Abundance and basal area, optionally by groups.
> ### Aliases: abundance basal_area
> 
> ### ** Examples
> 
> library(fgeo.tool)

Attaching package: 'fgeo.tool'

The following object is masked from 'package:stats':

    filter

> 
> # abundance() -------------------------------------------------------------
> 
> abundance(data.frame(1))
Error in n() : could not find function "n"
Calls: abundance ... .summary -> summarize -> summarise.tbl_df -> summarise_impl
Execution halted
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
** running tests for arch 'i386' ... [21s] OK
  Running 'spelling.R' [0s]
  Running 'testthat.R' [20s]
** running tests for arch 'x64' ... [23s] OK
  Running 'spelling.R' [0s]
  Running 'testthat.R' [22s]
* checking PDF version of manual ... OK
* DONE
Status: 2 ERRORs, 1 WARNING
```



## Downstream dependencies

I checked 1 reverse dependencies (0 from CRAN + 1 from <https://forestgeo.github.io/drat/>; summary at <https://github.com/forestgeo/fgeo.analyze/tree/master/revdep>):

 * I saw 0 new problems
 * I failed to check 0 packages
