
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Analyze ForestGEO data

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.analyze.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.analyze)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.analyze/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.analyze?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.analyze)](https://cran.r-project.org/package=fgeo.analyze)
<!-- badges: end -->

**fgeo.analyze** provides functions to analyze
[ForestGEO](http://www.forestgeo.si.edu/) data.

## Installation

Install the latest stable version of **fgeo.analyze** from CRAN with:

``` r
install.packages("fgeo.analyze", repos = these_repos)
```

Install the development version of **fgeo.analyze** with:

``` r
# install.packages("devtools")
devtools::install_github("forestgeo/fgeo.analyze")
```

Or [install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation).

## Example

``` r
library(fgeo.x)
library(fgeo.tool)
#> 
#> Attaching package: 'fgeo.tool'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(fgeo.analyze)
```

### Abundance

Your data may have multiple stems per treeid and even multiple measures
per stemid (if trees have
buttresses).

``` r
# Trees with buttresses may have multiple measurements of a single stem. 
# Main stems have highest `HOM`, then largest `DBH`.
vft <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,  # Main stem
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,  # Main stem
)
```

Fundamentally, `abundance()` counts rows. All of these results are the
same:

``` r
nrow(vft)
#> [1] 4
count(vft)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
summarize(vft, n = n())
#> Warning: Calling `n()` without importing or prefixing it is deprecated, use `dplyr::n()`.
#> This warning is displayed once per session.
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
abundance(vft)
#> Warning: `treeid`: Duplicated values were detected. Do you need to pick
#> main stems?
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     4
```

But that result is likely not what you expect. Instead, you likely
expect this:

``` r
summarize(vft, n = n_distinct(TreeID))
#> Warning: Calling `n_distinct()` without importing or prefixing it is deprecated, use `dplyr::n_distinct()`.
#> This warning is displayed once per session.
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2
```

As shown above, you can get a correct result by combining `summarize()`
and `n_distinct()` (from the **dplyr** package). But `abundance()`
includes some useful additional features (see `?abundance()`). This code
conveys your intention more clearly, i.e. to calculate tree abundance by
counting the number of main stems:

``` r
(main_stems <- pick_main_stem(vft))
#> # A tibble: 2 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.2       30   130
abundance(main_stems)
#> # A tibble: 1 x 1
#>       n
#>   <int>
#> 1     2
```

If you have data from multiple censuses, then you can compute by census
(or any other group).

``` r
vft2 <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   10,  130,
          1,     "1",   "1.2",   20,  130,  # Main stem
          2,     "1",   "1.1",   12,  130,
          2,     "1",   "1.2",   22,  130   # Main stem
)
by_census <- group_by(vft2, CensusID)
(main_stems_by_census <- pick_main_stem(by_census))
#> # A tibble: 2 x 5
#> # Groups:   CensusID [2]
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.2       20   130
#> 2        2 1      1.2       22   130
abundance(main_stems_by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID     n
#>      <dbl> <int>
#> 1        1     1
#> 2        2     1
```

Often you will need to first subset data (e.g. by `status` or `DBH`) and
then count.

``` r
over20 <- filter(main_stems_by_census, DBH > 20)
abundance(over20)
#> # A tibble: 1 x 2
#> # Groups:   CensusID [1]
#>   CensusID     n
#>      <dbl> <int>
#> 1        2     1
```

### Basal area

If trees have buttresses, then you may need to pick the main stemid of
each stem so you do not count the same stem more than once.

``` r
vft3 <- tribble(
  ~CensusID, ~TreeID, ~StemID, ~DBH, ~HOM,
          1,     "1",   "1.1",   88,  130,
          1,     "1",   "1.1",   10,  160,  # Main stem
          1,     "2",   "2.1",   20,  130,
          1,     "2",   "2.2",   30,  130,  # Main stem
          2,     "1",   "1.1",   98,  130,
          2,     "1",   "1.1",   20,  160,  # Main stem
          2,     "2",   "2.1",   30,  130,
          2,     "2",   "2.2",   40,  130,  # Main stem
)
(main_stemids <- pick_main_stemid(vft3))
#> # A tibble: 6 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.1       20   130
#> 3        1 2      2.2       30   130
#> 4        2 1      1.1       20   160
#> 5        2 2      2.1       30   130
#> 6        2 2      2.2       40   130
main_stemids
#> # A tibble: 6 x 5
#>   CensusID TreeID StemID   DBH   HOM
#>      <dbl> <chr>  <chr>  <dbl> <dbl>
#> 1        1 1      1.1       10   160
#> 2        1 2      2.1       20   130
#> 3        1 2      2.2       30   130
#> 4        2 1      1.1       20   160
#> 5        2 2      2.1       30   130
#> 6        2 2      2.2       40   130
basal_area(main_stemids)
#> Warning: `stemid`: Duplicated values were detected. Do you need to pick
#> largest `hom` values?
#> Warning: `censusid`: Multiple values were detected. Do you need to group by
#> censusid?
#> # A tibble: 1 x 1
#>   basal_area
#>        <dbl>
#> 1      3377.
```

`basal_area()` also allows you to compute by groups.

``` r
by_census <- group_by(main_stemids, CensusID)
basal_area(by_census)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1      1100.
#> 2        2      2278.
```

But if you want to compute on a subset of data, then you need to pick
the data first.

``` r
ten_to_twenty <- filter(by_census, DBH >= 10, DBH <= 20)
basal_area(ten_to_twenty)
#> # A tibble: 2 x 2
#> # Groups:   CensusID [2]
#>   CensusID basal_area
#>      <dbl>      <dbl>
#> 1        1       393.
#> 2        2       314.
```

### Abundance and basal area aggregated by year

Example data.

``` r
vft <- tibble(
  PlotName = c("luq", "luq", "luq", "luq", "luq", "luq", "luq", "luq"),
  CensusID = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
  TreeID = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
  StemID = c(1.1, 1.2, 2.1, 2.2, 1.1, 1.2, 2.1, 2.2),
  Status = c("alive", "dead", "alive", "alive", "alive", "gone",
    "dead", "dead"),
  DBH = c(10L, NA, 20L, 30L, 20L, NA, NA, NA),
  Genus = c("Gn", "Gn", "Gn", "Gn", "Gn", "Gn", "Gn", "Gn"),
  SpeciesName = c("spp", "spp", "spp", "spp", "spp", "spp", "spp", "spp"),
  ExactDate = c("2001-01-01", "2001-01-01", "2001-01-01", "2001-01-01",
    "2002-01-01", "2002-01-01", "2002-01-01",
    "2002-01-01"),
  PlotCensusNumber = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
  Family = c("f", "f", "f", "f", "f", "f", "f", "f"),
  Tag = c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L),
  HOM = c(130L, 130L, 130L, 130L, 130L, 130L, 130L, 130L)
)

vft
#> # A tibble: 8 x 13
#>   PlotName CensusID TreeID StemID Status   DBH Genus SpeciesName ExactDate
#>   <chr>       <int>  <int>  <dbl> <chr>  <int> <chr> <chr>       <chr>    
#> 1 luq             1      1    1.1 alive     10 Gn    spp         2001-01-…
#> 2 luq             1      1    1.2 dead      NA Gn    spp         2001-01-…
#> 3 luq             1      2    2.1 alive     20 Gn    spp         2001-01-…
#> 4 luq             1      2    2.2 alive     30 Gn    spp         2001-01-…
#> 5 luq             2      1    1.1 alive     20 Gn    spp         2002-01-…
#> 6 luq             2      1    1.2 gone      NA Gn    spp         2002-01-…
#> 7 luq             2      2    2.1 dead      NA Gn    spp         2002-01-…
#> 8 luq             2      2    2.2 dead      NA Gn    spp         2002-01-…
#> # … with 4 more variables: PlotCensusNumber <int>, Family <chr>,
#> #   Tag <int>, HOM <int>
```

Abundance by year.

``` r
abundance_byyr(vft, DBH >= 10, DBH < 20)
#> # A tibble: 1 x 3
#>   species family yr_2001
#>   <chr>   <chr>    <dbl>
#> 1 Gn spp  f            1
abundance_byyr(vft, DBH >= 10)
#> # A tibble: 1 x 4
#>   species family yr_2001 yr_2002
#>   <chr>   <chr>    <dbl>   <dbl>
#> 1 Gn spp  f            2       1
```

Basal area by year.

``` r
basal_area_byyr(vft, DBH >= 10)
#> # A tibble: 1 x 4
#>   species family yr_2001 yr_2002
#>   <chr>   <chr>    <dbl>   <dbl>
#> 1 Gn spp  f        1100.    314.
```

### Demography

``` r
census1 <- fgeo.x::tree5
census2 <- fgeo.x::tree6
```

Demography functions output a list that you can convert to a more
convenient dataframe with `as_tibble()`.

``` r
recruitment_ctfs(census1, census2)
#> Detected dbh ranges:
#>   * `census1` = 10.9-323.
#>   * `census2` = 10.5-347.
#> Using dbh `mindbh = 0` and above.
#> $N2
#> [1] 29
#> 
#> $R
#> [1] 3
#> 
#> $rate
#> [1] 0.02413113
#> 
#> $lower
#> [1] 0.0084585
#> 
#> $upper
#> [1] 0.06812388
#> 
#> $time
#> [1] 4.525246
#> 
#> $date1
#> [1] 18937.96
#> 
#> $date2
#> [1] 20600.72

as_tibble(
  recruitment_ctfs(census1, census2, quiet = TRUE)
)
#> # A tibble: 1 x 8
#>      N2     R   rate   lower  upper  time  date1  date2
#>   <dbl> <dbl>  <dbl>   <dbl>  <dbl> <dbl>  <dbl>  <dbl>
#> 1    29     3 0.0241 0.00846 0.0681  4.53 18938. 20601.
```

Except if you use `split2`: This argument creates a complex data
structure that `as_tibble()` cannot handle.

``` r
# Errs
as_tibble(
  recruitment_ctfs(
    census1, census2, 
    split1 = census1$sp, 
    split2 = census1$quadrat,  # `as_tibble()` can't handle this
    quiet = TRUE
  )
)
#> Warning: `split2` is deprecated.
#> * Bad: `split1 = x1, split2 = x2`
#> * Good: `split1 = interaction(x1, x2)`
#> This warning is displayed once per session.
#>   Can't deal with data created with `split2` (deprecated).
#>   * Bad: `split1 = x1, split2 = x2`
#>   * Good: `split1 = interaction(x1, x2)`
```

Instead, pass the multiple grouping variables to `split` via
`interaction()`. This approach allows you to use any number of grouping
variables and the output always works with `as_tibble()`.

``` r
# Recommended
by_sp_and_quadrat <- interaction(census1$sp, census1$quadrat)

as_tibble(
  recruitment_ctfs(
    census1, census2, 
    split1 = by_sp_and_quadrat, 
    quiet = TRUE
  )
)
#> # A tibble: 540 x 9
#>    groups         N2     R  rate lower upper  time date1 date2
#>    <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 MATDOM.1007     1     0     0     0 0.410  4.50 18891 20535
#>  2 CASSYL.1010     1     0     0     0 0.411  4.49 18914 20555
#>  3 SLOBER.110      1     0     0     0 0.409  4.51 18897 20543
#>  4 SLOBER.1106     1     0     0     0 0.404  4.56 18849 20516
#>  5 CECSCH.1114     1     0     0     0 0.413  4.47 18948 20580
#>  6 PSYBRA.1318     1     0     0     0 0.412  4.48 19011 20646
#>  7 HIRRUG.1403     1     0     0     0 0.403  4.58 18834 20506
#>  8 CASSYL.1411     1     0     0     0 0.414  4.45 18931 20558
#>  9 SLOBER.1414     1     0     0     0 0.403  4.57 18952 20622
#> 10 GUAGUI.1419     1     0     0     0 0.406  4.54 19012 20670
#> # … with 530 more rows
```

The same applies for other demography functions.

``` r
as_tibble(
  mortality_ctfs(
    census1, census2, 
    split1 = by_sp_and_quadrat, 
    quiet = TRUE
  )
)
#> # A tibble: 540 x 10
#>    groups          N     D  rate lower upper  time date1 date2 dbhmean
#>    <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
#>  1 MATDOM.1007     1     0     0     0 0.410  4.50 18891 20535   240  
#>  2 CASSYL.1010     1     0     0     0 0.411  4.49 18914 20555    67  
#>  3 SLOBER.110      1     0     0     0 0.409  4.51 18897 20543   150  
#>  4 SLOBER.1106     1     0     0     0 0.404  4.56 18849 20516    50  
#>  5 CECSCH.1114     1     0     0     0 0.413  4.47 18948 20580   228  
#>  6 PSYBRA.1318     1     0     0     0 0.412  4.48 19011 20646    14  
#>  7 HIRRUG.1403     1     0     0     0 0.403  4.58 18834 20506    12.9
#>  8 CASSYL.1411     1     0     0     0 0.414  4.45 18931 20558    13.1
#>  9 SLOBER.1414     1     0     0     0 0.403  4.57 18952 20622    16.6
#> 10 GUAGUI.1419     1     0     0     0 0.406  4.54 19012 20670   108  
#> # … with 530 more rows
```

A simple way to separate the grouping variables is with
`tidyr::separate()`.

``` r
growth <- growth_ctfs(
  census1, census2, 
  split1 = by_sp_and_quadrat, 
  quiet = TRUE
)
as_tibble(growth)
#> # A tibble: 540 x 8
#>    groups        rate     N  clim dbhmean  time date1 date2
#>    <chr>        <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>
#>  1 MATDOM.1007  0         1    NA   240    4.50 18891 20535
#>  2 CASSYL.1010  0.445     1    NA    67    4.49 18914 20555
#>  3 SLOBER.110   0.666     1    NA   150    4.51 18897 20543
#>  4 SLOBER.1106  0         1    NA    50    4.56 18849 20516
#>  5 CECSCH.1114  1.79      1    NA   228    4.47 18948 20580
#>  6 PSYBRA.1318  0.447     1    NA    14    4.48 19011 20646
#>  7 HIRRUG.1403  1.66      1    NA    12.9  4.58 18834 20506
#>  8 CASSYL.1411 NA         0    NA    NA   NA       NA    NA
#>  9 SLOBER.1414  1.40      1    NA    16.6  4.57 18952 20622
#> 10 GUAGUI.1419 NA         0    NA    NA   NA       NA    NA
#> # … with 530 more rows

as_tibble(growth) %>% 
  tidyr::separate(groups, into = c("species", "quadrats"))
#> # A tibble: 540 x 9
#>    species quadrats   rate     N  clim dbhmean  time date1 date2
#>    <chr>   <chr>     <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>
#>  1 MATDOM  1007      0         1    NA   240    4.50 18891 20535
#>  2 CASSYL  1010      0.445     1    NA    67    4.49 18914 20555
#>  3 SLOBER  110       0.666     1    NA   150    4.51 18897 20543
#>  4 SLOBER  1106      0         1    NA    50    4.56 18849 20516
#>  5 CECSCH  1114      1.79      1    NA   228    4.47 18948 20580
#>  6 PSYBRA  1318      0.447     1    NA    14    4.48 19011 20646
#>  7 HIRRUG  1403      1.66      1    NA    12.9  4.58 18834 20506
#>  8 CASSYL  1411     NA         0    NA    NA   NA       NA    NA
#>  9 SLOBER  1414      1.40      1    NA    16.6  4.57 18952 20622
#> 10 GUAGUI  1419     NA         0    NA    NA   NA       NA    NA
#> # … with 530 more rows
```

### Species-habitat associations

``` r
# Pick alive trees, of 10 mm or more
tree <- download_data("luquillo_tree5_random")
census <- filter(tree, status == "A", dbh >= 10)
# Pick sufficiently abundant species
pick <- filter(add_count(census, sp), n > 50)

# Use your habitat data or create it from elevation data
elevation <- download_data("luquillo_elevation")
habitat <- fgeo_habitat(elevation, gridsize = 20, n = 4)

tt_test_result <- tt_test(pick, habitat)
#> Using `plotdim = c(320, 500)`. To change this value see `?tt_test()`.
#> Using `gridsize = 20`. To change this value see `?tt_test()`.
#> Warning: Is `census` a tree table (not a stem table)? See `?tt_test()`.

# A list or matrices
tt_test_result
#> [[1]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> CASARB      35     1313      282        5              0       0.820625
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> CASARB      24      394     1204        2              0        0.24625
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> CASARB      11      482     1114        4              0        0.30125
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> CASARB       8     1217      377        6              0       0.760625
#> 
#> [[2]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> PREMON      94     1005      594        1              0       0.628125
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> PREMON      97     1478      120        2              0        0.92375
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> PREMON      39      230     1367        3              0        0.14375
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> PREMON      15      130     1465        5              0        0.08125
#> 
#> [[3]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> SLOBER      21      270     1328        2              0        0.16875
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> SLOBER      25      516     1082        2              0         0.3225
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> SLOBER      21     1336      260        4              0          0.835
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> SLOBER       8     1193      396       11              0       0.745625

# A dataframe
as_tibble(tt_test_result)
#> # A tibble: 12 x 8
#>    habitat sp     N.Hab Gr.Hab Ls.Hab Eq.Hab Rep.Agg.Neut Obs.Quantile
#>  * <chr>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>        <dbl>        <dbl>
#>  1 1       CASARB    35   1313    282      5            0       0.821 
#>  2 2       CASARB    24    394   1204      2            0       0.246 
#>  3 3       CASARB    11    482   1114      4            0       0.301 
#>  4 4       CASARB     8   1217    377      6            0       0.761 
#>  5 1       PREMON    94   1005    594      1            0       0.628 
#>  6 2       PREMON    97   1478    120      2            0       0.924 
#>  7 3       PREMON    39    230   1367      3            0       0.144 
#>  8 4       PREMON    15    130   1465      5            0       0.0812
#>  9 1       SLOBER    21    270   1328      2            0       0.169 
#> 10 2       SLOBER    25    516   1082      2            0       0.322 
#> 11 3       SLOBER    21   1336    260      4            0       0.835 
#> 12 4       SLOBER     8   1193    396     11            0       0.746

# A simple summary to help you interpret the results
summary(tt_test_result)
#> # A tibble: 12 x 3
#>    sp     habitat association
#>    <chr>  <chr>   <chr>      
#>  1 CASARB 1       neutral    
#>  2 CASARB 2       neutral    
#>  3 CASARB 3       neutral    
#>  4 CASARB 4       neutral    
#>  5 PREMON 1       neutral    
#>  6 PREMON 2       neutral    
#>  7 PREMON 3       neutral    
#>  8 PREMON 4       neutral    
#>  9 SLOBER 1       neutral    
#> 10 SLOBER 2       neutral    
#> 11 SLOBER 3       neutral    
#> 12 SLOBER 4       neutral
```

[Get started with **fgeo**](https://forestgeo.github.io/fgeo)

## Information

  - [Getting
    help](https://forestgeo.github.io/fgeo.analyze/SUPPORT.html).
  - [Contributing](https://forestgeo.github.io/fgeo.analyze/CONTRIBUTING.html).
  - [Contributor Code of
    Conduct](https://forestgeo.github.io/fgeo.analyze/CODE_OF_CONDUCT.html).
