
<!-- README.md is generated from README.Rmd. Please edit that file -->
# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Analyze ForestGEO data

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![CRAN status](https://www.r-pkg.org/badges/version/fgeo.analyze)](https://cran.r-project.org/package=fgeo.analyze) [![R-CMD-check](https://github.com/forestgeo/fgeo.analyze/workflows/R-CMD-check/badge.svg)](https://github.com/forestgeo/fgeo.analyze/actions) [![Codecov test coverage](https://codecov.io/gh/forestgeo/fgeo.analyze/branch/master/graph/badge.svg)](https://codecov.io/gh/forestgeo/fgeo.analyze?branch=master) <!-- badges: end -->

**fgeo.analyze** provides functions to analyze ForestGEO data.

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

Or [install all **fgeo** packages in one step](https://forestgeo.github.io/fgeo/index.html#installation).

## Example

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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

Your data may have multiple stems per treeid and even multiple measures per stemid (if trees have buttresses).

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

Fundamentally, `abundance()` counts rows. All of these results are the same:

``` r
nrow(vft)
#> [1] 4
count(vft)
#> <print(tibble::tibble())>
summarize(vft, n = n())
#> <print(tibble::tibble())>
abundance(vft)
#> Warning: `treeid`: Duplicated values were detected. Do you need to pick main
#> stems?
#> <print(tibble::tibble())>
```

But that result is likely not what you expect. Instead, you likely expect this:

``` r
summarize(vft, n = n_distinct(TreeID))
#> <print(tibble::tibble())>
```

As shown above, you can get a correct result by combining `summarize()` and `n_distinct()` (from the **dplyr** package). But `abundance()` includes some useful additional features (see `?abundance()`). This code conveys your intention more clearly, i.e. to calculate tree abundance by counting the number of main stems:

``` r
(main_stems <- pick_main_stem(vft))
#> Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
#> Please use the `.add` argument instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
#> <print(tibble::tibble())>
abundance(main_stems)
#> <print(tibble::tibble())>
```

If you have data from multiple censuses, then you can compute by census (or any other group).

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
#> <print(tibble::tibble())>
abundance(main_stems_by_census)
#> <print(tibble::tibble())>
```

Often you will need to first subset data (e.g. by `status` or `DBH`) and then count.

``` r
over20 <- filter(main_stems_by_census, DBH > 20)
abundance(over20)
#> <print(tibble::tibble())>
```

### Basal area

If trees have buttresses, then you may need to pick the main stemid of each stem so you do not count the same stem more than once.

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
#> <print(tibble::tibble())>
main_stemids
#> <print(tibble::tibble())>
basal_area(main_stemids)
#> Warning: `stemid`: Duplicated values were detected. Do you need to pick largest
#> `hom` values?
#> Warning: `censusid`: Multiple values were detected. Do you need to group by
#> censusid?
#> <print(tibble::tibble())>
```

`basal_area()` also allows you to compute by groups.

``` r
by_census <- group_by(main_stemids, CensusID)
basal_area(by_census)
#> <print(tibble::tibble())>
```

But if you want to compute on a subset of data, then you need to pick the data first.

``` r
ten_to_twenty <- filter(by_census, DBH >= 10, DBH <= 20)
basal_area(ten_to_twenty)
#> <print(tibble::tibble())>
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
#> <print(tibble::tibble())>
```

Abundance by year.

``` r
abundance_byyr(vft, DBH >= 10, DBH < 20)
#> <print(tibble::tibble())>
abundance_byyr(vft, DBH >= 10)
#> <print(tibble::tibble())>
```

Basal area by year.

``` r
basal_area_byyr(vft, DBH >= 10)
#> <print(tibble::tibble())>
```

### Demography

``` r
census1 <- fgeo.x::tree5
census2 <- fgeo.x::tree6
```

Demography functions output a list that you can convert to a more convenient dataframe with `as_tibble()`.

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
#> <print(tibble::tibble())>
```

Except if you use `split2`: This argument creates a complex data structure that `as_tibble()` cannot handle.

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
#> Error:   Can't deal with data created with `split2` (deprecated).
#>   * Bad: `split1 = x1, split2 = x2`
#>   * Good: `split1 = interaction(x1, x2)`
```

Instead, pass the multiple grouping variables to `split` via `interaction()`. This approach allows you to use any number of grouping variables and the output always works with `as_tibble()`.

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
#> <print(tibble::tibble())>
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
#> <print(tibble::tibble())>
```

A simple way to separate the grouping variables is with `tidyr::separate()`.

``` r
growth <- growth_ctfs(
  census1, census2, 
  split1 = by_sp_and_quadrat, 
  quiet = TRUE
)
as_tibble(growth)
#> <print(tibble::tibble())>

as_tibble(growth) %>% 
  tidyr::separate(groups, into = c("species", "quadrats"))
#> <print(tibble::tibble())>
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
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1 N.Hab.2
#> CASARB      35     1313      282        5              0       0.820625      24
#>        Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2 N.Hab.3
#> CASARB      394     1204        2              0        0.24625      11
#>        Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3 N.Hab.4
#> CASARB      482     1114        4              0        0.30125       8
#>        Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> CASARB     1217      377        6              0       0.760625
#> 
#> [[2]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1 N.Hab.2
#> PREMON      94     1005      594        1              0       0.628125      97
#>        Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2 N.Hab.3
#> PREMON     1478      120        2              0        0.92375      39
#>        Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3 N.Hab.4
#> PREMON      230     1367        3              0        0.14375      15
#>        Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> PREMON      130     1465        5              0        0.08125
#> 
#> [[3]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1 N.Hab.2
#> SLOBER      21      270     1328        2              0        0.16875      25
#>        Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2 N.Hab.3
#> SLOBER      516     1082        2              0         0.3225      21
#>        Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3 N.Hab.4
#> SLOBER     1336      260        4              0          0.835       8
#>        Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> SLOBER     1193      396       11              0       0.745625

# A dataframe
as_tibble(tt_test_result)
#> <print(tibble::tibble())>

# A simple summary to help you interpret the results
summary(tt_test_result)
#> <print(tibble::tibble())>
```

[Get started with **fgeo**](https://forestgeo.github.io/fgeo/)

## Information

-   [Getting help](https://forestgeo.github.io/fgeo.analyze/SUPPORT.html).
-   [Contributing](https://forestgeo.github.io/fgeo.analyze/CONTRIBUTING.html).
-   [Contributor Code of Conduct](https://forestgeo.github.io/fgeo.analyze/CODE_OF_CONDUCT.html).
