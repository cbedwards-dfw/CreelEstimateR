
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CreelEstimateR

<!-- badges: start -->
<!-- badges: end -->

The goal of CreelEstimateR is to …

This package is an adaptation of…

## Dev notes:

- Will need to update documentation
- Many functions include `...` with no apparent use. I suspect these
  reflect additional necessary arguments that have not been officially
  included in the function, like “params”?
- When using tidyverse tools in packages, `select`-type functions need
  to have all column names surrounded by quotes. `mutate`-type functions
  need to have `.data$` prepended to column names. I have attempted
  this, but may have mixed up some cases. Debugging may be necessary.

## Progress:

Some build cleanup will only be possible iteratively.

We should discuss which functions need to be exported. I haven’t reached
it yet, but there may a single “control” function that is called, and
which calls most/all the others?

### Initial conversion:

From <https://github.com/wdfw-fp/CreelEstimates/tree/main/R_functions>

- json_conversion.R: 3 issues.
- confirm_db_upload
- est_pe_catch
- est_pe_catch
- establish_db_con
- export_estimates
- fetch_db_table
- fetch_dwg
- fit_bss
- generate_analysis_lut
- get_bss_catch_daily
- get_bss_cpue_daily
- get_bss_effort_daily
- get_bss_overview
- map_data_grade
- plot_paired_census_index
- plot_est_pe_catch
- plot_est_pe_effort
- plot_inputs_pe_census_vs_index
- plot_inputs_pe_cpue_period
- prep_days
- prep_dwg_census_expan
- prep_dwg_effort_census
- prep_dwg_effort_index
- prep_dwg_interview_angler_types
- prep_dwg_interview_fishing_time
- prep_export

### finalizing documentation:

- 

## Installation

You can install the development version of CreelEstimateR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cbedwards-dfw/CreelEstimateR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CreelEstimateR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
