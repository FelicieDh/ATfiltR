
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ATfiltR

<!-- badges: start -->
<!-- badges: end -->

ATfiltR compiles your acoustic telemetry detection files, removes
duplicates (and saves them elsewhere), attributes animal ID to
detections (and save tags that don’t belong to you elsewhere), removes
detections outside of the deployment period, removes ghost detections
via a “solitary detection” filter (customizable) and a swimming speed
filter.

## Installation

You can install the development version of ATfiltR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FelicieDh/ATfiltR")
```
