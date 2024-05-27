
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spEcula <img src="man/figures/logo.png" align="right" height="140"/>

<!-- badges: start -->
<!-- badges: end -->

The goal of **spEcula** is to make it easier to use R for spatial
speculation based on *spatial dependence*, *spatial stratification
heterogeneity* and *geographical configuration similarity*.

## Installation

You can install the development version of `spEcula` like so:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/spEcula")
```

## Example

### Computationally optimized geographically optimal similarity (GOS) model

`geosimilarity` package has achieved `gos` model,but when data is
larger,`geosimilarity` may be slow. I develop the parallelized `gos`
model in `spEcula`,which can assing the `cores` argument in `gos()`
function.

``` r
library(spEcula)

system.time({
  g1 = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
           data = zn, newdata = grid, kappa = 0.08,cores = 6)
})
##    user  system elapsed 
##     0.0     0.0     3.1
```

``` r

g1
## # A tibble: 13,132 × 7
##       pred uncertainty90 uncertainty95 uncertainty99 uncertainty99.5
##      <dbl>         <dbl>         <dbl>         <dbl>           <dbl>
##  1 3.83e50        0.0785        0.0510       0.0280          0.0238 
##  2 3.86e50        0.0517        0.0349       0.00984         0.00919
##  3 3.88e50        0.0675        0.0416       0.0215          0.0143 
##  4 4.00e50        0.0651        0.0560       0.0134          0.00769
##  5 4.06e50        0.0740        0.0443       0.0176          0.0137 
##  6 3.93e50        0.0700        0.0465       0.0196          0.0165 
##  7 4.03e50        0.0445        0.0335       0.0179          0.0173 
##  8 3.90e50        0.0480        0.0424       0.0220          0.0114 
##  9 3.88e50        0.0428        0.0427       0.0180          0.00993
## 10 3.94e50        0.0214        0.0214       0.0177          0.0137 
## # ℹ 13,122 more rows
## # ℹ 2 more variables: uncertainty99.9 <dbl>, uncertainty100 <dbl>
```
