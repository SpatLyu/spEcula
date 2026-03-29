# function for the best kappa parameter

Computationally optimized function for determining the best kappa
parameter for the optimal similarity

## Usage

``` r
gos_bestkappa(formula,data = NULL,kappa=seq(0.05,1,0.05),
              nrepeat = 10,nsplit = 0.5,cores = 1)
```

## Arguments

- formula:

  A formula of GOS model

- data:

  A data.frame or tible of observation data

- kappa:

  (optional)A numeric vector of the optional percentages of observation
  locations with high similarity to a prediction location. kappa = 1 -
  tau, where tau is the probability parameter in quantile operator.
  kappa = 0.25 means that 25% of observations with high similarity to a
  prediction location are used for modelling.

- nrepeat:

  (optional)A numeric value of the number of cross-validation training
  times. The default value is 10.

- nsplit:

  (optional)The sample training set segmentation ratio,which in `(0,1)`,
  default is `0.5`.

- cores:

  positive integer(default is 1). If cores \> 1, a 'parallel' package
  cluster with that many cores is created and used. You can also supply
  a cluster object.

## Value

A list of the result of the best kappa and the computation process
curve.

## References

Song, Y. (2022). Geographically Optimal Similarity. Mathematical
Geosciences. doi: 10.1007/s11004-022-10036-8.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ggplot2)
library(ggrepel)
data(zn)
data(grid)
system.time({
  b1 = gos_bestkappa(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
                     data = zn,kappa = c(0.01, 0.05, 0.1, 0.2, 0.5, 1),
                     nrepeat = 2,cores = 1)
})
b1$bestkappa
b1$plot
} # }
```
