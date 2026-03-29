# geographically optimal similarity

Computationally optimized function for geographically optimal similarity
(GOS) model

## Usage

``` r
gos(formula, data = NULL, newdata = NULL, kappa = 0.25, cores = 1)
```

## Arguments

- formula:

  A formula of GOS model.

- data:

  A data.frame or tibble of observation data.

- newdata:

  A data.frame or tibble of prediction variables data.

- kappa:

  A numeric value of the percentage of observation locations with high
  similarity to a prediction location. kappa = 1 - tau, where tau is the
  probability parameter in quantile operator. The default kappa is 0.25,
  meaning that 25% of observations with high similarity to a prediction
  location are used for modelling.

- cores:

  positive integer(default is 1). If cores \> 1, a 'parallel' package
  cluster with that many cores is created and used. You can also supply
  a cluster object.

## Value

A tibble made up of predictions and uncertainties.

## References

Song, Y. (2022). Geographically Optimal Similarity. Mathematical
Geosciences. doi: 10.1007/s11004-022-10036-8.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
data(zn)
data(grid)
g = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
data = zn, newdata = grid, kappa = 0.08,cores = 6)
g
} # }
```
