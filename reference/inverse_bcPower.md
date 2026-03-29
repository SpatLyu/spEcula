# Inverse transform of `car::bcPower`

Inverse transform of
[`car::bcPower`](https://rdrr.io/pkg/car/man/bcPower.html)

## Usage

``` r
inverse_bcPower(z, alpha)
```

## Arguments

- z:

  A numeric vector to be inverse transformed.

- alpha:

  Power transformation parameter,which can be got from
  [`car::powerTransform()`](https://rdrr.io/pkg/car/man/powerTransform.html).

## Value

A numeric vector.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
library(car)
#> Loading required package: carData
library(moments)
turbidity = c(1.0, 1.2, 1.1, 1.1, 2.4, 2.2, 2.6, 4.1, 5.0, 10.0, 4.0, 4.1, 4.2, 4.1,
             5.1, 4.5, 5.0, 15.2, 10.0, 20.0, 1.1, 1.1, 1.2, 1.6, 2.2, 3.0, 4.0, 10.5)
moments::skewness(turbidity)
#> [1] 1.916622
shapiro.test(turbidity)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  turbidity
#> W = 0.74633, p-value = 1.383e-05
#> 
lambdapt = car::powerTransform(turbidity)
ttur = car::bcPower(turbidity,lambdapt$lambda)
moments::skewness(ttur)
#> [1] 0.04576271
shapiro.test(ttur)
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  ttur
#> W = 0.93361, p-value = 0.07607
#> 
inverse_bcPower(ttur,lambdapt$lambda)
#>  [1]  1.0  1.2  1.1  1.1  2.4  2.2  2.6  4.1  5.0 10.0  4.0  4.1  4.2  4.1  5.1
#> [16]  4.5  5.0 15.2 10.0 20.0  1.1  1.1  1.2  1.6  2.2  3.0  4.0 10.5
```
