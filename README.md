
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spEcula <img src="man/figures/logo.png" align="right" height="140"/>

<!-- badges: start -->

![CRAN](https://www.r-pkg.org/badges/version/spEcula)
![r-universe](https://spatlyu.r-universe.dev/badges/spEcula)

<!-- badges: end -->

The goal of **spEcula** is to make it easier to use R for spatial
prediction based on *spatial dependence*, *spatial stratification
heterogeneity* and *geographical configuration similarity*, and spatial
statistical inference based on **spatial relationships** (the **three
laws of geography**).

## Installation

You can install the development version of `spEcula` from `github`:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/spEcula",build_vignettes = T,dep = T)
```

or install `spEcula` from `r-universe`:

``` r
install.packages('spEcula', repos='https://spatlyu.r-universe.dev')
```

## Example

### Geographically Optimal Similarity (GOS) model

`geosimilarity` package has achieved `gos` model,but when data is
larger,`geosimilarity` may be slow. I develop the parallelized `gos`
model in `spEcula`,which can change the `cores` argument in `gos()` and
`bestkappa` function to parallel computation.

``` r
library(spEcula)
data(zn)
data(grid)

zn$Zn = log(zn$Zn)
tictoc::tic()
g1 = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
         data = zn, newdata = grid, kappa = 0.08,cores = 6)
tictoc::toc()
## 12.52 sec elapsed
```

``` r
g1$pred = exp(g1$pred)
grid$pred = g1$pred
grid$uc99 = g1$`uncertainty99`
g1
## # A tibble: 13,132 × 7
##     pred uncertainty90 uncertainty95 uncertainty99 uncertainty99.5
##    <dbl>         <dbl>         <dbl>         <dbl>           <dbl>
##  1  21.8        0.0818        0.0523        0.0287         0.0243 
##  2  22.5        0.0529        0.0356        0.0102         0.00954
##  3  22.9        0.0693        0.0429        0.0224         0.0148 
##  4  22.6        0.0665        0.0572        0.0140         0.00799
##  5  21.9        0.0736        0.0460        0.0181         0.0139 
##  6  21.5        0.0728        0.0480        0.0200         0.0169 
##  7  23.2        0.0453        0.0345        0.0185         0.0178 
##  8  24.8        0.0488        0.0434        0.0227         0.0118 
##  9  25.0        0.0435        0.0432        0.0186         0.0103 
## 10  24.5        0.0217        0.0217        0.0182         0.0141 
## # ℹ 13,122 more rows
## # ℹ 2 more variables: uncertainty99.9 <dbl>, uncertainty100 <dbl>
```

``` r
library(ggplot2)
library(cowplot)
library(viridis)
```

``` r
f1 = ggplot(grid, aes(x = Lon, y = Lat, fill = pred)) +
  geom_tile() +
  scale_fill_viridis(option="magma", direction = -1) + 
  coord_equal() +
  labs(fill='Prediction') +
  theme_bw() 
f2 = ggplot(grid, aes(x = Lon, y = Lat, fill = uc99)) +
  geom_tile() +
  scale_fill_viridis(option="mako", direction = -1) + 
  coord_equal() +
  labs(fill=bquote(Uncertainty~(zeta==0.99))) +
  theme_bw() 

plot_grid(f1,f2,nrow = 1,label_fontfamily = 'serif',
          labels = paste0('(',letters[1:2],')'),
          label_fontface = 'plain',label_size = 10,
          hjust = -1.5,align = 'hv')  -> p
p
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### Geographic detectors(geodetector) model

``` r
library(sf)
library(terra)
library(tidyverse)
library(spEcula)
fvcpath = system.file("extdata", "FVC.zip",package = 'spEcula')
fvc = terra::rast(paste0("/vsizip/",fvcpath))
fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
head(fvc)
## # A tibble: 6 × 13
##     fvc premax premin presum tmpmax tmpmin tmpavg    pop   ntl  lulc  elev slope
##   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 0.198   163.   7.95  3956.   20.8  -7.53   8.05  1.90   6.60    10 1758.  2.65
## 2 0.193   161.   6.80  3892.   20.7  -7.55   8.02  1.20   4.91    10 1754.  3.45
## 3 0.192   160.   5.24  3842.   20.9  -7.48   8.15  0.547  3.75    10 1722.  3.96
## 4 0.189   159.   5     3808.   21.1  -7.39   8.35  0.542  3.99    10 1672.  2.90
## 5 0.208   164.   9.98  4051.   20.6  -7.59   7.97 10.4    7.10    10 1780.  1.94
## 6 0.196   163.   8.15  3973.   20.7  -7.53   8.03  9.31   6.56    10 1755.  3.01
## # ℹ 1 more variable: aspect <dbl>
```

``` r
tictoc::tic()
g = gd_bestunidisc(fvc ~ .,data = select(fvc,-lulc),discnum = 2:15,cores = 6)
tictoc::toc()
## 22.82 sec elapsed
```

``` r
new.fvc = bind_cols(select(fvc,fvc,lulc),g$disv)
ssh.test(fvc ~ .,data = new.fvc,type = 'factor')
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6424    | 3.712e-10 |
|   lulc   |   0.5533    | 9.106e-10 |
|  premin  |   0.4423    | 7.392e-10 |
|  tmpmin  |   0.4028    | 5.13e-10  |
|  tmpmax  |   0.2284    | 5.111e-10 |
|   elev   |    0.209    |  1.5e-10  |
|  tmpavg  |   0.1976    | 9.163e-10 |
|  slope   |   0.1943    | 7.094e-10 |
|   pop    |   0.1856    | 3.221e-10 |
|  premax  |   0.1336    | 2.489e-10 |
|   ntl    |   0.02157   | 8.377e-10 |
|  aspect  |   0.00741   | 5.448e-10 |

``` r
ssh.test(fvc ~ .,data = new.fvc,type = 'interaction')
## Spatial Stratified Heterogeneity Test 
##  
##          Interaction detector
```

| Interactive variable |    Interaction     |
|:--------------------:|:------------------:|
|    lulc ∩ aspect     | Enhance, nonlinear |
|     lulc ∩ elev      |    Enhance, bi-    |
|      lulc ∩ ntl      | Enhance, nonlinear |
|      lulc ∩ pop      |    Enhance, bi-    |
|    lulc ∩ premax     |    Enhance, bi-    |
|    lulc ∩ premin     |    Enhance, bi-    |
|    lulc ∩ presum     |    Enhance, bi-    |
|     lulc ∩ slope     |    Enhance, bi-    |
|    lulc ∩ tmpavg     |    Enhance, bi-    |
|    lulc ∩ tmpmax     |    Enhance, bi-    |
|    lulc ∩ tmpmin     |    Enhance, bi-    |
|    aspect ∩ elev     | Enhance, nonlinear |
|     aspect ∩ ntl     | Enhance, nonlinear |
|     aspect ∩ pop     | Enhance, nonlinear |
|   aspect ∩ premax    | Enhance, nonlinear |
|   aspect ∩ premin    | Enhance, nonlinear |
|   aspect ∩ presum    |    Weaken, uni-    |
|    aspect ∩ slope    | Enhance, nonlinear |
|   aspect ∩ tmpavg    | Enhance, nonlinear |
|   aspect ∩ tmpmax    | Enhance, nonlinear |
|   aspect ∩ tmpmin    | Enhance, nonlinear |
|      elev ∩ ntl      | Enhance, nonlinear |
|      elev ∩ pop      |    Enhance, bi-    |
|    elev ∩ premax     | Enhance, nonlinear |
|    elev ∩ premin     |    Enhance, bi-    |
|    elev ∩ presum     |    Enhance, bi-    |
|     elev ∩ slope     |    Enhance, bi-    |
|    elev ∩ tmpavg     |    Enhance, bi-    |
|    elev ∩ tmpmax     | Enhance, nonlinear |
|    elev ∩ tmpmin     |    Enhance, bi-    |
|      ntl ∩ pop       | Enhance, nonlinear |
|     ntl ∩ premax     | Enhance, nonlinear |
|     ntl ∩ premin     | Enhance, nonlinear |
|     ntl ∩ presum     | Enhance, nonlinear |
|     ntl ∩ slope      | Enhance, nonlinear |
|     ntl ∩ tmpavg     | Enhance, nonlinear |
|     ntl ∩ tmpmax     | Enhance, nonlinear |
|     ntl ∩ tmpmin     | Enhance, nonlinear |
|     pop ∩ premax     | Enhance, nonlinear |
|     pop ∩ premin     |    Enhance, bi-    |
|     pop ∩ presum     |    Enhance, bi-    |
|     pop ∩ slope      |    Enhance, bi-    |
|     pop ∩ tmpavg     | Enhance, nonlinear |
|     pop ∩ tmpmax     | Enhance, nonlinear |
|     pop ∩ tmpmin     |    Enhance, bi-    |
|   premax ∩ premin    | Enhance, nonlinear |
|   premax ∩ presum    |    Enhance, bi-    |
|    premax ∩ slope    | Enhance, nonlinear |
|   premax ∩ tmpavg    | Enhance, nonlinear |
|   premax ∩ tmpmax    | Enhance, nonlinear |
|   premax ∩ tmpmin    | Enhance, nonlinear |
|   premin ∩ presum    |    Enhance, bi-    |
|    premin ∩ slope    |    Enhance, bi-    |
|   premin ∩ tmpavg    |    Enhance, bi-    |
|   premin ∩ tmpmax    |    Enhance, bi-    |
|   premin ∩ tmpmin    |    Enhance, bi-    |
|    presum ∩ slope    |    Enhance, bi-    |
|   presum ∩ tmpavg    |    Enhance, bi-    |
|   presum ∩ tmpmax    |    Enhance, bi-    |
|   presum ∩ tmpmin    |    Enhance, bi-    |
|    slope ∩ tmpavg    |    Enhance, bi-    |
|    slope ∩ tmpmax    |    Enhance, bi-    |
|    slope ∩ tmpmin    |    Enhance, bi-    |
|   tmpavg ∩ tmpmax    | Enhance, nonlinear |
|   tmpavg ∩ tmpmin    | Enhance, nonlinear |
|   tmpmax ∩ tmpmin    | Enhance, nonlinear |
