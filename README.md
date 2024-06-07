
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spEcula <img src="man/figures/logo.png" align="right" height="90"/>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/spEcula)](https://CRAN.R-project.org/package=spEcula)
[![r-universe](https://spatlyu.r-universe.dev/badges/spEcula)](https://spatlyu.r-universe.dev/spEcula)

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

``` r
library(spEcula)
data(zn)
data(grid)

zn$Zn = log(zn$Zn)
tictoc::tic()
g1 = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
         data = zn, newdata = grid, kappa = 0.08,cores = 6)
tictoc::toc()
## 9.3 sec elapsed
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

<img src="man/figures/README-gos_result-1.png" width="100%" />

### Spatial Stratified Heterogeneity Test

``` r
library(spEcula)
data(NTDs)
head(NTDs)
## # A tibble: 6 × 5
##   SP_ID incidence watershed elevation soiltype
##   <chr>     <dbl>     <int>     <int>    <int>
## 1 0          5.94         5         5        5
## 2 1          5.87         5         5        4
## 3 2          5.88         5         5        5
## 4 3          5.98         5         5        5
## 5 4          5.96         5         5        1
## 6 5          5.66         5         5        4
```

``` r
fd = ssh.test(incidence ~ watershed + elevation + soiltype,
              data = NTDs,type = 'factor')
fd
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable  | Q-statistic |  P-value  |
|:---------:|:-----------:|:---------:|
| watershed |   0.6378    | 0.0001288 |
| elevation |   0.6067    |  0.04338  |
| soiltype  |   0.3857    |  0.3721   |
