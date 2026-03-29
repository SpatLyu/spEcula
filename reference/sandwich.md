# sandwich mapping model

Spatial prediction based on spatial stratified heterogeneity using
sandwich mapping model.

## Usage

``` r
sandwich(
  sampling,
  stratification,
  reporting,
  sampling_attr,
  ssh_zone,
  reporting_id,
  weight_type = "area"
)
```

## Arguments

- sampling:

  Sampling layer, spatial point vector object which is `sf` or can be
  converted to `sf` object.

- stratification:

  Stratification layer, spatial polygon vector object which is `sf` or
  can be converted to `sf` object.

- reporting:

  Reporting layer, spatial polygon vector object which is `sf` or can be
  converted to `sf` object.

- sampling_attr:

  The `attribute` column for the sampling point in sampling layer.

- ssh_zone:

  The `zone` column for the stratification layer.

- reporting_id:

  The `id` column for the reporting layer.

- weight_type:

  (optional) Geographic area based on weight(`area`) or indicate human
  population size(`population`) , Default is `area`.

## Value

A `sf` object with estimated mean `sandwichest_mean` and standard error
`sandwichest_standarderror`.

## References

Lin, Y., Xu, C., & Wang, J. (2023). sandwichr: Spatial prediction in R
based on spatial stratified heterogeneity. Transactions in GIS: TG,
27(5), 1579–1598. https://doi.org/10.1111/tgis.13088

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
simpath = system.file("extdata", "sim.gpkg", package="spEcula")
sampling = read_sf(simpath,layer = 'sim_sampling')
ssh = read_sf(simpath,layer = 'sim_ssh')
reporting = read_sf(simpath,layer = 'sim_reporting')
sandwich(sampling = sampling,stratification = ssh,reporting = reporting,
        sampling_attr = 'Value',ssh_zone = 'X',reporting_id = 'Y',
        weight_type = 'population')
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Simple feature collection with 7 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.684342e-14 ymin: 2 xmax: 4 ymax: 6
#> Geodetic CRS:  WGS 84
#> # A tibble: 7 × 4
#>       Y sandwichest_mean sandwichest_standarderror                      geometry
#>   <dbl>            <dbl>                     <dbl>                 <POLYGON [°]>
#> 1     1            NaN                      NaN    ((0.8 4, 0.8 4, 1 4, 1.2 4, …
#> 2     2            266.                       2.11 ((2.8 6, 2.6 6, 2.4 6, 2.2 6…
#> 3     3            311.                       2.42 ((2.4 3, 2.4 2.8, 2.2 2.8, 2…
#> 4     4            413.                       3.06 ((4 3.6, 4 3.8, 4 4, 4 4.2, …
#> 5     5            NaN                      NaN    ((1 3.6, 1 3.4, 1.2 3.4, 1.4…
#> 6     6            NaN                      NaN    ((1.6 3, 1.6 2.8, 1.8 2.8, 2…
#> 7     7             93.8                      2.85 ((0.6 5, 0.6 5, 0.6 5.2, 0.6…
sandwich(sampling = sampling,stratification = ssh,reporting = reporting,
        sampling_attr = 'Value',ssh_zone = 'X',reporting_id = 'Y',
        weight_type = 'area')
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> Simple feature collection with 7 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 5.684342e-14 ymin: 2 xmax: 4 ymax: 6
#> Geodetic CRS:  WGS 84
#> # A tibble: 7 × 4
#>       Y sandwichest_mean sandwichest_standarderror                      geometry
#>   <dbl>            <dbl>                     <dbl>                 <POLYGON [°]>
#> 1     1             381.                      2.43 ((0.8 4, 0.8 4, 1 4, 1.2 4, …
#> 2     2             262.                      2.10 ((2.8 6, 2.6 6, 2.4 6, 2.2 6…
#> 3     3             298.                      2.49 ((2.4 3, 2.4 2.8, 2.2 2.8, 2…
#> 4     4             401.                      2.88 ((4 3.6, 4 3.8, 4 4, 4 4.2, …
#> 5     5             390.                      2.53 ((1 3.6, 1 3.4, 1.2 3.4, 1.4…
#> 6     6             357.                      2.15 ((1.6 3, 1.6 2.8, 1.8 2.8, 2…
#> 7     7             203.                      2.40 ((0.6 5, 0.6 5, 0.6 5.2, 0.6…
```
