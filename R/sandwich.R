#' @title sandwich mapping model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Spatial prediction based on spatial stratified heterogeneity using sandwich mapping model.
#' @references
#' Lin, Y., Xu, C., & Wang, J. (2023). sandwichr: Spatial prediction in R based on spatial stratified heterogeneity.
#' Transactions in GIS: TG, 27(5), 1579–1598. https://doi.org/10.1111/tgis.13088
#'
#' @param sampling Sampling layer, spatial point vector object which is `sf` or can be converted to `sf` object.
#' @param stratification Stratification layer, spatial polygon vector object which is `sf` or can be converted to `sf` object.
#' @param reporting Reporting layer, spatial polygon vector object which is `sf` or can be converted to `sf` object.
#' @param sampling_attr The `attribute` column for the sampling point in sampling layer.
#' @param ssh_zone The `zone` column for the stratification layer.
#' @param reporting_id The `id` column for the reporting layer.
#' @param weight_type (optional) Geographic area based on weight(`area`) or indicate human population size(`population`) ,
#' Default is `area`.
#'
#' @return A `sf` object with estimated mean `sandwichest_mean` and standard error `sandwichest_standarderror`.
#' @export
#'
#' @examples
#'library(sf)
#'simpath = system.file("extdata", "sim.gpkg", package="spEcula")
#'sampling = read_sf(simpath,layer = 'sim_sampling')
#'ssh = read_sf(simpath,layer = 'sim_ssh')
#'reporting = read_sf(simpath,layer = 'sim_reporting')
#'sandwich(sampling = sampling,stratification = ssh,reporting = reporting,
#'         sampling_attr = 'Value',ssh_zone = 'X',reporting_id = 'Y',
#'         weight_type = 'population')
#'sandwich(sampling = sampling,stratification = ssh,reporting = reporting,
#'         sampling_attr = 'Value',ssh_zone = 'X',reporting_id = 'Y',
#'         weight_type = 'area')
#'
sandwich = \(sampling,stratification,reporting,sampling_attr,
             ssh_zone, reporting_id, weight_type = 'area'){
  if (!inherits(sampling,"sf")){
    sampling = sf::st_as_sf(sampling)
  }
  if (!inherits(stratification,"sf")){
    stratification = sf::st_as_sf(stratification)
  }
  if (!inherits(reporting,"sf")){
    reporting = sf::st_as_sf(reporting)
  }

  sampling_zone = sampling %>%
    sf::st_join(stratification[ssh_zone]) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(dplyr::pick({{ssh_zone}})) %>%
    dplyr::summarise(Nh = dplyr::n(),
                     yh_bar = mean(dplyr::pick({{sampling_attr}})[[1]]),
                     sh2 = stats::var(dplyr::pick({{sampling_attr}})[[1]]),
                     v_yh_bar = sqrt(sh2 / Nh)) %>% # same as sandwichr package calculations
    dplyr::ungroup() %>%                            # But it is inconsistent with the corresponding formulas in the paper
    dplyr::select({{ssh_zone}},Nh,yh_bar,sh2,v_yh_bar)

  stratification = stratification %>%
    dplyr::left_join(sampling_zone,by = ssh_zone) %>%
    dplyr::select({{ssh_zone}},Nh,yh_bar,sh2,v_yh_bar)

  reporting = reporting %>%
    sf::st_intersection(stratification) %>%
    st_rename_geometry('geometry')

  if (weight_type == 'population') {
    reporting = reporting %>%
      dplyr::mutate(sampling_id = 1:nrow(.))
    sampling = sampling %>%
      sf::st_join(reporting['sampling_id']) %>%
      sf::st_drop_geometry() %>%
      dplyr::summarise(Nrh = dplyr::n(),
                       .by = sampling_id) %>%
      dplyr::select(sampling_id,Nrh)
    reporting = reporting %>%
      dplyr::left_join(sampling,by = 'sampling_id') %>%
      tidyr::replace_na(list(Nrh = 0)) %>%
      dplyr::group_by(dplyr::pick({{reporting_id}})) %>%
      dplyr::summarise(Nr = sum(Nrh),
                       yr_bar = sum(Nrh/Nr*yh_bar),
                       v_yr_bar = sum((Nrh/Nr)^2*v_yh_bar),
                       s_yr_bar = sqrt(v_yr_bar),
                       geometry = sf::st_union(geometry))
  } else if (weight_type == 'area') {
    reporting = reporting %>%
      dplyr::mutate(Nrh = st_geographical_area(geometry)) %>%
      dplyr::group_by(dplyr::pick({{reporting_id}})) %>%
      dplyr::summarise(Nr = sum(Nrh),
                       yr_bar = sum(Nrh/Nr*yh_bar),
                       v_yr_bar = sum((Nrh/Nr)^2*v_yh_bar),
                       s_yr_bar = sqrt(v_yr_bar),
                       geometry = sf::st_union(geometry))
  } else {
    stop(" `weight_type` can only be `area` or `population` ! ")
  }

  reporting = reporting %>%
    dplyr::select(dplyr::all_of({{reporting_id}}),
                  sandwichest_mean = yr_bar,
                  sandwichest_standarderror = s_yr_bar)
  return(reporting)
}
