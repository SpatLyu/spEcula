#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param x Covariate X, \code{factor} or \code{character}.
#' @param y Variable Y, continuous numeric variable.
#'
#' @return A numeric vector containing the Q-statistic and the p-value.
#' @importFrom stats var pf
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by n filter ungroup mutate
#' @export
factor_detector = \(x,y){
  gdf = tibble::tibble(x = x, y = y) %>%
    dplyr::group_by(x) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = factor(x))
  x = gdf$x
  y = gdf$y
  rss = \(y) (length(y) - 1) * stats::var(y)
  qv = 1 - sum(tapply(y, x, rss)) / rss(y)
  N = length(y)
  L = length(levels(x))
  Fv = ((N - L) * qv) / ((L - 1) * (1 - qv))
  hmean = tapply(y, x, mean)
  Nh = tapply(y, x, length)
  v1 = sum(hmean ^ 2)
  v2 = sum(sqrt(Nh) * hmean)^2 / N
  lambda = (v1 - v2) / (var(y) * (N - 1) / N)
  p0 = stats::pf(Fv, df1 = (L - 1), df2 = (N - L), ncp = lambda)
  sig = 1 - p0
  qv.sig = c('Q statistic' = qv, 'P value' = sig)
  return(qv.sig)
}
