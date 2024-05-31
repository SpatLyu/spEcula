#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param y Variable Y, continuous numeric variable.
#' @param x Covariate X, \code{factor} or \code{character}.
#'
#' @return A numeric vector containing the Q-statistic and the p-value.
#' @importFrom stats var pf
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by n filter ungroup mutate
#' @export
factor_detector = \(y,x){
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
  qv.sig = c("Q statistic" = qv, "P value" = sig)
  return(qv.sig)
}


interaction_detector = \(y,x1,x2){
  x12 = paste0(x1,x2,'_')
  pv1 = factor_detector(y,x1)[1]
  pv2 = factor_detector(y,x2)[1]
  pv12 = factor_detector(y,x12)[1]

  if (qv12 < min(qv1, qv2)) {
    interaction = c("Weaken, nonlinear")
  } else if (qv12 >= min(qv1, qv2) & qv12 <= max(qv1, qv2)) {
    interaction = c("Weaken, uni-")
  } else if (qv12 > max(qv1, qv2) & (qv12 < qv1 + qv2)) {
    interaction = c("Enhance, bi-")
  } else if (qv12 == qv1 + qv2) {
    interaction = c("Independent")
  } else {
    interaction = c("Enhance, nonlinear")
  }

  return(c(pv1,pv2,pv12,interaction))
}
