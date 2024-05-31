#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor} or \code{character}.
#'
#' @return A numeric vector contains the Q-statistic and the p-value.
#' @importFrom stats var pf
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by n filter ungroup mutate
#' @export
#' @example
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
  qv.sig = c("Q-statistic" = qv, "P-value" = sig)
  return(qv.sig)
}

#' @title interaction detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Identify the interaction between different risk factors, that is, assess whether factors X1 and X2 together
#' increase or decrease the explanatory power of the dependent variable Y, or whether the effects of these factors
#' on Y are independent of each other.
#'
#' @param y Dependent variable, continuous numeric vector.
#' @param x1 Covariate \eqn{X_1}, \code{factor} or \code{character}.
#' @param x2 Covariate \eqn{X_2}, \code{factor} or \code{character}.
#'
#' @return A vector contains the Q statistic when the factors \eqn{X_1} and \eqn{X_1} act on \eqn{Y} alone
#' and the Q statistic when the two interact on \eqn{Y} together with the result type of the interaction detector.
#'
#' @export
#' @example
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

  return(c("Variable1 Q-statistics" = pv1,
           "Variable2 Q-statistics" = pv2,
           "Variable1 and Variable2 interact Q-statistics" = pv12,
           "Interaction" = interaction))
}

#' @title risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Determine whether there is a significant difference between the attribute means of two subregions.
#'
#'
#' @param y1 Dependent variable \code{Y} of subregion 1 for risk detection.
#' @param y2 Dependent variable \code{Y} of subregion 2 for risk detection.
#' @param alpha (optional) Confidence level of the interval,default is 0.95.
#'
#' @return A vector contains student t-test statistics, degrees of freedom, p-values, and whether has risk (Yes or No).
#' @importFrom stats t.test
#' @export
#'
#' @examples
risk_detector = \(y1,y2,alpha = 0.95){
  tryCatch({
    tt = stats::t.test(y1,y2,conf.level = alpha)
    risk = ifelse(tt$p.value < (1 - alpha), "Y", "N")
    risk = factor(risk, levels = c("Y", "N"))
    return(
      c("T-statistic" = tt$statistic,
        "Degree-freedom" = tt$parameter,
        "P-value" = tt$p.value,
        "Risk" = risk)
    )
  }, error = function(y1,y2){
    return(
      c("T-statistic" = 0,
        "Degree-freedom" = min(c(length(y1),length(y2))) - 1,
        "P-value" = 1,
        "Risk" = NA)
      )
  })
}
