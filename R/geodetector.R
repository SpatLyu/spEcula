#' @title factor detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' The factor detector q-statistic measures the spatial stratified heterogeneity of a variable Y,
#' or the determinant power of a covariate X of Y.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor} or \code{character}.
#'
#' @return A list contains the Q-statistic and the p-value.
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
  qv.sig = list("Q-statistic" = qv, "P-value" = sig)
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
#' @return A list contains the Q statistic when the factors \eqn{X_1} and \eqn{X_1} act on \eqn{Y} alone
#' and the Q statistic when the two interact on \eqn{Y} together with the result type of the interaction detector.
#'
#' @export
#' @example
interaction_detector = \(y,x1,x2){
  x12 = paste0(x1,x2,'_')
  pv1 = factor_detector(y,x1)[[1]]
  pv2 = factor_detector(y,x2)[[1]]
  pv12 = factor_detector(y,x12)[[1]]

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
  interd = list(pv1,pv2,pv12,interaction)
  names(interd) = c("Variable1 Q-statistics","Variable2 Q-statistics",
                    "Variable1 and Variable2 interact Q-statistics",
                    "Interaction")
  return(interd)
}

#' @title risk detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Determine whether there is a significant difference between the attribute means of two subregions.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, \code{factor} or \code{character}.
#' @param alpha (optional) Confidence level of the interval,default is 0.95.
#'
#' @return A tibble contains different combinations of covariate \code{X's} level and student t-test statistics,
#' degrees of freedom, p-values, and whether has risk (Yes or No).
#'
#' @importFrom stats t.test
#' @importFrom tidyr crossing
#' @importFrom dplyr filter pull
#' @importFrom tibble tibble
#' @export
#'
#' @examples
risk_detector = \(y,x,alpha = 0.95){
   x = factor(x)
   gdf = tibble::tibble(x = x, y = y)
   paradf = tidyr::crossing(x1 = levels(x),
                            x2 = levels(x)) %>%
     dplyr::filter(x1 != x2)
   x1 = paradf$x1
   x2 = paradf$x2

   twounit_risk_detector = \(y1,y2,alpha){
     tryCatch({
       tt = stats::t.test(y1,y2,conf.level = alpha)
       risk = ifelse(tt$p.value < (1 - alpha), "Yes", "No")
       risk = factor(risk,levels = c("Yes", "No"), labels = c("Yes", "No"))
       riskd = list(tt$statistic,tt$parameter,tt$p.value,risk)
     }, error = function(y1,y2){
       riskd = list(0,min(c(length(y1),length(y2))) - 1,1,NA)
     })

     names(riskd) = c("T-statistic","Degree-freedom","P-value","Risk")
     return(riskd)
   }

   calcul_rd = \(n1,n1,cutoff = 0.95){
     y1 = dplyr::filter(x == n1) %>% dplyr::pull(y)
     y2 = dplyr::filter(x == n2) %>% dplyr::pull(y)
     return(twounit_risk_detector(y1,y2,cutoff))
   }

   rd = purrr::map2_dfr(x1,x2,calcul_rd,cutoff = alpha)
   return(rd)
}

#' @title ecological detector
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Compare the effects of two factors \eqn{X_1} and \eqn{X_2} on the spatial distribution of the attribute \eqn{Y}.
#'
#' @param y Dependent variable, continuous numeric vector.
#' @param x1 Covariate \eqn{X_1}, \code{factor} or \code{character}.
#' @param x2 Covariate \eqn{X_2}, \code{factor} or \code{character}.
#' @param alpha (optional) Confidence level of the interval,default is 0.95.
#'
#' @return A list contains \code{F} statistics, p-values, and is there a significant difference between the
#' two factors \eqn{X_1} and \eqn{X_2} on the spatial distribution of the attribute \eqn{Y}
#' @importFrom stats pf
#' @export
#'
#' @examples
ecological_detector = \(y,x1,x2,alpha = 0.95){
  q1 = factor_detector(y,x1)[[1]]
  q2 = factor_detector(y,x2)[[1]]
  fv = (1 - q1) / (1 - q2)
  n = length(y)
  p0 = stats::pf(fv, df1 = n - 1, df2 = n - 1, lower.tail = FALSE)
  eco = ifelse(p0 < (1 - alpha), "Yes", "No")
  eco = factor(eco,levels = c("Yes", "No"),labels = c("Yes", "No"))
  ecod = list(fv,p0,eco)
  names(ecod) = c("F-statistic","P-value","Ecological")
  return(ecod)
}
