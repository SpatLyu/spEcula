#' @title Univariate discretization
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function to classify univariate vector to interval,a wrapper of `classInt::classify_intervals()`.
#'
#' @param x A continuous numerical variable.
#' @param k (optional) Number of classes required, if missing, `grDevices::nclass.Sturges()` is used;
#' see also the "dpih" and "headtails" styles for automatic choice of the number of classes.
#' @param method Chosen classify style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans",
#' "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", or "maximum".Default is `quantile`.
#' @param factor (optional) Default is `FALSE`, if `TRUE` returns cols as a factor with intervals as
#' labels rather than integers.
#' @param ... (optional) Other arguments passed to `classInt::classify_intervals()`,
#' see `?classInt::classify_intervals()`.
#'
#' @return A discrete vectors after being classified.
#' @importFrom classInt classify_intervals
#' @export
#'
#' @examples
#' xvar = c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408,
#'          17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892,
#'          2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073,
#'          7508, 5203)
#' st_unidisc(xvar,k = 6,method = 'sd')
st_unidisc = \(x,k,method,factor = FALSE,...){
  return(classInt::classify_intervals(x,k,style = method,
                                      ...,factor = factor))
}
