#' @title Inverse transform of `car::bcPower`
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @param z A numeric vector to be inverse transformed.
#' @param alpha Power transformation parameter,which can be got from `car::powerTransform()`.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' library(car)
#'library(moments)
#'turbidity = c(1.0, 1.2, 1.1, 1.1, 2.4, 2.2, 2.6, 4.1, 5.0, 10.0, 4.0, 4.1, 4.2, 4.1,
#'              5.1, 4.5, 5.0, 15.2, 10.0, 20.0, 1.1, 1.1, 1.2, 1.6, 2.2, 3.0, 4.0, 10.5)
#'moments::skewness(turbidity)
#'shapiro.test(turbidity)
#'lambdapt = car::powerTransform(turbidity)
#'ttur = car::bcPower(turbidity,lambdapt$lambda)
#'moments::skewness(ttur)
#'shapiro.test(ttur)
#'inverse_bcPower(ttur,lambdapt$lambda)
#'
inverse_bcPower = \(z,alpha){
  return((z * alpha + 1) ^ (1 / alpha))
}


