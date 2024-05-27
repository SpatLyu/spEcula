#' @title Geographically optimal similarity
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Computationally optimized function for geographically optimal similarity (GOS) model
#'
#' @usage gos(formula, data = NULL, newdata = NULL, kappa = 0.25, cores = 1)
#'
#' @param formula A formula of GOS model.
#' @param data A data.frame or tibble of observation data.
#' @param newdata A data.frame or tibble of prediction variables data.
#' @param kappa A numeric value of the percentage of observation locations
#'              with high similarity to a prediction location.
#'              kappa = 1 - tau, where tau is the probability parameter
#'              in quantile operator. The default kappa is 0.25, meaning
#'              that 25% of observations with high similarity to a prediction
#'              location are used for modelling.
#'@param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#'cluster with that many cores is created and used. You can also supply a cluster object
#'
#' @return A tibble made up of predictions and uncertainties.
#'
#' @importFrom stats as.formula sd quantile
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' g = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#' data = zn, newdata = grid, kappa = 0.08,cores = 6)
#' g
#' }

#' @export

gos = \(formula, data = NULL, newdata = NULL, kappa = 0.25, cores = 1){
  doclust = FALSE
  tau = 1 - kappa
  formula = as.formula(formula)
  response = data[,colnames(data) == as.character(formula[[2]])]
  response = as.vector(as.matrix(response))
  no = nrow(data)
  np = nrow(newdata)

  if (formula[[3]]=="."){
    nv = ncol(data) - 1
  } else {
    nv = length(all.vars(formula)[-1])
  }

  if (formula[[3]]=="."){
    obs_explanatory = data[,-which(colnames(data) == as.character(formula[[2]]))]
  } else {
    obs_explanatory = subset(data, TRUE, match(all.vars(formula)[-1], colnames(data)))
  }

  if (formula[[3]]=="."){
    pred_explanatory = newdata[,-which(colnames(newdata) == as.character(formula[[2]]))]
  } else {
    pred_explanatory = subset(newdata, TRUE, match(all.vars(formula)[-1], colnames(newdata)))
  }

  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  Ej = function(xobs, xp1, snp, sdv){
    sdj = sqrt(sum((xp1 - xobs)^2)/snp)
    ej = exp(-(xp1 - xobs)^2/(2*(sdv*sdv/sdj)^2))
    return(ej)
  }

  obs_explanatory = as.matrix(obs_explanatory)
  pred_explanatory = as.matrix(pred_explanatory)
  xall = rbind(obs_explanatory, pred_explanatory)
  sdv = sapply(1:nv, function(x) sd(xall[,x]))

  calculgos = \(u){
    xpredvalues = pred_explanatory[u,]
    ej1 = lapply(1:nv, function(x) Ej(obs_explanatory[,x], xpredvalues[x], np, sdv[x]))
    ej = do.call(pmin, ej1)
    k = which(ej >= quantile(ej, tau))
    ej2 = ej[k]

    c(sum(response[k] * ej2) / sum(ej2),
      1 - quantile(ej2, 0.9),
      1 - quantile(ej2, 0.95),
      1 - quantile(ej2, 0.99),
      1 - quantile(ej2, 0.995),
      1 - quantile(ej2, 0.999),
      1 - quantile(ej2, 1)) -> pred_unce
    names(pred_unce) = c('pred',paste0('uncertainty',
                                       c(90, 95, 99, 99.5, 99.9, 100)))
    return(pred_unce)
  }

  if (doclust) {
    out = parallel::parLapply(cores, 1:np, calculgos)
    out = tibble::as_tibble(do.call(rbind, out))
  } else {
    out = purrr::map_dfr(1:np,calculgos)
  }

  return(out)
}
