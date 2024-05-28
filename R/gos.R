#' @title Geographically optimal similarity
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Computationally optimized function for geographically optimal similarity (GOS) model
#' @references
#' Song, Y. (2022). Geographically Optimal Similarity. Mathematical Geosciences. doi: 10.1007/s11004-022-10036-8.
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
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#'
#' @return A tibble made up of predictions and uncertainties.
#'
#' @importFrom stats as.formula sd quantile
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr select
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
  formula.vars = all.vars(formula)
  response = data[, formula.vars[1], drop = TRUE]
  no = nrow(data)
  np = nrow(newdata)
  nv = ifelse(formula.vars[2] == ".", ncol(data) - 1, length(formula.vars) - 1)

  if (formula.vars[2] == "."){
    obs_explanatory = data[,-which(colnames(data) == formula.vars[1])]
    pred_explanatory = newdata[,-which(colnames(newdata) == formula.vars[1])]
  } else {
    obs_explanatory = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
    pred_explanatory = subset(newdata, TRUE, match(formula.vars[-1], colnames(newdata)))
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
  sdv = sapply(1:nv, \(x) sd(xall[,x]))

  calculgos = \(u){
    xpredvalues = pred_explanatory[u,]
    ej1 = lapply(1:nv, \(x) Ej(obs_explanatory[,x], xpredvalues[x], np, sdv[x]))
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

#' @title Function for the best kappa parameter
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Computationally optimized function for determining the best kappa parameter for the optimal similarity
#' @references
#' Song, Y. (2022). Geographically Optimal Similarity. Mathematical Geosciences. doi: 10.1007/s11004-022-10036-8.
#'
#' @usage
#' bestkappa(formula,data = NULL,kappa=seq(0.05,1,0.05),
#'           nrepeat = 10,nsplit = 0.5,cores = 1)
#'
#' @param formula A formula of GOS model
#' @param data A data.frame or tible of observation data
#' @param kappa (optional)A numeric vector of the optional percentages of observation locations
#'              with high similarity to a prediction location.
#'              kappa = 1 - tau, where tau is the probability parameter
#'              in quantile operator. kappa = 0.25 means
#'              that 25% of observations with high similarity to a prediction
#'              location are used for modelling.
#' @param nrepeat (optional)A numeric value of the number of cross-validation training times.
#'                The default value is 10.
#' @param nsplit (optional)The sample training set segmentation ratio,which in `(0,1)`,
#' default is `0.5`.
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#'
#' @return A list of the result of the best kappa and the computation process curve.
#'
#' @importFrom DescTools RMSE
#' @importFrom dplyr %>% summarise
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous scale_y_continuous theme_bw
#' @importFrom ggrepel geom_label_repel
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(ggrepel)
#' system.time({
#'   b1 = bestkappa(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#'                  data = zn,kappa = c(0.01, 0.05, 0.1, 0.2, 0.5, 1),
#'                  nrepeat = 2,cores = 1)
#' })
#' b1$bestkappa
#' b1$plot
#' }
#' @export

bestkappa = \(formula, data = NULL, kappa = seq(0.05,1,0.05),
              nrepeat = 10,nsplit = 0.5,cores = 1){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  no = nrow(data)
  namey = all.vars(formula)[1]

  calcvrmse = \(paramp){# The function is wrapped this way to use `parallel::parLapply`.
    i = paramp[[1]]
    seed = paramp[[2]]
    kappa = paramp[[3]]
    set.seed(seed)
    trainindex = sample.int(n = no,
                            size = floor(nsplit * no),
                            replace = F)
    cvtrain = data[trainindex, ]
    cvtest = data[-trainindex, ]

    g = gos(formula, data = cvtrain, newdata = cvtest,
            kappa = kappa, cores = 1)
    pred = g$pred

    cvrmse = c(kappa,DescTools::RMSE(cvtest[[namey]], pred))
    names(cvrmse) = c('kappa','rmse')
    return(cvrmse)
  }

  paradf = data.frame("i" = seq_along(rep(kappa, times = nrepeat)),
                      "seed" = rep(c(1:nrepeat), each = length(kappa)),
                      "kappa" = rep(kappa, times = nrepeat))
  parak = split(paradf, seq_len(nrow(paradf)))

  if (doclust) {
    parallel::clusterExport(cores,'gos')
    out_rmse = parallel::parLapply(cores,parak,calcvrmse)
    out_rmse = tibble::as_tibble(do.call(rbind, out_rmse))
  } else {
    out_rmse = purrr::map_dfr(parak,calcvrmse)
  }

  cv.out = out_rmse %>%
    dplyr::summarise(rmse = mean(rmse,na.rm = T),
                     .by = kappa)

  k = which(cv.out$rmse == min(cv.out$rmse))[1]
  best_kappa = cv.out$kappa[k]

  l1 = (max(cv.out$rmse)-min(cv.out$rmse))*0.1
  best_x = cv.out$kappa[k]
  best_y = cv.out$rmse[k]

  p1 = ggplot2::ggplot(cv.out, aes(x = kappa, y = rmse))+
    ggplot2::geom_point()+
    ggplot2::geom_line() +
    ggrepel::geom_label_repel(data = data.frame(kappa=best_x, rmse=best_y),
                              label=as.character(best_kappa)) +
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    ggplot2::scale_y_continuous(limits = c(min(cv.out$rmse) - l1,
                                           max(cv.out$rmse))) +
    ggplot2::theme_bw()


  out = list("bestkappa" = best_kappa,
             "cvrmse" = out_rmse,
             "cvmean" = cv.out,
             "plot" = p1)

  return(out)
}
