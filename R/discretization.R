#' @title Univariate discretization
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function to classify univariate vector to interval,a wrapper of `classInt::classify_intervals()`.
#'
#' @param x A continuous numerical variable.
#' @param k (optional) Number of classes required, if missing, `grDevices::nclass.Sturges()` is used;
#' see also the "dpih" and "headtails" styles for automatic choice of the number of classes.
#' @param method Chosen classify style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans",
#' "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".Default is `quantile`.
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
st_unidisc = \(x,k,method = "quantile",factor = FALSE,...){
  return(classInt::classify_intervals(x,k,style = method,
                                      ...,factor = factor))
}

#' @title Best univariate discretize parameters based on geodetector q-statistic
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining the best univariate discretize parameters based on geodetector q-statistic.
#'
#' @param y Variable Y, continuous numeric vector.
#' @param x Covariate X, continuous numeric vector.
#' @param k A vector of number of classes for discretization.
#' @param method (optional) A vector of methods for discretization,default is all can
#' used in `spEcula`.
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param return_disc (optional) Whether or not return discretized result used the optimal parameter.
#' Default is `TRUE`.
#' @param ... (optional) Other arguments passed to `st_unidisc()`.
#'
#' @return A list with the optimal parameter in the provided parameter combination with `k`
#' and `method`.
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply
#' @importFrom tidyr crossing
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols arrange desc
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
gd_bestunidisc = \(y,x,k,method = c("fixed","sd","equal","pretty","quantile","kmeans","hclust",
                                    "bclust","fisher","jenks","dpih","headtails","maximum","box"),
                   cores = 1,return_disc = TRUE,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  calcul_disc = \(paramgd){
    x = st_unidisc(x, k = paramgd[[1]],
                   method = paramgd[[2]],...)
    fd = factor_detector(y,x)
    q = fd[[1]]
    names(q) = "qstatistic"
    return(q)
  }

  paradf = tidyr::crossing("k" = k,
                           "method" = method)
  parak = split(paradf, seq_len(nrow(paradf)))

  if (doclust) {
    parallel::clusterExport(cores,c('st_unidisc','factor_detector'))
    out_g = parallel::parLapply(cores,parak,calcul_disc)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_disc)
  }

  out_g = dplyr::bind_cols(paradf,out_g) %>%
    dplyr::arrange(dplyr::desc(qstatistic))
  out_g = as.list(out_g[1,])

  if(return_disc){
    resdisc = st_unidisc(x,k = out_g$`k`,method = out_g$`method`,...)
    out_g = append(out_g,list("disv" = resdisc))
  }
  return(out_g)
}
