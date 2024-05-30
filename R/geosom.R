#' @title Spatially-aware Self-Organizing Maps(GeoSOM) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Implementation of spatially-aware self-Organizing maps(GeoSOM) model model
#' based on `kohonen::supersom()`.
#'
#' @param data A data.frame or tibble
#' @param coords The coordinate column name in the `data`.
#' @param wt The weight of spatial coordination and the weight of the non-spatial attribute is 1.
#' If wt is 0,now it equal to som model.
#' @param grid A grid for the codebook vectors: see `geosomgrid`.
#' @param normalize (optional)Boolean, indicating whether non-spatial feature data should
#' be normal standardization,default is `True`.
#' @param ... Other arguments passed to `kohonen::supersom()`,see `?kohonen::supersom()`.
#'
#' @return An object of class "kohonen" with components.
#' @importFrom kohonen supersom
#' @export
#'
#' @examples
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
geosom = \(data,coords,wt,grid,normalize = TRUE,...) {
  geodata = data.matrix(data[,(names(data) %in% coords)])
  data = data.matrix(data[,!(names(data) %in% coords)])
  spatial.weight = c(1, wt)
  if (normalize) {
    data = scale(data)
  }

  if (wt == 0) {
    res_som = kohonen::supersom(list(data), grid,
                                normalizeDataLayers=FALSE, ...)
  } else {
    res_som = kohonen::supersom(list(data, geodata), grid,
                                user.weights = spatial.weight,
                                normalizeDataLayers = FALSE, ...)
  }
  return(res_som)
}

#' @title GeoSOM-grid related functions
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Set up a grid of units, of a specified size and topology.Wrapper of `kohonen::somgrid`.
#'
#' @param xdim X dimensions of the grid
#' @param ydim Y dimensions of the grid.
#' @param topo (optional)Choose between a `hexagonal` or `rectangular` topology,default is `hexagonal`.
#' @param neighbourhood.fct (optional)Choose between bubble and gaussian neighbourhoods when training a
#' GeoSOM."bubble" or"gaussian"(default).
#' @param toroidal (optional)Logical, whether the grid is toroidal or not.Default is `FALSE`.
#'
#' @return An object of class "somgrid", with elements pts, and the input arguments to the function.
#' @importFrom kohonen somgrid
#' @export
geosomgrid = \(xdim,ydim,topo = "hexagonal",
               neighbourhood.fct = "gaussian",
               toroidal = FALSE) {
  kohonen::somgrid(xdim,ydim,topo,neighbourhood.fct,toroidal)
}

#' @title GeoSOM quality measures
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Computes several quality measures on a trained GeoSOM.This function is a wrapper
#' of `aweSOM::somQuality()`.
#'
#' @param gsom A `kohonen` object,get from `geosom()`.
#'
#' @return A list containing quality measures : quantization error, share of explained variance,
#' topographic error and Kaski-Lagus error.
#' @importFrom aweSOM somQuality
#' @export
#'
#' @examples
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' geosom_quality(g)
geosom_quality = \(gsom){
  return(aweSOM::somQuality(gsom, gsom$data[[1]]))
}

#' @title Best param for geosom model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Function for determining the best parameter for the geosom model
#'
#' @param data A data.frame or tibble
#' @param coords The coordinate column name in the `data`.
#' @param wt The weight vector of spatial coordination.
#' @param xdim X dimensions of the grid, a numeric vector.
#' @param ydim Y dimensions of the grid, a numeric vector.
#' @param topo (optional)Default use `hexagonal` and `rectangular`.
#' @param neighbourhood.fct (optional)Default use bubble and gaussian neighbourhoods when training a
#' GeoSOM.
#' @param cores positive integer(default is 1). If cores > 1, a 'parallel' package
#' cluster with that many cores is created and used. You can also supply a cluster
#' object.
#' @param ... (optional)Other arguments passed to `geosom()`.
#'
#' @return A list with the optimal parameter in the provided parameter combination and
#' the corresponding error.
#' @export
#'
#' @examples
#' \dontrun{
#' data(pmc)
#' set.seed(2004)
#' geosom_bestparam(data = pmc, coords = c("centroidx","centroidy"),
#' wt = seq(0.1,5,by = 0.1),xdim = 3:10, ydim = 3:10,cores = 6)
#' }
geosom_bestparam = \(data,coords,wt,xdim,ydim,
                     topo = c("rectangular", "hexagonal"),
                     neighbourhood.fct = c("bubble", "gaussian"),
                     cores = 1,...){
  doclust = FALSE
  if (inherits(cores, "cluster")) {
    doclust = TRUE
  } else if (cores > 1) {
    doclust = TRUE
    cores = parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cores), add=TRUE)
  }

  coordf = data.matrix(data[,(names(data) %in% coords)])
  feadf = data.matrix(data[,!(names(data) %in% coords)])

  geosom_est = \(data,geodata,wt,grid,normalize = TRUE,...) {
    spatial.weight = c(1, wt)
    if (normalize) {
      data = scale(data)
    }

    if (wt == 0) {
      res_som = kohonen::supersom(list(data), grid,
                                  normalizeDataLayers=FALSE, ...)
    } else {
      res_som = kohonen::supersom(list(data, geodata), grid,
                                  user.weights = spatial.weight,
                                  normalizeDataLayers = FALSE, ...)
    }
    return(res_som)
  }

  calcul_geosom = \(paramgeosom){
    wt = paramgeosom[[1]]
    grid = geosomgrid(paramgeosom[[2]],paramgeosom[[3]],
                      paramgeosom[[4]],paramgeosom[[5]])
    gsom = geosom_est(feadf,coordf,wt,grid,...)
    gp = geosom_quality(gsom)
    gerr = c(gp$err.quant,gp$err.varratio,gp$err.topo,gp$err.kaski)
    names(gerr) = c('err_quant','err_varratio','err_topo','err_kaski')
    return(gerr)
  }

  paradf = tidyr::crossing("wt" = wt,
                           "xdim" = xdim,
                           "ydim" = ydim,
                           "topo" = topo,
                           "neighbourhoodfct" = neighbourhood.fct)
  parak = split(paradf, seq_len(nrow(paradf)))

  if (doclust) {
    parallel::clusterExport(cores,c('geosomgrid','geosom_quality'))
    out_g = parallel::parLapply(cores,parak,calcul_geosom)
    out_g = tibble::as_tibble(do.call(rbind, out_g))
  } else {
    out_g = purrr::map_dfr(parak,calcul_geosom)
  }

  out_g = dplyr::bind_cols(paradf,out_g) %>%
    dplyr::arrange(dplyr::desc(err_kaski)) %>%
    dplyr::arrange(dplyr::desc(err_quant)) %>%
    dplyr::arrange(dplyr::desc(err_topo)) %>%
    dplyr::arrange(err_varratio)
  return(as.list(out_g[1,]))
}

#' @title Superclasses of GeoSOM
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' It is common to further cluster the GeoSOM map into superclasses, groups of cells with similar
#' profiles. This is done using classic clustering algorithms on the map’s prototypes.Remeber to
#' use it when the non-spatial variable is more than 1.
#'
#' @param gsom A `kohonen` object,get from `geosom()`.
#' @param k The number of superclasses.
#' @param method (optional)The clustering algorithms used on the map’s prototypes,two methods are
#' implemented in `spEcula`, PAM(k-medians) and hierarchical clustering.When method is `pam`,PAM
#' (k-medians) is used,otherwise hierarchical clustering.Default is `pam`.
#' @param hmethod For hierarchicical clustering, the clustering method, by
#' default "complete". See the \code{stats::hclust} documentation for more
#' details.
#' @param bindcoord Does the cluster of superclass in GeoSOM consider spatial coordination.
#' Defaul is `FALSE`.
#'
#' @return A numeric vector representing the superclass.
#' @importFrom cluster pam
#' @importFrom stats dist hclust cutree
#' @export
#' @examples
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' g_superclass = geosom_superclass(g,12)
#' g_superclass
geosom_superclass = \(gsom,k,method = 'pam',hmethod= "complete",
                      bindcoord = FALSE){
  stopifnot("Input `gsom` argument must be `kohonen` object!" = inherits(gsom,"kohonen"))
  if (bindcoord) {
    codebook = cbind(as.data.frame(gsom$codes[[1]]),
                     as.data.frame(scale(gsom$codes[[2]])))
  } else {
    codebook = gsom$codes[[1]]
  }

  switch(method,
         'pam' = {
           super_pam = cluster::pam(codebook,k)
           superclasses = as.vector(super_pam$clustering)
         },
         'hclust' = {
           super_hclust = stats::hclust(stats::dist(codebook),hmethod)
           superclasses = as.vector(stats::cutree(super_hclust, k))
         })
  return(superclasses)
}

#' @title Cluster label of GeoSOM
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Get the cluster label(result) of GeoSOM model.
#'
#' @param gsom A `kohonen` object,get from `geosom()`.
#' @param superclass A numeric vector get form `geosom_superclass()`
#'
#' @return A numeric vector representing the cluster label.
#' @export
#'
#' @examples
#' data(pmc)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' g_superclass = geosom_superclass(g,12)
#' g_label = geosom_clusterlabel(g,g_superclass)
#' g_label
geosom_clusterlabel = \(gsom,superclass){
  return(superclass[gsom$unit.classif])
}

#' @title Interactive GeoSOM plots
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Plot interactive visualizations of GeoSOM, as an html page. The plot can represent general map
#' informations, or selected categorical or numeric variables (not necessarily the ones used during training).
#' Hover over the map to focus on the selected cell or variable, and display further information.This function
#' is a wrapper of `aweSOM::aweSOMplot()`.
#'
#' @param gsom A `kohonen` object,get from `geosom()`.
#' @param type Character, the plot type. The default "Hitmap" is a population map.
#' "Cloud" plots the observations as a scatterplot within each cell (see Details).
#' "UMatrix" plots the average distance of each cell to its neighbors, on a color scale.
#' "Circular" (barplot), "Barplot", "Boxplot", "Radar" and "Line" are for numeric variables.
#' "Color" (heat map) is for a single numeric variable. "Pie" (pie chart) and "CatBarplot"
#' are for a single categorical (factor) variable.
#' @param superclass A numeric vector get form `geosom_superclass()`.
#' @param ... Other arguments passed to `aweSOM::aweSOMplot()`.
#'
#' @return An object of class htmlwidget.
#' @importFrom aweSOM aweSOMplot
#' @export
#'
#' @examples
#' \dontrun{
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' g_superclass = geosom_superclass(g,12)
#' geosom_plot(g,type = "Circular",superclass = g_superclass)
#' }
geosom_plot = \(gsom,type,superclass,...){
  return(aweSOM::aweSOMplot(som = gsom,type = type,
                            data = gsom$data[[1]],
                            superclass = superclass,
                            ...))

}

#' @title Screeplot of GeoSOM superclasses
#' @description
#'
#' The screeplot, helps deciding the optimal number of superclasses. Available
#' for both PAM(`pam`) and hierarchical clustering(`hclust`).
#'
#' @param gsom \code{kohonen} object, a SOM created by the \code{spEcula::geosom}
#'   function.
#' @param nclass number of superclasses to be visualized in the screeplot.
#'   Default is 2.
#' @param method Method used for clustering. Hierarchical clustering
#'   ("hclust") and Partitioning around medoids ("pam") can be used.
#'   Default is Partitioning around medoids.
#' @param hmethod For hierarchicical clustering, the clustering method, by
#'   default "complete". See the \code{stats::hclust} documentation for more
#'   details.
#' @param bindcoord Does the cluster of superclass in GeoSOM consider spatial coordination.
#' Defaul is `FALSE`.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' data(pmc)
#' set.seed(2004)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' geosom_bestsuperclass(g)
#' }

geosom_bestsuperclass = \(gsom, nclass= 2,method = "pam",
                    hmethod= "complete",bindcoord = FALSE){
  stopifnot("Input `gsom` argument must be `kohonen` object!" = inherits(gsom,"kohonen"))
  if (bindcoord) {
    codebook = cbind(as.data.frame(gsom$codes[[1]]),
                     as.data.frame(scale(gsom$codes[[2]])))
  } else {
    codebook = gsom$codes[[1]]
  }

  if (method == "hclust")
    ok.hclust = stats::hclust(stats::dist(codebook), hmethod)

  ncells = nrow(codebook)
  nvalues = max(nclass, min(ncells, max(ceiling(sqrt(ncells)),
                            (length(unique(gsom$unit.classif)) - 1))))
  clust.var = sapply(1:nvalues, function(k) {
    if (method == "hclust") {
      clust = stats::cutree(ok.hclust, k)
    } else if (method == "pam")
      clust = cluster::pam(codebook, k)$clustering
    clust.means = do.call(rbind, by(codebook, clust, colMeans))[clust, ]
    mean(rowSums((codebook - clust.means)^2))
  })
  unexpl = 100 * round(clust.var /
                          (sum(apply(codebook, 2, stats::var)) * (ncells - 1) / ncells), 3)
  plot(unexpl, t= "b", ylim= c(0, 100),
       xlab= "Nb. Superclasses", ylab= "% Unexpl. Variance")
  graphics::grid()
  graphics::abline(h= unexpl[nclass], col= 2)
}
