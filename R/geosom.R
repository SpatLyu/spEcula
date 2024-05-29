#' @title Spatially-aware Self-Organizing Maps(GeoSOM) model
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Implementation of spatially-aware self-Organizing maps(GeoSOM) model model
#' based on `kohonen::supersom()`.
#'
#' @param data A data.frame or tibble
#' @param coords The coordinate column name in the `data`.
#' @param wt A decimal number from 0 to 1,the weight of spatial coordination.
#' @param grid A grid for the codebook vectors: see `geosomgrid`.
#' @param normalize (optional)Boolean, indicating whether non-spatial feature data should
#' be normal standardization,default is `True`.
#' @param ... Other arguments passed to `kohonen::supersom()`,see `?kohonen::supersom()`.
#'
#' @return An object of class "kohonen" with components.
#' @export
#'
#' @examples
#' data(pmc)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 5,grid = geosomgrid(6,6),normalize = TRUE)
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
#' @param neighbourhood.fct (optional)
#' @param toroidal (optional)
#'
#' @return An object of class "somgrid", with elements pts, and the input arguments to the function.
#' @export
geosomgrid = \(xdim,ydim,topo = "hexagonal",
               neighbourhood.fct = "gaussian",
               toroidal = FALSE) {
  kohonen::somgrid(xdim,ydim,topo,neighbourhood.fct,toroidal)
}

#' @title GeoSOM quality measures
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Computes several quality measures on a trained GeoSOM.
#'
#' @param gsom A `kohonen` object,get from `geosom()`.
#'
#' @return
#' @export
#'
#' @examples
#' data(pmc)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 3,grid = geosomgrid(6,10),normalize = TRUE)
#' geosom_quality(g)
geosom_quality = \(gsom){
  return(aweSOM::somQuality(gsom, gsom$data[[1]]))
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
#'
#' @return A numeric vector representing the superclass.
#' @export
#' @examples
#' data(pmc)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 5,grid = geosomgrid(6,6),normalize = TRUE)
#' g_superclass = geosom_superclass(g,12)
#' g_superclass
geosom_superclass = \(gsom,k,method = 'pam'){
  stopifnot("Input `gsom` argument must be `kohonen` object!" = inherits(gsom,"kohonen"))
  switch(method,
         'pam' = {
           super_pam = cluster::pam(gsom$codes[[1]], k)
           superclasses = as.vector(super_pam$clustering)
         },
         'hclust' = {
           super_hclust = stats::hclust(stats::dist(gsom$codes[[1]]), "complete")
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
#' @param gsc A numeric vector get form `geosom_superclass()`
#'
#' @return A numeric vector representing the cluster label.
#' @export
#'
#' @examples
#' data(pmc)
#' g = geosom(data = pmc, coords = c("centroidx","centroidy"),
#' wt = 5,grid = geosomgrid(6,6),normalize = TRUE)
#' g_superclass = geosom_superclass(g,12)
#' g_label = geosom_clusterlabel(g,g_superclass)
#' g_label
geosom_clusterlabel = \(gsom,gsc){
  return(gsc[gsom$unit.classif])
}

geosom_plot = \(gsom,)
