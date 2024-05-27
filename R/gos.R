#' Geographically optimal similarity
#'
#' @description Function for geographically optimal similarity (GOS) model
#'
#' @usage gos(formula, data = NULL, newdata = NULL, kappa = 0.25)
#'
#' @param formula A formula of GOS model
#' @param data A data.frame of observation data
#' @param newdata A data.frame of prediction variables data
#' @param kappa A numeric value of the percentage of observation locations
#'              with high similarity to a prediction location.
#'              kappa = 1 - tau, where tau is the probability parameter
#'              in quantile operator. The default kappa is 0.25, meaning
#'              that 25% of observations with high similarity to a prediction
#'              location are used for modelling.
#'
#' @return A list of predictions and uncertainties.
#'
#' @importFrom stats as.formula sd quantile
#' @importFrom SecDim rmvoutlier
#'
#' @examples
#' data("zn")
#' # log-transformation
#' hist(zn$Zn)
#' zn$Zn = log(zn$Zn)
#' hist(zn$Zn)
#' # remove outliers
#' require(SecDim)
#' k = rmvoutlier(zn$Zn, coef = 2.5)
#' dt = zn[-k,]
#' # split data for validation: 70% training; 30% testing
#' split = sample(1:nrow(dt), round(nrow(dt)*0.7))
#' train = dt[split,]
#' test = dt[-split,]
#' system.time({ # 0.33s
#' g1 = gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#'            data = train, newdata = test, kappa = 0.25)
#' })
#' test$pred = g1$pred
#' plot(test$Zn, test$pred)
#' cor(test$Zn, test$pred)
#' @export


gos = \(formula, data = NULL, newdata = NULL, kappa = 0.25){
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

  Ej = function(xobs, xp1, snp, sdv){
    sdj = sqrt(sum((xp1 - xobs)^2)/snp)
    ej = exp(-(xp1 - xobs)^2/(2*(sdv*sdv/sdj)^2))
    return(ej)
  }


  obs_explanatory = as.matrix(obs_explanatory)
  pred_explanatory = as.matrix(pred_explanatory)
  xall = rbind(obs_explanatory, pred_explanatory)
  sdv = sapply(1:nv, function(x) sd(xall[,x]))


  zp1 = c()
  uczeta = c(0.9, 0.95, 0.99, 0.995, 0.999, 1)
  uncertaintyj = data.frame(matrix(NA, np, 6))
  names(uncertaintyj) = uczeta


  for (u in 1:np){
    yobs = response
    xobsmatrix = obs_explanatory
    xpredvalues = pred_explanatory[u,]

    ej1 = lapply(1:nv, function(x) Ej(xobsmatrix[,x], xpredvalues[x], np, sdv[x]))
    ej = do.call(pmin, ej1)

    k = which(ej >= quantile(ej, tau))
    ej2 = ej[k]

    zp1[u] = sum(yobs[k] * ej2) / sum(ej2)

    # uncertaintyj = 1 - max(ej)
    uncertaintyj[u, 1] = 1 - quantile(ej2, 0.9)
    uncertaintyj[u, 2] = 1 - quantile(ej2, 0.95)
    uncertaintyj[u, 3] = 1 - quantile(ej2, 0.99)
    uncertaintyj[u, 4] = 1 - quantile(ej2, 0.995)
    uncertaintyj[u, 5] = 1 - quantile(ej2, 0.999)
    uncertaintyj[u, 6] = 1 - quantile(ej2, 1)
  }

  out = list("pred" = zp1, "uncertainty" = uncertaintyj)
  return(out)
}

