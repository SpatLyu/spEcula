#' @title spatial stratified heterogeneity test
#' @author Wenbo Lv \email{lyu.geosocial@gmail.com}
#' @description
#' Spatial stratified heterogeneity test based on geographic detector.
#'
#' @param formula A formula of spatial stratified heterogeneity test.
#' @param data A data.frame or tibble of observation data.
#' @param type (optional) The type of geographical detector,which must be one of `factor`(default),
#' `interaction`, `risk`, `ecological`.
#' @param ... (optional) Specifies the size of the alpha (confidence level).Default is `0.95`.
#'
#' @return A tibble of the corresponding result is stored under the corresponding detector type
#' @export
#'
#' @examples
#' ssh.test(y ~ x1 + x2,
#'          tibble::tibble(y = 1:7,
#'          x1 = c('x',rep('y',3),rep('z',3)),
#'          x2 = c(rep('a',2),rep('b',2),rep('c',3))))
#'
#' ssh.test(y ~ x1 + x2,
#'          tibble::tibble(y = 1:7,
#'                         x1 = c('x',rep('y',3),rep('z',3)),
#'                         x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'          type = 'interaction')
#'
#' ssh.test(y ~ x1 + x2,
#'          tibble::tibble(y = 1:7,
#'                         x1 = c('x',rep('y',3),rep('z',3)),
#'                         x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'          type = 'risk',alpha = 0.95)
#'
#' ssh.test(y ~ x1 + x2,
#'          tibble::tibble(y = 1:7,
#'                         x1 = c('x',rep('y',3),rep('z',3)),
#'                         x2 = c(rep('a',2),rep('b',2),rep('c',3))),
#'          type = 'ecological',alpha = 0.95)
#'
ssh.test = \(formula,data,type = 'factor',...){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  response = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    explanatory = data[,-which(colnames(data) == formula.vars[1])]
  } else {
    explanatory = subset(data, TRUE, match(formula.vars[-1], colnames(data)))
  }

  switch (type,
    "factor" = {
      res = purrr::map_dfr(names(explanatory),
                           \(i) factor_detector(response,data[,i,drop = TRUE])) %>%
        dplyr::mutate(variable = names(explanatory)) %>%
        dplyr::select(variable,dplyr::everything()) %>%
        dplyr::arrange(dplyr::desc(`Q-statistic`))
      res = list("factor" = res)
      class(res) = "factor_detector"
    },
    "interaction" = {
      res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
        purrr::map_dfr(\(i) interaction_detector(response,
                                             data[,i[1],drop = TRUE],
                                             data[,i[2],drop = TRUE]) %>%
                         tibble::as_tibble() %>%
                         dplyr::mutate(variable1 = i[1],
                                       variable2 = i[2]) %>%
                         dplyr::select(variable1,variable2,Interaction,
                                       dplyr::everything()))
      res = list("interaction" = res)
      class(res) = "interaction_detector"
    },
    "risk" = {
      res = purrr::map_dfr(names(explanatory),
                           \(i) risk_detector(response,
                                              data[,i,drop = TRUE],
                                              ...) %>%
                             dplyr::mutate(variable = i) %>%
                             dplyr::select(variable,zone1,zone2,Risk,
                                           dplyr::everything()))
      res = list("risk" = res)
      class(res) = "risk_detector"
    },
    "ecological" = {
      res = utils::combn(names(explanatory), 2, simplify = FALSE) %>%
        purrr::map_dfr(\(i) ecological_detector(response,
                                                data[,i[1],drop = TRUE],
                                                data[,i[2],drop = TRUE],
                                                ...) %>%
                         tibble::as_tibble() %>%
                         dplyr::mutate(variable1 = i[1],
                                       variable2 = i[2]) %>%
                         dplyr::select(variable1,variable2,Ecological,
                                       dplyr::everything()))
      res = list("ecological" = res)
      class(res) = "ecological_detector"
    }
  )
  return(res)
}
