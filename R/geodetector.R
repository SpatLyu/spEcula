

factor_detector = \(x,y){
  gdf = tibble::tibble(x = x, y = y)
  onestrata = gdf %>%
    dplyr::summarise(n = dplyr::n(),.by = x) %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(x)
  gdf = dplyr::filter(gdf,x %in% onestrata)
  x = gdf$x
  y = gdf$y
  rss = \(y) (length(y) - 1) * stats::var(y)
  qv = 1 - sum(tapply(y, x, rss))/rss(y)
  v1 = length(x) - 1
  v2 = length(y) - length(x)
  Fv = (v2 * qv)/(v1 * (1 - qv))
  m0 = tapply(y, x, mean)
  m1 = sum(m0^2)
  m2 = sum(m0 * sqrt(count.x))^2/ny
  lambda = (m1 - m2) / (var(y) * (ny - 1) / ny)
  p0 = pf(Fv, df1 = v1, df2 = v2, ncp = lambda)
  sig = 1 - p0
  # return
  qv.sig <- c(qv = qv, sig = sig)
  return(qv.sig)
}
