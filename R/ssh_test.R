

gd <- function(formula, data = NULL){
  formula <- as.formula(formula)
  formula.vars <- all.vars(formula)
  response <- data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    explanatory <- data[, !(colnames(data) %in% formula.vars[1]), drop = FALSE]
  } else {
    explanatory <- data[, formula.vars[-1], drop = FALSE]
  }
  ncolx <- ncol(explanatory)

  FunQ <- function(y, x){
    # debug: use FunQ to calculate q values to increase speed
    nx <- length(levels(factor(x)))
    ny <- length(y)
    # remove strata containing one observation
    count.x <- table(x)
    k <- match(names(count.x)[which(count.x == 1)], x)
    if (length(k) > 0) {
      y <- y[-k]
      x <- factor(x[-k], levels = levels(factor(x))[-which(count.x == 1)])
      count.x <- table(x)
    }
    # q value
    rss <- function(y) (length(y) - 1) * var(y) # debug: use tapply
    qv <- 1 - sum(tapply(y, x, rss))/rss(y)
    # non-central F test
    v1 <- nx - 1
    v2 <- ny - nx
    Fv <- (v2 * qv)/(v1 * (1 - qv))
    m0 <- tapply(y, x, mean) # debug: use tapply
    m1 <- sum(m0^2)
    m2 <- sum(m0 * sqrt(count.x))^2/ny
    lambda <- (m1 - m2) / (var(y) * (ny - 1) / ny)
    p0 <- pf(Fv, df1 = v1, df2 = v2, ncp = lambda)
    sig <- 1 - p0
    # return
    qv.sig <- c(qv = qv, sig = sig)
    return(qv.sig)
  }

  result <- do.call(rbind, lapply(1:ncolx, function(x) FunQ(response, explanatory[, x, drop = TRUE])))
  result <- cbind(variable = colnames(explanatory), data.frame(result))

  result <- list("Factor" = result)
  ## define class
  class(result) <- "gd"
  result
}

print.gd <- function(x, ...){
  rs0 <- x[[1]]
  print(rs0)
  invisible(x)
}

plot.gd <- function(x, sig = TRUE, ...){
  rs0 <- x[[1]]
  rs1 <- rs0[order(rs0$qv, decreasing = TRUE),]
  if (isTRUE(sig)) {
    rs2_k <- which(rs1$sig < 0.05) # debug
  } else {
    rs2_k <- 1:length(rs1$sig)
  }
  if (length(rs2_k) == 0){
    warning("all spatial associations are not significant at the 0.05 level.")
  } else {
    rs2 <- rs1[rs2_k,]
  }
  if (nrow(rs2) > nrow(rs1)){
    nrow21 <- nrow(rs2) - nrow(rs1)
    cat("\n",nrow21,"variable/variables are removed due to the signficance higher than 0.05.\n")
  }

  vec <- rs2[, "qv", drop = TRUE]
  names(vec) <- as.character(rs2$variable)
  vec.col <- ifelse(vec == max(vec), "red", "gray")

  nchar.names <- max(nchar(as.character(rs2$variable)))
  par(mar = c(5.1, 3.1 + nchar.names/4, 2.1, 2.1))
  p1 <- barplot(height = rev(vec), names = rev(names(vec)),
                horiz = TRUE, col = rev(vec.col), xlim = c(0, min(max(vec)*1.1, 1)),
                xlab = "Q value", las = 1)
  vec.lable <- round(vec, digits = 4)
  k1 <- length(which(vec > max(vec)/2))
  k2 <- length(vec) - k1
  text.pos <- c(rep(2, k1), rep(4, k2))
  text(x = vec, rev(p1), pos = text.pos, label = vec.lable)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}
