#' Code for Utility Functions for Sections 3 and 2A
#'
#' @author F. Capello 


library(dplyr)
library(progress)
library(tidyverse)
library(pbapply)
library(parallel)

library(hexbin)
library(MASS)
library(DepthProc)


uniPermTest = function(target_val, by_val, B = 1e5) {
  pb <- progress_bar$new(total = B)
  
  n = length(target_val)
  T_stat <- numeric(B) # Vector where we will store the values of T*
  actual_means = tapply(target_val, by_val, mean, na.rm = TRUE)
  actual_T = abs(actual_means[1] - actual_means[2])
  
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n)
    perm_indices = by_val[permutation]
    means = tapply(target_val, perm_indices, mean, na.rm = TRUE)
    # test statistic:
    T_stat[perm] <- abs(means[1] - means[2])
    pb$tick()
  }
  pv = sum(T_stat >= actual_T)/B
  
  hist(T_stat, main=pv)
  abline(v = actual_T)
  return(pv)
  
}



multiPermTest = function(target_vals, by_val, B = 1e5) {
  pb <- progress_bar$new(total = B)
  
  n = length(by_val)
  T_stat <- numeric(B) # Vector where we will store the values of T*
  actual.fit <- manova(as.matrix(target_vals) ~ by_val)
  actual_T = -summary.manova(actual.fit, test="Wilks")$stats[1,2]
  
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n)
    perm_indices = by_val[permutation]
    perm.fit <- manova(as.matrix(target_vals) ~ perm_indices)
    # test statistic:
    T_stat[perm] <- -summary.manova(perm.fit, test="Wilks")$stats[1,2]
    pb$tick()
  }
  pv = sum(T_stat >= actual_T)/B
  hist(T_stat, main=pv)
  abline(v = actual_T)
  return(pv)
  
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

bootstrapReg = function(regressor, response, alpha=0.05, B=1e4, title="Reverse Boot. Intervals", xtitle="x", ytitle="y") {
  d = data.frame(regressor, response)
  x = d[(!is.na(d[1])) & (!is.na(d[2])),]
  regressor = x[,1]
  response = x[,2]
  
  # Fit a linear model
  fm <- lm(response ~ regressor)
  
  ### Bootstrap Inference
  
  # Compute residuals and fitted values
  fitted.obs <- fitted(fm)
  res.obs    <- residuals(fm)
  
  b0.obs <- coefficients(fm)[1]
  b1.obs <- coefficients(fm)[2]
  
  set.seed(24021979)
  T.boot.b0 <- numeric(B)
  T.boot.b1 <- numeric(B)
  pb <- progress_bar$new(total = B)
  
  for(b in 1:B)
  {
    response.b <- fitted.obs + sample(res.obs, replace = T)
    fm.b <- lm(response.b ~ regressor)
    T.boot.b0[b] <- coefficients(fm.b)[1]
    T.boot.b1[b] <- coefficients(fm.b)[2]
    pb$tick()
  }
  
  plot(ecdf(T.boot.b0), main='Intercept')
  abline(v=b0.obs, lty=2)
  plot(ecdf(T.boot.b1), main='Slope', col='red')
  abline(v=b1.obs, lty=2, col='red')
  
  # Bootstrap estimates of standard deviations of the OLS estimates (NB: without any analytical computation)
  sd(T.boot.b0)
  sd(T.boot.b1)
  cov(T.boot.b0, T.boot.b1)
  
  right.quantile.b1 <- quantile(T.boot.b1, 1 - alpha/2)
  left.quantile.b1  <- quantile(T.boot.b1, alpha/2)
  
  b1.obs
  right.quantile.b1 - b1.obs
  left.quantile.b1  - b1.obs
  
  CI.RP.b1 <- c(b1.obs - (right.quantile.b1 - b1.obs), b1.obs - (left.quantile.b1 - b1.obs))
  CI.RP.b1
  
  plot(ecdf(T.boot.b1), col='red', main=title, xlab=xtitle, ylab=ytitle, sub=paste("(", specify_decimal(CI.RP.b1[1], 5), ", ", specify_decimal(CI.RP.b1[2], 5), ")" ))
  abline(v = b1.obs, lty=2, col='red')
  abline(v = CI.RP.b1, col='red')
  
  return()
}



prop_cols <- function(data, col){
  d = as.data.frame.matrix(table(data$cod_scu_anonimo, col))
  d = d/rowSums(d)#(d[,2:ncol(d)])
  names(d)[1] = 'nan'
  return(d)
}


mannWhitneyTest = function(target_val, by_val, B = 1e5) {
  pb <- progress_bar$new(total = B)
  
  G0 <- target_val[by_val == 0]
  G1 <- target_val[by_val == 1]
  GS   <- c(G0, G1)
  n0 <- length(G0)
  n1 <- length(G1)
  n  <- length(GS)
  
  ranks.GS <- rank(GS)
  
  R0 <- sum(ranks.GS[1:n0])
  U0 <- R0 - n0*(n0+1)/2  # Nr of wins of the 1st sample
  
  R1 <- sum(ranks.GS[(n0+1):n])
  U1 <- R1 - n1*(n1+1)/2  # Nr of wins of the 2nd sample
  
  n0*n1 # Nr of contests
  
  
  # MC computation of the p-value
  # Generation of U1 and U2 under the null hypothesis
  
  set.seed(22031997)
  U0.sim <- numeric(B)
  U1.sim <- numeric(B)
  for (k in 1:B)
  {
    ranks.temp <- sample(1:n)
    R0.temp <- sum(ranks.temp[1:n0])
    R1.temp <- sum(ranks.temp[(n0+1):(n0+n1)])
    U0.temp <- R0.temp - n0*(n0+1)/2
    U1.temp <- R1.temp - n1*(n1+1)/2
    U0.sim[k] <- U0.temp
    U1.sim[k] <- U1.temp
    pb$tick()
  }
  ma = range(U0.sim)[1]
  mb = range(U0.sim)[2]
  U.star <- max(U0, U1)
  
  hist(U0.sim, xlim=c(min(ma, U.star), max(mb, U.star)))
  abline(v = c(U0, U1), col='red')
  abline(v = n0*n1/2, lwd=3)
  
  
  p.value <- sum(U0.sim >= U.star)/B
  p.value
  
  return(p.value)
  
}






