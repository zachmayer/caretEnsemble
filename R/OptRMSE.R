#' Greedy optimization of the reduced mean square error
#' @description This algorithm optimizes the RMSE for regression models
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @param iter an integer for the number of iterations
#' @return A numeric of the weights for each model
#' @export
greedOptRMSE <- function(X, Y, iter = 100L){
  
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  
  while(sum.weights < iter) {
    
    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- sqrt(colSums((pred - Y) ^ 2L))
    best          <- which.min(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  return(weights)
}
