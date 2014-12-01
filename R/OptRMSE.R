#' Greedy optimization of the reduced mean square error
#' @description This algorithm optimizes the RMSE for regression models
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @param iter an integer for the number of iterations
#' @return A numeric of the weights for each model
#' @details If the optimization fails to produce an error term better than the best
#' component model, a message is returned and the best optimization after iterations
#' is returned.
#' @export
#' @examples
#' x <- matrix(runif(10), ncol=2)
#' y <- runif(5)
#' greedOptRMSE(x, y)
greedOptRMSE <- function(X, Y, iter = 100L){
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  stopper     <- min(sqrt(colSums((X - Y) ^ 2L, na.rm=TRUE)))

  while(sum.weights < iter) {

    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- sqrt(colSums((pred - Y) ^ 2L, na.rm=TRUE))
    best          <- which.min(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
    maxtest       <- min(errors)
  }
  if(stopper < maxtest){
    testresult <- round(maxtest/stopper, 5) * 100
    wstr <- paste0("Optimized weights not better than best model. Ensembled result is ",
                   testresult, "%", " of best model RMSE. Try more iterations.")
    message(wstr)
  }
  return(weights)
}
