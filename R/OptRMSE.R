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

#' Quadratic optimization of the reduced mean square error
#' @description This algorithm optimizes the RMSE for regression models to avoid convex combinations
#' @param X the matrix of predictors
#' @param Y the dependent variable
#' @author Sam Lendle
#' @return A numeric of the weights for each model
#' @export
qpOptRMSE <- function(x, y) {
  weights <- rep(0, ncol(x))
  lin_indep_cols <- unique(cummax(qr(x)$pivot))

  if (length(lin_indep_cols) < ncol(x)) {
    message("Some model predictions are identical or lin. dep. to other models and are assigned zero weights")
    #or warning
  }
  x <- x[, lin_indep_cols]

  D <- crossprod(x)
  d <- crossprod(x, y)
  A <- cbind(rep(1, ncol(x)), diag(ncol(x)))
  bvec <- c(1, rep(0, ncol(x)))
  weights[lin_indep_cols] <- quadprog::solve.QP(Dmat=D, dvec=d, Amat=A, bvec=bvec, meq=1)$solution
  return(round(weights*100))
}
